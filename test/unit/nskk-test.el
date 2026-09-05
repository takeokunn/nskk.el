;;; nskk-test.el --- Tests for nskk.el (main entry point) -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Tests for nskk.el (main entry point).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nskk)
(require 'nskk-test-framework)
(require 'nskk-test-macros)

(nskk-describe "nskk-mode definition"
  ;; The fboundp check is subsumed by the behavioral tests below, but
  ;; interactivity is not: adding `:interactive nil' to the `define-minor-mode'
  ;; leaves every other test in this file green, so `M-x nskk-mode' can break
  ;; silently.  Drive it through `call-interactively' to pin that down.
  (nskk-it "is callable as an interactive command"
    (with-temp-buffer
      (should (commandp 'nskk-mode))
      (call-interactively 'nskk-mode)
      (should nskk-mode)))

  (nskk-it "registers the dynamic modeline lighter without an extra quote"
    (let ((entry (assq (quote nskk-mode) minor-mode-alist))
          (expected (quote (:eval (nskk-modeline-indicator)))))
      (should entry)
      (should (equal (cadr entry) expected))))

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
  ;; Same reasoning as `nskk-mode' above: the drives-nskk-mode test below does
  ;; not notice `:interactive nil' on the `define-globalized-minor-mode',
  ;; so interactivity gets its own case.
  (nskk-it "is callable as an interactive command"
    (unwind-protect
        (progn
          (should (commandp 'nskk-global-mode))
          (call-interactively 'nskk-global-mode)
          (should nskk-global-mode))
      (nskk-global-mode -1)))

  (nskk-it "turns nskk-mode on in an existing non-minibuffer buffer and off again"
    (let ((buffer (generate-new-buffer "*nskk-global-mode-test*")))
      (unwind-protect
          (progn
            (with-current-buffer buffer (should-not nskk-mode))
            (nskk-global-mode 1)
            (with-current-buffer buffer (should nskk-mode))
            (nskk-global-mode -1)
            (with-current-buffer buffer (should-not nskk-mode)))
        (nskk-global-mode -1)
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(nskk-describe "nskk-mode-map keymap"
  ;; keymapp existence check removed.  The `lookup-key' calls in this block do
  ;; NOT cover it: `(lookup-key nil (kbd "C-x C-j"))' returns 1 — a truthy
  ;; partial-match code — rather than signalling, so they pass against a nil
  ;; keymap.  What covers it is `main-keymap-bindings' below, whose `eq' against
  ;; the expected command fails on every row once the map stops being one.
  (nskk-it "has C-x C-j binding"
    (should (lookup-key nskk-mode-map (kbd "C-x C-j"))))

  (nskk-it "has C-j binding"
    (should (lookup-key nskk-mode-map (kbd "C-j")))))

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

(nskk-property-test-exhaustive main-keymap-critical-keys-survive-mode-enable
  '("C-j" "L" "C-g")
  (with-temp-buffer
    (nskk-mode 1)
    (let ((binding (lookup-key nskk-mode-map (kbd item))))
      (nskk-mode -1)
      (and binding (symbolp binding) (fboundp binding)))))

(nskk-deftest-table main-keymap-bindings
  :columns (key expected-command)
  :rows (("<remap> <self-insert-command>" nskk-self-insert)
         ("C-x C-j" nskk-toggle-mode)
         ("C-j"     nskk-kakutei)
         ("q"       nskk-handle-q)
         ("l"       nskk-handle-l)
         ("SPC"     nskk-handle-space)
         ("RET"     nskk-handle-return)
         ("L"       nskk-handle-upper-l)
         ("/"       nskk-handle-slash)
         ("x"       nskk-handle-x)
         ("C-n"     nskk-handle-ctrl-n)
         ("C-p"     nskk-handle-ctrl-p)
         ("C-f"     nskk-handle-ctrl-f)
         ("<right>" nskk-handle-ctrl-f)
         ("C-b"     nskk-handle-ctrl-b)
         ("<left>"  nskk-handle-ctrl-b)
         ("<down>"  nskk-handle-ctrl-n)
         ("<up>"    nskk-handle-ctrl-p)
         ("C-a"     nskk-handle-ctrl-a)
         ("<home>"  nskk-handle-ctrl-a)
         ("C-e"     nskk-handle-ctrl-e)
         ("<end>"   nskk-handle-ctrl-e)
         ("C-g"     nskk-handle-cancel)
         ("DEL"     nskk-handle-backspace)
         (";"       nskk-handle-semicolon-key)
         ("TAB"     nskk-handle-tab)
         ("#"       nskk-handle-hash)
         ("C-/"     nskk-undo-kakutei)
         ("X"       nskk-handle-upper-x))
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
  (nskk-it "does not enable nskk-mode in a minibuffer"
    (with-current-buffer (window-buffer (minibuffer-window))
      (should (minibufferp))
      (let ((nskk-mode nil))
        (nskk--turn-on-mode)
        (should-not nskk-mode))))

  (nskk-it "enables nskk-mode in a normal (non-minibuffer) buffer"
    (with-temp-buffer
      (should-not (minibufferp))
      (unwind-protect
          (progn
            (nskk--turn-on-mode)
            (should nskk-mode))
        (when nskk-mode (nskk-mode -1))))))

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
      (nskk-state-set-mode nskk-current-state 'katakana-半角)
      (nskk-given (nskk-should-mode 'katakana-半角))
      (nskk-when  (nskk-kakutei))
      (nskk-then  (nskk-should-mode 'hiragana))
      (nskk-should-buffer "")))

  (nskk-it "clears pending romaji buffer in hiragana and stays in hiragana"
    (nskk-with-test-buffer 'hiragana
      (nskk-given (nskk-state-set-romaji-buffer "k"))
      (nskk-when  (nskk-kakutei))
      (nskk-then
       (should (string= (nskk-state-romaji-buffer) ""))
       (nskk-should-mode 'hiragana))))

  (nskk-it "clears pending romaji buffer in katakana and stays in katakana"
    (nskk-with-test-buffer 'katakana
      (nskk-given (nskk-state-set-romaji-buffer "k"))
      (nskk-when  (nskk-kakutei))
      (nskk-then
       (should (string= (nskk-state-romaji-buffer) ""))
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
                        (nskk-has-preedit (lambda () t))
                        (nskk-get-conversion-start (lambda () 1))
                        (nskk-henkan-kakutei (lambda () (insert "変換"))))
        (nskk-kakutei)
        (nskk-should-buffer "変換"))))

  (nskk-it "does nothing when kakutei-action matches no dispatch arm"
    (nskk-with-test-buffer 'hiragana
      (let ((original (symbol-function 'nskk-prolog-query-value)))
        (cl-letf (((symbol-function 'nskk-prolog-query-value)
                   (lambda (goal var)
                     (if (eq (car goal) 'kakutei-action)
                         'no-such-kakutei-action
                       (funcall original goal var)))))
          (nskk-should-not-error (nskk-kakutei))))
      (nskk-should-buffer "")
      (nskk-should-mode 'hiragana))))

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
                              (buffer-local-value 'post-command-hook (current-buffer)))))))

  (nskk-it "collects commands reachable through both sub-keymaps and the top level"
    (with-temp-buffer
      (nskk--setup-buffer)
      ;; nskk-toggle-mode is only reachable via the "C-x" sub-keymap of
      ;; nskk-mode-map; nskk-kakutei is bound directly at the top level.
      (should (memq 'nskk-toggle-mode nskk--bound-commands))
      (should (memq 'nskk-kakutei nskk--bound-commands))))

  (nskk-it "records the pre-command point via nskk--pre-command-handler"
    (with-temp-buffer
      (insert "abcdef")
      (goto-char 3)
      (let (nskk--point-before-command)
        (nskk--pre-command-handler)
        (should (= nskk--point-before-command 3))))))

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
          (nskk--enable)
          (should (nskk-state-p nskk-current-state))))))

  (nskk-it "nskk--disable sets nskk-current-state to nil"
    (with-temp-buffer
      (nskk-mode 1)
      (nskk--disable)
      (should (null nskk-current-state))))

  (nskk-it "retains candidate resources until the last buffer disables"
    (let ((nskk--active-buffers nil)
          (nskk--candidate-show-hook-owned nil)
          (nskk--candidate-hide-hook-owned nil)
          (nskk--candidate-select-function-owned nil)
          (nskk--saved-candidate-select-function nil)
          (nskk-henkan-show-candidates-functions nil)
          (nskk-henkan-hide-candidates-functions nil)
          (nskk-henkan-select-candidate-by-key-function nil)
          (a (generate-new-buffer " *nskk-a*"))
          (b (generate-new-buffer " *nskk-b*")))
      (unwind-protect
          (progn
            (with-current-buffer a (nskk-mode 1))
            (with-current-buffer b (nskk-mode 1))
            (with-current-buffer a (nskk-mode -1))
            (should (memq #'nskk-candidate-show-list
                          nskk-henkan-show-candidates-functions))
            (should (eq nskk-henkan-select-candidate-by-key-function
                        #'nskk-candidate-list-select-by-key))
            (with-current-buffer b (nskk-mode -1))
            (should-not nskk-henkan-show-candidates-functions)
            (should-not nskk-henkan-hide-candidates-functions)
            (should-not nskk-henkan-select-candidate-by-key-function))
        (when (buffer-live-p a) (kill-buffer a))
        (when (buffer-live-p b) (kill-buffer b)))))

  (nskk-it "retains candidate resources when one active buffer is killed"
    (let ((nskk--active-buffers nil)
          (nskk--candidate-show-hook-owned nil)
          (nskk--candidate-hide-hook-owned nil)
          (nskk--candidate-select-function-owned nil)
          (nskk--saved-candidate-select-function nil)
          (nskk-henkan-show-candidates-functions nil)
          (nskk-henkan-hide-candidates-functions nil)
          (nskk-henkan-select-candidate-by-key-function nil)
          (a (generate-new-buffer " *nskk-kill-a*"))
          (b (generate-new-buffer " *nskk-kill-b*")))
      (unwind-protect
          (progn
            (with-current-buffer a (nskk-mode 1))
            (with-current-buffer b (nskk-mode 1))
            (kill-buffer b)
            (should (memq #'nskk-candidate-show-list
                          nskk-henkan-show-candidates-functions))
            (kill-buffer a)
            (should-not nskk-henkan-show-candidates-functions)
            (should-not nskk-henkan-select-candidate-by-key-function))
        (when (buffer-live-p a) (kill-buffer a))
        (when (buffer-live-p b) (kill-buffer b)))))

  (nskk-it "keeps repeated operations idempotent and restores external globals"
  (let* ((external-show (lambda (&rest _)))
         (external-hide (lambda (&rest _)))
         (external-select (lambda (&rest _) 'external))
         (nskk--active-buffers nil)
         (nskk--candidate-show-hook-owned nil)
         (nskk--candidate-hide-hook-owned nil)
         (nskk--candidate-select-function-owned nil)
         (nskk--saved-candidate-select-function nil)
         (nskk-henkan-show-candidates-functions
          (list #'nskk-candidate-show-list external-show))
         (nskk-henkan-hide-candidates-functions
          (list #'nskk-candidate-hide-list external-hide))
         (nskk-henkan-select-candidate-by-key-function external-select))
    (with-temp-buffer
      (nskk-mode 1)
      (nskk-mode 1)
      (should (= 1 (cl-count (current-buffer) nskk--active-buffers)))
      (nskk-mode -1)
      (nskk-mode -1))
    (should (equal nskk-henkan-show-candidates-functions
                   (list #'nskk-candidate-show-list external-show)))
    (should (equal nskk-henkan-hide-candidates-functions
                   (list #'nskk-candidate-hide-list external-hide)))
    (should (eq nskk-henkan-select-candidate-by-key-function
                external-select))))

  (nskk-it "rolls back local and global resources when setup fails"
    (let* ((external-select (lambda (&rest _) 'external))
           (nskk--active-buffers nil)
           (nskk--candidate-show-hook-owned nil)
           (nskk--candidate-hide-hook-owned nil)
           (nskk--candidate-select-function-owned nil)
           (nskk--saved-candidate-select-function nil)
           (nskk-henkan-show-candidates-functions '(external-show))
           (nskk-henkan-hide-candidates-functions '(external-hide))
           (nskk-henkan-select-candidate-by-key-function external-select)
           (kill-emacs-hook kill-emacs-hook))
      (with-temp-buffer
        (setq-local pre-command-hook '(external-pre)
                    post-command-hook '(external-post)
                    completion-at-point-functions '(external-capf)
                    kill-buffer-hook '(external-kill))
        (cl-letf (((symbol-function 'nskk--setup-buffer)
                   (lambda () (error "injected setup failure"))))
          (should-error (nskk-mode 1) :type 'error))
        (should-not nskk-mode)
        (should-not nskk-current-state)
        (should (equal pre-command-hook '(external-pre)))
        (should (equal post-command-hook '(external-post)))
        (should (equal completion-at-point-functions '(external-capf)))
        (should (equal kill-buffer-hook '(external-kill)))
        (should-not (memq (current-buffer) nskk--active-buffers)))
      (should (equal nskk-henkan-show-candidates-functions '(external-show)))
      (should (equal nskk-henkan-hide-candidates-functions '(external-hide)))
      (should (eq nskk-henkan-select-candidate-by-key-function
                  external-select)))))

(nskk-describe "nskk--activation-allowed-p"
  (nskk-it "allows activation when no buffer owns the lock"
    (let ((nskk-activation-lock-owner nil))
      (with-temp-buffer
        (should (nskk--activation-allowed-p)))))

  (nskk-it "allows activation when the current buffer owns the lock"
    (with-temp-buffer
      (let ((nskk-activation-lock-owner (current-buffer)))
        (should (nskk--activation-allowed-p)))))

  (nskk-it "refuses activation when a different live buffer owns the lock"
    (let ((other (generate-new-buffer " *nskk-lock-other*")))
      (unwind-protect
          (let ((nskk-activation-lock-owner other))
            (with-temp-buffer
              (should-not (nskk--activation-allowed-p))))
        (kill-buffer other))))

  (nskk-it "allows activation and clears the lock when the owner is dead"
    (let* ((dead (generate-new-buffer " *nskk-lock-dead*"))
           (nskk-activation-lock-owner dead))
      (kill-buffer dead)
      (with-temp-buffer
        (should (nskk--activation-allowed-p))
        (should-not nskk-activation-lock-owner)))))

(nskk-describe "nskk--save-learning-data non-inhibited path"
  (nskk-it "saves learning data and study data when study is loaded"
    (let ((nskk--persistence-inhibited nil)
          (calls nil))
      (cl-letf (((symbol-function 'nskk-search-save-learning-data)
                 (lambda () (push 'search calls)))
                ((symbol-function 'nskk-study-save)
                 (lambda () (push 'study calls)))
                ((symbol-function 'featurep)
                 (lambda (feature) (eq feature 'nskk-study))))
        (nskk--save-learning-data))
      (should (equal calls '(study search)))))

  (nskk-it "saves learning data but skips study save when study is not loaded"
    (let ((nskk--persistence-inhibited nil)
          (calls nil))
      (cl-letf (((symbol-function 'nskk-search-save-learning-data)
                 (lambda () (push 'search calls)))
                ((symbol-function 'nskk-study-save)
                 (lambda () (push 'study calls)))
                ((symbol-function 'featurep)
                 (lambda (_feature) nil)))
        (nskk--save-learning-data))
      (should (equal calls '(search))))))

(nskk-describe "nskk--handle-buffer-kill real effect"
  (nskk-it "releases NSKK state when the owning buffer is actually killed"
    (let ((nskk--active-buffers nil)
          (nskk--candidate-show-hook-owned nil)
          (nskk--candidate-hide-hook-owned nil)
          (nskk--candidate-select-function-owned nil)
          (nskk--saved-candidate-select-function nil)
          (buffer (generate-new-buffer " *nskk-kill-real*")))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (nskk-mode 1)
              (should nskk-mode)
              (should (nskk-state-p nskk-current-state)))
            (kill-buffer buffer)
            (should-not (buffer-live-p buffer))
            (should-not (memq buffer nskk--active-buffers)))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

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
      (nskk-mode -1)))

  (nskk-it "commits when point moves outside the overlay (right of overlay-end)"
    (with-temp-buffer
      (insert "▼かんじ!!")   ; positions: 1=▼, 2=か, 3=ん, 4=じ, 5=!, 6=!, 7=(eob)
      (let ((nskk-mode t)
            (nskk-current-state (nskk-state-create 'hiragana))
            (commit-called nil))
        (nskk-state-set-conversion-overlay (make-overlay 1 5))
        (nskk-state-force-henkan-phase nskk-current-state 'active)
        (nskk-set-conversion-start-marker 1)
        (goto-char 6)  ; point > overlay-end(5)
        (nskk-with-mocks ((nskk-commit-current (lambda () (setq commit-called t)))
                          (nskk-modeline-update (lambda () nil)))
          (nskk--post-command-handler))
        (delete-overlay (nskk-state-conversion-overlay))
        (should commit-called))))

  (nskk-it "commits when point moves inside the overlay (between conv-start and overlay-end)"
    (with-temp-buffer
      (insert "▼かんじ")   ; overlay will span [1, 5]
      (let ((nskk-mode t)
            (nskk-current-state (nskk-state-create 'hiragana))
            (commit-called nil))
        (nskk-state-set-conversion-overlay (make-overlay 1 5))
        (nskk-state-force-henkan-phase nskk-current-state 'active)
        (nskk-set-conversion-start-marker 1)
        (goto-char 3)  ; inside overlay [1,5], not at overlay-end
        (nskk-with-mocks ((nskk-commit-current (lambda () (setq commit-called t)))
                          (nskk-modeline-update (lambda () nil)))
          (nskk--post-command-handler))
        (delete-overlay (nskk-state-conversion-overlay))
        (should commit-called))))

  (nskk-it "does not commit when point is exactly at overlay-end"
    (with-temp-buffer
      (insert "▼かんじ")   ; overlay will span [1, 5]
      (let ((nskk-mode t)
            (nskk-current-state (nskk-state-create 'hiragana))
            (commit-called nil))
        (nskk-state-set-conversion-overlay (make-overlay 1 5))
        (nskk-state-force-henkan-phase nskk-current-state 'active)
        (nskk-set-conversion-start-marker 1)
        (goto-char 5)  ; exactly at overlay-end
        (nskk-with-mocks ((nskk-commit-current (lambda () (setq commit-called t)))
                          (nskk-modeline-update (lambda () nil)))
          (nskk--post-command-handler))
        (delete-overlay (nskk-state-conversion-overlay))
        (should-not commit-called)))))

(nskk-describe "nskk--post-command-preedit-guard behavior"
  (nskk-it "fires when point moved and this-command is not in nskk--bound-commands"
    (with-temp-buffer
      (insert "▽かんじ")   ; conv-start=1
      (let* ((nskk-current-state (nskk-state-create 'hiragana))
             (nskk--bound-commands '(nskk-handle-space))
             (this-command 'some-unbound-movement-command)
             (kakutei-called nil))
        (nskk-state-force-henkan-phase nskk-current-state 'on)
        (nskk-set-conversion-start-marker 1)
        (setq nskk--point-before-command 1)
        (goto-char 4)  ; point moved away from point-before-command
        (nskk-with-mocks ((nskk-henkan-kakutei
                           (lambda () (setq kakutei-called t))))
          (nskk--post-command-preedit-guard))
        (should kakutei-called))))

  (nskk-it "does not fire when this-command is in nskk--bound-commands"
    (with-temp-buffer
      (insert "▽かんじ")
      (let* ((nskk-current-state (nskk-state-create 'hiragana))
             (nskk--bound-commands '(nskk-handle-space))
             (this-command 'nskk-handle-space)
             (kakutei-called nil))
        (nskk-state-force-henkan-phase nskk-current-state 'on)
        (nskk-set-conversion-start-marker 1)
        (setq nskk--point-before-command 1)
        (goto-char 4)
        (nskk-with-mocks ((nskk-henkan-kakutei
                           (lambda () (setq kakutei-called t))))
          (nskk--post-command-preedit-guard))
        (should-not kakutei-called))))

  (nskk-it "does not fire when point did not move"
    (with-temp-buffer
      (insert "▽かんじ")
      (let* ((nskk-current-state (nskk-state-create 'hiragana))
             (nskk--bound-commands '(nskk-handle-space))
             (this-command 'some-unbound-movement-command)
             (kakutei-called nil))
        (nskk-state-force-henkan-phase nskk-current-state 'on)
        (nskk-set-conversion-start-marker 1)
        (goto-char 4)
        (setq nskk--point-before-command 4)  ; matches current point
        (nskk-with-mocks ((nskk-henkan-kakutei
                           (lambda () (setq kakutei-called t))))
          (nskk--post-command-preedit-guard))
        (should-not kakutei-called)))))

(nskk-describe "nskk--enable cursor rollback"
  (nskk-it "restores mode, local state, resources, and per-frame cursor after a late failure"
    (let* ((external-select (lambda (&rest _) (quote external)))
           (nskk--active-buffers nil)
           (nskk--candidate-show-hook-owned nil)
           (nskk--candidate-hide-hook-owned nil)
           (nskk--candidate-select-function-owned nil)
           (nskk--saved-candidate-select-function nil)
           (nskk-use-color-cursor t)
           (nskk-henkan-show-candidates-functions (quote (external-show)))
           (nskk-henkan-hide-candidates-functions (quote (external-hide)))
           (nskk-henkan-select-candidate-by-key-function external-select)
           (frame (quote frame))
           (parameters (make-hash-table :test (quote equal)))
           (missing (make-symbol "missing"))
           (cursor-color "before")
           (kill-emacs-hook kill-emacs-hook))
      (puthash (cons frame (quote cursor-color)) "before" parameters)
      (with-temp-buffer
        (setq-local pre-command-hook (quote (external-pre))
                    post-command-hook (quote (external-post))
                    completion-at-point-functions (quote (external-capf))
                    kill-buffer-hook (quote (external-kill)))
        (cl-letf (((symbol-function (quote selected-frame))
                   (lambda () frame))
                  ((symbol-function (quote frame-parameter))
                   (lambda (target parameter)
                     (gethash (cons target parameter) parameters)))
                  ((symbol-function (quote set-frame-parameter))
                   (lambda (target parameter value)
  (if value
      (puthash (cons target parameter) value parameters)
    (remhash (cons target parameter) parameters))
  (when (eq parameter (quote cursor-color))
    (setq cursor-color value))))
                  ((symbol-function (quote set-cursor-color))
                   (lambda (color) (setq cursor-color color)))
                  ((symbol-function (quote nskk--other-nskk-buffers-active-p))
                   (lambda (&optional _target-frame) nil))
                  ((symbol-function (quote nskk-modeline-update))
                   (lambda ()
                     (set-cursor-color "nskk")
                     (set-frame-parameter
                      frame nskk--last-cursor-color-parameter "nskk")
                     (error "injected modeline failure"))))
          (should-error (nskk-mode 1) :type (quote error)))
        (should-not nskk-mode)
        (should-not nskk-current-state)
        (should (equal pre-command-hook (quote (external-pre))))
        (should (equal post-command-hook (quote (external-post))))
        (should (equal completion-at-point-functions (quote (external-capf))))
        (should (equal kill-buffer-hook (quote (external-kill))))
        (should-not (memq (current-buffer) nskk--active-buffers)))
      (should (equal nskk-henkan-show-candidates-functions
                     (quote (external-show))))
      (should (equal nskk-henkan-hide-candidates-functions
                     (quote (external-hide))))
      (should (eq nskk-henkan-select-candidate-by-key-function
                  external-select))
      (should (equal cursor-color "before"))
      (should
       (eq missing
           (gethash
            (cons frame nskk--saved-cursor-color-parameter)
            parameters missing)))
      (should
       (eq missing
           (gethash
            (cons frame nskk--last-cursor-color-parameter)
            parameters missing))))))
(define-error (quote nskk-test-teardown-error)
              "Injected NSKK teardown failure")

(ert-deftest nskk-test/teardown-restores-all-changed-frame-cursors ()
  "One buffer teardown restores every frame whose cursor it changed."
  (let ((frame-a (quote frame-a))
        (frame-b (quote frame-b))
        (current-frame (quote frame-a))
        (parameters (make-hash-table :test (function equal))))
    (puthash (cons frame-a (quote cursor-color)) "red" parameters)
    (puthash (cons frame-b (quote cursor-color)) "blue" parameters)
    (with-temp-buffer
      (setq-local nskk-mode t)
      (setq-local nskk-current-state (nskk-state-create 'hiragana))
      (cl-letf (((symbol-function (quote selected-frame))
                 (lambda () current-frame))
                ((symbol-function (quote frame-list))
                 (lambda () (list frame-a frame-b)))
                ((symbol-function (quote frame-parameter))
                 (lambda (frame parameter)
                   (gethash (cons frame parameter) parameters)))
                ((symbol-function (quote set-frame-parameter))
                 (lambda (frame parameter value)
                   (if value
                       (puthash (cons frame parameter) value parameters)
                     (remhash (cons frame parameter) parameters))))
                ((symbol-function (quote set-cursor-color))
                 (lambda (color)
                   (puthash
                    (cons current-frame (quote cursor-color))
                    color
                    parameters)))
                ((symbol-function (quote nskk-state-mode))
                 (lambda (_) (quote hiragana)))
                ((symbol-function (quote nskk--cursor-with-color))
                 (lambda (_) "gold"))
                ((symbol-function (quote nskk--other-nskk-buffers-active-p))
                 (lambda (&optional _) nil))
                ((symbol-function (quote nskk-clear-conversion-context))
                 (function ignore))
                ((symbol-function (quote nskk--cleanup-buffer))
                 (function ignore))
                ((symbol-function (quote nskk-show-mode-hide))
                 (function ignore))
                ((symbol-function (quote nskk--release-candidate-resources))
                 (function ignore)))
        (let ((nskk-use-color-cursor t))
          (nskk-cursor-update)
          (setq current-frame frame-b)
          (nskk-cursor-update)
          (nskk--teardown nil))))
    (should
     (equal (gethash (cons frame-a (quote cursor-color)) parameters) "red"))
    (should
     (equal (gethash (cons frame-b (quote cursor-color)) parameters) "blue"))
    (dolist (frame (list frame-a frame-b))
      (should-not
       (gethash (cons frame nskk--saved-cursor-color-parameter) parameters))
      (should-not
       (gethash (cons frame nskk--last-cursor-color-parameter) parameters)))))


(ert-deftest nskk-test/disable-completes-cleanup-before-resignaling ()
  "Failing off hooks do not block later hooks, cleanup, or prior conditions."
  (let ((nskk--active-buffers nil)
        (nskk--candidate-show-hook-owned nil)
        (nskk--candidate-hide-hook-owned nil)
        (nskk--candidate-select-function-owned nil)
        (nskk--saved-candidate-select-function nil)
        (nskk-mode-off-hook nil)
        (observer-calls 0))
    (add-hook 'nskk-mode-off-hook
              (lambda () (cl-incf observer-calls)))
    (with-temp-buffer
      (nskk-mode 1)
      (nskk-show-mode-set-overlay (make-overlay (point) (point)))
      (nskk-show-mode-set-timer (run-with-timer 60 nil #'ignore))
      (let ((timer (nskk-show-mode-timer)))
        (add-hook
         'nskk-mode-off-hook
         (lambda ()
           (signal 'nskk-test-teardown-error '(original-payload)))
         nil
         t)
        (should
         (equal
          (should-error (nskk-mode -1) :type 'nskk-test-teardown-error)
          '(nskk-test-teardown-error original-payload)))
        (should (= observer-calls 1))
        (should-not nskk-mode)
        (should-not nskk-current-state)
        (should-not (nskk-show-mode-overlay))
        (should-not (nskk-show-mode-timer))
        (should-not (memq timer timer-list))
        (should-not (memq #'nskk--pre-command-handler pre-command-hook))
        (should-not (memq #'nskk--post-command-handler post-command-hook))
        (should-not
         (memq #'nskk-completion-at-point completion-at-point-functions))
        (should-not (memq (current-buffer) nskk--active-buffers))
        (should-not (memq #'nskk--handle-buffer-kill kill-buffer-hook))
        (should-not
         (memq #'nskk--handle-major-mode-change change-major-mode-hook))))
    (with-temp-buffer
      (setq-local nskk-mode t)
      (add-hook
       'nskk-mode-off-hook
       (lambda () (signal 'error '(later-hook-payload)))
       nil
       t)
      (cl-letf (((symbol-function 'nskk-clear-conversion-context)
                 (lambda ()
                   (signal 'nskk-test-teardown-error '(earlier-payload)))))
        (should
         (equal
          (should-error (nskk--teardown t)
                        :type 'nskk-test-teardown-error)
          '(nskk-test-teardown-error earlier-payload))))
      (should (= observer-calls 2))
      (should-not nskk-mode)
      (should-not nskk-current-state))))

(ert-deftest nskk-test/teardown-cursor-color-restore-failure-wins-first ()
  "A cursor-color-restore failure is rethrown even when a later step also fails."
  (let ((nskk--active-buffers nil)
        (nskk--candidate-show-hook-owned nil)
        (nskk--candidate-hide-hook-owned nil)
        (nskk--candidate-select-function-owned nil)
        (nskk--saved-candidate-select-function nil)
        (cleanup-buffer-called nil))
    (with-temp-buffer
      (nskk-mode 1)
      (cl-letf (((symbol-function 'nskk-cursor-color-restore)
                 (lambda (&rest _)
                   (signal 'nskk-test-teardown-error '(cursor-payload))))
                ((symbol-function 'nskk--cleanup-buffer)
                 (lambda ()
                   (setq cleanup-buffer-called t)
                   (signal 'error '(cleanup-payload)))))
        (should
         (equal
          (should-error (nskk--teardown nil) :type 'nskk-test-teardown-error)
          '(nskk-test-teardown-error cursor-payload))))
      (should cleanup-buffer-called)
      (should-not nskk-mode)
      (should-not nskk-current-state))))

(ert-deftest nskk-test/teardown-cleanup-buffer-failure-wins-first ()
  "A nskk--cleanup-buffer failure is rethrown even when a later step also fails."
  (let ((nskk--active-buffers nil)
        (nskk--candidate-show-hook-owned nil)
        (nskk--candidate-hide-hook-owned nil)
        (nskk--candidate-select-function-owned nil)
        (nskk--saved-candidate-select-function nil)
        (release-called nil))
    (with-temp-buffer
      (nskk-mode 1)
      (cl-letf (((symbol-function 'nskk--cleanup-buffer)
                 (lambda ()
                   (signal 'nskk-test-teardown-error '(cleanup-payload))))
                ((symbol-function 'nskk--release-candidate-resources)
                 (lambda ()
                   (setq release-called t)
                   (signal 'error '(release-payload)))))
        (should
         (equal
          (should-error (nskk--teardown nil) :type 'nskk-test-teardown-error)
          '(nskk-test-teardown-error cleanup-payload))))
      (should release-called)
      (should-not nskk-mode)
      (should-not nskk-current-state))))

(ert-deftest nskk-test/rollback-step-failure-does-not-replace-activation-error ()
  "A rollback step that itself signals must not displace the activation error.
`nskk--rollback-activation' documents this: the caller is waiting for the
condition that aborted activation, not for whatever went wrong while undoing
it.  Injecting a failure into the last restore step is what distinguishes the
two, since that step runs through ATTEMPT and is therefore swallowed."
  (let ((nskk--active-buffers nil)
        (nskk--candidate-show-hook-owned nil)
        (nskk--candidate-hide-hook-owned nil)
        (nskk--candidate-select-function-owned nil)
        (nskk--saved-candidate-select-function nil)
        (restore-step-ran nil))
    (with-temp-buffer
      (cl-letf (((symbol-function 'nskk-modeline-update)
                 (lambda (&rest _)
                   (signal 'nskk-test-teardown-error '(activation-payload))))
                ((symbol-function 'nskk--restore-buffer-snapshot)
                 (lambda (&rest _)
                   (setq restore-step-ran t)
                   (signal 'error '(rollback-payload)))))
        (should
         (equal
          (should-error (nskk-mode 1) :type 'nskk-test-teardown-error)
          '(nskk-test-teardown-error activation-payload))))
      (should restore-step-ran))))

(ert-deftest nskk-test/major-mode-change-runs-full-teardown ()
  "Changing major mode must release NSKK resources."
  (let ((nskk--active-buffers nil)
        (nskk--candidate-show-hook-owned nil)
        (nskk--candidate-hide-hook-owned nil)
        (nskk--candidate-select-function-owned nil)
        (nskk--saved-candidate-select-function nil))
    (with-temp-buffer
      (let ((buffer (current-buffer)))
        (nskk-mode 1)
        (nskk-show-mode-set-overlay (make-overlay (point) (point)))
        (nskk-show-mode-set-timer (run-with-timer 60 nil #'ignore))
        (let ((timer (nskk-show-mode-timer)))
          (fundamental-mode)
          (should-not nskk-current-state)
          (should-not (nskk-show-mode-overlay))
          (should-not (nskk-show-mode-timer))
          (should-not (memq timer timer-list))
          (should-not (memq buffer nskk--active-buffers)))))))

(ert-deftest nskk-test/kill-emacs-save-respects-persistence-inhibition ()
  "The kill-emacs save hook must be inert during a transaction."
  (let ((nskk--persistence-inhibited t)
        (calls nil))
    (cl-letf (((symbol-function 'nskk-search-save-learning-data)
               (lambda () (push 'search calls)))
              ((symbol-function 'nskk-study-save)
               (lambda () (push 'study calls)))
              ((symbol-function 'featurep)
               (lambda (feature)
                 (eq feature 'nskk-study))))
      (nskk--save-learning-data))
    (should-not calls)))

(ert-deftest nskk-test/disable-completes-cleanup-after-quit ()
  "A quitting off hook does not block later hooks or mandatory teardown."
  (let ((nskk--active-buffers nil)
        (nskk--candidate-show-hook-owned nil)
        (nskk--candidate-hide-hook-owned nil)
        (nskk--candidate-select-function-owned nil)
        (nskk--saved-candidate-select-function nil)
        (nskk-mode-off-hook nil)
        (observer-calls 0))
    (add-hook 'nskk-mode-off-hook
              (lambda () (cl-incf observer-calls)))
    (with-temp-buffer
      (nskk-mode 1)
      (nskk-show-mode-set-overlay (make-overlay (point) (point)))
      (nskk-show-mode-set-timer (run-with-timer 60 nil #'ignore))
      (let ((timer (nskk-show-mode-timer))
            condition-data)
        (add-hook
         'nskk-mode-off-hook
         (lambda () (signal 'quit '(original-payload)))
         nil
         t)
        (setq condition-data
              (condition-case data
                  (progn
                    (nskk-mode -1)
                    nil)
                (quit data)))
        (should (equal condition-data '(quit original-payload)))
        (should (= observer-calls 1))
        (should-not nskk-mode)
        (should-not nskk-current-state)
        (should-not (nskk-show-mode-overlay))
        (should-not (nskk-show-mode-timer))
        (should-not (memq timer timer-list))
        (should-not (memq #'nskk--pre-command-handler pre-command-hook))
        (should-not (memq #'nskk--post-command-handler post-command-hook))
        (should-not
         (memq #'nskk-completion-at-point completion-at-point-functions))
        (should-not (memq (current-buffer) nskk--active-buffers))
        (should-not (memq #'nskk--handle-buffer-kill kill-buffer-hook))
        (should-not
         (memq #'nskk--handle-major-mode-change change-major-mode-hook))))))
(ert-deftest nskk-test/enable-quit-restores-complete-snapshot ()
  "QUIT during enable restores local and process-global resources."
  (let* ((external-select (lambda (&rest _) 'external))
         (nskk--active-buffers nil)
         (nskk--candidate-show-hook-owned nil)
         (nskk--candidate-hide-hook-owned nil)
         (nskk--candidate-select-function-owned nil)
         (nskk--saved-candidate-select-function nil)
         (nskk-henkan-show-candidates-functions '(external-show))
         (nskk-henkan-hide-candidates-functions '(external-hide))
         (nskk-henkan-select-candidate-by-key-function external-select)
         (kill-emacs-hook '(external-exit))
         condition-data)
    (with-temp-buffer
      (setq-local pre-command-hook '(external-pre)
                  post-command-hook '(external-post)
                  completion-at-point-functions '(external-capf)
                  kill-buffer-hook '(external-kill))
      (cl-letf (((symbol-function 'nskk--setup-buffer)
                 (lambda () (signal 'quit '(enable-payload)))))
        (setq condition-data
              (condition-case data
                  (progn (nskk-mode 1) nil)
                (quit data))))
      (should (equal condition-data '(quit enable-payload)))
      (should-not nskk-mode)
      (should-not nskk-current-state)
      (should (equal pre-command-hook '(external-pre)))
      (should (equal post-command-hook '(external-post)))
      (should (equal completion-at-point-functions '(external-capf)))
      (should (equal kill-buffer-hook '(external-kill)))
      (should-not nskk--active-buffers))
    (should (equal nskk-henkan-show-candidates-functions
                   '(external-show)))
    (should (equal nskk-henkan-hide-candidates-functions
                   '(external-hide)))
    (should (eq nskk-henkan-select-candidate-by-key-function
                external-select))
    (should (equal kill-emacs-hook '(external-exit)))))

(ert-deftest nskk-test/teardown-rejects-off-hook-reenable ()
  "An off hook cannot reacquire resources during teardown."
  (let ((nskk--active-buffers nil)
        (nskk--candidate-show-hook-owned nil)
        (nskk--candidate-hide-hook-owned nil)
        (nskk--candidate-select-function-owned nil)
        (nskk--saved-candidate-select-function nil))
    (with-temp-buffer
      (nskk-mode 1)
      (add-hook 'nskk-mode-off-hook (lambda () (nskk-mode 1)) nil t)
      (should-error (nskk-mode -1) :type 'user-error)
      (should-not nskk-mode)
      (should-not nskk-current-state)
      (should-not (memq (current-buffer) nskk--active-buffers))
      (should-not (memq #'nskk--handle-buffer-kill kill-buffer-hook))
      (should-not (memq #'nskk--handle-major-mode-change
                        change-major-mode-hook)))))

(ert-deftest nskk-test/enable-restores-isearch-resource-snapshot ()
  "Every enable stage restores mode locality and isearch ownership exactly."
  (let ((saved-isearch-state (nskk--isearch-transaction-state))
        (saved-watcher-state (nskk--isearch-watcher-state))
        (saved-default-mode (default-value 'nskk-mode))
        (stage-specs
         '((nskk-state-initialize-prolog (nil nil nil))
           (nskk-kana-initialize (nil nil t))
           (nskk-converter-initialize (nil t nil))
           (nskk-henkan-initialize (nil t t))
           (nskk-input-initialize (t nil nil))
           (nskk--acquire-candidate-resources (t nil t))
           (nskk-isearch-setup (t t nil))
           (nskk-maybe-load-azik-style (t t t))))
        (case-count 0))
    (unwind-protect
        (dolist (initial-mode-value '(nil t))
          (dolist (local-binding '(nil t))
            (dolist (condition '(error quit))
              (progn (dolist (cleanup-timing '(before after)) (cl-loop
                                                               for (stage resource-state) in stage-specs
                                                               for stage-index from 0
                                                               do
                                                               (cl-incf case-count)
                                                               (let* ((default-mode-value
                                                                       (if local-binding
                                                                           (not initial-mode-value)
                                                                         initial-mode-value))
                                                                      (ownership-state
                                                                       (mapcar (lambda (present)
                                                                                 (and present initial-mode-value))
                                                                               resource-state))
                                                                      (watcher-present (zerop (% stage-index 2)))
                                                                      (watcher-state
                                                                       (list watcher-present
                                                                             (and watcher-present local-binding)))
                                                                      (payload
                                                                       (list 'enable-payload stage-index
                                                                             initial-mode-value local-binding
                                                                             cleanup-timing))
                                                                      (primary-data (list payload))
                                                                      (cleanup-condition
                                                                       (if (eq condition 'error) 'quit 'error))
                                                                      (cleanup-payload
                                                                       (list 'cleanup-payload stage-index
                                                                             cleanup-timing))
                                                                      (original-stage (symbol-function stage))
                                                                      (original-restore
                                                                       (symbol-function
                                                                        'nskk--isearch-restore-transaction-state))
                                                                      (nskk-isearch-enable t)
                                                                      (nskk--active-buffers nil)
                                                                      (nskk--candidate-show-hook-owned nil)
                                                                      (nskk--candidate-hide-hook-owned nil)
                                                                      (nskk--candidate-select-function-owned nil)
                                                                      (nskk--saved-candidate-select-function nil)
                                                                      (nskk-henkan-show-candidates-functions nil)
                                                                      (nskk-henkan-hide-candidates-functions nil)
                                                                      (nskk-henkan-select-candidate-by-key-function nil)
                                                                      (kill-emacs-hook nil)
                                                                      (nskk--learning-loaded nil)
                                                                      (nskk-search-auto-save-learning nil)
                                                                      stage-fired cleanup-fired caught)
                                                                 (set-default 'nskk-mode default-mode-value)
                                                                 (nskk--isearch-restore-transaction-state
                                                                  (list resource-state ownership-state))
                                                                 (nskk--isearch-restore-watcher-state watcher-state)
                                                                 (with-temp-buffer
                                                                   (if local-binding
                                                                       (setq-local nskk-mode initial-mode-value)
                                                                     (kill-local-variable 'nskk-mode))
                                                                   (setq-local
                                                                    local-minor-modes
                                                                    (if initial-mode-value
                                                                        (cons 'nskk-mode
                                                                              (delq 'nskk-mode local-minor-modes))
                                                                      (delq 'nskk-mode local-minor-modes)))
                                                                   (let ((initial-minor-membership
                                                                          (and (memq 'nskk-mode local-minor-modes) t)))
                                                                     (unwind-protect
                                                                         (progn
                                                                           (setq
                                                                            caught
                                                                            (cl-letf
                                                                                (((symbol-function stage)
                                                                                  (lambda (&rest arguments)
                                                                                    (apply original-stage arguments)
                                                                                    (setq stage-fired t)
                                                                                    (signal condition primary-data)))
                                                                                 ((symbol-function
                                                                                   'nskk--isearch-restore-transaction-state)
                                                                                  (lambda (state)
                                                                                    (if cleanup-fired
                                                                                        (funcall original-restore state)
                                                                                      (setq cleanup-fired t)
                                                                                      (if (eq cleanup-timing 'before)
                                                                                          (signal cleanup-condition
                                                                                                  cleanup-payload)
                                                                                        (funcall original-restore state)
                                                                                        (signal cleanup-condition
                                                                                                cleanup-payload))))))
                                                                              (condition-case data
                                                                                  (progn
                                                                                    (nskk-mode 1)
                                                                                    nil)
                                                                                ((error quit) data))))
                                                                           (should stage-fired)
                                                                           (should cleanup-fired)
                                                                           (should (eq (car caught) condition))
                                                                           (should (eq (cdr caught) primary-data))
                                                                           (should (eq (car (cdr caught)) payload))
                                                                           (should
                                                                            (eq (local-variable-p 'nskk-mode)
                                                                                local-binding))
                                                                           (should (eq nskk-mode initial-mode-value))
                                                                           (should
                                                                            (eq (and (memq 'nskk-mode local-minor-modes) t)
                                                                                initial-minor-membership))
                                                                           (should
                                                                            (equal (nskk-isearch-resource-state)
                                                                                   resource-state))
                                                                           (should
                                                                            (equal (nskk--isearch-ownership-state)
                                                                                   ownership-state))
                                                                           (should
                                                                            (equal (nskk--isearch-watcher-state)
                                                                                   watcher-state))
                                                                           (should-not nskk--active-buffers)
                                                                           (nskk-mode 1)
                                                                           (should nskk-mode)
                                                                           (should
                                                                            (equal (nskk-isearch-resource-state)
                                                                                   '(t t t)))
                                                                           (should
                                                                            (equal
                                                                             (nskk--isearch-ownership-state)
                                                                             (cl-mapcar
                                                                              (lambda (present owned)
                                                                                (or owned (not present)))
                                                                              resource-state ownership-state)))
                                                                           (should
                                                                            (equal (nskk--isearch-watcher-state)
                                                                                   watcher-state)))
                                                                       (when nskk-mode
                                                                         (nskk-mode -1))))))))))))
      (set-default 'nskk-mode saved-default-mode)
      (nskk--isearch-restore-transaction-state saved-isearch-state)
      (nskk--isearch-restore-watcher-state saved-watcher-state))
    (should (= case-count 128))))

(provide 'nskk-test)

;;; nskk-test.el ends here
