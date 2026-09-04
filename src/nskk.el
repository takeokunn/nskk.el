;;; nskk.el --- NSKK main entry point -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

;; Author: takeokunn <bararararatty@gmail.com>
;; Maintainer: takeokunn <bararararatty@gmail.com>
;; URL: https://github.com/takeokunn/nskk.el
;; Version: 0.3.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: i18n convenience

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; NSKK main entry point.

;;; Code:

;; L0: Foundation
(require 'nskk-custom)
(require 'nskk-prolog)

;; L2: Domain
(require 'nskk-state)

;; L3: Application
(require 'nskk-henkan)

;; L5: Presentation
(require 'nskk-input)
(require 'nskk-keymap)
(require 'nskk-candidate-window)
(require 'nskk-modeline)

;; Optional (L0, L2)
(require 'nskk-debug nil t)
(require 'nskk-server nil t)
(require 'nskk-program-dictionary nil t)
;; Optional (L5) - inline mode indicator on mode switch
(require 'nskk-show-mode nil t)
;; Optional (L5) - annotation display for dictionary candidates
(require 'nskk-annotation nil t)
;; Optional (L5) - inline candidate display
(require 'nskk-inline nil t)
;; Optional (L5) - region operation commands
(require 'nskk-region nil t)
;; Optional (L5) - context-aware auto mode switching
(require 'nskk-context nil t)
;; Optional (L5) - isearch integration
(require 'nskk-isearch nil t)

(declare-function nskk-set-mode-hiragana "nskk-input")
(declare-function nskk-henkan-kakutei "nskk-henkan")
(declare-function nskk-clear-pending-romaji "nskk-henkan" ())
(declare-function nskk-current-kakutei-state "nskk-keymap")
(declare-function nskk-maybe-load-azik-style "nskk-input")
(declare-function nskk--dict-maybe-save "nskk-dictionary")
(declare-function nskk-search-load-learning-data "nskk-search")
(declare-function nskk-search-save-learning-data "nskk-search")
(declare-function nskk-study-load "nskk-study")
(declare-function nskk-study-save "nskk-study")
(declare-function nskk-handle-ctrl-a "nskk-keymap")
(declare-function nskk-handle-ctrl-e "nskk-keymap")
(declare-function nskk-handle-tab "nskk-keymap")
(declare-function nskk-handle-hash "nskk-keymap")
(declare-function nskk-handle-semicolon-key "nskk-input")
(declare-function nskk-converting-p "nskk-henkan")
(declare-function nskk-get-conversion-start "nskk-henkan")
(declare-function nskk-commit-current "nskk-henkan")
(declare-function nskk-cancel-conversion-to-reading "nskk-henkan")
(declare-function nskk-cancel-preedit "nskk-henkan")
(declare-function nskk-clear-conversion-context "nskk-henkan")
(declare-function nskk-invalidate-undo-kakutei "nskk-henkan")
(declare-function nskk-undo-kakutei "nskk-henkan")
(declare-function nskk-purge-from-jisyo "nskk-henkan")
(declare-function nskk-completion-at-point "nskk-henkan")
(declare-function nskk-commit-by-phase "nskk-keymap")

(defvar nskk-mode-off-hook nil
  "Hook run when NSKK mode is disabled.")

(defvar-local nskk-mode nil
  "Non-nil when NSKK mode is enabled in the current buffer.")

(defvar-local nskk--mode-set-snapshot nil
  "State captured immediately before the minor-mode setter runs.")

(defun nskk--set-mode-state (value)
  "Set `nskk-mode' to VALUE after capturing its exact prior state."
  (setq nskk--mode-set-snapshot
        (list (local-variable-p 'nskk-mode)
              nskk-mode
              (and (boundp 'local-minor-modes)
                   (memq 'nskk-mode local-minor-modes))))
  (setq-local nskk-mode value))

(defun nskk--restore-mode-set-snapshot ()
  "Restore the state captured by `nskk--set-mode-state'."
  (let ((snapshot nskk--mode-set-snapshot))
    (if (nth 0 snapshot)
        (setq-local nskk-mode (nth 1 snapshot))
      (kill-local-variable 'nskk-mode))
    (when (boundp 'local-minor-modes)
      (if (nth 2 snapshot)
          (add-to-list 'local-minor-modes 'nskk-mode)
        (setq local-minor-modes
              (delq 'nskk-mode local-minor-modes))))))

(defvar-keymap nskk-mode-map
  :doc "Keymap for NSKK minor mode."
  "<remap> <self-insert-command>" #'nskk-self-insert
  "C-x C-j" #'nskk-toggle-mode
  "C-j"     #'nskk-kakutei
  "q"       #'nskk-handle-q
  "l"       #'nskk-handle-l
  "SPC"     #'nskk-handle-space
  "RET"     #'nskk-handle-return
  "L"       #'nskk-handle-upper-l
  "/"       #'nskk-handle-slash
  "x"       #'nskk-handle-x
  "C-n"     #'nskk-handle-ctrl-n
  "C-p"     #'nskk-handle-ctrl-p
  "C-f"     #'nskk-handle-ctrl-f
  "<right>" #'nskk-handle-ctrl-f
  "C-b"     #'nskk-handle-ctrl-b
  "<left>"  #'nskk-handle-ctrl-b
  "<down>"  #'nskk-handle-ctrl-n
  "<up>"    #'nskk-handle-ctrl-p
  "C-a"     #'nskk-handle-ctrl-a
  "<home>"  #'nskk-handle-ctrl-a
  "C-e"     #'nskk-handle-ctrl-e
  "<end>"   #'nskk-handle-ctrl-e
  "C-g"     #'nskk-handle-cancel
  "DEL"     #'nskk-handle-backspace
  ";"       #'nskk-handle-semicolon-key
  "TAB"     #'nskk-handle-tab
  "#"       #'nskk-handle-hash
  "C-/"     #'nskk-undo-kakutei
  "X"       #'nskk-handle-upper-x)

;;;###autoload
(define-minor-mode nskk-mode
  "Enable NSKK (Next-generation SKK) Japanese input method in current buffer.

The mode-line indicator is generated dynamically by `nskk-modeline-indicator'.
Use `nskk-modeline-format' to customize its format, and `nskk-use-color-cursor'
to control per-mode cursor color changes.

\\{nskk-mode-map}"
  :variable (nskk-mode . nskk--set-mode-state)
  :lighter (:eval (nskk-modeline-indicator))
  :keymap nskk-mode-map
  :group 'nskk
  (if nskk-mode
      (condition-case condition-data
          (nskk--enable)
        ((error quit)
         (let ((condition-symbol (car condition-data))
               (condition-payload (cdr condition-data)))
           (condition-case nil
               (nskk--restore-mode-set-snapshot)
             ((error quit) nil))
           (signal condition-symbol condition-payload))))
    (nskk--disable)))

(defvar-keymap nskk-global-mode-map
  :doc "Global keymap for `nskk-global-mode'.
This provides global bindings that work even when nskk-mode is not yet active."
  "C-x C-j" #'nskk-toggle-mode)

;;;###autoload
(define-globalized-minor-mode nskk-global-mode
  nskk-mode
  nskk--turn-on-mode
  :global t
  :keymap nskk-global-mode-map
  :group 'nskk)

;; Internal implementation functions

(defvar nskk--learning-loaded nil
    "Non-nil once learning data has been loaded in this Emacs session.
Guards `nskk--enable' so that learning data is loaded only once, not on
every buffer activation.")

  (defun nskk-learning-loaded ()
    "Return non-nil if learning data has been loaded this session."
    nskk--learning-loaded)

  (defun nskk-set-learning-loaded (value)
    "Set the learning-data-loaded flag to VALUE and return VALUE."
    (setq nskk--learning-loaded value))

  (defvar nskk--active-buffers nil
    "Live buffers that currently own an NSKK activation.")

  (defun nskk-active-buffers ()
    "Return the list of live buffers that currently own an NSKK activation."
    nskk--active-buffers)

  (defun nskk-set-active-buffers (value)
    "Set the list of live NSKK-activation-owning buffers to VALUE."
    (setq nskk--active-buffers value))

  (defvar nskk-activation-lock-owner nil
    "Live buffer that exclusively owns process-global NSKK state.")

  (defun nskk--activation-allowed-p ()
    "Return non-nil when current buffer may activate NSKK."
    (when (and nskk-activation-lock-owner
               (not (buffer-live-p nskk-activation-lock-owner)))
      (setq nskk-activation-lock-owner nil))
    (or (null nskk-activation-lock-owner)
        (eq nskk-activation-lock-owner (current-buffer))))

  (defvar nskk--candidate-show-hook-owned nil
    "Non-nil when NSKK installed its global candidate show hook.")

  (defvar nskk--candidate-hide-hook-owned nil
    "Non-nil when NSKK installed its global candidate hide hook.")

  (defvar nskk--candidate-select-function-owned nil
    "Non-nil when NSKK replaced the global candidate select function.")

  (defvar nskk--saved-candidate-select-function nil
    "Candidate select function that preceded the first NSKK activation.")

  (defun nskk--acquire-candidate-resources ()
    "Acquire process-global candidate resources for the current buffer.
Return non-nil when this call registered the buffer for the first time."
    (setq nskk--active-buffers
          (cl-delete-if-not #'buffer-live-p nskk--active-buffers))
    (let ((new-activation (not (memq (current-buffer) nskk--active-buffers))))
      (when (and new-activation (null nskk--active-buffers))
        (setq nskk--candidate-show-hook-owned
              (not (memq #'nskk-candidate-show-list
                         nskk-henkan-show-candidates-functions))
              nskk--candidate-hide-hook-owned
              (not (memq #'nskk-candidate-hide-list
                         nskk-henkan-hide-candidates-functions))
              nskk--saved-candidate-select-function
              nskk-henkan-select-candidate-by-key-function
              nskk--candidate-select-function-owned
              (not (eq nskk-henkan-select-candidate-by-key-function
                       #'nskk-candidate-list-select-by-key)))
        (when nskk--candidate-show-hook-owned
          (add-hook 'nskk-henkan-show-candidates-functions
                    #'nskk-candidate-show-list))
        (when nskk--candidate-hide-hook-owned
          (add-hook 'nskk-henkan-hide-candidates-functions
                    #'nskk-candidate-hide-list))
        (when nskk--candidate-select-function-owned
          (setq nskk-henkan-select-candidate-by-key-function
                #'nskk-candidate-list-select-by-key)))
      (when new-activation
        (push (current-buffer) nskk--active-buffers))
      new-activation))

  (defun nskk--release-candidate-resources ()
    "Release candidate resources owned by the current buffer."
    (setq nskk--active-buffers
          (cl-delete-if-not
           (lambda (buffer)
             (and (buffer-live-p buffer)
                  (not (eq buffer (current-buffer)))))
           nskk--active-buffers))
    (when (null nskk--active-buffers)
      (when nskk--candidate-show-hook-owned
        (remove-hook 'nskk-henkan-show-candidates-functions
                     #'nskk-candidate-show-list))
      (when nskk--candidate-hide-hook-owned
        (remove-hook 'nskk-henkan-hide-candidates-functions
                     #'nskk-candidate-hide-list))
      (when (and nskk--candidate-select-function-owned
                 (eq nskk-henkan-select-candidate-by-key-function
                     #'nskk-candidate-list-select-by-key))
        (setq nskk-henkan-select-candidate-by-key-function
              nskk--saved-candidate-select-function))
      (setq nskk--candidate-show-hook-owned nil
            nskk--candidate-hide-hook-owned nil
            nskk--candidate-select-function-owned nil
            nskk--saved-candidate-select-function nil)))

  (defun nskk--restore-buffer-snapshot (snapshot)
    "Restore buffer-local variables recorded in SNAPSHOT."
    (dolist (entry snapshot)
      (pcase-let ((`(,symbol ,local-p ,value) entry))
        (if local-p
            (set (make-local-variable symbol) value)
          (kill-local-variable symbol)))))

  (defvar-local nskk--teardown-in-progress nil
  "Non-nil while the current buffer is releasing NSKK resources.")

(defun nskk--teardown-step (function condition-data)
  "Call FUNCTION and preserve the first condition in CONDITION-DATA."
  (condition-case new-condition
      (progn
        (funcall function)
        condition-data)
    ((error quit) (or condition-data new-condition))))

(defun nskk--teardown (&optional run-off-hook)
  "Release all NSKK resources owned by the current buffer.
When RUN-OFF-HOOK is non-nil, run every function in
`nskk-mode-off-hook'.  Every cleanup step runs even if an earlier step
signals; the first signal is rethrown only after mandatory cleanup finishes."
  (unless nskk--teardown-in-progress
    (let ((nskk--teardown-in-progress t)
          (condition-data nil))
      (cl-labels ((attempt (function)
                    (setq condition-data
                          (nskk--teardown-step function condition-data))))
        (when (and (boundp 'nskk-current-state)
                   (nskk-state-p nskk-current-state))
          (attempt
           (lambda ()
             (let* ((phase (nskk-state-henkan-phase nskk-current-state))
                    (action (when phase
                              (nskk-prolog-query-value
                               `(disable-cleanup ,phase \?a) '\?a))))
               (pcase action
                 ('cancel-conversion (nskk-cancel-conversion-to-reading))
                 ('cancel-preedit (nskk-cancel-preedit)))))))
        (attempt #'nskk-clear-conversion-context)
        (attempt (lambda () (nskk-cursor-color-restore nil t)))
        (when run-off-hook
          (run-hook-wrapped
           'nskk-mode-off-hook
           (lambda (function)
             (attempt function)
             nil)))
        (attempt #'nskk--cleanup-buffer)
        (when (fboundp 'nskk-show-mode-hide)
          (attempt #'nskk-show-mode-hide))
        (attempt (lambda ()
                   (remove-hook 'completion-at-point-functions
                                #'nskk-completion-at-point t)))
        (attempt (lambda ()
                   (remove-hook 'kill-buffer-hook
                                #'nskk--handle-buffer-kill t)))
        (attempt (lambda ()
                   (remove-hook 'change-major-mode-hook
                                #'nskk--handle-major-mode-change t)))
        (attempt #'nskk--release-candidate-resources)
        (setq nskk-mode nil
              nskk-current-state nil)
        (when condition-data
          (signal (car condition-data) (cdr condition-data)))))))

(defun nskk--handle-buffer-kill ()
  "Release all NSKK resources when the current buffer is killed."
  (nskk--teardown nil))

(defun nskk--handle-major-mode-change ()
  "Release all NSKK resources before changing the current major mode."
  (nskk--teardown t))

(defun nskk--save-learning-data ()
  "Save learning data (and study data when loaded) on Emacs exit.
Registered on `kill-emacs-hook' by `nskk--enable' when
`nskk-search-auto-save-learning' is non-nil."
  (unless (and (boundp 'nskk--persistence-inhibited)
               nskk--persistence-inhibited)
    (nskk-search-save-learning-data)
    (when (featurep 'nskk-study)
      (nskk-study-save))))

(defun nskk--activation-snapshot ()
  "Capture state that must survive a failed NSKK activation."
  (let ((buffer-symbols
         '(nskk-current-state nskk--bound-commands
                              pre-command-hook post-command-hook
                              completion-at-point-functions kill-buffer-hook
                              change-major-mode-hook)))
    (list
     :buffer
     (mapcar (lambda (symbol)
               (list symbol (local-variable-p symbol) (symbol-value symbol)))
             buffer-symbols)
     :active-buffers (copy-sequence nskk--active-buffers)
     :show-mode-overlay
     (when (fboundp 'nskk-show-mode-overlay) (nskk-show-mode-overlay))
     :show-mode-timer
     (when (fboundp 'nskk-show-mode-timer) (nskk-show-mode-timer))
     :show-functions (copy-sequence nskk-henkan-show-candidates-functions)
     :hide-functions (copy-sequence nskk-henkan-hide-candidates-functions)
     :select-function nskk-henkan-select-candidate-by-key-function
     :show-owned nskk--candidate-show-hook-owned
     :hide-owned nskk--candidate-hide-hook-owned
     :select-owned nskk--candidate-select-function-owned
     :saved-select nskk--saved-candidate-select-function
     :kill-emacs-hook (copy-sequence kill-emacs-hook)
     :learning-loaded nskk--learning-loaded
     :isearch-transaction
     (when (and (fboundp 'nskk-isearch-resource-state)
                (boundp 'nskk-isearch-enable)
                nskk-isearch-enable)
       (nskk--isearch-transaction-state)))))

(defun nskk--initialize-activation ()
  "Initialize resources required by the current NSKK activation."
  (nskk-debug-message "NSKK is enabled in buffer: %s" (buffer-name))
  (nskk-state-initialize-prolog)
  (nskk-kana-initialize)
  (nskk-converter-initialize)
  (unless (nskk-prolog-holds-p '(dict-initialized))
    (nskk-dict-initialize))
  (nskk-henkan-initialize)
  (nskk-input-initialize)
  (unless nskk-current-state
    (setq nskk-current-state
          (nskk-state-create nskk-state-default-mode))
    (nskk-debug-message "Created initial state: mode=%s"
                        nskk-state-default-mode))
  (nskk--acquire-candidate-resources)
  (add-hook 'kill-buffer-hook #'nskk--handle-buffer-kill nil t)
  (add-hook 'change-major-mode-hook #'nskk--handle-major-mode-change nil t)
  (when (eq nskk-dcomp-style 'capf)
    (add-hook 'completion-at-point-functions
              #'nskk-completion-at-point nil t))
  (when (fboundp 'nskk-annotation-initialize)
    (nskk-annotation-initialize))
  (when (and (fboundp 'nskk-isearch-setup)
             (boundp 'nskk-isearch-enable)
             nskk-isearch-enable)
    (nskk-isearch-setup))
  (nskk-maybe-load-azik-style)
  (add-hook 'kill-emacs-hook #'nskk--dict-maybe-save)
  (when nskk-search-auto-save-learning
    (unless nskk--learning-loaded
      (setq nskk--learning-loaded t)
      (nskk-search-load-learning-data)
      (when (featurep 'nskk-study)
        (nskk-study-load)))
    (add-hook 'kill-emacs-hook #'nskk--save-learning-data)))

(defun nskk--restore-isearch-activation (snapshot attempt)
  "Restore isearch state from SNAPSHOT by calling ATTEMPT for each step."
  (let ((isearch-transaction (plist-get snapshot :isearch-transaction)))
    (when (and isearch-transaction
               (fboundp 'nskk--isearch-restore-transaction-state))
      (funcall attempt
               (lambda ()
                 (nskk--isearch-restore-transaction-state
                  isearch-transaction)))
      (unless (equal (nskk--isearch-transaction-state) isearch-transaction)
        (funcall attempt
                 (lambda ()
                   (nskk--isearch-restore-transaction-state
                    isearch-transaction)))))))

(defun nskk--rollback-activation (snapshot cursor-save-started condition-data)
  "Restore SNAPSHOT after activation failed with CONDITION-DATA.
When CURSOR-SAVE-STARTED is non-nil, restore cursor state as well."
  (let ((original-condition condition-data))
    (cl-labels ((attempt (function)
                         (setq condition-data
                               (nskk--teardown-step function condition-data))))
               (when (fboundp 'nskk-show-mode-hide)
                 (attempt #'nskk-show-mode-hide))
               (when (fboundp 'nskk-show-mode-set-overlay)
                 (attempt
                  (lambda ()
                    (nskk-show-mode-set-overlay
                     (plist-get snapshot :show-mode-overlay)))))
               (when (fboundp 'nskk-show-mode-set-timer)
                 (attempt
                  (lambda ()
                    (nskk-show-mode-set-timer
                     (plist-get snapshot :show-mode-timer)))))
               (when cursor-save-started
                 (attempt #'nskk-cursor-color-restore))
               (attempt
                (lambda ()
                  (setq kill-emacs-hook (plist-get snapshot :kill-emacs-hook)
                        nskk--learning-loaded (plist-get snapshot :learning-loaded))))
               (attempt
                (lambda ()
                  (setq nskk--active-buffers (plist-get snapshot :active-buffers)
                        nskk-henkan-show-candidates-functions
                        (plist-get snapshot :show-functions)
                        nskk-henkan-hide-candidates-functions
                        (plist-get snapshot :hide-functions)
                        nskk-henkan-select-candidate-by-key-function
                        (plist-get snapshot :select-function)
                        nskk--candidate-show-hook-owned
                        (plist-get snapshot :show-owned)
                        nskk--candidate-hide-hook-owned
                        (plist-get snapshot :hide-owned)
                        nskk--candidate-select-function-owned
                        (plist-get snapshot :select-owned)
                        nskk--saved-candidate-select-function
                        (plist-get snapshot :saved-select))))
               (nskk--restore-isearch-activation snapshot #'attempt)
               (attempt
                (lambda ()
                  (nskk--restore-buffer-snapshot (plist-get snapshot :buffer))))
               (signal (car original-condition) (cdr original-condition)))))

(defun nskk--enable ()
  "Enable NSKK in current buffer transactionally."
  (unless (nskk--activation-allowed-p)
    (user-error "Cannot enable NSKK while another buffer owns global state"))
  (when nskk--teardown-in-progress
    (user-error "Cannot enable NSKK while teardown is in progress"))
  (let ((snapshot (nskk--activation-snapshot))
        (cursor-save-started nil))
    (condition-case condition-data
        (progn
          (nskk--initialize-activation)
          (nskk--setup-buffer)
          (setq cursor-save-started t)
          (nskk-cursor-color-save)
          (nskk-modeline-update))
      ((error quit)
       (nskk--rollback-activation
        snapshot cursor-save-started condition-data)))))

(defun nskk--disable ()
  "Disable NSKK in current buffer."
  (nskk--teardown t))

(defun nskk--turn-on-mode ()
  "Turn on nskk-mode in appropriate buffers."
  (nskk-debug-message "Turning on NSKK mode in buffer: %s" (buffer-name))
  (unless (minibufferp)
    (nskk-mode 1)))

(defvar-local nskk--bound-commands nil
  "List of interactive commands bound in `nskk-mode-map'.
Used by `nskk--post-command-handler' to distinguish NSKK-internal
commands from unbound movement commands in the preedit (▽) guard.")

(defvar-local nskk--point-before-command nil
  "Point position recorded before each command, for preedit movement detection.
Set by `nskk--pre-command-handler' and read by `nskk--post-command-handler'.")

(defun nskk--setup-buffer ()
  "Setup buffer-local NSKK state."
  ;; Collect all commands reachable from nskk-mode-map (including sub-keymaps
  ;; like C-x C-j) for the preedit point-escape guard.
  (let ((cmds nil))
    (dolist (km (accessible-keymaps nskk-mode-map))
      (map-keymap (lambda (_key binding)
                    (when (commandp binding)
                      (push binding cmds)))
                  (cdr km)))
    (setq nskk--bound-commands cmds))
  (add-hook 'pre-command-hook  #'nskk--pre-command-handler  nil t)
  (add-hook 'post-command-hook #'nskk--post-command-handler nil t))

(defun nskk--cleanup-buffer ()
  "Cleanup buffer-local NSKK state."
  (remove-hook 'pre-command-hook  #'nskk--pre-command-handler  t)
  (remove-hook 'post-command-hook #'nskk--post-command-handler t))

(defun nskk--pre-command-handler ()
  "Record point before each command for preedit movement detection.
Used by `nskk--post-command-handler' to detect point changes caused by
unbound cursor-movement commands while in preedit (▽) state."
  (setq nskk--point-before-command (point)))

(defun nskk--post-command-handler ()
  "Handle post-command hook for NSKK state update.
Guards against point escaping the active conversion or preedit area due
to unmapped cursor-movement commands
\(mouse clicks, \[forward-word], page-up, etc.).

Converting (▼) guard: point must be exactly at `overlay-end'.  Any
deviation triggers implicit kakutei (確定).

Preedit (▽) guard: if point moved and `this-command' is not an
NSKK-bound command, commits the reading as-is via `nskk-henkan-kakutei'.

Handlers bound in `nskk-mode-map' call `nskk-commit-by-phase'
explicitly before moving, so by the time this hook fires for them the
relevant phase is already nil and both guards are no-ops."
  (when (and nskk-mode nskk-current-state)
    ;; Skip when okurigana is in progress AND this-command is an
    ;; NSKK-bound command: point is legitimately past overlay-end
    ;; because the okurigana kana sits after the overlay.
    ;; For unbound commands (M-b, mouse click, etc.) proceed with the
    ;; guard even during okurigana so the conversion is committed.
    (when (and (nskk-converting-p)
              (or (not (nskk-state-get-metadata nskk-current-state
                                                'okurigana-in-progress))
                  (not (memq this-command nskk--bound-commands))))
      (let* ((conv-start (nskk-get-conversion-start))
             (overlay-end (when (overlayp (nskk-state-conversion-overlay))
                            (overlay-end (nskk-state-conversion-overlay)))))
        (when (and conv-start overlay-end
                   (/= (point) overlay-end))
          (nskk-commit-current))))
    (when (and (nskk-prolog-holds-p
                `(preedit-phase ,(nskk-state-henkan-phase nskk-current-state)))
               (nskk-get-conversion-start)
               nskk--point-before-command
               (/= (point) nskk--point-before-command)
               (not (memq this-command nskk--bound-commands)))
      (nskk-henkan-kakutei))
    (when (and (not (eq this-command 'nskk-undo-kakutei))
               (nskk-last-kakutei-record))
      (nskk-invalidate-undo-kakutei))
    (nskk-modeline-update)))

;; User commands

;;;###autoload
(defun nskk-toggle-mode ()
  "Toggle NSKK mode on/off in current buffer."
  (interactive)
  (if nskk-mode
      (nskk-mode 0)
    (nskk-mode 1))
  (message "NSKK mode is %s" (if nskk-mode "enabled" "disabled")))

;;;###autoload
(defun nskk-kakutei ()
  "Commit conversion or switch to hiragana mode (確定).
Dispatches via the `kakutei-action/2' Prolog predicate based on state:
- converting (▼): commit current candidate
- preedit (▽): commit preedit text as-is
- romaji-pending: flush incomplete romaji buffer
- hiragana-idle (hiragana): insert newline
- katakana-idle (katakana/katakana-半角): switch to hiragana
- direct-idle (ascii/latin/jisx0208-latin/abbrev): switch to hiragana"
  (interactive)
  (let* ((state (nskk-current-kakutei-state))
         (action (nskk-prolog-query-value
                  `(kakutei-action ,state ,'\?a) '\?a)))
    (pcase action
      ('commit-candidate (nskk-commit-current))
      ('commit-preedit   (nskk-henkan-kakutei))
      ('clear-romaji     (nskk-clear-pending-romaji) (nskk-state-set-romaji-buffer ""))
      ('enter-hiragana   (nskk-set-mode-hiragana))
      ('insert-newline   (newline))
      (_                 nil))))

(provide 'nskk)

;;; nskk.el ends here
