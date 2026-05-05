;;; nskk.el --- NSKK main entry point -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

;; Author: takeokunn <bararararatty@gmail.com>
;; Maintainer: takeokunn <bararararatty@gmail.com>
;; URL: https://github.com/takeokunn/nskk.el
;; Version: 0.1.0
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

;; NSKK is a next-generation SKK (Simple Kana to Kanji) Japanese input
;; method for Emacs.  SKK converts romaji keystrokes to kana/kanji using
;; a dictionary, with the user explicitly marking word boundaries.
;;
;; Features:
;; - Zero external dependencies (Emacs 29.1+ only)
;; - < 1ms key response, < 10ms dictionary search
;; - Modular architecture with clean layer separation
;; - AZIK extended romaji support (optional, via nskk-azik.el)
;;
;; Quick start:
;;   (require 'nskk)
;;   (nskk-global-mode 1)
;;
;; Key bindings (in nskk-mode):
;;   C-x C-j  Toggle NSKK on/off in current buffer
;;   C-j      Kakutei (commit) / enter hiragana from ASCII / newline in Japanese mode
;;   SPC      Start conversion (▽→▼) / cycle next candidate
;;   x        Previous candidate
;;   RET      Commit candidate and insert newline
;;   q        Toggle hiragana ↔ katakana
;;   l        Switch to ASCII (latin) mode
;;   L        Switch to full-width latin (JIS X 0208) mode
;;   /        Enter abbrev mode (▽word SPC → dictionary lookup)
;;   DEL      Delete last preedit char / cancel empty abbrev preedit
;;   C-g      Cancel conversion or preedit (abbrev: restores previous mode)
;;   C-n      Next candidate (in conversion mode) / next-line
;;   C-p      Previous candidate (in conversion mode) / previous-line
;;   C-f      Forward-char (commits active conversion/preedit first)
;;   C-b      Backward-char (commits active conversion/preedit first)
;;   C-a      Beginning-of-line (commits active conversion/preedit first)
;;   C-e      End-of-line (commits active conversion/preedit first)
;;   TAB      Dynamic completion in preedit / pass-through
;;   #        Enter numeric input mode
;;   ;        Sticky shift (next char uppercase) / AZIK small-tsu (っ)
;;
;; Input modes: ascii (default), hiragana, katakana, latin,
;;   abbrev, jisx0208-latin.
;;
;; SKK server (skkserv) configuration (optional):
;;   (setq nskk-server-enable t)
;;   (setq nskk-server-host "localhost")

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
(declare-function nskk--clear-pending-romaji "nskk-henkan" ())
(declare-function nskk--current-kakutei-state "nskk-keymap")
(declare-function nskk--maybe-load-azik-style "nskk-input")
(declare-function nskk--dict-maybe-save "nskk-dictionary")
(declare-function nskk-handle-ctrl-a "nskk-keymap")
(declare-function nskk-handle-ctrl-e "nskk-keymap")
(declare-function nskk-handle-tab "nskk-keymap")
(declare-function nskk-handle-hash "nskk-keymap")
(declare-function nskk-handle-semicolon-key "nskk-input")
(declare-function nskk-converting-p "nskk-henkan")
(declare-function nskk--get-conversion-start "nskk-henkan")
(declare-function nskk-commit-current "nskk-henkan")
(declare-function nskk-cancel-conversion-to-reading "nskk-henkan")
(declare-function nskk-cancel-preedit "nskk-henkan")
(declare-function nskk--clear-conversion-context "nskk-henkan")
(declare-function nskk--invalidate-undo-kakutei "nskk-henkan")
(declare-function nskk-undo-kakutei "nskk-henkan")
(declare-function nskk-purge-from-jisyo "nskk-henkan")
(declare-function nskk-completion-at-point "nskk-henkan")
(declare-function nskk--commit-by-phase "nskk-keymap")

(defvar nskk-mode-off-hook nil
  "Hook run when NSKK mode is disabled.")

;; Define the minor mode
(defvar-keymap nskk-mode-map
  :doc "Keymap for NSKK minor mode."
  ;; Remap self-insert for romaji->kana conversion
  "<remap> <self-insert-command>" #'nskk-self-insert
  ;; Mode switching
  "C-x C-j" #'nskk-toggle-mode
  "C-j"     #'nskk-kakutei
  ;; State-aware special key dispatch (see nskk-keymap.el)
  "q"       #'nskk-handle-q
  "l"       #'nskk-handle-l
  "SPC"     #'nskk-handle-space
  "RET"     #'nskk-handle-return
  ;; Additional special-key bindings
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
  ;; Sticky shift, dynamic completion, numeric input
  ";"       #'nskk-handle-semicolon-key
  "TAB"     #'nskk-handle-tab
  "#"       #'nskk-handle-hash
  ;; Undo-kakutei
  "C-/"     #'nskk-undo-kakutei
  ;; Purge candidate from dictionary (DDSKK skk-purge-from-jisyo)
  "X"       #'nskk-handle-upper-x)

;;;###autoload
(define-minor-mode nskk-mode
  "Enable NSKK (Next-generation SKK) Japanese input method in current buffer.

The mode-line indicator is generated dynamically by `nskk-modeline-indicator'.
Use `nskk-modeline-format' to customize its format, and `nskk-use-color-cursor'
to control per-mode cursor color changes.

\\{nskk-mode-map}"
  :init-value nil
  :lighter '(:eval (nskk-modeline-indicator))
  :keymap nskk-mode-map
  :group 'nskk
  (if nskk-mode
      (nskk--enable)
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

(defun nskk--enable ()
  "Enable NSKK in current buffer."
  (nskk-debug-message "NSKK is enabled in buffer: %s" (buffer-name))
  ;; Initialize global Prolog predicates (idempotent, guard-protected)
  (nskk-state-initialize-prolog)
  (nskk-kana-initialize)
  (nskk-converter-initialize)
  (unless (nskk-prolog-holds-p '(dict-initialized))
    (nskk-dict-initialize))
  (nskk-henkan-initialize)
  (nskk-input-initialize)
  (unless nskk-current-state
    (setq nskk-current-state (nskk-state-create nskk-state-default-mode))
    (nskk-debug-message "Created initial state: mode=%s" nskk-state-default-mode))
  ;; Wire candidate display hooks
  (add-hook 'nskk-henkan-show-candidates-functions #'nskk-candidate-show-list)
  (add-hook 'nskk-henkan-hide-candidates-functions #'nskk-candidate-hide-list)
  (setq nskk-henkan-select-candidate-by-key-function #'nskk-candidate-list-select-by-key)
  ;; Register CAPF backend for dynamic completion when capf style is active
  (when (eq nskk-dcomp-style 'capf)
    (add-hook 'completion-at-point-functions #'nskk-completion-at-point nil t))
  ;; Initialize optional features
  (when (fboundp 'nskk-annotation-initialize)
    (nskk-annotation-initialize))
  ;; Setup isearch integration if enabled
  (when (and (fboundp 'nskk-isearch-setup)
             (boundp 'nskk-isearch-enable)
             nskk-isearch-enable)
    (nskk-isearch-setup))
  ;; Load AZIK style if configured
  (nskk--maybe-load-azik-style)
  ;; Register save-on-exit hook; add-hook deduplicates same symbol safely
  (add-hook 'kill-emacs-hook #'nskk--dict-maybe-save)
  (nskk--setup-buffer)
  (nskk--cursor-color-save)
  (nskk-modeline-update))

(defun nskk--disable ()
  "Disable NSKK in current buffer."
  ;; Cancel any in-progress conversion or preedit before tearing down state.
  (when (and (boundp 'nskk-current-state)
             (nskk-state-p nskk-current-state))
    (let* ((phase (nskk-state-henkan-phase nskk-current-state))
           (action (when phase
                     (nskk-prolog-query-value
                      `(disable-cleanup ,phase \?a) '\?a))))
      (pcase action
        ('cancel-conversion (nskk-cancel-conversion-to-reading))
        ('cancel-preedit    (nskk-cancel-preedit)))))
  ;; Clear remaining input state: pending romaji, dcomp, numeric, sticky shift, AZIK.
  ;; Internally guarded via nskk-when-bound and nskk-with-current-state; safe when nil.
  (nskk--clear-conversion-context)
  (nskk--cursor-color-restore)
  (run-hooks 'nskk-mode-off-hook)
  (nskk--cleanup-buffer)
  (remove-hook 'completion-at-point-functions #'nskk-completion-at-point t)
  (remove-hook 'nskk-henkan-show-candidates-functions #'nskk-candidate-show-list)
  (remove-hook 'nskk-henkan-hide-candidates-functions #'nskk-candidate-hide-list)
  (setq nskk-henkan-select-candidate-by-key-function nil)
  (setq nskk-current-state nil))

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

Handlers bound in `nskk-mode-map' call `nskk--commit-by-phase'
explicitly before moving, so by the time this hook fires for them the
relevant phase is already nil and both guards are no-ops."
  (when (and nskk-mode nskk-current-state)
    ;; Converting (▼) point-escape guard.
    ;; Skip when okurigana is in progress AND this-command is an
    ;; NSKK-bound command: point is legitimately past overlay-end
    ;; because the okurigana kana sits after the overlay.
    ;; For unbound commands (M-b, mouse click, etc.) proceed with the
    ;; guard even during okurigana so the conversion is committed.
    (when (and (nskk-converting-p)
              (or (not (nskk-state-get-metadata nskk-current-state
                                                'okurigana-in-progress))
                  (not (memq this-command nskk--bound-commands))))
      (let* ((conv-start (nskk--get-conversion-start))
             (overlay-end (when (and (boundp 'nskk--conversion-overlay)
                                     (overlayp nskk--conversion-overlay))
                            (overlay-end nskk--conversion-overlay))))
        (when (and conv-start overlay-end
                   (/= (point) overlay-end))
          (nskk-commit-current))))
    ;; Preedit (▽) point-escape guard: commit reading when an unbound command moved point.
    (when (and (nskk-prolog-holds-p
                `(preedit-phase ,(nskk-state-henkan-phase nskk-current-state)))
               (nskk--get-conversion-start)
               nskk--point-before-command
               (/= (point) nskk--point-before-command)
               (not (memq this-command nskk--bound-commands)))
      (nskk-henkan-kakutei))
    ;; Invalidate undo-kakutei record when any non-undo command runs.
    (when (and (not (eq this-command 'nskk-undo-kakutei))
               (boundp 'nskk--last-kakutei-record)
               nskk--last-kakutei-record)
      (nskk--invalidate-undo-kakutei))
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
  (let* ((state (nskk--current-kakutei-state))
         (action (nskk-prolog-query-value
                  `(kakutei-action ,state ,'\?a) '\?a)))
    (pcase action
      ('commit-candidate (nskk-commit-current))
      ('commit-preedit   (nskk-henkan-kakutei))
      ('clear-romaji     (nskk--clear-pending-romaji) (setq nskk--romaji-buffer ""))
      ('enter-hiragana   (nskk-set-mode-hiragana))
      ('insert-newline   (newline))
      (_                 nil))))

(provide 'nskk)

;;; nskk.el ends here
