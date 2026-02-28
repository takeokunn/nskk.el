;;; nskk.el --- NSKK main entry point -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

;; Author: takeokunn <bararararatty@gmail.com>
;; Maintainer: takeokunn <bararararatty@gmail.com>
;; URL: https://github.com/takeokunn/nskk.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: i18n

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
;;   /        Enter abbrev mode (▽word SPC → dictionary lookup, DDSKK-compatible)
;;   DEL      Delete last preedit char / cancel empty abbrev preedit
;;   C-g      Cancel conversion or preedit (abbrev: restores previous mode)
;;   C-n      Next candidate (in conversion mode) / next-line
;;   C-p      Previous candidate (in conversion mode) / previous-line
;;
;; Input modes: ascii (default), hiragana, katakana, latin,
;;   abbrev, jisx0208-latin.
;;
;; Dictionary configuration:
;;   (setq nskk-dict-file "/path/to/SKK-JISYO.L")

;;; Code:

;; Foundation modules
(require 'nskk-custom)
(require 'nskk-prolog)
(require 'nskk-state)

;; Core modules
(require 'nskk-henkan)
(require 'nskk-input)

;; UI modules
(require 'nskk-keymap)
(require 'nskk-candidate-window)
(require 'nskk-modeline)

;; Optional
(require 'nskk-debug nil t)

(declare-function nskk-set-mode-hiragana "nskk-input")
(declare-function nskk-henkan-kakutei "nskk-henkan")
(declare-function nskk--current-kakutei-state "nskk-keymap")
(declare-function nskk--maybe-load-azik-style "nskk-input")
(declare-function nskk-dict--maybe-save "nskk-dictionary")

(defvar nskk-mode-hook nil
  "Hook run when NSKK mode is enabled.
DDSKK equivalent: skk-mode-hook")

(defvar nskk-mode-off-hook nil
  "Hook run when NSKK mode is disabled.
DDSKK equivalent: skk-mode-off-hook")

(defvar nskk-input-mode-hook nil
  "Hook run when input mode changes.
DDSKK equivalent: skk-input-mode-hook")

(defvar nskk--system-dict-index)

;; Define the minor mode
(defvar nskk-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Remap self-insert for romaji->kana conversion
    (define-key map [remap self-insert-command] 'nskk-self-insert)
    ;; Mode switching
    (define-key map (kbd "C-x C-j") 'nskk-toggle-mode)
    (define-key map (kbd "C-j") 'nskk-kakutei)
    ;; State-aware special key dispatch (see nskk-keymap.el)
    (define-key map (kbd "q") 'nskk-handle-q)
    (define-key map (kbd "l") 'nskk-handle-l)
    (define-key map (kbd "SPC") 'nskk-handle-space)
    (define-key map (kbd "RET") 'nskk-handle-return)
    ;; Additional ddskk-compatible bindings
    (define-key map (kbd "L") 'nskk-handle-upper-l)
    (define-key map (kbd "/") 'nskk-handle-slash)
    (define-key map (kbd "x") 'nskk-handle-x)
    (define-key map (kbd "C-n") 'nskk-handle-ctrl-n)
    (define-key map (kbd "C-p") 'nskk-handle-ctrl-p)
    (define-key map (kbd "C-g") 'nskk-handle-cancel)
    (define-key map (kbd "DEL") 'nskk-handle-backspace)
    map)
  "Keymap for NSKK minor mode.")

;;;###autoload
(define-minor-mode nskk-mode
  "Minor mode for NSKK (Next-generation SKK) input method.

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

(defvar nskk-global-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x C-j") 'nskk-toggle-mode)
    map)
  "Global keymap for `nskk-global-mode'.
This provides global bindings that work even when nskk-mode is not yet active.")

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
  (nskk-debug-message "NSKK enabled in buffer: %s" (buffer-name))
  ;; Initialize global Prolog predicates (idempotent, guard-protected)
  (nskk-state-initialize-prolog)
  (nskk-kana-initialize)
  (nskk-henkan-initialize)
  (nskk-input-initialize)
  (nskk-converter-initialize)
  (unless (nskk-prolog-holds-p '(dict-initialized))
    (nskk-dict-initialize))
  (unless nskk-current-state
    (setq nskk-current-state (nskk-state-create nskk-state-default-mode))
    (nskk-debug-message "Created initial state: mode=%s" nskk-state-default-mode))
  ;; Wire candidate display hooks
  (add-hook 'nskk-henkan-show-candidates-functions #'nskk-candidate-show-list)
  (add-hook 'nskk-henkan-hide-candidates-functions #'nskk-candidate-hide-list)
  (setq nskk-henkan-select-candidate-by-key-function #'nskk-candidate-list-select-by-key)
  ;; Load AZIK style if configured
  (nskk--maybe-load-azik-style)
  ;; Register save-on-exit hook; add-hook deduplicates same symbol safely
  (add-hook 'kill-emacs-hook #'nskk-dict--maybe-save)
  (nskk--setup-buffer)
  (nskk-modeline-update))

(defun nskk--disable ()
  "Disable NSKK in current buffer."
  (run-hooks 'nskk-mode-off-hook)
  (nskk--cleanup-buffer)
  (setq nskk-current-state nil))

(defun nskk--turn-on-mode ()
  "Turn on nskk-mode in appropriate buffers."
  (nskk-debug-message "Turning on NSKK mode in buffer: %s" (buffer-name))
  (unless (minibufferp)
    (nskk-mode 1)))

(defun nskk--setup-buffer ()
  "Setup buffer-local NSKK state."
  (add-hook 'post-command-hook 'nskk--post-command-handler nil t))

(defun nskk--cleanup-buffer ()
  "Cleanup buffer-local NSKK state."
  (remove-hook 'post-command-hook 'nskk--post-command-handler t))

(defun nskk--post-command-handler ()
  "Handle post-command hook for NSKK state update."
  (when (and nskk-mode nskk-current-state)
    (nskk-modeline-update)))

;; User commands

;;;###autoload
(defun nskk-toggle-mode ()
  "Toggle NSKK mode on/off in current buffer."
  (interactive)
  (if nskk-mode
      (nskk-mode 0)
    (nskk-mode 1))
  (message "NSKK mode %s" (if nskk-mode "enabled" "disabled")))

;;;###autoload
(defun nskk-kakutei ()
  "Commit conversion or switch to hiragana mode (確定).
Dispatches via the `kakutei-action/2' Prolog predicate based on state:
- converting (▼): commit current candidate
- preedit (▽): commit preedit text as-is
- romaji-pending: flush incomplete romaji buffer
- japanese-idle (hiragana/katakana/katakana-半角): insert newline
- direct-idle (ascii/latin/jisx0208-latin/abbrev): switch to hiragana"
  (interactive)
  (let* ((state (nskk--current-kakutei-state))
         (action (nskk-prolog-query-value
                  `(kakutei-action ,state ,'\?a) '\?a)))
    (pcase action
      ('commit-candidate (nskk-commit-current))
      ('commit-preedit   (nskk-henkan-kakutei))
      ('clear-romaji     (setq nskk--romaji-buffer ""))
      ('enter-hiragana   (nskk-set-mode-hiragana))
      ('insert-newline   (newline))
      (_                 nil))))

(provide 'nskk)

;;; nskk.el ends here
