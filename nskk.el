;;; nskk.el --- NSKK main entry point -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

;; Author: NSKK Contributors
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

;; NSKK is a next-generation SKK implementation with:
;; - Zero external dependencies (Emacs 29.1+ only)
;; - Extreme performance (< 1ms key response)
;; - Modern architecture with clean separation of concerns
;; - Extensible plugin system
;;
;; Installation:
;;   (require 'nskk)
;;   (nskk-global-mode 1)
;;
;; Usage:
;;   C-x C-j  Toggle NSKK mode
;;   C-j      Enter hiragana mode / commit conversion (kakutei)

;;; Code:

;; Foundation modules (needed for defcustom, state types)
(require 'nskk-custom)
(require 'nskk-state)

;; Layer modules (bottom-up dependency order)
(require 'nskk-layer-infrastructure)
(require 'nskk-layer-data)
(require 'nskk-layer-core)
(require 'nskk-layer-extension)
(require 'nskk-layer-application)
(require 'nskk-layer-presentation)

;; Cross-cutting concerns (optional)
(require 'nskk-optimize nil t)
(require 'nskk-native-compile nil t)
(require 'nskk-architecture nil t)
(require 'nskk-debug nil t)

(declare-function nskk-modeline-update "nskk-modeline")
(declare-function nskk-converting-p "nskk-input-commands")
(declare-function nskk-commit-current "nskk-input-commands")
(declare-function nskk--has-preedit "nskk-input-commands")
(declare-function nskk--clear-conversion-start-marker "nskk-input-commands")
(declare-function nskk--get-conversion-start "nskk-input-commands")
(declare-function nskk-enter-hiragana-mode "nskk-layer-application")

(defvar nskk--system-dict-index)
(defvar nskk--romaji-buffer)
(defvar nskk-henkan-on-marker)
(defvar nskk-henkan-on-marker-regexp)
(defvar nskk--conversion-start-marker)

(declare-function nskk--delete-marker-at "nskk-input-commands")

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
    (define-key map (kbd "C-g") 'nskk-handle-cancel)
    map)
  "Keymap for NSKK minor mode.")

;;;###autoload
(define-minor-mode nskk-mode
  "Minor mode for NSKK (Next-generation SKK) input method.

\\{nskk-mode-map}"
  :init-value nil
  :lighter " NSKK"
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
  (unless nskk--system-dict-index
    (nskk-dict-initialize))
  (unless nskk-current-state
    (setq nskk-current-state (nskk-state-create nskk-state-default-mode))
    (nskk-debug-message "Created initial state: mode=%s" nskk-state-default-mode))
  (nskk--setup-buffer)
  (nskk--update-modeline))

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
    (nskk--update-modeline)))

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
  "Commit the current conversion or enter hiragana mode (確定).
In SKK, C-j serves as both the kakutei (commit) key and the key
to enter hiragana mode from ASCII/Latin.  This function dispatches:
- Converting → commit current candidate
- Preedit active → commit preedit text as-is (without conversion)
- ASCII/Latin mode → switch to hiragana mode
- Pending romaji → flush romaji buffer
- Otherwise → insert newline"
  (interactive)
  (cond
   ;; If converting, commit the current candidate
   ((nskk-converting-p)
    (nskk-commit-current))
   ;; If preedit text exists (▽ mode), commit as-is without conversion
   ((nskk--has-preedit)
    ;; Remove ▽ marker
    (let ((start (nskk--get-conversion-start)))
      (when start
        (nskk--delete-marker-at start nskk-henkan-on-marker-regexp)))
    (nskk--clear-conversion-start-marker)
    (setq nskk--romaji-buffer "")
    (when (and (boundp 'nskk-current-state) (nskk-state-p nskk-current-state))
      (nskk-state-set-henkan-phase nskk-current-state nil)))
   ;; If in ASCII/Latin mode, switch to hiragana
   ((and nskk-current-state
         (memq (nskk-state-mode nskk-current-state) '(ascii latin)))
    (nskk-enter-hiragana-mode))
   ;; Flush any pending romaji
   ((and (boundp 'nskk--romaji-buffer)
         (not (string-empty-p nskk--romaji-buffer)))
    (setq nskk--romaji-buffer ""))
   ;; Otherwise, insert newline
   (t
    (newline))))

;; Mode switching commands — delegate to Application Layer
;;;###autoload
(defalias 'nskk-switch-to-hiragana #'nskk-enter-hiragana-mode)
;;;###autoload
(defalias 'nskk-switch-to-katakana #'nskk-enter-katakana-mode)
;;;###autoload
(defalias 'nskk-switch-to-ascii #'nskk-enter-latin-mode)
;;;###autoload
(defalias 'nskk-toggle-kana #'nskk-toggle-japanese-mode)

;; Convenience alias for global toggle
;;;###autoload
(defalias 'nskk-toggle #'nskk-toggle-mode)

;; Modeline update — delegate to nskk-modeline.el
(defalias 'nskk--update-modeline #'nskk-modeline-update)

(provide 'nskk)

;;; nskk.el ends here
