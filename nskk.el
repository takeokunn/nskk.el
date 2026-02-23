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
;;   C-j      Commit conversion (in hiragana mode)

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

(declare-function nskk-modeline-update "nskk-modeline")

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

;;;###autoload
(define-globalized-minor-mode nskk-global-mode
  nskk-mode
  nskk--turn-on-mode
  :group 'nskk)

;; Internal implementation functions

(defun nskk--enable ()
  "Enable NSKK in current buffer."
  (unless nskk--system-dict-index
    (nskk-dict-initialize))
  (unless nskk-current-state
    (setq nskk-current-state (nskk-state-create nskk-state-default-mode)))
  (nskk--setup-buffer))

(defun nskk--disable ()
  "Disable NSKK in current buffer."
  (run-hooks 'nskk-mode-off-hook)
  (nskk--cleanup-buffer)
  (setq nskk-current-state nil))

(defun nskk--turn-on-mode ()
  "Turn on nskk-mode in appropriate buffers."
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
  "Commit the current conversion (決定)."
  (interactive)
  (when nskk-current-state
    (let ((input (nskk-state-input-buffer nskk-current-state)))
      (when (> (length input) 0)
        (insert input)
        (nskk-state-clear-input nskk-current-state)
        (nskk--update-modeline)))))

;; Mode switching commands — delegate to Application Layer
;;;###autoload
(defalias 'nskk-switch-to-hiragana #'nskk-enter-hiragana-mode)
;;;###autoload
(defalias 'nskk-switch-to-katakana #'nskk-enter-katakana-mode)
;;;###autoload
(defalias 'nskk-switch-to-ascii #'nskk-enter-latin-mode)
;;;###autoload
(defalias 'nskk-toggle-kana #'nskk-toggle-japanese-mode)

;; Modeline update — delegate to nskk-modeline.el
(defalias 'nskk--update-modeline #'nskk-modeline-update)

(provide 'nskk)

;;; nskk.el ends here
