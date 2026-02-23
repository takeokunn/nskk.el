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

(require 'nskk-custom)
(require 'nskk-state)

;; Define the minor mode
(defvar nskk-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Global key bindings for NSKK
    (define-key map (kbd "C-x C-j") 'nskk-toggle-mode)
    (define-key map (kbd "C-j") 'nskk-kakutei)
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
  (unless (boundp 'nskk--state)
    (setq-local nskk--state (nskk-state-create nskk-state-default-mode)))
  (nskk--setup-buffer))

(defun nskk--disable ()
  "Disable NSKK in current buffer."
  (nskk--cleanup-buffer))

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
  (when (and nskk-mode
             (boundp 'nskk--state)
             (nskk-state-p nskk--state))
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
  (when (and (boundp 'nskk--state)
             (nskk-state-p nskk--state))
    (let ((input (nskk-state-input-buffer nskk--state)))
      (when (> (length input) 0)
        (insert input)
        (nskk-state-clear-input nskk--state)
        (nskk--update-modeline)))))

;; Mode switching commands

;;;###autoload
(defun nskk-switch-to-hiragana ()
  "Switch to hiragana input mode."
  (interactive)
  (nskk--switch-mode 'hiragana))

;;;###autoload
(defun nskk-switch-to-katakana ()
  "Switch to katakana input mode."
  (interactive)
  (nskk--switch-mode 'katakana))

;;;###autoload
(defun nskk-switch-to-ascii ()
  "Switch to ASCII input mode."
  (interactive)
  (nskk--switch-mode 'ascii))

;;;###autoload
(defun nskk-toggle-kana ()
  "Toggle between hiragana and katakana modes."
  (interactive)
  (when (boundp 'nskk--state)
    (let ((current-mode (nskk-state-mode nskk--state)))
      (cond
       ((eq current-mode 'hiragana)
        (nskk-switch-to-katakana))
       ((eq current-mode 'katakana)
        (nskk-switch-to-hiragana))
       (t
        (nskk-switch-to-hiragana))))))

;; Internal helper functions

(defun nskk--switch-mode (new-mode)
  "Internal function to switch to NEW-MODE."
  (when (and (boundp 'nskk--state)
             (nskk-state-p nskk--state))
    (let ((old-mode (nskk-state-mode nskk--state)))
      (when (nskk-state-transition nskk--state old-mode new-mode)
        (nskk--update-modeline)
        (message "NSKK mode: %s" new-mode)))))

(defun nskk--update-modeline ()
  "Update modeline to reflect current NSKK state."
  (when (and (boundp 'nskk--state)
             (nskk-state-p nskk--state))
    (let* ((state nskk--state)
           (mode (nskk-state-mode state))
           (mode-name (cdr (assq mode nskk-modeline-mode-names)))
           (indicator (if (nskk-state-in-henkan-mode-p state) "▼" "▽"))
           (format-string (format-spec nskk-modeline-format
                                       `((?m . ,mode-name)
                                         (?s . ,indicator)))))
      (setq mode-name format-string))))

(provide 'nskk)

;;; nskk.el ends here
