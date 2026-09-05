;;; nskk-context.el --- Context-aware auto mode switching for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

;; Author: takeokunn <bararararatty@gmail.com>
;; Maintainer: takeokunn <bararararatty@gmail.com>
;; URL: https://github.com/takeokunn/nskk.el
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

;; Context-aware auto mode switching for NSKK.

;;; Code:

(require 'cl-lib)
(require 'nskk-state)
(require 'nskk-cps-macros)

;; `nskk-context-global-mode' is autoloaded, so this file can run in a session
;; where `nskk.el' and `nskk-input.el' were never loaded.  `nskk-mode' is read
;; through `bound-and-true-p' for that reason.  `nskk-set-mode-latin' needs no
;; runtime guard: every call site is dominated by that `nskk-mode' test, and a
;; non-nil `nskk-mode' implies `nskk.el' already required `nskk-input'.
(declare-function nskk-set-mode-latin "nskk-input")

;;;; Customization

(defgroup nskk-context nil
  "Context-aware NSKK mode switching settings."
  :prefix "nskk-context-"
  :group 'nskk)

(defcustom nskk-context-programming-mode t
  "When non-nil, enable context-based auto-switching in programming modes.
When this is a list of mode symbols, only those modes trigger auto-switching.
When t, all `prog-mode' derived modes trigger auto-switching."
  :type '(choice (const :tag "All programming modes" t)
                 (repeat :tag "Specific modes" symbol))
  :safe (lambda (v) (or (booleanp v) (and (listp v) (cl-every #'symbolp v))))
  :package-version '(nskk . "0.1.0")
  :group 'nskk-context)

(defcustom nskk-context-mode-off-message "[context-nskk] 日本語入力 off"
  "Message shown in echo area when switching to ASCII mode automatically."
  :type 'string
  :safe #'stringp
  :package-version '(nskk . "0.1.0")
  :group 'nskk-context)

(defcustom nskk-context-check-interval 0
  "Run a context check once every this many commands.
The values 0 and 1 both check on every command.  Larger values leave
`post-command-hook' cheaper at the cost of switching later."
  :type 'natnum
  :safe #'natnump
  :package-version '(nskk . "0.1.0")
  :group 'nskk-context)

;;;; Internal State

(defvar-local nskk--context-command-count 0
  "Commands seen since the last context check.")

;;;; Internal Helpers

(defun nskk--context-programming-mode-p ()
  "Return non-nil if context-based switching should apply in the current buffer.
Checks `nskk-context-programming-mode' against the current major mode."
  (pcase nskk-context-programming-mode
    ('t (derived-mode-p 'prog-mode))
    ((pred listp) (apply #'derived-mode-p nskk-context-programming-mode))
    (_ nil)))

(defun nskk--context-in-japanese-context-p ()
  "Return non-nil if point is inside a string literal or a comment."
  (let ((ppss (syntax-ppss)))
    (or (nth 3 ppss)
        (nth 4 ppss))))

(defun/done nskk--context-switch-to-ascii ()
  "Switch NSKK to ASCII mode and echo `nskk-context-mode-off-message'."
  (nskk-set-mode-latin)
  (let ((message-log-max nil))
    (message "%s" nskk-context-mode-off-message)))

;;;; Post-Command Handler

(defun/done nskk--context-post-command ()
  "Switch NSKK to ASCII mode when point sits in code rather than prose.
Runs from `post-command-hook', throttled by `nskk-context-check-interval'."
  (when (>= (cl-incf nskk--context-command-count) nskk-context-check-interval)
    (setq nskk--context-command-count 0)
    (let ((mode (and (bound-and-true-p nskk-mode)
                     (nskk--context-programming-mode-p)
                     (nskk-state-get-mode))))
      (when (and mode
                 (not (memq mode '(ascii latin)))
                 (not (nskk--context-in-japanese-context-p)))
        (nskk--context-switch-to-ascii)))))

;;;; Minor Mode

;;;###autoload
(define-minor-mode nskk-context-mode
  "Context-aware NSKK mode: auto-switch to ASCII outside strings/comments.
When enabled in programming modes, automatically switches NSKK to ASCII
mode when editing code outside string literals and comments.  This prevents
accidental Japanese input when writing code.

The mode indicator \" ;▽\" is shown in the mode-line when enabled."
  :lighter " ;▽"
  :group 'nskk-context
  (if nskk-context-mode
      (add-hook 'post-command-hook #'nskk--context-post-command nil t)
    (remove-hook 'post-command-hook #'nskk--context-post-command t)
    (setq nskk--context-command-count 0)))

(defun nskk--context-maybe-enable ()
  "Enable `nskk-context-mode' in programming buffers."
  (when (nskk--context-programming-mode-p)
    (nskk-context-mode 1)))

;;;###autoload
(define-globalized-minor-mode nskk-context-global-mode
  nskk-context-mode
  nskk--context-maybe-enable
  :group 'nskk-context)

(provide 'nskk-context)

;;; nskk-context.el ends here
