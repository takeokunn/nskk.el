;;; nskk-context.el --- Context-aware auto mode switching for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

;; Author: takeokunn <bararararatty@gmail.com>
;; Maintainer: takeokunn <bararararatty@gmail.com>
;; URL: https://github.com/takeokunn/nskk.el
;; Version: 0.1.0
;; Keywords: i18n

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Context-aware automatic input mode switching for NSKK (Layer 5: Presentation).
;;
;; Layer position: L5 (Presentation) -- depends on nskk-state, nskk-custom,
;;   and nskk-cps-macros.
;;
;; When `nskk-context-mode' is enabled, NSKK automatically switches to
;; ASCII mode when editing outside of string literals and comments in
;; programming language modes.  This prevents accidental Japanese input
;; when coding, and avoids the need to manually switch modes when moving
;; between code and comments.
;;
;; This is the nskk.el equivalent of ddskk's `context-skk.el'.
;;
;; The mode works by hooking into `post-command-hook' and using
;; `syntax-ppss' to determine the current syntactic context:
;; - Inside a string: Japanese input may be appropriate
;; - Inside a comment: Japanese input may be appropriate
;; - Outside both: automatically switch to ASCII mode
;;
;; Configuration:
;;   (add-hook 'prog-mode-hook #'nskk-context-mode)
;;
;; Or globally:
;;   (nskk-context-global-mode 1)
;;
;; Prolog predicates maintained by this module: none.

;;; Code:

(require 'nskk-state)
(require 'nskk-custom)
(require 'nskk-cps-macros)

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
  "Minimum number of commands between context checks.
Zero means check on every command (most responsive but slight overhead).
Higher values reduce overhead but may lag in mode switching."
  :type 'natnum
  :safe #'natnump
  :package-version '(nskk . "0.1.0")
  :group 'nskk-context)

;;;; Internal State

(defvar-local nskk--context-was-ascii nil
  "Non-nil if we explicitly switched to ASCII mode via context-skk.
Used to avoid redundant mode switches when already in ASCII.")

(defvar-local nskk--context-command-count 0
  "Counter for throttling context checks.")

;;;; Internal Helpers

(defun nskk--context-programming-mode-p ()
  "Return non-nil if context-based switching should apply in the current buffer.
Checks `nskk-context-programming-mode' against the current major mode."
  (cond
   ((eq nskk-context-programming-mode t)
    (derived-mode-p 'prog-mode))
   ((listp nskk-context-programming-mode)
    (apply #'derived-mode-p nskk-context-programming-mode))
   (t nil)))

(defun nskk--context-in-japanese-context-p ()
  "Return non-nil if point is in a context where Japanese input is appropriate.
Returns t when inside a string literal, comment, or doc string.
Uses `syntax-ppss' for O(log n) context detection."
  (let* ((ppss (syntax-ppss)))
    (or (nth 3 ppss)   ; inside string
        (nth 4 ppss)   ; inside comment
        )))

(defun nskk--context-get-current-mode ()
  "Return the current NSKK input mode symbol, or nil if NSKK is not active."
  (when (and (boundp 'nskk-current-state) nskk-current-state)
    (nskk-state-mode nskk-current-state)))

(defun nskk--context-switch-to-ascii ()
  "Switch NSKK to ASCII mode due to context auto-detection.
Displays `nskk-context-mode-off-message' in the echo area."
  (when (fboundp 'nskk-set-mode-latin)
    (nskk-set-mode-latin)
    (setq nskk--context-was-ascii t)
    (let ((message-log-max nil))
      (message "%s" nskk-context-mode-off-message))))

;;;; Post-Command Handler

(defun nskk--context-post-command ()
  "Post-command hook for context-aware NSKK mode switching.
Checks if point is outside a string/comment context in a programming mode,
and switches to ASCII mode automatically if so."
  ;; Throttle based on check interval
  (when (>= (cl-incf nskk--context-command-count) nskk-context-check-interval)
    (setq nskk--context-command-count 0)
      (when (and (boundp 'nskk-mode) nskk-mode
                 (boundp 'nskk-current-state) nskk-current-state
                 (nskk--context-programming-mode-p))
      (let ((current-mode (nskk--context-get-current-mode)))
        (cond
         ;; In Japanese mode, not in a Japanese context: switch to ASCII
         ((and current-mode
               (not (memq current-mode '(ascii latin)))
               (not (nskk--context-in-japanese-context-p)))
          (nskk--context-switch-to-ascii))
         ;; In ASCII mode (set by context), re-entered Japanese context:
         ;; do nothing — user must manually switch back to Japanese
         ((and nskk--context-was-ascii
               (nskk--context-in-japanese-context-p))
          ;; Could optionally restore Japanese mode here, but ddskk does not
          (setq nskk--context-was-ascii nil)))))))

;;;; Minor Mode

;;;###autoload
(define-minor-mode nskk-context-mode
  "Context-aware NSKK mode: auto-switch to ASCII outside strings/comments.
When enabled in programming modes, automatically switches NSKK to ASCII
mode when editing code outside string literals and comments.  This prevents
accidental Japanese input when writing code.

The mode indicator \";\u25bd\" is shown in the mode-line when enabled."
  :lighter " ;▽"
  :group 'nskk-context
  (if nskk-context-mode
      (add-hook 'post-command-hook #'nskk--context-post-command nil t)
    (remove-hook 'post-command-hook #'nskk--context-post-command t)
    (setq nskk--context-was-ascii nil
          nskk--context-command-count 0)))

;;;###autoload
(define-globalized-minor-mode nskk-context-global-mode
  nskk-context-mode
  nskk--context-maybe-enable
  :group 'nskk-context)

(defun nskk--context-maybe-enable ()
  "Enable `nskk-context-mode' in programming buffers."
  (when (nskk--context-programming-mode-p)
    (nskk-context-mode 1)))

(provide 'nskk-context)

;;; nskk-context.el ends here
