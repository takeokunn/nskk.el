;;; nskk-debug.el --- NSKK unified debug mode -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

;; Author: takeokunn <bararararatty@gmail.com>
;; Maintainer: takeokunn <bararararatty@gmail.com>
;; URL: https://github.com/takeokunn/nskk.el
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

;; Debug logging for NSKK (Layer 0: Foundation).
;;
;; Layer position: L0 (Foundation) -- depends only on nskk-custom.
;;
;; Provides centralized debug logging to the *NSKK Debug* buffer with
;; zero overhead when disabled.  The `nskk-debug-log' macro expands to
;; nothing when `nskk-debug-enabled' is nil at compile time; the
;; `nskk-debug-message' function provides a safe runtime variant.
;;
;; No Prolog predicates are maintained by this module.
;;
;; Key public API:
;; - `nskk-debug-log'     -- macro: log with zero overhead when disabled
;; - `nskk-debug-message' -- function: log, safe for use in all contexts
;; - `nskk-debug-toggle'  -- interactive: toggle debug mode on/off
;; - `nskk-debug-show'    -- interactive: display the *NSKK Debug* buffer
;; - `nskk-debug-clear'   -- interactive: clear all entries from the buffer
;;
;; Usage:
;;
;;   M-x nskk-debug-toggle        ;; Enable debug mode
;;
;;   (setq nskk-debug-enabled t)  ;; Or programmatically
;;
;;   (nskk-debug-log "Processing key: %s" key)
;;   (nskk-debug-log "State changed from %s to %s" old-state new-state)
;;
;;   M-x nskk-debug-show          ;; View the debug buffer
;;   M-x nskk-debug-clear         ;; Clear the debug buffer

;;; Code:

(require 'cl-lib)
(require 'nskk-custom)

;;;; Logging Macro

(defmacro nskk-debug-log (format-string &rest args)
  "Log message if debug enabled.  Zero overhead when disabled.
FORMAT-STRING is passed to `format' with ARGS.
Usage: (nskk-debug-log \"Processing: %s\" value)"
  (declare (indent 0) (debug t))
  `(when (bound-and-true-p nskk-debug-enabled)
     (nskk-debug--append (format ,format-string ,@args))))

;;;; Convenience function for use outside macro context

(defun nskk-debug-message (format-string &rest args)
  "Log a debug message.
FORMAT-STRING and ARGS are passed to `format'.
Safe to call even if nskk-debug module has issues."
  (when (bound-and-true-p nskk-debug-enabled)
    (condition-case err
        (nskk-debug--append (apply #'format format-string args))
      (error
       (message "[NSKK-DEBUG-ERROR] %s" err)))))

;;;; Buffer Management

(defconst nskk-debug--buffer-name "*NSKK Debug*"
  "Name of the NSKK debug buffer.")

(defun nskk-debug--buffer ()
  "Get or create the NSKK debug buffer."
  (or (get-buffer nskk-debug--buffer-name)
      (let ((buffer (get-buffer-create nskk-debug--buffer-name)))
        (with-current-buffer buffer
          (setq buffer-read-only t)
          (special-mode))
        buffer)))

(defun nskk-debug--append (message)
  "Append MESSAGE to the debug buffer with timestamp."
  (let ((buffer (nskk-debug--buffer))
        (timestamp (format-time-string "%H:%M:%S.%3N")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (format "[%s] %s\n" timestamp message)))
      (nskk-debug--trim))))

(defun nskk-debug--trim ()
  "Trim debug buffer if it exceeds `nskk-debug-max-entries'.
Must be called with the debug buffer current.
Uses O(1) backward iteration from buffer end instead of O(2n) full scan."
  (let ((max-entries nskk-debug-max-entries))
    (when (> (buffer-size) 0)
      (save-excursion
        (goto-char (point-max))
        (forward-line (- max-entries))
        (when (> (point) (point-min))
          (let ((inhibit-read-only t))
            (delete-region (point-min) (point))))))))

;;;; Interactive Commands

;;;###autoload
(defun nskk-debug-toggle ()
  "Toggle NSKK debug mode."
  (interactive)
  (setq nskk-debug-enabled (not nskk-debug-enabled))
  (if nskk-debug-enabled
      (message "NSKK debug mode enabled")
    (message "NSKK debug mode disabled")))

;;;###autoload
(defun nskk-debug-show ()
  "Show the NSKK debug buffer."
  (interactive)
  (display-buffer (nskk-debug--buffer)))

;;;###autoload
(defun nskk-debug-clear ()
  "Clear the NSKK debug buffer."
  (interactive)
  (let ((buffer (get-buffer nskk-debug--buffer-name)))
    (when buffer
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)))))
  (message "NSKK debug buffer cleared"))

;;;; Provide

;; Handle case where nskk-debug-enabled was set before module loaded
(when (bound-and-true-p nskk-debug-enabled)
  (nskk-debug--append "NSKK debug module loaded (debug was pre-enabled)"))

(provide 'nskk-debug)

;;; nskk-debug.el ends here
