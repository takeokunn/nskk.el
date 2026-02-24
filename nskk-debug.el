;;; nskk-debug.el --- NSKK unified debug mode -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Takeshi Umeda

;; Author: NSKK Contributors
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

;; Unified debug mode for NSKK.
;; Provides centralized debug logging with zero overhead when disabled.
;; Syncs debug state with layer-specific debug flags.
;;
;; Usage Examples:
;;
;;   ;; Enable debug mode
;;   M-x nskk-debug-toggle
;;
;;   ;; Or programmatically
;;   (setq nskk-debug-enabled t)
;;
;;   ;; Log messages from code
;;   (nskk-debug-log "Processing key: %s" key)
;;   (nskk-debug-log "State changed from %s to %s" old-state new-state)
;;
;;   ;; View the debug buffer
;;   M-x nskk-debug-show
;;
;;   ;; Clear the debug buffer
;;   M-x nskk-debug-clear
;;
;; Public Commands:
;;
;;   `nskk-debug-toggle' - Toggle debug mode on/off
;;   `nskk-debug-show'   - Display the *NSKK Debug* buffer
;;   `nskk-debug-clear'  - Clear all entries from the debug buffer
;;
;; Layer Synchronization:
;;
;;   When `nskk-debug-enabled' is changed, the following layer-specific
;;   debug flags are automatically synchronized:
;;
;;   - `nskk-architecture-enable-debug' (Architecture layer)
;;   - `nskk-infrastructure--debug-enabled' (Infrastructure layer)
;;   - `nskk-data--debug-enabled' (Data layer)
;;
;;   This ensures consistent debug output across all NSKK layers.

;;; Code:

(require 'cl-lib)

;;;; Customization Group

(defgroup nskk-debug nil
  "NSKK debugging configuration."
  :prefix "nskk-debug-"
  :group 'nskk)

;;;; Customization Variables

(defun nskk-debug--sync-layer-flags (enabled)
  "Sync ENABLED state with existing layer debug flags."
  (when (boundp 'nskk-architecture-enable-debug)
    (setq nskk-architecture-enable-debug enabled))
  (when (boundp 'nskk-infrastructure--debug-enabled)
    (setq nskk-infrastructure--debug-enabled enabled))
  (when (boundp 'nskk-data--debug-enabled)
    (setq nskk-data--debug-enabled enabled)))

(defun nskk-debug--set-enabled (symbol value)
  "Setter for nskk-debug-enabled.
SYMBOL is the variable being set, VALUE is the new value."
  (set-default-toplevel-value symbol value)
  (nskk-debug--sync-layer-flags value))

(defcustom nskk-debug-enabled nil
  "Whether NSKK debug mode is enabled.
When enabled, debug messages are logged to the *NSKK Debug* buffer.
Setting this also syncs with layer-specific debug flags."
  :type 'boolean
  :set #'nskk-debug--set-enabled
  :group 'nskk-debug)

(defcustom nskk-debug-max-entries 1000
  "Maximum number of log entries before trimming the debug buffer."
  :type 'integer
  :group 'nskk-debug)

;;;; Logging Macro

(defmacro nskk-debug-log (format-string &rest args)
  "Log message if debug enabled. Zero overhead when disabled.
FORMAT-STRING is passed to `format' with ARGS.
Usage: (nskk-debug-log \"Processing: %s\" value)"
  (declare (debug (stringp body)))
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
  (nskk-debug--sync-layer-flags nskk-debug-enabled)
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
;; The :set handler couldn't run because the function didn't exist yet
(when (bound-and-true-p nskk-debug-enabled)
  (nskk-debug--sync-layer-flags t)
  (nskk-debug--append "NSKK debug module loaded (debug was pre-enabled)"))

(provide 'nskk-debug)

;;; nskk-debug.el ends here
