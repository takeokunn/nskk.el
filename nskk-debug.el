;;; nskk-debug.el --- NSKK unified debug mode -*- lexical-binding: t; -*-

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
;; near-zero runtime overhead when disabled.  The `nskk-debug-log' macro
;; expands to a `when' guard; when `nskk-debug-enabled' is nil the guard
;; short-circuits and arguments are never evaluated.  The
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
;;
;; Log messages from other modules use a bracketed category prefix:
;;   [INPUT]  -- nskk-input.el  (mode switching, romaji routing, kana conversion)
;;   [HENKAN] -- nskk-henkan.el (candidate search, commit, registration)
;;   [SEARCH] -- nskk-search.el (dictionary lookup, cache, learning)

;;; Code:

(require 'nskk-custom)

;;;; Data

(defconst nskk-debug--buffer-name "*NSKK Debug*"
  "Name of the NSKK debug buffer.")

(defconst nskk-debug--timestamp-format "%H:%M:%S.%3N"
  "Format string for debug log timestamps.")

(defconst nskk-debug--message-format "[%s] %s\n"
  "Format string for a complete debug log entry.
The first %s is the timestamp, the second %s is the message text.")

;;;; Logging Macro

(defmacro nskk-debug-log (format-string &rest args)
  "Log message with a timestamp if debug is enabled.
FORMAT-STRING is passed to `format' with ARGS.
Expands to a runtime `when' guard; overhead when disabled is one
`bound-and-true-p' test per call site.
Usage: (nskk-debug-log \"Processing: %s\" value)"
  (declare (indent 0) (debug t))
  `(when (bound-and-true-p nskk-debug-enabled)
     (nskk-debug--append (format ,format-string ,@args))))

;;;; Convenience Function

(defun nskk-debug-message (format-string &rest args)
  "Log a debug message, safe for use in all contexts.
FORMAT-STRING and ARGS are passed to `format'.
Unlike `nskk-debug-log', this is a first-class function: it can be
stored, passed to `apply', or called from inside `condition-case' forms
where signaling a secondary error must be avoided.  A `condition-case'
handler absorbs any format error and issues a warning instead.
Note: unlike the macro, arguments are always evaluated even when
debug is disabled — avoid this function on hot paths."
  (when (bound-and-true-p nskk-debug-enabled)
    (let ((msg (condition-case err
                   (apply #'format format-string args)
                 (error
                  (display-warning 'nskk
                                   (format "Debug logging error: %s" (error-message-string err))
                                   :warning)
                  nil))))
      (when msg (nskk-debug--append msg)))))

;;;; Buffer Management

(defun nskk-debug--buffer ()
  "Get or create the NSKK debug buffer."
  (or (get-buffer nskk-debug--buffer-name)
      (let ((buffer (get-buffer-create nskk-debug--buffer-name)))
        (with-current-buffer buffer
          (special-mode))
        buffer)))

(defun nskk-debug--append (message)
  "Append MESSAGE to the debug buffer with a timestamp prefix."
  (with-current-buffer (nskk-debug--buffer)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert (format nskk-debug--message-format
                      (format-time-string nskk-debug--timestamp-format)
                      message))
      (nskk-debug--trim))))

(defun nskk-debug--trim ()
  "Trim the debug buffer to at most `nskk-debug-max-entries' lines.
Must be called with the debug buffer current.
Positions point at `point-max', steps backward by max-entries newlines,
then deletes everything before that position."
  (when (> (buffer-size) 0)
    (save-excursion
      (goto-char (point-max))
      (forward-line (- nskk-debug-max-entries))
      (when (> (point) (point-min))
        (let ((inhibit-read-only t))
          (delete-region (point-min) (point)))))))

;;;; Interactive Commands

;;;###autoload
(defun nskk-debug-toggle ()
  "Toggle NSKK debug mode."
  (interactive)
  (setq nskk-debug-enabled (not nskk-debug-enabled))
  (message "NSKK debug mode %s"
           (if nskk-debug-enabled "enabled" "disabled")))

;;;###autoload
(defun nskk-debug-show ()
  "Show the NSKK debug buffer."
  (interactive)
  (display-buffer (nskk-debug--buffer)))

;;;###autoload
(defun nskk-debug-clear ()
  "Clear the NSKK debug buffer."
  (interactive)
  (when-let* ((buffer (get-buffer nskk-debug--buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer))))
  (message "NSKK debug buffer cleared"))

;;;; Provide

;; Log a startup message when debug was pre-enabled before this module loaded.
(nskk-debug-log "NSKK debug module loaded (debug was pre-enabled)")

(provide 'nskk-debug)

;;; nskk-debug.el ends here
