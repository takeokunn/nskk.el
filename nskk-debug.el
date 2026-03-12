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
;; Layer position: L0 (Foundation) -- depends on nskk-custom, nskk-prolog,
;;   and nskk-cps-macros (all L0).
;;
;; Provides centralized debug logging to the *NSKK Debug* buffer using
;; CPS-based dispatch.  `nskk-debug-log' formats and appends a message when
;; debug is enabled; unlike the former macro variant, arguments are always
;; evaluated.  Use `(when nskk-debug-enabled (nskk-debug-log ...))' to guard
;; expensive argument expressions on hot paths.
;; `nskk-debug-message' provides a safe runtime variant with format-error
;; recovery via the CPS helper `nskk--debug-format/k'.
;;
;; Prolog predicates maintained by this module:
;; - `debug-buffer-name/1'   -- name of the *NSKK Debug* buffer
;; - `debug-timestamp-fmt/1' -- timestamp format string (%H:%M:%S.%3N)
;; - `debug-message-fmt/1'   -- entry format: "[timestamp] message\n"
;; - `debug-category/2'      -- (debug-category SYMBOL PREFIX-STRING)
;;                              maps category symbols to bracket prefixes
;;
;; Key public API:
;; - `nskk-debug-log'     -- function: log formatted message when debug enabled
;; - `nskk-debug-log/k'   -- CPS: succeed with message, fail when disabled
;; - `nskk-debug-message' -- function: log with format-error recovery
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
;; Log messages from other modules use a bracketed category prefix.
;; Query a prefix at runtime with:
;;   (nskk-prolog-query-value `(debug-category ,cat ?Prefix) '?Prefix)
;;   [INPUT]  -- nskk-input.el  (mode switching, romaji routing, kana conversion)
;;   [HENKAN] -- nskk-henkan.el (candidate search, commit, registration)
;;   [SEARCH] -- nskk-search.el (dictionary lookup, cache, learning)

;;; Code:

(require 'nskk-cps-macros)
(require 'nskk-prolog)
(require 'nskk-custom)

;;;; Prolog Facts
;;
;; Config facts for buffer/format constants.  These are the authoritative
;; source; the `defconst' forms below are derived from them via query.

(nskk-prolog-assert '((debug-buffer-name "*NSKK Debug*")))
(nskk-prolog-assert '((debug-timestamp-fmt "%H:%M:%S.%3N")))
(nskk-prolog-assert '((debug-message-fmt "[%s] %s\n")))

;; Category routing facts: hash-indexed on the category symbol (position 0)
;; for O(1) prefix lookup via `nskk-prolog-query-value'.
(nskk-prolog-set-index 'debug-category 0 :hash)
(nskk-prolog-assert '((debug-category input   "[INPUT]")))
(nskk-prolog-assert '((debug-category henkan  "[HENKAN]")))
(nskk-prolog-assert '((debug-category search  "[SEARCH]")))

;;;; Data
;;
;; Derived from the Prolog facts above; retained as `defconst' for
;; zero-cost access on hot paths and backward compatibility with tests.

(defconst nskk--debug-buffer-name
  (nskk-prolog-query-value '(debug-buffer-name \?Name) '\?Name)
  "Name of the NSKK debug buffer.")

(defconst nskk--debug-timestamp-format
  (nskk-prolog-query-value '(debug-timestamp-fmt \?Fmt) '\?Fmt)
  "Format string for debug log timestamps.")

(defconst nskk--debug-message-format
  (nskk-prolog-query-value '(debug-message-fmt \?Fmt) '\?Fmt)
  "Format string for a complete debug log entry.
The first %s is the timestamp, the second %s is the message text.")

;;;; CPS Helper: safe format with error recovery
;;
;; Explicit pair because `condition-case' is not in the CPS AST dispatch
;; table and cannot be auto-transformed by `defun/k'.

(defun nskk--debug-format/k (format-string args on-found on-not-found)
  "Format FORMAT-STRING with ARGS list.
Calls ON-FOUND with the formatted string on success.
Calls ON-NOT-FOUND on format error, after issuing a `display-warning'. [CPS]"
  (condition-case err
      (funcall on-found (apply #'format format-string args))
    (error
     (display-warning 'nskk
                      (format "Debug logging error: %s" (error-message-string err))
                      :warning)
     (funcall on-not-found))))

(defun nskk--debug-format (format-string args)
  "Format FORMAT-STRING with ARGS list.  Return nil on format error."
  (nskk--debug-format/k format-string args #'identity #'ignore))

(put 'nskk--debug-format/k 'nskk--cps-continuation-pattern :found-not-found)

;;;; Buffer Management

(defun nskk--debug-buffer ()
  "Get or create the NSKK debug buffer."
  (or (get-buffer nskk--debug-buffer-name)
      (let ((buffer (get-buffer-create nskk--debug-buffer-name)))
        (with-current-buffer buffer
          (special-mode))
        buffer)))

(defun/done nskk--debug-trim ()
  "Trim the debug buffer to at most `nskk-debug-max-entries' lines.
Must be called with the debug buffer current."
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (forward-line (- nskk-debug-max-entries))
    (when (> (point) (point-min))
      (delete-region (point-min) (point)))))

(defun/k nskk--debug-append (message)
  "Append MESSAGE to the debug buffer with a timestamp prefix."
  (with-current-buffer (nskk--debug-buffer)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert (format nskk--debug-message-format
                      (format-time-string nskk--debug-timestamp-format)
                      message))
      (nskk--debug-trim)))
  (succeed message))

;;;; Logging Functions

(defun/k nskk-debug-log (format-string &rest args)
  "Log message with a timestamp if debug is enabled.
FORMAT-STRING is passed to `format' with ARGS.
Unlike the former macro variant, arguments are always evaluated.
Use `(when nskk-debug-enabled (nskk-debug-log ...))' to guard
expensive argument expressions on hot paths.
Usage: (nskk-debug-log \"Processing: %s\" value)"
  (when (bound-and-true-p nskk-debug-enabled)
    (<- result nskk--debug-append (apply #'format format-string args))
    (succeed result)))

(defun/k nskk-debug-message (format-string &rest args)
  "Log a debug message, safe for use in all contexts.
FORMAT-STRING and ARGS are passed to `format'.
Unlike `nskk-debug-log', `nskk--debug-format/k' absorbs any format
error and issues a warning instead of signaling.
Note: arguments are always evaluated even when debug is disabled."
  (when (bound-and-true-p nskk-debug-enabled)
    (<- msg nskk--debug-format format-string args)
    (<- result nskk--debug-append msg)
    (succeed result)))

;;;; Interactive Commands

;;;###autoload
(defun/done nskk-debug-toggle ()
  "Toggle NSKK debug mode."
  :interactive t
  (setq nskk-debug-enabled (not nskk-debug-enabled))
  (message "NSKK debug mode is %s"
           (if nskk-debug-enabled "enabled" "disabled")))

;;;###autoload
(defun/done nskk-debug-show ()
  "Show the NSKK debug buffer."
  :interactive t
  (display-buffer (nskk--debug-buffer)))

;;;###autoload
(defun/done nskk-debug-clear ()
  "Clear the NSKK debug buffer."
  :interactive t
  (let ((buf (get-buffer nskk--debug-buffer-name))
        (inhibit-read-only t))
    (when buf
      (with-current-buffer buf
        (erase-buffer))))
  (message "NSKK debug buffer is cleared"))

;;;; Provide

;; Log a startup message when debug was pre-enabled before this module loaded.
(nskk-debug-log "NSKK debug module loaded (debug was pre-enabled)")

(provide 'nskk-debug)

;;; nskk-debug.el ends here
