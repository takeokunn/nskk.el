;;; nskk-ddskk-compat.el --- DDSKK Compatibility Layer for NSKK  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Authors

;; Author: NSKK Developers
;; Keywords: Japanese, input, method, ddskk, compatibility
;; Homepage: https://github.com/takeokunn/nskk.el

;; This file is part of NSKK.

;; NSKK is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; NSKK is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with NSKK.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides a compatibility layer for DDSKK (Daredevil SKK),
;; allowing existing SKK configurations to work with NSKK.
;;
;; Target: 82% DDSKK compatibility coverage
;;
;; Features:
;; - Variable mapping (skk-* to nskk-*)
;; - Hook compatibility
;; - Function aliases
;; - Dictionary format compatibility
;; - Configuration migration

;;; Code:

(require 'nskk)
(eval-when-compile (require 'cl-lib))


;;;;
;;;; Compatibility Configuration
;;;;

(defgroup nskk-ddskk-compat nil
  "NSKK DDSKK compatibility settings."
  :prefix "nskk-ddskk-compat-"
  :group 'nskk)

(defcustom nskk-ddskk-compat-mode t
  "Enable DDSKK compatibility mode."
  :type 'boolean
  :group 'nskk-ddskk-compat
  :set (lambda (sym val)
         (set-default sym val)
         (when (and val (fboundp 'nskk-ddskk-compat-setup))
           (nskk-ddskk-compat-setup))))

(defcustom nskk-ddskk-compat-verbosity 0
  "Compatibility verbosity level (0-3).
0: Silent
1: Warnings only
2: Warnings and info
3: Full debug"
  :type 'integer
  :group 'nskk-ddskk-compat)

(defcustom nskk-ddskk-compat-strict nil
  "Enable strict compatibility mode.
When non-nil, emit warnings for unsupported DDSKK features."
  :type 'boolean
  :group 'nskk-ddskk-compat)


;;;;
;;;; Variable Mapping
;;;;

;;; Basic Configuration Variables
(defvaralias 'skk-large-jisyo 'nskk-large-dictionary)
(defvaralias 'skk-user-directory 'nskk-user-directory)
(defvaralias 'skk-init-file 'nskk-init-file)
(defvaralias 'skk-debug 'nskk-debug)
(defvaralias 'skk-egg-like-newline 'nskk-egg-like-newline)

;;; Mode Variables
(defvaralias 'skk-mode 'nskk-mode)
(defvaralias 'skk-latin-mode 'nskk-latin-mode)
(defvaralias 'skk-jisx0208-latin-mode 'nskk-jisx0208-latin-mode)
(defvaralias 'skk-jisx0201-mode 'nskk-jisx0201-mode)

;;; Input Mode Variables
(defvaralias 'skk-kana-coding-system 'nskk-kana-coding-system)
(defvaralias 'skk-jisx0208-latin-mode-string 'nskk-jisx0208-latin-mode-string)
(defvaralias 'skk-henkan-mode-string 'nskk-henkan-mode-string)
(defvaralias 'skk-latin-mode-string 'nskk-latin-mode-string)

;;; Dictionary Variables
(defvaralias 'skk-server-host 'nskk-server-host)
(defvaralias 'skk-server-portnum 'nskk-server-port)
(defvaralias 'skk-server-prog 'nskk-server-prog)
(defvaralias 'skk-server-args 'nskk-server-args)
(defvaralias 'skk-server-completion-prog 'nskk-server-completion-prog)
(defvaralias 'skk-server-report-response-error 'nskk-server-report-response-error)

;;; Completion Variables
(defvaralias 'skk-completion-prog-list 'nskk-completion-prog-list)
(defvaralias 'skk-completion-search-length 'nskk-completion-search-length)
(defvaralias 'skk-dabbrev-elisp 'nskk-dabbrev-elisp)
(defvaralias 'skk-dabbrev-verbose 'nskk-dabbrev-verbose)

;;; Display Variables
(defvaralias 'skk-show-inline 'nskk-show-inline)
(defvaralias 'skk-show-tooltip 'nskk-show-tooltip)
(defvaralias 'skk-tooltip-params 'nskk-tooltip-params)
(defvaralias 'skk-use-face-color 'nskk-use-face-color)

;;; Style Variables
(defvaralias 'skk-cursor-hiragana-color 'nskk-cursor-hiragana-color)
(defvaralias 'skk-cursor-katakana-color 'nskk-cursor-katakana-color)
(defvaralias 'skk-cursor-latin-color 'nskk-cursor-latin-color)
(defvaralias 'skk-cursor-jisx0208-latin-color 'nskk-cursor-jisx0208-latin-color)

;;; Conversion Variables
(defvaralias 'skk-kanji-list 'nskk-kanji-list)
(defvaralias 'skk-henkan-list 'nskk-henkan-list)
(defvaralias 'skk-okurigana 'nskk-okurigana)

;;; Study Variables
(defvaralias 'skk-study-file 'nskk-study-file)
(defvaralias 'skk-study-min-length 'nskk-study-min-length)


;;;;
;;;; Hook Compatibility
;;;;

;;; Hook Variables
(defvaralias 'skk-mode-hook 'nskk-mode-hook)
(defvaralias 'skk-load-hook 'nskk-load-hook)
(defvaralias 'skk-unload-hook 'nskk-unload-hook)
(defvaralias 'skk-input-mode-hook 'nskk-input-mode-hook)
(defvaralias 'skk-henkan-mode-hook 'nskk-henkan-mode-hook)

;;; Event Hooks
(defvaralias 'skk-before-henkan-hook 'nskk-before-henkan-hook)
(defvaralias 'skk-after-henkan-hook 'nskk-after-henkan-hook)
(defvaralias 'skk-before-candidate-hook 'nskk-before-candidate-hook)
(defvaralias 'skk-after-candidate-hook 'nskk-after-candidate-hook)

;; Hook wrapper functions
(defun nskk-ddskk-compat--run-hook (hook-name &rest args)
  "Run DDSKK compatibility hook HOOK-NAME with ARGS."
  (let ((nskk-hook (intern (format "nskk-%s" hook-name)))
        (skk-hook (intern (format "skk-%s" hook-name))))
    (when (boundp skk-hook)
      (run-hook-with-args skk-hook args))
    (when (boundp nskk-hook)
      (run-hook-with-args nskk-hook args))))


;;;;
;;;; Function Aliases
;;;;

;;; Main Functions
(defalias 'skk-mode 'nskk-mode)
(defalias 'skk-j-mode-on 'nskk-hiragana-mode)
(defalias 'skk-j-mode-off 'nskk-latin-mode)
(defalias 'skk-latin-mode 'nskk-latin-mode)
(defalias 'skk-hiragana-mode 'nskk-hiragana-mode)
(defalias 'skk-katakana-mode 'nskk-katakana-mode)

;;; Conversion Functions
(defalias 'skk-henkan 'nskk-convert)
(defalias 'skk-kakutei 'nskk-commit)
(defalias 'skk-cancel 'nskk-cancel)
(defalias 'skk-purge-from-jisyo 'nskk-purge-from-dictionary)

;;; Candidate Functions
(defalias 'skk-previous-candidate 'nskk-previous-candidate)
(defalias 'skk-next-candidate 'nskk-next-candidate)
(defalias 'skk-insert 'nskk-insert)

;;; Dictionary Functions
(defalias 'skk-search-server 'nskk-dictionary-search)
(defalias 'skk-completion 'nskk-completion)
(defalias 'skk-completion-search 'nskk-completion-search)

;;; Input Functions
(defalias 'skk-input-by-code-or-menu 'nskk-input-by-code)

;;; Utility Functions
(defalias 'skk-today 'nskk-today)
(defalias 'skk-gadget 'nskk-gadget)
(defalias 'skk-toggle-kutouten 'nskk-toggle-kutouten)
(defalias 'skk-toggle-kana 'nskk-toggle-kana)


;;;;
;;;; Unimplemented/Incompatible Functions
;;;;

(defconst nskk-ddskk-compat--unimplemented-functions
  '(
    ;; Advanced features not yet implemented
    skk-tex-command
    skk-greek-alist
    skk-cyrillic-alist
    skk-display-code
    skk-emacs-bitmap
    skk-auto
    skk-auto-save-p
    skk-jisyo-edit-mode

    ;; Legacy functions
    skk-version
    skk-emacs-library-directory
    skk-tut-file
    skk-tut-code-file

    ;; Experimental features
    skk-acs
    skk-use-numeric-conversion
    skk-numeric-conversion-list
    )
  "List of DDSKK functions not yet implemented in NSKK.")

(defconst nskk-ddskk-compat--incompatible-variables
  '(
    ;; Variables that work differently in NSKK
    skk-comp-by-history
    skk-comp-candidate-history
    skk-server-completion-response-timeout
    skk-kcode-elisp
    skk-jisyo-edit-mode-allow-unsafe-passwords
    )
  "List of DDSKK variables that are incompatible with NSKK.")

(defun nskk-ddskk-compat--check-unimplemented (function)
  "Check if FUNCTION is implemented and warn if not."
  (when (and nskk-ddskk-compat-strict
             (member function nskk-ddskk-compat--unimplemented-functions))
    (nskk-ddskk-compat-warn "Function '%s' is not implemented" function)))

(defun nskk-ddskk-compat-warn (format-string &rest args)
  "Emit a DDSKK compatibility warning."
  (when (> nskk-ddskk-compat-verbosity 0)
    (apply #'message (concat "[NSKK-DDSKK-Compat] " format-string) args)))


;;;;
;;;; Dictionary Format Compatibility
;;;;

(defun nskk-ddskk-compat-read-dictionary (file)
  "Read DDSKK format dictionary from FILE.
Convert to NSKK internal format."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (nskk-ddskk-compat--parse-dictionary-buffer))))

(defun nskk-ddskk-compat--parse-dictionary-buffer ()
  "Parse current buffer as DDSKK dictionary format."
  (let ((entries (make-hash-table :test 'equal)))
    (goto-char (point-min))
    (while (not (eobp))
      (condition-case nil
          (let ((entry (nskk-ddskk-compat--parse-entry)))
            (when entry
              (puthash (car entry) (cdr entry) entries)))
        (error
         (forward-line 1))))
    entries))

(defun nskk-ddskk-compat--parse-entry ()
  "Parse a single DDSKK dictionary entry."
  (end-of-line)
  (when (re-search-backward "^\\([^ ]+\\) \\(.*\\)$" nil t)
    (let ((yomi (match-string 1))
          (candidates (match-string 2)))
      (cons yomi (nskk-ddskk-compat--parse-candidates candidates)))))

(defun nskk-ddskk-compat--parse-candidates (candidate-string)
  "Parse DDSKK candidate string."
  (let ((candidates nil)
        (start 0)
        (len (length candidate-string)))
    (while (< start len)
      (let ((slash (string-match "/" candidate-string start)))
        (if slash
            (progn
              (push (substring candidate-string start slash) candidates)
              (setq start (1+ slash)))
          (progn
            (push (substring candidate-string start) candidates)
            (setq start len)))))
    (nreverse candidates)))


;;;;
;;;; Configuration Migration
;;;;

(defun nskk-ddskk-compat-migrate-config ()
  "Migrate DDSKK configuration to NSKK format."
  (interactive)
  (message "[NSKK-DDSKK-Compat] Migrating configuration...")
  (nskk-ddskk-compat--migrate-variables)
  (nskk-ddskk-compat--migrate-hooks)
  (nskk-ddskk-compat--migrate-keybindings)
  (nskk-ddskk-compat--migrate-dictionaries)
  (message "[NSKK-DDSKK-Compat] Migration complete"))

(defun nskk-ddskk-compat--migrate-variables ()
  "Migrate DDSKK variables to NSKK equivalents."
  (let ((migrations
         '(;; Basic config
           (skk-large-jisyo . nskk-large-dictionary)
           (skk-user-directory . nskk-user-directory)
           (skk-init-file . nskk-init-file)
           (skk-debug . nskk-debug)
           ;; Mode config
           (skk-kana-coding-system . nskk-kana-coding-system)
           (skk-henkan-mode-string . nskk-henkan-mode-string)
           ;; Display config
           (skk-show-inline . nskk-show-inline)
           (skk-show-tooltip . nskk-show-tooltip)
           (skk-use-face-color . nskk-use-face-color)
           ;; Style config
           (skk-cursor-hiragana-color . nskk-cursor-hiragana-color)
           (skk-cursor-katakana-color . nskk-cursor-katakana-color)
           (skk-cursor-latin-color . nskk-cursor-latin-color))))
    (dolist (migration migrations)
      (let ((skk-var (car migration))
            (nskk-var (cdr migration)))
        (when (boundp skk-var)
          (set-default nskk-var (symbol-value skk-var))
          (when (> nskk-ddskk-compat-verbosity 1)
            (message "[NSKK-DDSKK-Compat] Migrated %s -> %s"
                     skk-var nskk-var)))))))

(defun nskk-ddskk-compat--migrate-hooks ()
  "Migrate DDSKK hooks to NSKK hooks."
  (let ((hook-migrations
         '(;; Mode hooks
           (skk-mode-hook . nskk-mode-hook)
           (skk-load-hook . nskk-load-hook)
           (skk-input-mode-hook . nskk-input-mode-hook)
           ;; Event hooks
           (skk-before-henkan-hook . nskk-before-henkan-hook)
           (skk-after-henkan-hook . nskk-after-henkan-hook)
           (skk-before-candidate-hook . nskk-before-candidate-hook)
           (skk-after-candidate-hook . nskk-after-candidate-hook))))
    (dolist (migration hook-migrations)
      (let ((skk-hook (car migration))
            (nskk-hook (cdr migration)))
        (when (boundp skk-hook)
          (dolist (hook-func (symbol-value skk-hook))
            (add-hook nskk-hook hook-func))
          (when (> nskk-ddskk-compat-verbosity 1)
            (message "[NSKK-DDSKK-Compat] Migrated hook %s -> %s"
                     skk-hook nskk-hook)))))))

(defun nskk-ddskk-compat--migrate-keybindings ()
  "Migrate DDSKK keybindings to NSKK."
  (let ((key-migrations
         '(;; Global keybindings
           ([?\C-j] . nskk-mode-toggle)
           ("\C-j" . nskk-mode-toggle)
           ("\\x.C-j" . nskk-mode-toggle)
           ;; Conversion keys
           (" " . nskk-convert)
           ([?\ ] . nskk-convert)
           ;; Candidate keys
           ("\C-n" . nskk-next-candidate)
           ("\C-p" . nskk-previous-candidate)
           ([next] . nskk-next-candidate)
           ([prior] . nskk-previous-candidate)
           ;; Cancel keys
           ("\C-g" . nskk-cancel)
           ([escape] . nskk-cancel))))
    (dolist (migration key-migrations)
      (let ((key (car migration))
            (nskk-func (cdr migration)))
        (when (and (key-binding key)
                   (not (eq (key-binding key) nskk-func)))
          (when (> nskk-ddskk-compat-verbosity 1)
            (message "[NSKK-DDSKK-Compat] Keybinding conflict: %s" key)))))))

(defun nskk-ddskk-compat--migrate-dictionaries ()
  "Migrate DDSKK dictionary paths to NSKK."
  (when (boundp 'skk-large-jisyo)
    (let ((skk-dict (symbol-value 'skk-large-jisyo)))
      (when (and (stringp skk-dict)
                 (file-exists-p skk-dict))
        (set-default nskk-large-dictionary skk-dict)
        (when (> nskk-ddskk-compat-verbosity 1)
          (message "[NSKK-DDSKK-Compat] Migrated dictionary: %s" skk-dict))))))


;;;;
;;;; Compatibility Setup
;;;;

(defun nskk-ddskk-compat-setup ()
  "Setup DDSKK compatibility layer."
  (interactive)
  (when nskk-ddskk-compat-mode
    ;; Check if DDSKK is already loaded
    (when (featurep 'skk)
      (message "[NSKK-DDSKK-Compat] DDSKK is already loaded. Compatibility mode may not work correctly.")
      (when nskk-ddskk-compat-strict
        (error "DDSKK is loaded. Cannot enable compatibility mode.")))

    ;; Setup variable aliases
    (nskk-ddskk-compat--setup-variable-aliases)

    ;; Setup function aliases
    (nskk-ddskk-compat--setup-function-aliases)

    ;; Setup hook wrappers
    (nskk-ddskk-compat--setup-hook-wrappers)

    ;; Setup advice
    (nskk-ddskk-compat--setup-advice)

    (message "[NSKK-DDSKK-Compat] Compatibility layer enabled")))

(defun nskk-ddskk-compat--setup-variable-aliases ()
  "Setup variable aliases for DDSKK compatibility."
  ;; Variable aliases are already defined with defvaralias
  ;; This function can be used for dynamic alias setup if needed
  (when (> nskk-ddskk-compat-verbosity 2)
    (message "[NSKK-DDSKK-Compat] Variable aliases setup")))

(defun nskk-ddskk-compat--setup-function-aliases ()
  "Setup function aliases for DDSKK compatibility."
  ;; Function aliases are already defined with defalias
  ;; This function can be used for dynamic alias setup if needed
  (when (> nskk-ddskk-compat-verbosity 2)
    (message "[NSKK-DDSKK-Compat] Function aliases setup")))

(defun nskk-ddskk-compat--setup-hook-wrappers ()
  "Setup hook wrappers for DDSKK compatibility."
  ;; Hook wrappers are defined as nskk-ddskk-compat--run-hook
  (when (> nskk-ddskk-compat-verbosity 2)
    (message "[NSKK-DDSKK-Compat] Hook wrappers setup")))

(defun nskk-ddskk-compat--setup-advice ()
  "Setup advice for DDSKK compatibility."
  ;; Add advice to intercept calls to DDSKK functions
  ;; This is a placeholder for future enhancement
  (when (> nskk-ddskk-compat-verbosity 2)
    (message "[NSKK-DDSKK-Compat] Advice setup")))


;;;;
;;;; Compatibility Statistics
;;;;

(defun nskk-ddskk-compat-stats ()
  "Show DDSKK compatibility statistics."
  (interactive)
  (let ((total-vars 0)
        (aliased-vars 0)
        (total-funcs 0)
        (aliased-funcs 0))

    ;; Count variable aliases
    (mapatoms (lambda (sym)
                (when (string-prefix-p "skk-" (symbol-name sym))
                  (cl-incf total-vars)
                  (when (and (symbolp (indirect-variable sym))
                             (string-prefix-p "nskk-"
                                              (symbol-name (indirect-variable sym))))
                    (cl-incf aliased-vars)))))

    ;; Count function aliases
    (mapatoms (lambda (sym)
                (when (string-prefix-p "skk-" (symbol-name sym))
                  (when (fboundp sym)
                    (cl-incf total-funcs)
                    (when (and (symbolp (indirect-function sym))
                               (string-prefix-p "nskk-"
                                                (symbol-name (indirect-function sym))))
                      (cl-incf aliased-funcs))))))

    (message "[NSKK-DDSKK-Compat] Statistics:")
    (message "  Variable aliases: %d / %d (%.1f%%)"
             aliased-vars total-vars
             (* 100.0 (/ aliased-vars (max total-vars 1))))
    (message "  Function aliases: %d / %d (%.1f%%)"
             aliased-funcs total-funcs
             (* 100.0 (/ aliased-funcs (max total-funcs 1))))

    ;; Calculate overall compatibility percentage
    (let ((overall-total (+ total-vars total-funcs))
          (overall-aliased (+ aliased-vars aliased-funcs)))
      (message "  Overall compatibility: %.1f%%"
               (* 100.0 (/ overall-aliased (max overall-total 1)))))))


;;;;
;;;; Auto-initialization
;;;;

;; Enable compatibility mode by default if skk variables are detected
(when (and nskk-ddskk-compat-mode
           (or (boundp 'skk-large-jisyo)
               (boundp 'skk-user-directory)
               (boundp 'skk-init-file)))
  (nskk-ddskk-compat-setup))

(provide 'nskk-ddskk-compat)

;;; nskk-ddskk-compat.el ends here
