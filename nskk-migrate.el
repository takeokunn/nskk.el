;;; nskk-migrate.el --- Migration Tools from DDSKK to NSKK  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Authors

;; Author: NSKK Developers
;; Keywords: Japanese, input, method, migration, ddskk
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

;; This file provides migration tools to help users transition from
;; DDSKK to NSKK. It handles configuration migration, dictionary
;; conversion, and provides guidance for the migration process.
;;
;; Features:
;; - Configuration file migration
;; - Dictionary format conversion
;; - Interactive migration wizard
;; - Backup and rollback support
;; - Migration reporting

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (declare-function nskk-ddskk-compat-setup "nskk-ddskk-compat"))

(require 'nskk)
(require 'nskk-ddskk-compat)

(defvar nskk-user-directory (expand-file-name "~/.nskk/")
  "Directory for NSKK user data files.")

;;;;
;;;; Migration Configuration
;;;;

(defgroup nskk-migration nil
  "NSKK migration settings."
  :prefix "nskk-migrate-"
  :group 'nskk)

(defcustom nskk-migrate-backup-directory
  (expand-file-name "nskk-backups" user-emacs-directory)
  "Directory to store DDSKK backups during migration."
  :type 'directory
  :group 'nskk-migration)

(defcustom nskk-migrate-interactive t
  "Enable interactive migration prompts."
  :type 'boolean
  :group 'nskk-migration)

(defcustom nskk-migrate-verbose t
  "Enable verbose migration output."
  :type 'boolean
  :group 'nskk-migration)

(defcustom nskk-migrate-dry-run nil
  "When non-nil, perform a dry run without making changes."
  :type 'boolean
  :group 'nskk-migration)


;;;;
;;;; Migration State
;;;;

(defvar nskk--migrate-state nil
  "Current migration state.")

(cl-defstruct nskk-migrate-state
  "Migration state container."
  (session-id nil :read-only t)
  (start-time nil :read-only t)
  (end-time nil)
  (backup-location nil)
  (migrated-files nil :type list)
  (migrated-variables nil :type list)
  (migrated-dictionaries nil :type list)
  (warnings nil :type list)
  (errors nil :type list)
  (log nil :type list))


;;;;
;;;; Security Functions
;;;;

(defun nskk-migrate--validate-path (path)
  "Validate PATH is safe for file operations.
Returns t if path is valid, nil otherwise.
Prevents path traversal attacks by ensuring:
- Path is absolute or within user directories
- No path traversal sequences (..)
- Path is within expected boundaries"
  (and (stringp path)
       (> (length path) 0)
       ;; Check for path traversal attempts
       (not (string-match-p "\\.\\." path))
       ;; Ensure path is absolute or in user-emacs-directory
       (or (file-name-absolute-p path)
           (string-prefix-p user-emacs-directory path))
       ;; Check if path exists (for source files)
       (file-exists-p path)))

(defun nskk-migrate--validate-dict-path (dict-path)
  "Validate DICT-PATH is an allowed dictionary location.
Returns t if path is in allowlist, nil otherwise.
Allowlist includes:
- User's Emacs directory
- System-wide SKK dictionary locations
- NSKK user directory"
  (and (stringp dict-path)
       (> (length dict-path) 0)
       (nskk-migrate--validate-path dict-path)
       ;; Check against allowlist
       (or (string-prefix-p user-emacs-directory dict-path)
           (string-prefix-p nskk-user-directory dict-path)
           ;; Common system dictionary locations
           (string-prefix-p "/usr/share/skk/" dict-path)
           (string-prefix-p "/usr/local/share/skk/" dict-path)
           (string-prefix-p "/opt/homebrew/share/skk/" dict-path)
           (string-prefix-p (expand-file-name "~/.local/share/skk/") dict-path))
       ;; Must be a .jisyo file
       (string-match-p "\\.jisyo\\'" dict-path)))

;;;;
;;;; Migration Functions
;;;;

(defun nskk-migrate-start ()
  "Start a new migration session."
  (interactive)
  (when (or (not nskk-migrate-interactive)
            (yes-or-no-p "Start migration from DDSKK to NSKK? "))
    (setq nskk--migrate-state
          (make-nskk-migrate-state
           :session-id (format "nskk-migrate-%s"
                              (format-time-string "%Y%m%d-%H%M%S"))
           :start-time (current-time)))
    (message "[NSKK Migrate] Migration session started: %s"
             (nskk-migrate-state-session-id nskk--migrate-state))
    (nskk-migrate-run)))

(defun nskk-migrate-run ()
  "Run the full migration process."
  (interactive)
  (unless nskk--migrate-state
    (nskk-migrate-start))

  (message "[NSKK Migrate] Running migration...")

  ;; Step 1: Backup
  (nskk-migrate-backup)

  ;; Step 2: Migrate configuration
  (nskk-migrate-config)

  ;; Step 3: Migrate dictionaries
  (nskk-migrate-dictionaries)

  ;; Step 4: Generate report
  (nskk-migrate-report)

  ;; Step 5: Cleanup
  (setf (nskk-migrate-state-end-time nskk--migrate-state) (current-time))

  (message "[NSKK Migrate] Migration complete"))


;;;;
;;;; Backup Functions
;;;;

(defun nskk-migrate-backup ()
  "Create backup of DDSKK configuration and dictionaries."
  (interactive)
  (unless nskk--migrate-state
    (error "No active migration session"))

  (when nskk-migrate-verbose
    (message "[NSKK Migrate] Creating backup..."))

  ;; Create backup directory
  (let* ((backup-dir
          (expand-file-name
           (nskk-migrate-state-session-id nskk--migrate-state)
           nskk-migrate-backup-directory))
         (_backup-timestamp (current-time)))

    (unless nskk-migrate-dry-run
      (make-directory backup-dir t))

    (setf (nskk-migrate-state-backup-location nskk--migrate-state) backup-dir)

    ;; Backup configuration file
    (when (boundp 'skk-init-file)
      (let ((skk-init (symbol-value 'skk-init-file)))
        (when (and skk-init (file-exists-p skk-init))
          (nskk-migrate--backup-file skk-init backup-dir))))

    ;; Backup user directory
    (when (boundp 'skk-user-directory)
      (let ((skk-user-dir (symbol-value 'skk-user-directory)))
        (when (and skk-user-dir (file-directory-p skk-user-dir))
          (nskk-migrate--backup-directory skk-user-dir backup-dir))))

    ;; Backup dictionaries
    (when (boundp 'skk-large-jisyo)
      (let ((skk-dict (symbol-value 'skk-large-jisyo)))
        (when (and skk-dict (file-exists-p skk-dict))
          (nskk-migrate--backup-file skk-dict backup-dir))))

    (when nskk-migrate-verbose
      (message "[NSKK Migrate] Backup created at: %s" backup-dir))))

(defun nskk-migrate--backup-file (file target-dir)
  "Backup FILE to TARGET-DIR."
  (when (nskk-migrate--validate-path file)
    (unless nskk-migrate-dry-run
      (copy-file file
                 (expand-file-name (file-name-nondirectory file) target-dir)
                 t))))

(defun nskk-migrate--backup-directory (dir target-dir)
  "Backup DIR to TARGET-DIR."
  (when (nskk-migrate--validate-path dir)
    (unless nskk-migrate-dry-run
      (copy-directory dir
                     (expand-file-name (file-name-nondirectory dir)
                                       target-dir)
                     t t))))


;;;;
;;;; Configuration Migration
;;;;

(defconst nskk--migrate-variable-map
  '((skk-large-jisyo . nskk-large-dictionary)
    (skk-user-directory . nskk-user-directory)
    (skk-init-file . nskk-init-file)
    (skk-debug . nskk-debug)
    (skk-kana-coding-system . nskk-kana-coding-system)
    (skk-henkan-mode-string . nskk-henkan-mode-string)
    (skk-latin-mode-string . nskk-latin-mode-string)
    (skk-jisx0208-latin-mode-string . nskk-jisx0208-latin-mode-string)
    (skk-show-inline . nskk-show-inline)
    (skk-show-tooltip . nskk-show-tooltip)
    (skk-use-face-color . nskk-use-face-color)
    (skk-cursor-hiragana-color . nskk-cursor-hiragana-color)
    (skk-cursor-katakana-color . nskk-cursor-katakana-color)
    (skk-cursor-latin-color . nskk-cursor-latin-mode-color)
    (skk-server-host . nskk-server-host)
    (skk-server-portnum . nskk-server-port)
    (skk-study-file . nskk-study-file)
    (skk-study-min-length . nskk-study-min-length))
  "Map DDSKK variables to NSKK equivalents.")

(defun nskk-migrate-config ()
  "Migrate DDSKK configuration to NSKK format."
  (interactive)
  (unless nskk--migrate-state
    (error "No active migration session"))

  (when nskk-migrate-verbose
    (message "[NSKK Migrate] Migrating configuration..."))

  (let* ((migrated-vars nil)
         (nskk-config-file
          (expand-file-name "nskk-config.el" nskk-user-directory)))
    ;; Create NSKK user directory if needed
    (unless nskk-migrate-dry-run
      (make-directory nskk-user-directory t))

    ;; Collect DDSKK configuration
    (let ((config-lines nil))

      ;; Migrate basic configuration
      (dolist (var nskk--migrate-variable-map)
        (let ((skk-var (car var))
              (nskk-var (cdr var)))
          (when (and (boundp skk-var)
                     (not (equal (symbol-value skk-var)
                                 (default-value skk-var))))
            (let ((value (symbol-value skk-var)))
              (push (nskk-migrate--format-variable nskk-var value)
                    config-lines)
              (push skk-var migrated-vars)))))

      ;; Migrate customizations
      (when (boundp 'skk-global-map)
        (push (nskk-migrate--format-keybindings)
              config-lines))

      ;; Write configuration file
      (unless nskk-migrate-dry-run
        (with-temp-file nskk-config-file
          (insert ";; NSKK Configuration (migrated from DDSKK)\n")
          (insert (format ";; Generated: %s\n"
                         (format-time-string "%Y-%m-%d %H:%M:%S")))
          (insert "\n")
          (dolist (line (nreverse config-lines))
            (insert line)
            (insert "\n"))))

      (setf (nskk-migrate-state-migrated-variables nskk--migrate-state)
            migrated-vars))

    (when nskk-migrate-verbose
      (message "[NSKK Migrate] Configuration written to: %s"
               nskk-config-file))))

(defun nskk-migrate--format-variable (var value)
  "Format variable assignment for NSKK config.
VAR must be a symbol. Use %S to safely quote the variable name."
  (format "(setq %S %S)" (symbol-name var) value))

(defun nskk-migrate--format-keybindings ()
  "Format keybinding migrations for NSKK config."
  ";; Keybinding migration
;; Please customize keybindings in nskk-keymap.el")


;;;;
;;;; Dictionary Migration
;;;;

(defun nskk-migrate-dictionaries ()
  "Migrate DDSKK dictionaries to NSKK format."
  (interactive)
  (unless nskk--migrate-state
    (error "No active migration session"))

  (when nskk-migrate-verbose
    (message "[NSKK Migrate] Migrating dictionaries..."))

  (let ((migrated-dicts nil))

    ;; Migrate main dictionary
    (when (boundp 'skk-large-jisyo)
      (let ((skk-dict (symbol-value 'skk-large-jisyo)))
        (when (and skk-dict (file-exists-p skk-dict))
          (when (nskk-migrate--convert-dictionary skk-dict)
            (push skk-dict migrated-dicts)))))

    ;; Migrate user dictionary
    (when (boundp 'skk-user-directory)
      (let ((user-dir (symbol-value 'skk-user-directory)))
        (when (and user-dir (file-directory-p user-dir))
          (dolist (dict-file (directory-files user-dir nil "\\.jisyo\\'"))
            (let ((full-path (expand-file-name dict-file user-dir)))
              (when (nskk-migrate--convert-dictionary full-path)
                (push full-path migrated-dicts)))))))

    (setf (nskk-migrate-state-migrated-dictionaries nskk--migrate-state)
          migrated-dicts)

    (when nskk-migrate-verbose
      (message "[NSKK Migrate] Migrated %d dictionaries"
               (length migrated-dicts)))))

(defun nskk-migrate--convert-dictionary (dict-file)
  "Convert DDSKK dictionary DICT-FILE to NSKK format."
  ;; Validate dictionary path before processing
  (unless (nskk-migrate--validate-dict-path dict-file)
    (error "Dictionary path not in allowlist: %s" dict-file))

  (let* ((nskk-dict-name
          (concat (file-name-sans-extension
                   (file-name-nondirectory dict-file))
                  ".eldict"))
         (nskk-dict-path
          (expand-file-name nskk-dict-name nskk-user-directory)))

    (when nskk-migrate-verbose
      (message "[NSKK Migrate] Converting: %s -> %s"
               dict-file nskk-dict-path))

    (unless nskk-migrate-dry-run
      (condition-case err
          (nskk-migrate--convert-dictionary-file dict-file nskk-dict-path)
        (error
         (push (list "dictionary" dict-file err)
               (nskk-migrate-state-errors nskk--migrate-state))
         (message "[NSKK Migrate] Error converting %s: %s"
                  dict-file err)
         nil)
        (:success
         nskk-dict-path)))))

(defun nskk-migrate--convert-dictionary-file (input-file output-file)
  "Convert dictionary file from INPUT-FILE to OUTPUT-FILE."
  (with-temp-buffer
    (insert-file-contents input-file)
    (let ((entries (make-hash-table :test 'equal))
          (entry-count 0))

      ;; Parse DDSKK format
      (goto-char (point-min))
      (while (not (eobp))
        (condition-case nil
            (let ((entry (nskk-ddskk-compat--parse-entry)))
              (when entry
                (puthash (car entry) (cdr entry) entries)
                (cl-incf entry-count)))
          (error
           nil))
        (forward-line 1))

      ;; Write NSKK format
      (when (> entry-count 0)
        (with-temp-file output-file
          ;; NSKK dictionary format is compatible with SKK format
          ;; So we can keep the same format but ensure encoding
          (insert ";; NSKK Dictionary\n")
          (insert (format ";; Converted from: %s\n" input-file))
          (insert (format ";; Entries: %d\n" entry-count))
          (insert "\n")

          ;; Write entries
          (maphash (lambda (yomi candidates)
                     (insert (format "%s /%s/\n"
                                    yomi
                                    (mapconcat #'identity candidates "/"))))
                   entries))

        t))))


;;;;
;;;; Migration Report
;;;;

(defun nskk-migrate-report ()
  "Generate migration report."
  (interactive)
  (unless nskk--migrate-state
    (error "No active migration session"))

  (let ((report-buffer (get-buffer-create "*NSKK Migration Report*")))

    (with-current-buffer report-buffer
      (erase-buffer)
      (insert "NSKK Migration Report\n")
      (insert "--------------------\n\n")
      (insert (format "Session: %s\n"
                     (nskk-migrate-state-session-id nskk--migrate-state)))
      (insert (format "Time: %s\n"
                     (format-time-string "%Y-%m-%d %H:%M:%S")))
      (insert "\n")

      ;; Summary
      (insert "Summary:\n")
      (insert (format "  Migrated variables: %d\n"
                     (length (nskk-migrate-state-migrated-variables
                              nskk--migrate-state))))
      (insert (format "  Migrated dictionaries: %d\n"
                     (length (nskk-migrate-state-migrated-dictionaries
                              nskk--migrate-state))))
      (insert (format "  Warnings: %d\n"
                     (length (nskk-migrate-state-warnings nskk--migrate-state))))
      (insert (format "  Errors: %d\n"
                     (length (nskk-migrate-state-errors nskk--migrate-state))))
      (insert "\n")

      ;; Backup location
      (insert "Backup location:\n")
      (insert (format "  %s\n"
                     (or (nskk-migrate-state-backup-location nskk--migrate-state)
                         "None")))
      (insert "\n")

      ;; Migrated files
      (when (nskk-migrate-state-migrated-files nskk--migrate-state)
        (insert "Migrated files:\n")
        (dolist (file (nskk-migrate-state-migrated-files nskk--migrate-state))
          (insert (format "  - %s\n" file)))
        (insert "\n"))

      ;; Warnings
      (when (nskk-migrate-state-warnings nskk--migrate-state)
        (insert "Warnings:\n")
        (dolist (warning (nskk-migrate-state-warnings nskk--migrate-state))
          (insert (format "  - %s\n" warning)))
        (insert "\n"))

      ;; Errors
      (when (nskk-migrate-state-errors nskk--migrate-state)
        (insert "Errors:\n")
        (dolist (error (nskk-migrate-state-errors nskk--migrate-state))
          (insert (format "  - %s: %s\n"
                         (cadr error)
                         (caddr error))))
        (insert "\n"))

      ;; Next steps
      (insert "Next steps:\n")
      (insert "  1. Review the migration report\n")
      (insert "  2. Test NSKK with M-x nskk-mode\n")
      (insert "  3. Customize configuration if needed\n")
      (insert "  4. Remove or disable DDSKK from init file\n")
      (insert "\n")
      (insert "To rollback, restore files from backup location.\n"))

    (display-buffer report-buffer)))

(defun nskk-migrate-save-log ()
  "Save migration log to file."
  (unless nskk--migrate-state
    (error "No active migration session"))

  (let* ((log-file
          (expand-file-name
           (concat (nskk-migrate-state-session-id nskk--migrate-state) ".log")
           nskk-migrate-backup-directory)))

    (when nskk-migrate-verbose
      (message "[NSKK Migrate] Saving log to: %s" log-file))

    (unless nskk-migrate-dry-run
      (with-temp-file log-file
        (insert "NSKK Migration Log\n")
        (insert "------------------\n\n")
        (dolist (log-entry (nskk-migrate-state-log nskk--migrate-state))
          (insert log-entry)
          (insert "\n"))))))


;;;;
;;;; Interactive Wizard
;;;;

(defun nskk-migrate-wizard ()
  "Interactive migration wizard."
  (interactive)

  (message "=== NSKK Migration Wizard ===\n")
  (message "This wizard will help you migrate from DDSKK to NSKK.\n")

  ;; Step 1: Confirm
  (unless (yes-or-no-p "Continue with migration? ")
    (user-error "Migration cancelled"))

  ;; Step 2: Select migration options
  (let ((migrate-config (yes-or-no-p "Migrate configuration? "))
        (migrate-dicts (yes-or-no-p "Migrate dictionaries? "))
        (create-backup (yes-or-no-p "Create backup? ")))

    (when create-backup
      (message "Creating backup...")
      (nskk-migrate-backup))

    (when migrate-config
      (message "Migrating configuration...")
      (nskk-migrate-config))

    (when migrate-dicts
      (message "Migrating dictionaries...")
      (nskk-migrate-dictionaries))

    (message "\nMigration complete!")
    (nskk-migrate-report)))


;;;;
;;;; Rollback Functions
;;;;

(defun nskk-migrate-rollback ()
  "Rollback migration by restoring from backup."
  (interactive)
  (unless nskk--migrate-state
    (error "No migration session to rollback"))

  (let ((backup-dir (nskk-migrate-state-backup-location nskk--migrate-state)))

    (unless backup-dir
      (error "No backup available"))

    (when (or (not nskk-migrate-interactive)
              (yes-or-no-p
               (format "Rollback migration from %s? " backup-dir)))

      (when nskk-migrate-verbose
        (message "[NSKK Migrate] Rolling back migration..."))

      ;; Restore files
      (dolist (file (nskk-migrate-state-migrated-files nskk--migrate-state))
        (nskk-migrate--restore-file file backup-dir))

      (when nskk-migrate-verbose
        (message "[NSKK Migrate] Rollback complete")))))

(defun nskk-migrate--restore-file (file backup-dir)
  "Restore FILE from BACKUP-DIR."
  (let ((backup-file (expand-file-name
                      (file-name-nondirectory file)
                      backup-dir)))
    (when (file-exists-p backup-file)
      (copy-file backup-file file t t))))


;;;;
;;;; Utility Functions
;;;;

(defun nskk-migrate-log (format-string &rest args)
  "Add log entry."
  (when nskk--migrate-state
    (let ((message (apply #'format format-string args))
          (timestamp (format-time-string "%H:%M:%S")))
      (push (format "[%s] %s" timestamp message)
            (nskk-migrate-state-log nskk--migrate-state))
      (when nskk-migrate-verbose
        (message "[NSKK Migrate] %s" message)))))

(defun nskk-migrate-warn (format-string &rest args)
  "Add warning entry."
  (when nskk--migrate-state
    (let ((warning (apply #'format format-string args)))
      (push warning
            (nskk-migrate-state-warnings nskk--migrate-state))
      (nskk-migrate-log "WARNING: %s" warning))))

(defun nskk-migrate-error (format-string &rest args)
  "Add error entry."
  (when nskk--migrate-state
    (let ((error (apply #'format format-string args)))
      (push (list "error" error)
            (nskk-migrate-state-errors nskk--migrate-state))
      (nskk-migrate-log "ERROR: %s" error))))


;;;;
;;;; Diagnostics
;;;;

(defun nskk-migrate-detect-ddskk ()
  "Detect if DDSKK is installed and configured."
  (let ((ddskk-loaded (featurep 'skk))
        (ddskk-configured nil)
        (ddskk-files nil))

    ;; Check for DDSKK configuration
    (when (boundp 'skk-init-file)
      (let ((skk-init (symbol-value 'skk-init-file)))
        (when (and skk-init (file-exists-p skk-init))
          (push skk-init ddskk-files)
          (setq ddskk-configured t))))

    ;; Check for DDSKK dictionaries
    (when (boundp 'skk-large-jisyo)
      (let ((skk-dict (symbol-value 'skk-large-jisyo)))
        (when (and skk-dict (file-exists-p skk-dict))
          (push skk-dict ddskk-files))))

    (list :loaded ddskk-loaded
          :configured ddskk-configured
          :files ddskk-files)))

(defun nskk-migrate-show-status ()
  "Show DDSKK migration status."
  (interactive)
  (let ((status (nskk-migrate-detect-ddskk)))

    (message "=== DDSKK Migration Status ===\n")
    (message "DDSKK loaded: %s" (if (plist-get status :loaded) "Yes" "No"))
    (message "DDSKK configured: %s" (if (plist-get status :configured) "Yes" "No"))

    (when (plist-get status :files)
      (message "\nDDSKK files found:")
      (dolist (file (plist-get status :files))
        (message "  - %s" file)))

    (if (or (plist-get status :loaded)
            (plist-get status :configured))
        (message "\nRun M-x nskk-migrate-wizard to start migration")
      (message "\nNo DDSKK installation detected"))))

(provide 'nskk-migrate)

;;; nskk-migrate.el ends here
