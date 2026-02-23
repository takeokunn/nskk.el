;;; nskk-migrate-test.el --- Tests for nskk-migrate.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Authors

;; Author: NSKK Developers
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Tests for nskk-migrate.el covering:
;; - Migration state management
;; - Path validation
;; - Dictionary path validation
;; - Variable formatting
;; - Keybinding formatting
;; - DDSKK detection
;; - Logging and warning
;; - Migration state struct

;;; Code:

(require 'ert)
(require 'nskk-migrate)
(require 'nskk-test-framework)

;;;
;;; Migration State Tests
;;;

(nskk-deftest-unit migrate-state-struct-exists
  "Test that nskk-migrate-state struct is defined."
  (should (fboundp 'nskk-migrate-state-p)))

(nskk-deftest-unit migrate-state-create-basic
  "Test creating a migration state."
  (let ((state (make-nskk-migrate-state
                :session-id "test-session"
                :start-time (current-time))))
    (should (nskk-migrate-state-p state))
    (should (string= (nskk-migrate-state-session-id state) "test-session"))
    (should (nskk-migrate-state-start-time state))
    (should (null (nskk-migrate-state-end-time state)))
    (should (null (nskk-migrate-state-backup-location state)))
    (should (null (nskk-migrate-state-migrated-files state)))
    (should (null (nskk-migrate-state-migrated-variables state)))
    (should (null (nskk-migrate-state-migrated-dictionaries state)))
    (should (null (nskk-migrate-state-warnings state)))
    (should (null (nskk-migrate-state-errors state)))
    (should (null (nskk-migrate-state-log state)))))

(nskk-deftest-unit migrate-state-set-end-time
  "Test setting end time on migration state."
  (let ((state (make-nskk-migrate-state
                :session-id "test"
                :start-time (current-time))))
    (setf (nskk-migrate-state-end-time state) (current-time))
    (should (nskk-migrate-state-end-time state))))

;;;
;;; Path Validation Tests
;;;

(nskk-deftest-unit migrate-validate-path-nil
  "Test path validation with nil."
  (should (not (nskk-migrate--validate-path nil))))

(nskk-deftest-unit migrate-validate-path-empty
  "Test path validation with empty string."
  (should (not (nskk-migrate--validate-path ""))))

(nskk-deftest-unit migrate-validate-path-traversal
  "Test path validation rejects path traversal."
  (should (not (nskk-migrate--validate-path "/etc/../passwd")))
  (should (not (nskk-migrate--validate-path "~/.emacs.d/../../etc/passwd"))))

(nskk-deftest-unit migrate-validate-path-valid-absolute
  "Test path validation with valid absolute path that exists."
  ;; /tmp should exist on all systems
  (should (nskk-migrate--validate-path "/tmp")))

(nskk-deftest-unit migrate-validate-path-nonexistent
  "Test path validation with non-existent path."
  (should (not (nskk-migrate--validate-path "/nonexistent/path/does/not/exist"))))

;;;
;;; Dictionary Path Validation Tests
;;;

(nskk-deftest-unit migrate-validate-dict-path-nil
  "Test dictionary path validation with nil."
  (should (not (nskk-migrate--validate-dict-path nil))))

(nskk-deftest-unit migrate-validate-dict-path-empty
  "Test dictionary path validation with empty string."
  (should (not (nskk-migrate--validate-dict-path ""))))

(nskk-deftest-unit migrate-validate-dict-path-wrong-extension
  "Test dictionary path validation requires .jisyo extension."
  ;; Even if path is valid, must have .jisyo extension
  (should (not (nskk-migrate--validate-dict-path "/tmp/not-a-jisyo.txt"))))

;;;
;;; Variable Formatting Tests
;;;

(nskk-deftest-unit migrate-format-variable-string
  "Test formatting a variable with string value."
  (let ((result (nskk-migrate--format-variable 'nskk-test-var "value")))
    (should (stringp result))
    (should (string-match-p "setq" result))
    (should (string-match-p "nskk-test-var" result))))

(nskk-deftest-unit migrate-format-variable-number
  "Test formatting a variable with number value."
  (let ((result (nskk-migrate--format-variable 'nskk-port 1178)))
    (should (stringp result))
    (should (string-match-p "1178" result))))

;;;
;;; Keybinding Formatting Tests
;;;

(nskk-deftest-unit migrate-format-keybindings-returns-string
  "Test keybinding formatting returns a string."
  (let ((result (nskk-migrate--format-keybindings)))
    (should (stringp result))
    (should (> (length result) 0))))

;;;
;;; DDSKK Detection Tests
;;;

(nskk-deftest-unit migrate-detect-ddskk-returns-plist
  "Test that DDSKK detection returns a plist."
  (let ((result (nskk-migrate-detect-ddskk)))
    (should (listp result))
    (should (plist-member result :loaded))
    (should (plist-member result :configured))
    (should (plist-member result :files))))

(nskk-deftest-unit migrate-detect-ddskk-not-loaded
  "Test that DDSKK is not loaded in test environment."
  (let ((result (nskk-migrate-detect-ddskk)))
    ;; In test environment, DDSKK should not be loaded
    (should (not (plist-get result :loaded)))))

;;;
;;; Logging Tests
;;;

(nskk-deftest-unit migrate-log-with-state
  "Test logging with active migration state."
  (let ((nskk--migrate-state (make-nskk-migrate-state
                              :session-id "test"
                              :start-time (current-time)))
        (nskk-migrate-verbose nil))
    (nskk-migrate-log "Test message %s" "arg1")
    (should (= (length (nskk-migrate-state-log nskk--migrate-state)) 1))
    (should (string-match-p "Test message arg1"
                            (car (nskk-migrate-state-log nskk--migrate-state))))))

(nskk-deftest-unit migrate-log-without-state
  "Test logging without active migration state."
  (let ((nskk--migrate-state nil))
    ;; Should not error
    (nskk-migrate-log "Test message")))

(nskk-deftest-unit migrate-warn-adds-warning
  "Test that warnings are added to state."
  (let ((nskk--migrate-state (make-nskk-migrate-state
                              :session-id "test"
                              :start-time (current-time)))
        (nskk-migrate-verbose nil))
    (nskk-migrate-warn "Warning: %s" "test issue")
    (should (= (length (nskk-migrate-state-warnings nskk--migrate-state)) 1))
    (should (string-match-p "Warning: test issue"
                            (car (nskk-migrate-state-warnings nskk--migrate-state))))))

(nskk-deftest-unit migrate-error-adds-error
  "Test that errors are added to state."
  (let ((nskk--migrate-state (make-nskk-migrate-state
                              :session-id "test"
                              :start-time (current-time)))
        (nskk-migrate-verbose nil))
    (nskk-migrate-error "Error: %s" "test failure")
    (should (= (length (nskk-migrate-state-errors nskk--migrate-state)) 1))))

;;;
;;; Configuration Tests
;;;

(nskk-deftest-unit migrate-custom-group-exists
  "Test that the nskk-migration custom group exists."
  (should (get 'nskk-migration 'custom-group)))

(nskk-deftest-unit migrate-interactive-default
  "Test default interactive setting."
  (should (eq (default-value 'nskk-migrate-interactive) t)))

(nskk-deftest-unit migrate-verbose-default
  "Test default verbose setting."
  (should (eq (default-value 'nskk-migrate-verbose) t)))

(nskk-deftest-unit migrate-dry-run-default
  "Test default dry-run setting."
  (should (eq (default-value 'nskk-migrate-dry-run) nil)))

;;;
;;; Variable Map Tests
;;;

(nskk-deftest-unit migrate-variable-map-exists
  "Test that the variable map constant is defined."
  (should (listp nskk--migrate-variable-map))
  (should (> (length nskk--migrate-variable-map) 0)))

(nskk-deftest-unit migrate-variable-map-pairs
  "Test that variable map contains proper pairs."
  (dolist (entry nskk--migrate-variable-map)
    (should (consp entry))
    (should (symbolp (car entry)))
    (should (symbolp (cdr entry)))
    ;; SKK variable should start with skk-
    (should (string-prefix-p "skk-" (symbol-name (car entry))))
    ;; NSKK variable should start with nskk-
    (should (string-prefix-p "nskk-" (symbol-name (cdr entry))))))

;;;
;;; Function Existence Tests
;;;

(nskk-deftest-unit migrate-functions-exist
  "Test that all public migration functions exist."
  (should (fboundp 'nskk-migrate-start))
  (should (fboundp 'nskk-migrate-run))
  (should (fboundp 'nskk-migrate-backup))
  (should (fboundp 'nskk-migrate-config))
  (should (fboundp 'nskk-migrate-dictionaries))
  (should (fboundp 'nskk-migrate-report))
  (should (fboundp 'nskk-migrate-rollback))
  (should (fboundp 'nskk-migrate-wizard))
  (should (fboundp 'nskk-migrate-show-status))
  (should (fboundp 'nskk-migrate-save-log)))

;;;
;;; Error Handling Tests
;;;

(nskk-deftest-unit migrate-backup-without-session-errors
  "Test that backup without session signals error."
  (let ((nskk--migrate-state nil))
    (should-error (nskk-migrate-backup))))

(nskk-deftest-unit migrate-config-without-session-errors
  "Test that config migration without session signals error."
  (let ((nskk--migrate-state nil))
    (should-error (nskk-migrate-config))))

(nskk-deftest-unit migrate-dictionaries-without-session-errors
  "Test that dictionary migration without session signals error."
  (let ((nskk--migrate-state nil))
    (should-error (nskk-migrate-dictionaries))))

(nskk-deftest-unit migrate-report-without-session-errors
  "Test that report without session signals error."
  (let ((nskk--migrate-state nil))
    (should-error (nskk-migrate-report))))

(nskk-deftest-unit migrate-rollback-without-session-errors
  "Test that rollback without session signals error."
  (let ((nskk--migrate-state nil))
    (should-error (nskk-migrate-rollback))))

(nskk-deftest-unit migrate-save-log-without-session-errors
  "Test that save-log without session signals error."
  (let ((nskk--migrate-state nil))
    (should-error (nskk-migrate-save-log))))

;;;
;;; Module Feature Tests
;;;

(nskk-deftest-unit migrate-provides-feature
  "Test that nskk-migrate provides its feature."
  (should (featurep 'nskk-migrate)))

(provide 'nskk-migrate-test)

;;; nskk-migrate-test.el ends here
