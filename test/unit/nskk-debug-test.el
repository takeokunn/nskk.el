;;; nskk-debug-test.el --- Unit tests for nskk-debug.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test, debug

;; This file is part of NSKK.

;;; Commentary:

;; Unit tests for nskk-debug.el covering:
;; - Debug toggle functionality
;; - Debug buffer management
;; - Debug logging macro behavior

;;; Code:

(require 'ert)
(require 'nskk-debug)
(require 'nskk-test-framework)
(require 'nskk-test-macros)

;;; Test Setup/Teardown

(defconst nskk-debug-test--buffer-name "*NSKK Debug*"
  "Name of the NSKK debug buffer for testing.")

(defun nskk-debug-test--cleanup-buffer ()
  "Clean up the debug buffer if it exists."
  (let ((buffer (get-buffer nskk-debug-test--buffer-name)))
    (when buffer
      (kill-buffer buffer))))

(defun nskk-debug-test--get-buffer-contents ()
  "Get the contents of the debug buffer."
  (let ((buffer (get-buffer nskk-debug-test--buffer-name)))
    (if buffer
        (with-current-buffer buffer
          (buffer-string))
      "")))

;;; Toggle Tests

(nskk-deftest-unit debug-enabled-toggle
  "Test that `nskk-debug-toggle' toggles the flag."
  (let ((original-value nskk-debug-enabled))
    (unwind-protect
        (progn
          ;; Set to known state
          (setq nskk-debug-enabled nil)
          ;; Toggle should enable
          (nskk-debug-toggle)
          (should (eq nskk-debug-enabled t))
          ;; Toggle again should disable
          (nskk-debug-toggle)
          (should (eq nskk-debug-enabled nil)))
      ;; Restore original value
      (setq nskk-debug-enabled original-value)
      (nskk-debug-test--cleanup-buffer))))

;;; Buffer Management Tests

(nskk-deftest-unit debug-buffer-creation
  "Test that `nskk-debug--buffer' creates the debug buffer."
  (unwind-protect
      (progn
        ;; Ensure buffer doesn't exist
        (nskk-debug-test--cleanup-buffer)
        ;; Call the buffer function
        (let ((buffer (nskk-debug--buffer)))
          (should (bufferp buffer))
          (should (equal (buffer-name buffer) nskk-debug-test--buffer-name))
          ;; Buffer should be read-only
          (with-current-buffer buffer
            (should buffer-read-only))))
    (nskk-debug-test--cleanup-buffer)))

(nskk-deftest-unit debug-buffer-clear
  "Test that `nskk-debug-clear' clears the debug buffer."
  (unwind-protect
      (progn
        ;; Create buffer and add content
        (let ((buffer (nskk-debug--buffer)))
          (with-current-buffer buffer
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert "Test content\n"))))
        ;; Verify content exists
        (should (> (length (nskk-debug-test--get-buffer-contents)) 0))
        ;; Clear the buffer
        (nskk-debug-clear)
        ;; Verify buffer is empty
        (should (equal (nskk-debug-test--get-buffer-contents) "")))
    (nskk-debug-test--cleanup-buffer)))

;;; Log Macro Tests

(nskk-deftest-unit debug-log-macro-disabled
  "Test that `nskk-debug-log' does nothing when disabled."
  (let ((original-value nskk-debug-enabled))
    (unwind-protect
        (progn
          ;; Ensure debug is disabled
          (setq nskk-debug-enabled nil)
          ;; Clear buffer
          (nskk-debug-test--cleanup-buffer)
          ;; Log should do nothing
          (nskk-debug-log "Test message: %s" "arg1")
          ;; Buffer should not exist or be empty
          (let ((contents (nskk-debug-test--get-buffer-contents)))
            (should (equal contents ""))))
      ;; Restore original value
      (setq nskk-debug-enabled original-value)
      (nskk-debug-test--cleanup-buffer))))

(nskk-deftest-unit debug-log-macro-enabled
  "Test that `nskk-debug-log' appends to buffer when enabled."
  (let ((original-value nskk-debug-enabled))
    (unwind-protect
        (progn
          ;; Enable debug mode
          (setq nskk-debug-enabled t)
          ;; Clear buffer
          (nskk-debug-test--cleanup-buffer)
          ;; Log a message
          (nskk-debug-log "Test message: %s" "hello")
          ;; Check buffer contains the message
          (let ((contents (nskk-debug-test--get-buffer-contents)))
            (should (string-match-p "Test message: hello" contents))
            ;; Should have timestamp format
            (should (string-match-p "\\[[0-9]+:[0-9]+:[0-9]+\\.[0-9]+\\]" contents))))
      ;; Restore original value
      (setq nskk-debug-enabled original-value)
      (nskk-debug-test--cleanup-buffer))))

;;; Custom Variable Tests

(nskk-deftest-unit debug-custom-group-exists
  "Test that nskk-debug custom group is defined."
  (should (get 'nskk-debug 'custom-group)))

(nskk-deftest-unit debug-enabled-default
  "Test default value of `nskk-debug-enabled'."
  ;; Default should be nil
  (should (eq (default-value 'nskk-debug-enabled) nil)))

(nskk-deftest-unit debug-max-entries-default
  "Test default value of `nskk-debug-max-entries'."
  (should (= nskk-debug-max-entries 1000)))

(nskk-deftest-unit debug-buffer-name-constant
  "Test that buffer name constant is defined correctly."
  (should (equal nskk-debug--buffer-name "*NSKK Debug*")))

;;; Trim Tests

(nskk-deftest-unit debug-trim-removes-oldest-entries
  "Test that `nskk-debug--trim' removes oldest entries when buffer exceeds max."
  (let ((original-max nskk-debug-max-entries))
    (unwind-protect
        (progn
          ;; Set a low max for testing
          (setq nskk-debug-max-entries 3)
          ;; Create buffer and add content
          (nskk-debug-test--cleanup-buffer)
          (let ((buffer (nskk-debug--buffer)))
            (with-current-buffer buffer
              (let ((inhibit-read-only t))
                (erase-buffer)
                ;; Add 5 entries
                (insert "[00:00:00.000] Entry 1\n")
                (insert "[00:00:00.001] Entry 2\n")
                (insert "[00:00:00.002] Entry 3\n")
                (insert "[00:00:00.003] Entry 4\n")
                (insert "[00:00:00.004] Entry 5\n"))
              ;; Call trim - should keep only last 3
              (nskk-debug--trim)
              (let ((contents (buffer-string)))
                ;; Should contain entries 3, 4, 5
                (should (string-match-p "Entry 3" contents))
                (should (string-match-p "Entry 4" contents))
                (should (string-match-p "Entry 5" contents))
                ;; Should NOT contain entries 1, 2
                (should-not (string-match-p "Entry 1" contents))
                (should-not (string-match-p "Entry 2" contents))))))
      ;; Restore original value
      (setq nskk-debug-max-entries original-max)
      (nskk-debug-test--cleanup-buffer))))

(nskk-deftest-unit debug-trim-no-op-when-under-limit
  "Test that `nskk-debug--trim' does nothing when buffer is under max."
  (let ((original-max nskk-debug-max-entries))
    (unwind-protect
        (progn
          ;; Set a high max for testing
          (setq nskk-debug-max-entries 100)
          ;; Create buffer and add content
          (nskk-debug-test--cleanup-buffer)
          (let ((buffer (nskk-debug--buffer)))
            (with-current-buffer buffer
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert "[00:00:00.000] Entry 1\n")
                (insert "[00:00:00.001] Entry 2\n"))
              ;; Call trim - should not remove anything
              (nskk-debug--trim)
              (let ((contents (buffer-string)))
                ;; Should still contain both entries
                (should (string-match-p "Entry 1" contents))
                (should (string-match-p "Entry 2" contents))))))
      ;; Restore original value
      (setq nskk-debug-max-entries original-max)
      (nskk-debug-test--cleanup-buffer))))

(nskk-deftest-unit debug-trim-empty-buffer
  "Test that `nskk-debug--trim' handles empty buffer correctly."
  (nskk-debug-test--cleanup-buffer)
  (let ((buffer (nskk-debug--buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer))
      ;; Should not error on empty buffer
      (nskk-debug--trim)
      (should (equal (buffer-string) ""))))
  (nskk-debug-test--cleanup-buffer))

;;; Show Command Tests

(nskk-deftest-unit debug-show-displays-buffer
  "Test that `nskk-debug-show' displays the debug buffer."
  (unwind-protect
      (progn
        ;; Ensure buffer exists with some content
        (nskk-debug-test--cleanup-buffer)
        (let ((buffer (nskk-debug--buffer)))
          (with-current-buffer buffer
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert "[00:00:00.000] Test entry\n"))))
        ;; Call show - should display the buffer
        (nskk-debug-show)
        ;; Verify buffer is displayed in some window
        (should (get-buffer-window nskk-debug-test--buffer-name)))
    ;; Cleanup - close the buffer window
    (let ((window (get-buffer-window nskk-debug-test--buffer-name)))
      (when window
        (delete-window window)))
    (nskk-debug-test--cleanup-buffer)))

;;;
;;; PBT-001 — Timestamp format invariant (enabled debug, various messages)
;;;

(nskk-deftest-table debug-timestamp-format-invariant
  :columns (format-str arg)
  :rows (("Test message: %s" "hello")
         ("Value: %d" 42)
         ("Key input: %s" "romaji")
         ("Buffer content: %s" "かんじ")
         ("Debug info: %s" "foo bar"))
  :body (let ((original-value nskk-debug-enabled))
          (unwind-protect
              (progn
                (setq nskk-debug-enabled t)
                (nskk-debug-test--cleanup-buffer)
                (nskk-debug-log format-str arg)
                (let ((contents (nskk-debug-test--get-buffer-contents)))
                  (should (string-match-p "\\[[0-9]+:[0-9]+:[0-9]+\\.[0-9]+\\]" contents))))
            (setq nskk-debug-enabled original-value)
            (nskk-debug-test--cleanup-buffer))))

;;;
;;; PBT-002 — Log idempotency after clear (table-driven over initial states)
;;;

(nskk-deftest-table debug-clear-idempotency
  :columns (initial-content)
  :rows (("Single line\n")
         ("[00:00:00.000] Entry 1\n[00:00:00.001] Entry 2\n")
         ("")
         ("[00:00:00.000] A\n[00:00:00.001] B\n[00:00:00.002] C\n[00:00:00.003] D\n")
         ("Large content to clear\n"))
  :body (unwind-protect
            (progn
              ;; Setup: put initial-content into debug buffer
              (nskk-debug-test--cleanup-buffer)
              (let ((buffer (nskk-debug--buffer)))
                (with-current-buffer buffer
                  (let ((inhibit-read-only t))
                    (erase-buffer)
                    (when (and (stringp initial-content)
                               (> (length initial-content) 0))
                      (insert initial-content)))))
              ;; Act: clear the buffer
              (nskk-debug-clear)
              ;; Assert: buffer is always empty after clear
              (should (equal (nskk-debug-test--get-buffer-contents) "")))
          (nskk-debug-test--cleanup-buffer)))

;;;
;;; PBT-003 — Max entries enforcement (exhaustive over small max values)
;;;

(nskk-property-test-exhaustive debug-max-entries-enforcement
  '(1 2 3 5 10)
  (let ((original-max nskk-debug-max-entries)
        (result t))
    (unwind-protect
        (progn
          (setq nskk-debug-max-entries item)
          (nskk-debug-test--cleanup-buffer)
          (let ((buffer (nskk-debug--buffer)))
            (with-current-buffer buffer
              (let ((inhibit-read-only t))
                (erase-buffer)
                ;; Insert item+3 lines (more than max)
                (dotimes (i (+ item 3))
                  (insert (format "[00:00:%02d.000] Entry %d\n" i i))))
              ;; Trim to max
              (nskk-debug--trim)
              ;; Count remaining lines
              (let* ((contents (buffer-string))
                     (lines (if (string= contents "")
                                0
                              (length (split-string (string-trim-right contents "\n") "\n")))))
                (setq result (<= lines item)))))
          result)
      (setq nskk-debug-max-entries original-max)
      (nskk-debug-test--cleanup-buffer))))

;;; Provide

(provide 'nskk-debug-test)

;;; nskk-debug-test.el ends here
