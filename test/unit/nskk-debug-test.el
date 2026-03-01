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

(nskk-describe "nskk-debug-toggle"
  (nskk-it "toggles the debug flag on and off"
    (let ((original-value nskk-debug-enabled))
      (unwind-protect
          (progn
            (setq nskk-debug-enabled nil)
            (nskk-given (should (eq nskk-debug-enabled nil)))
            (nskk-when  (nskk-debug-toggle))
            (nskk-then  (should (eq nskk-debug-enabled t)))
            (nskk-when  (nskk-debug-toggle))
            (nskk-then  (should (eq nskk-debug-enabled nil))))
        (setq nskk-debug-enabled original-value)
        (nskk-debug-test--cleanup-buffer)))))

(nskk-describe "debug buffer management"
  (nskk-it "creates the debug buffer with correct name and read-only"
    (unwind-protect
        (progn
          (nskk-debug-test--cleanup-buffer)
          (let ((buffer (nskk-debug--buffer)))
            (should (bufferp buffer))
            (should (equal (buffer-name buffer) nskk-debug-test--buffer-name))
            (with-current-buffer buffer
              (should buffer-read-only))))
      (nskk-debug-test--cleanup-buffer)))

  (nskk-it "clears the debug buffer contents"
    (unwind-protect
        (progn
          (let ((buffer (nskk-debug--buffer)))
            (with-current-buffer buffer
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert "Test content\n"))))
          (nskk-given (should (> (length (nskk-debug-test--get-buffer-contents)) 0)))
          (nskk-when  (nskk-debug-clear))
          (nskk-then  (should (equal (nskk-debug-test--get-buffer-contents) ""))))
      (nskk-debug-test--cleanup-buffer))))

(nskk-describe "nskk-debug-log macro"
  (nskk-it "does nothing when debug is disabled"
    (let ((original-value nskk-debug-enabled))
      (unwind-protect
          (progn
            (nskk-given (setq nskk-debug-enabled nil))
            (nskk-debug-test--cleanup-buffer)
            (nskk-when  (nskk-debug-log "Test message: %s" "arg1"))
            (nskk-then  (should (equal (nskk-debug-test--get-buffer-contents) ""))))
        (setq nskk-debug-enabled original-value)
        (nskk-debug-test--cleanup-buffer))))

  (nskk-it "appends to buffer when debug is enabled"
    (let ((original-value nskk-debug-enabled))
      (unwind-protect
          (progn
            (nskk-given (setq nskk-debug-enabled t))
            (nskk-debug-test--cleanup-buffer)
            (nskk-when  (nskk-debug-log "Test message: %s" "hello"))
            (nskk-then
             (let ((contents (nskk-debug-test--get-buffer-contents)))
               (should (string-match-p "Test message: hello" contents))
               (should (string-match-p "\\[[0-9]+:[0-9]+:[0-9]+\\.[0-9]+\\]" contents)))))
        (setq nskk-debug-enabled original-value)
        (nskk-debug-test--cleanup-buffer)))))

(nskk-describe "nskk-debug custom variables"
  (nskk-it "nskk-debug custom group is defined"
    (should (get 'nskk-debug 'custom-group)))

  (nskk-it "nskk-debug-enabled default value is nil"
    (should (eq (default-value 'nskk-debug-enabled) nil)))

  (nskk-it "nskk-debug-max-entries default is 1000"
    (should (= nskk-debug-max-entries 1000)))

  (nskk-it "buffer name constant is defined correctly"
    (should (equal nskk-debug--buffer-name "*NSKK Debug*"))))

(nskk-describe "nskk-debug--trim"
  (nskk-it "removes oldest entries when buffer exceeds max"
    (let ((original-max nskk-debug-max-entries))
      (unwind-protect
          (progn
            (setq nskk-debug-max-entries 3)
            (nskk-debug-test--cleanup-buffer)
            (let ((buffer (nskk-debug--buffer)))
              (with-current-buffer buffer
                (let ((inhibit-read-only t))
                  (erase-buffer)
                  (insert "[00:00:00.000] Entry 1\n")
                  (insert "[00:00:00.001] Entry 2\n")
                  (insert "[00:00:00.002] Entry 3\n")
                  (insert "[00:00:00.003] Entry 4\n")
                  (insert "[00:00:00.004] Entry 5\n"))
                (nskk-debug--trim)
                (let ((contents (buffer-string)))
                  (should (string-match-p "Entry 3" contents))
                  (should (string-match-p "Entry 4" contents))
                  (should (string-match-p "Entry 5" contents))
                  (should-not (string-match-p "Entry 1" contents))
                  (should-not (string-match-p "Entry 2" contents))))))
        (setq nskk-debug-max-entries original-max)
        (nskk-debug-test--cleanup-buffer))))

  (nskk-it "does nothing when buffer is under max"
    (let ((original-max nskk-debug-max-entries))
      (unwind-protect
          (progn
            (setq nskk-debug-max-entries 100)
            (nskk-debug-test--cleanup-buffer)
            (let ((buffer (nskk-debug--buffer)))
              (with-current-buffer buffer
                (let ((inhibit-read-only t))
                  (erase-buffer)
                  (insert "[00:00:00.000] Entry 1\n")
                  (insert "[00:00:00.001] Entry 2\n"))
                (nskk-debug--trim)
                (let ((contents (buffer-string)))
                  (should (string-match-p "Entry 1" contents))
                  (should (string-match-p "Entry 2" contents))))))
        (setq nskk-debug-max-entries original-max)
        (nskk-debug-test--cleanup-buffer))))

  (nskk-it "handles empty buffer without error"
    (nskk-debug-test--cleanup-buffer)
    (let ((buffer (nskk-debug--buffer)))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer))
        (nskk-debug--trim)
        (should (equal (buffer-string) ""))))
    (nskk-debug-test--cleanup-buffer)))

(nskk-describe "nskk-debug-show"
  (nskk-it "displays the debug buffer in a window"
    (unwind-protect
        (progn
          (nskk-debug-test--cleanup-buffer)
          (let ((buffer (nskk-debug--buffer)))
            (with-current-buffer buffer
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert "[00:00:00.000] Test entry\n"))))
          (nskk-when  (nskk-debug-show))
          (nskk-then  (should (get-buffer-window nskk-debug-test--buffer-name))))
      (let ((window (get-buffer-window nskk-debug-test--buffer-name)))
        (when window
          (delete-window window)))
      (nskk-debug-test--cleanup-buffer))))

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
