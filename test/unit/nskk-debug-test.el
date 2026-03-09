;;; nskk-debug-test.el --- Unit tests for nskk-debug.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test, debug

;; This file is part of NSKK.

;;; Commentary:

;; Unit tests for nskk-debug.el covering:
;; - Debug toggle functionality (flag, message side-effect)
;; - Debug buffer management (create, clear, clear-when-absent, trim)
;; - nskk-debug-log macro (enabled / disabled / timestamp)
;; - nskk-debug-message function (enabled / disabled / timestamp / error recovery)
;; - nskk-debug-show interactive command
;; - Custom variable defaults (nskk-debug-max-entries safe predicate)
;; - PBT: timestamp format invariant for log and message (table-driven)
;; - PBT: clear idempotency (table-driven)
;; - PBT: max-entries enforcement including boundary (exhaustive)

;;; Code:

(require 'ert)
(require 'nskk-debug)
(require 'nskk-test-framework)
(require 'nskk-test-macros)

;;; Fixtures

(nskk-deffixture with-debug-enabled ()
  `(let ((original nskk-debug-enabled))
     (unwind-protect
         (progn
           (setq nskk-debug-enabled t)
           (nskk-debug-clear)
           ,@body)
       (setq nskk-debug-enabled original)
       (nskk-debug-clear))))

(nskk-deffixture with-debug-disabled ()
  `(let ((original nskk-debug-enabled))
     (unwind-protect
         (progn
           (setq nskk-debug-enabled nil)
           (nskk-debug-clear)
           ,@body)
       (setq nskk-debug-enabled original)
       (nskk-debug-clear))))

(nskk-deffixture with-max-entries (n)
  `(let ((original nskk-debug-max-entries))
     (unwind-protect
         (progn
           (setq nskk-debug-max-entries ,n)
           ,@body)
       (setq nskk-debug-max-entries original))))

;;; Helpers

(defun nskk-debug-test--buffer-contents ()
  "Return the contents of the NSKK debug buffer, or \"\" if absent."
  (let ((buf (get-buffer nskk-debug--buffer-name)))
    (if buf (with-current-buffer buf (buffer-string)) "")))

(defun nskk-debug-test--insert-lines (&rest lines)
  "Insert LINES verbatim into the debug buffer, bypassing `nskk-debug-log'."
  (with-current-buffer (nskk-debug--buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (dolist (line lines)
        (insert line "\n")))))

;;; nskk-debug-toggle

(nskk-describe "nskk-debug-toggle"
  (nskk-it "toggles the debug flag from nil to t and back"
    (with-debug-disabled
      (nskk-given (should (eq nskk-debug-enabled nil)))
      (nskk-when  (nskk-debug-toggle))
      (nskk-then  (should (eq nskk-debug-enabled t)))
      (nskk-when  (nskk-debug-toggle))
      (nskk-then  (should (eq nskk-debug-enabled nil)))))

  (nskk-it "emits \"NSKK debug mode enabled\" when toggling on"
    ;; `current-message' is nil in batch mode; mock `message' to capture output.
    (with-debug-disabled
      (let ((captured nil))
        (nskk-with-mocks ((message (lambda (fmt &rest args)
                                     (setq captured (apply #'format fmt args)))))
          (nskk-when (nskk-debug-toggle))
          (nskk-then (should (equal captured "NSKK debug mode enabled")))))))

  (nskk-it "emits \"NSKK debug mode disabled\" when toggling off"
    (with-debug-enabled
      (let ((captured nil))
        (nskk-with-mocks ((message (lambda (fmt &rest args)
                                     (setq captured (apply #'format fmt args)))))
          (nskk-when (nskk-debug-toggle))
          (nskk-then (should (equal captured "NSKK debug mode disabled"))))))))

;;; Debug buffer management

(nskk-describe "debug buffer management"
  (nskk-it "creates the debug buffer with the correct name"
    (nskk-debug-clear)
    (let ((buf (nskk-debug--buffer)))
      (unwind-protect
          (progn
            (should (bufferp buf))
            (should (equal (buffer-name buf) nskk-debug--buffer-name)))
        (when (buffer-live-p buf)
          (kill-buffer buf)))))

  (nskk-it "creates the debug buffer as read-only"
    (nskk-debug-clear)
    (let ((buf (nskk-debug--buffer)))
      (unwind-protect
          (with-current-buffer buf
            (should buffer-read-only))
        (when (buffer-live-p buf)
          (kill-buffer buf)))))

  (nskk-it "clears the debug buffer contents"
    (nskk-debug-test--insert-lines "[00:00:00.000] Test content")
    (nskk-given (should (> (length (nskk-debug-test--buffer-contents)) 0)))
    (nskk-when  (nskk-debug-clear))
    (nskk-then  (should (equal (nskk-debug-test--buffer-contents) "")))))

;;; nskk-debug-log macro

(nskk-describe "nskk-debug-log macro"
  (nskk-it "does nothing when debug is disabled"
    (with-debug-disabled
      (nskk-when (nskk-debug-log "Test message: %s" "arg1"))
      (nskk-then (should (equal (nskk-debug-test--buffer-contents) "")))))

  (nskk-it "appends formatted message to buffer when debug is enabled"
    (with-debug-enabled
      (nskk-when (nskk-debug-log "Test message: %s" "hello"))
      (nskk-then
       (let ((contents (nskk-debug-test--buffer-contents)))
         (should (string-match-p "Test message: hello" contents))))))

  (nskk-it "writes a timestamp prefix in [HH:MM:SS.mmm] format"
    (with-debug-enabled
      (nskk-when (nskk-debug-log "Timestamp test"))
      (nskk-then
       (should (string-match-p "\\[[0-9]+:[0-9]+:[0-9]+\\.[0-9]+\\]"
                               (nskk-debug-test--buffer-contents)))))))

;;; nskk-debug-message function

(nskk-describe "nskk-debug-message function"
  (nskk-it "does nothing when debug is disabled"
    (with-debug-disabled
      (nskk-when (nskk-debug-message "Should not appear: %s" "x"))
      (nskk-then (should (equal (nskk-debug-test--buffer-contents) "")))))

  (nskk-it "appends formatted message to buffer when debug is enabled"
    (with-debug-enabled
      (nskk-when (nskk-debug-message "Runtime message: %s" "world"))
      (nskk-then
       (let ((contents (nskk-debug-test--buffer-contents)))
         (should (string-match-p "Runtime message: world" contents))))))

  (nskk-it "writes a timestamp prefix in [HH:MM:SS.mmm] format"
    (with-debug-enabled
      (nskk-when (nskk-debug-message "Timestamp via message"))
      (nskk-then
       (should (string-match-p "\\[[0-9]+:[0-9]+:[0-9]+\\.[0-9]+\\]"
                               (nskk-debug-test--buffer-contents))))))

  (nskk-it "recovers from format errors without signaling"
    ;; Passing too few args to %s/%d would signal in `format'; the
    ;; condition-case in nskk-debug-message must absorb that.
    (with-debug-enabled
      (nskk-then
       (should-not (condition-case _
                       (progn (nskk-debug-message "Bad format %d") nil)
                     (error t)))))))

;;; nskk-debug-show

(nskk-describe "nskk-debug-show"
  (nskk-it "makes the debug buffer visible in a window"
    (nskk-debug-test--insert-lines "[00:00:00.000] Test entry")
    (unwind-protect
        (progn
          (nskk-when (nskk-debug-show))
          (nskk-then (should (get-buffer-window nskk-debug--buffer-name))))
      (when-let* ((win (get-buffer-window nskk-debug--buffer-name)))
        (delete-window win))
      (nskk-debug-clear))))

;;; nskk-debug-clear — absent-buffer path

(nskk-describe "nskk-debug-clear when buffer absent"
  (nskk-it "does not error when the debug buffer does not exist"
    ;; Kill the buffer so get-buffer returns nil — when-let must short-circuit cleanly.
    (when-let* ((buf (get-buffer nskk-debug--buffer-name)))
      (kill-buffer buf))
    (nskk-when  (nskk-debug-clear))
    ;; Calling nskk-debug-clear on a non-existent buffer should still emit its
    ;; confirmation message and leave us with an empty (newly created) buffer.
    (nskk-then  (should (equal (nskk-debug-test--buffer-contents) "")))))

;;; nskk-debug--trim

(nskk-describe "nskk-debug--trim"
  (nskk-it "removes oldest entries when buffer exceeds max-entries"
    (with-max-entries 3
      (nskk-debug-test--insert-lines
       "[00:00:00.000] Entry 1"
       "[00:00:00.001] Entry 2"
       "[00:00:00.002] Entry 3"
       "[00:00:00.003] Entry 4"
       "[00:00:00.004] Entry 5")
      (with-current-buffer (nskk-debug--buffer)
        (nskk-debug--trim))
      (let ((contents (nskk-debug-test--buffer-contents)))
        (should     (string-match-p "Entry 3" contents))
        (should     (string-match-p "Entry 4" contents))
        (should     (string-match-p "Entry 5" contents))
        (should-not (string-match-p "Entry 1" contents))
        (should-not (string-match-p "Entry 2" contents)))))

  (nskk-it "preserves all entries when buffer is under max-entries"
    (with-max-entries 100
      (nskk-debug-test--insert-lines
       "[00:00:00.000] Entry 1"
       "[00:00:00.001] Entry 2")
      (with-current-buffer (nskk-debug--buffer)
        (nskk-debug--trim))
      (let ((contents (nskk-debug-test--buffer-contents)))
        (should (string-match-p "Entry 1" contents))
        (should (string-match-p "Entry 2" contents)))))

  (nskk-it "preserves all entries when buffer holds exactly max-entries lines"
    ;; This is the boundary: forward-line lands on point-min so nothing is deleted.
    (with-max-entries 3
      (nskk-debug-test--insert-lines
       "[00:00:00.000] Entry 1"
       "[00:00:00.001] Entry 2"
       "[00:00:00.002] Entry 3")
      (with-current-buffer (nskk-debug--buffer)
        (nskk-debug--trim))
      (let ((contents (nskk-debug-test--buffer-contents)))
        (should (string-match-p "Entry 1" contents))
        (should (string-match-p "Entry 2" contents))
        (should (string-match-p "Entry 3" contents)))))

  (nskk-it "handles an empty buffer without error"
    (nskk-debug-clear)
    (with-current-buffer (nskk-debug--buffer)
      (nskk-debug--trim))
    (should (equal (nskk-debug-test--buffer-contents) ""))))

;;; Custom variable defaults

(nskk-describe "nskk-debug custom variables"
  (nskk-it "nskk-debug custom group is defined"
    (should (get 'nskk-debug 'custom-group)))

  (nskk-it "nskk-debug-enabled defaults to nil"
    (should (eq (default-value 'nskk-debug-enabled) nil)))

  (nskk-it "nskk-debug-max-entries defaults to 1000"
    (should (= (default-value 'nskk-debug-max-entries) 1000)))

  (nskk-it "nskk-debug--buffer-name is the expected string"
    (should (equal nskk-debug--buffer-name "*NSKK Debug*")))

  (nskk-it "nskk-debug--timestamp-format is a non-empty string"
    (should (and (stringp nskk-debug--timestamp-format)
                 (> (length nskk-debug--timestamp-format) 0)))))

;;;
;;; PBT-001 — Timestamp format invariant
;;; Every logged message must contain a [HH:MM:SS.mmm] timestamp prefix.
;;;

(nskk-deftest-table debug-log-timestamp-format-invariant
  :columns (format-str arg)
  :rows (("Message: %s"      "hello")
         ("Value: %d"        42)
         ("Key input: %s"    "romaji")
         ("Buffer: %s"       "かんじ")
         ("Debug info: %s"   "foo bar"))
  :body (with-debug-enabled
          (nskk-debug-log format-str arg)
          (should (string-match-p "\\[[0-9]+:[0-9]+:[0-9]+\\.[0-9]+\\]"
                                  (nskk-debug-test--buffer-contents)))))

(nskk-deftest-table debug-message-timestamp-format-invariant
  :columns (format-str arg)
  :rows (("Message: %s"      "hello")
         ("Value: %d"        42)
         ("Key input: %s"    "romaji")
         ("Buffer: %s"       "かんじ")
         ("Debug info: %s"   "foo bar"))
  :body (with-debug-enabled
          (nskk-debug-message format-str arg)
          (should (string-match-p "\\[[0-9]+:[0-9]+:[0-9]+\\.[0-9]+\\]"
                                  (nskk-debug-test--buffer-contents)))))

;;;
;;; PBT-002 — Clear idempotency
;;; nskk-debug-clear always leaves the buffer empty regardless of initial state.
;;;

(nskk-deftest-table debug-clear-idempotency
  :columns (initial-content)
  :rows (("[00:00:00.000] Single line")
         ("[00:00:00.000] Entry 1\n[00:00:00.001] Entry 2")
         ("")
         ("[00:00:00.000] A\n[00:00:00.001] B\n[00:00:00.002] C")
         ("Large content to clear"))
  :body (progn
          (let ((buf (nskk-debug--buffer)))
            (with-current-buffer buf
              (let ((inhibit-read-only t))
                (erase-buffer)
                (unless (string= initial-content "")
                  (insert initial-content)))))
          (nskk-debug-clear)
          (should (equal (nskk-debug-test--buffer-contents) ""))))

;;;
;;; PBT-003 — Max-entries enforcement
;;; After trim, the buffer never holds more than max-entries lines.
;;;

(nskk-property-test-exhaustive debug-max-entries-enforcement
  '(1 2 3 5 10)
  (with-max-entries item
    (nskk-debug-test--insert-lines
     ;; Insert item+3 lines (always more than max).
     (mapconcat #'identity
                (cl-loop for i from 0 below (+ item 3)
                         collect (format "[00:00:%02d.000] Entry %d" i i))
                "\n"))
    (with-current-buffer (nskk-debug--buffer)
      (nskk-debug--trim))
    (let* ((raw (nskk-debug-test--buffer-contents))
           (lines (if (string= raw "")
                      0
                    (length (split-string (string-trim-right raw "\n") "\n")))))
      (<= lines item))))

;;;
;;; nskk-debug--append
;;;

(nskk-describe "nskk-debug--append"
  (nskk-it "appends message text to the debug buffer"
    (with-max-entries 1000
      (nskk-debug-clear)
      (nskk-debug--append "my-test-message")
      (should (string-match-p "my-test-message"
                              (nskk-debug-test--buffer-contents)))))

  (nskk-it "includes a timestamp prefix in the appended entry"
    (with-max-entries 1000
      (nskk-debug-clear)
      (nskk-debug--append "timestamped")
      (should (string-match-p "\\[[0-9]+:[0-9]+:[0-9]+\\.[0-9]+\\]"
                              (nskk-debug-test--buffer-contents)))))

  (nskk-it "appends multiple messages in order"
    (with-max-entries 1000
      (nskk-debug-clear)
      (nskk-debug--append "first")
      (nskk-debug--append "second")
      (let ((contents (nskk-debug-test--buffer-contents)))
        (should (string-match-p "first" contents))
        (should (string-match-p "second" contents))
        ;; "first" appears before "second"
        (should (< (string-match "first" contents)
                   (string-match "second" contents)))))))

;;; Provide

(provide 'nskk-debug-test)

;;; nskk-debug-test.el ends here
