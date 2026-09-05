;;; nskk-debug-test.el --- Unit tests for nskk-debug.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test, debug

;; This file is part of NSKK.

;;; Commentary:

;; Unit tests for nskk-debug.el.

;;; Code:

(require 'ert)
(require 'subr-x)
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
  (let ((buf (get-buffer nskk--debug-buffer-name)))
    (if buf (with-current-buffer buf (buffer-string)) "")))

(defun nskk-debug-test--insert-lines (&rest lines)
  "Insert LINES verbatim into the debug buffer, bypassing `nskk-debug-log'."
  (with-current-buffer (nskk--debug-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (dolist (line lines)
        (insert line "\n")))))

(defun nskk-debug-test--capture-signal (thunk)
  "Call THUNK and return the `error' or `quit' condition it signalled.
Return nil when THUNK completes without signalling."
  (condition-case data
      (progn (funcall thunk) nil)
    (error data)
    (quit data)))

(defmacro nskk-debug-test--capturing-warnings (var &rest body)
  "Evaluate BODY with `display-warning' captured into VAR instead of shown.
VAR is bound to a list of (TYPE MESSAGE LEVEL), most recent first."
  (declare (indent 1))
  `(let ((,var nil))
     (cl-letf (((symbol-function 'display-warning)
                (lambda (type message &optional level &rest _)
                  (push (list type message level) ,var))))
       ,@body)))

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
    (with-debug-disabled
      (let ((captured nil))
        (nskk-with-mocks ((message (lambda (fmt &rest args)
                                     (setq captured (apply #'format fmt args)))))
          (nskk-when (nskk-debug-toggle))
          (nskk-then (should (equal captured "NSKK debug mode is enabled")))))))

  (nskk-it "emits \"NSKK debug mode disabled\" when toggling off"
    (with-debug-enabled
      (let ((captured nil))
        (nskk-with-mocks ((message (lambda (fmt &rest args)
                                     (setq captured (apply #'format fmt args)))))
          (nskk-when (nskk-debug-toggle))
          (nskk-then (should (equal captured "NSKK debug mode is disabled"))))))))

;;; Debug buffer management

(nskk-describe "debug buffer management"
  (nskk-it "creates the debug buffer with the correct name"
    (nskk-debug-clear)
    (let ((buf (nskk--debug-buffer)))
      (unwind-protect
          (progn
            (should (bufferp buf))
            (should (equal (buffer-name buf) nskk--debug-buffer-name)))
        (when (buffer-live-p buf)
          (kill-buffer buf)))))

  (nskk-it "creates the debug buffer as read-only"
    (nskk-debug-clear)
    (let ((buf (nskk--debug-buffer)))
      (unwind-protect
          (with-current-buffer buf
            (should buffer-read-only))
        (when (buffer-live-p buf)
          (kill-buffer buf)))))

  (nskk-it "clears the debug buffer contents"
    (nskk-debug-test--insert-lines "[00:00:00.000] Test content")
    (nskk-given (should (not (string-empty-p (nskk-debug-test--buffer-contents)))))
    (nskk-when  (nskk-debug-clear))
    (nskk-then  (should (equal (nskk-debug-test--buffer-contents) ""))))

  (nskk-it "emits \"NSKK debug buffer cleared\" after clearing"
    (nskk-debug-test--insert-lines "[00:00:00.000] Content")
    (let ((captured nil))
      (nskk-with-mocks ((message (lambda (fmt &rest args)
                                   (setq captured (apply #'format fmt args)))))
        (nskk-when (nskk-debug-clear))
        (nskk-then (should (equal captured "NSKK debug buffer is cleared"))))))

  (nskk-it "returns the same buffer object on repeated calls"
    (nskk-debug-clear)
    (let ((buf1 (nskk--debug-buffer))
          (buf2 (nskk--debug-buffer)))
      (unwind-protect
          (progn
            (nskk-given (should (bufferp buf1)))
            (nskk-then  (should (eq buf1 buf2))))
        (when (buffer-live-p buf1)
          (kill-buffer buf1))))))

;;; nskk-debug-log

(nskk-describe "nskk-debug-log"
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

  (nskk-it "always evaluates arguments even when debug is disabled"
    (with-debug-disabled
      (let ((eval-count 0))
        (nskk-given (should (= eval-count 0)))
        (nskk-when (nskk-debug-log "msg: %s" (progn (cl-incf eval-count) "side-effect")))
        (nskk-then (should (= eval-count 1)))))))

;;; nskk--debug-format/k

(nskk-describe "nskk--debug-format/k"
  (nskk-it "propagates an on-found signal without taking the fallback"
    (dolist (injected '((error injected-on-found-error)
                        (quit injected-on-found-quit)))
      (let ((found 0)
            (not-found 0))
        (should (equal injected
                       (nskk-debug-test--capture-signal
                        (lambda ()
                          (nskk--debug-format/k
                           "%s" '("formatted")
                           (lambda (_message)
                             (cl-incf found)
                             (signal (car injected) (cdr injected)))
                           (lambda () (cl-incf not-found)))))))
        (should (= found 1))
        (should (= not-found 0)))))

  (nskk-it "warns once and takes the not-found branch on a format error"
    (let* ((bad-format (copy-sequence "injected format"))
           (real-format (symbol-function 'format))
           (found 0)
           (not-found 0))
      (nskk-debug-test--capturing-warnings warnings
        (cl-letf (((symbol-function 'format)
                   (lambda (template &rest arguments)
                     (if (eq template bad-format)
                         (signal 'error '("injected format error"))
                       (apply real-format template arguments)))))
          (should (eq 'not-found
                      (nskk--debug-format/k
                       bad-format nil
                       (lambda (_message) (cl-incf found) 'found)
                       (lambda () (cl-incf not-found) 'not-found)))))
        (should (= (length warnings) 1))
        (let ((warning (car warnings)))
          (should (eq (nth 0 warning) 'nskk))
          (should (string-match-p "injected format error" (nth 1 warning)))
          (should (eq (nth 2 warning) :warning))))
      (should (= found 0))
      (should (= not-found 1))))

  (nskk-it "propagates an on-not-found signal without re-entering the warning"
    (dolist (injected '((error injected-on-not-found-error)
                        (quit injected-on-not-found-quit)))
      (let ((found 0)
            (not-found 0))
        (nskk-debug-test--capturing-warnings warnings
          (should (equal injected
                         (nskk-debug-test--capture-signal
                          (lambda ()
                            (nskk--debug-format/k
                             "%d" nil
                             (lambda (_message) (cl-incf found))
                             (lambda ()
                               (cl-incf not-found)
                               (signal (car injected) (cdr injected))))))))
          (should (= (length warnings) 1)))
        (should (= found 0))
        (should (= not-found 1))))))

;;; nskk-debug-message

(nskk-describe "nskk-debug-message"
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

  (nskk-it "warns once and logs nothing when the format string is malformed"
    (with-debug-enabled
      (nskk-debug-test--capturing-warnings warnings
        (nskk-then
         (should (null (nskk-debug-test--capture-signal
                        (lambda () (nskk-debug-message "Bad format %d")))))
         (should (= (length warnings) 1))
         (should (equal (nskk-debug-test--buffer-contents) ""))))))

  (nskk-it "logs an entry without warning when the format string is empty"
    (with-debug-enabled
      (nskk-debug-test--capturing-warnings warnings
        (nskk-then
         (should (null (nskk-debug-test--capture-signal
                        (lambda () (nskk-debug-message "")))))
         (should (= (length warnings) 0))
         (should (string-match-p "\\[[0-9]+:[0-9]+:[0-9]+\\.[0-9]+\\]"
                                 (nskk-debug-test--buffer-contents))))))))

;;; nskk-debug-show

(nskk-describe "nskk-debug-show"
  (nskk-it "makes the debug buffer visible in a window"
    (nskk-debug-test--insert-lines "[00:00:00.000] Test entry")
    (unwind-protect
        (progn
          (nskk-when (nskk-debug-show))
          (nskk-then (should (get-buffer-window nskk--debug-buffer-name t))))
      (when-let* ((win (get-buffer-window nskk--debug-buffer-name t)))
        (delete-window win))
      (nskk-debug-clear)))

  (nskk-it "leaves the buffer visible when called twice in a row"
    (nskk-debug-test--insert-lines "[00:00:00.000] Test entry")
    (unwind-protect
        (progn
          (should (null (nskk-debug-test--capture-signal
                         (lambda () (nskk-debug-show) (nskk-debug-show)))))
          (should (get-buffer-window nskk--debug-buffer-name t)))
      (when-let* ((win (get-buffer-window nskk--debug-buffer-name t)))
        (delete-window win))
      (nskk-debug-clear))))

;;; nskk-debug-clear — absent-buffer path

(nskk-describe "nskk-debug-clear when buffer absent"
  (nskk-it "does not error when the debug buffer does not exist"
    (when-let* ((buf (get-buffer nskk--debug-buffer-name)))
      (kill-buffer buf))
    (nskk-when  (nskk-debug-clear))
    (nskk-then  (should (equal (nskk-debug-test--buffer-contents) "")))))

;;; nskk--debug-trim

(nskk-describe "nskk--debug-trim"
  (nskk-it "removes oldest entries when buffer exceeds max-entries"
    (with-max-entries 3
      (nskk-debug-test--insert-lines
       "[00:00:00.000] Entry 1"
       "[00:00:00.001] Entry 2"
       "[00:00:00.002] Entry 3"
       "[00:00:00.003] Entry 4"
       "[00:00:00.004] Entry 5")
      (with-current-buffer (nskk--debug-buffer)
        (nskk--debug-trim))
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
      (with-current-buffer (nskk--debug-buffer)
        (nskk--debug-trim))
      (let ((contents (nskk-debug-test--buffer-contents)))
        (should (string-match-p "Entry 1" contents))
        (should (string-match-p "Entry 2" contents)))))

  (nskk-it "preserves all entries when buffer holds exactly max-entries lines"
    (with-max-entries 3
      (nskk-debug-test--insert-lines
       "[00:00:00.000] Entry 1"
       "[00:00:00.001] Entry 2"
       "[00:00:00.002] Entry 3")
      (with-current-buffer (nskk--debug-buffer)
        (nskk--debug-trim))
      (let ((contents (nskk-debug-test--buffer-contents)))
        (should (string-match-p "Entry 1" contents))
        (should (string-match-p "Entry 2" contents))
        (should (string-match-p "Entry 3" contents)))))

  (nskk-it "handles an empty buffer without error"
    (nskk-debug-clear)
    (with-current-buffer (nskk--debug-buffer)
      (nskk--debug-trim))
    (should (equal (nskk-debug-test--buffer-contents) ""))))

;;; Custom variable defaults

(nskk-describe "nskk-debug custom variables"
  (nskk-it "nskk-debug custom group is defined"
    (should (get 'nskk-debug 'custom-group)))

  (nskk-it "nskk-debug-enabled defaults to nil"
    (should (eq (default-value 'nskk-debug-enabled) nil)))

  (nskk-it "nskk-debug-max-entries defaults to 1000"
    (should (= (default-value 'nskk-debug-max-entries) 1000)))

  (nskk-it "nskk--debug-buffer-name is the expected string"
    (should (equal nskk--debug-buffer-name "*NSKK Debug*")))

  (nskk-it "nskk--debug-timestamp-format is a non-empty string"
    (should (and (stringp nskk--debug-timestamp-format)
                 (not (string-empty-p nskk--debug-timestamp-format))))))

;;; Timestamp format invariant

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

;;; Clear idempotency

(nskk-deftest-table debug-clear-idempotency
  :columns (initial-content)
  :rows (("[00:00:00.000] Single line")
         ("[00:00:00.000] Entry 1\n[00:00:00.001] Entry 2")
         ("")
         ("[00:00:00.000] A\n[00:00:00.001] B\n[00:00:00.002] C")
         ("Large content to clear"))
  :body (progn
          (let ((buf (nskk--debug-buffer)))
            (with-current-buffer buf
              (let ((inhibit-read-only t))
                (erase-buffer)
                (unless (string= initial-content "")
                  (insert initial-content)))))
          (nskk-debug-clear)
          (should (equal (nskk-debug-test--buffer-contents) ""))))

;;; Max-entries enforcement

(nskk-property-test-exhaustive debug-max-entries-enforcement
  '(1 2 3 5 10)
  (with-max-entries item
    (apply #'nskk-debug-test--insert-lines
           (cl-loop for i from 0 below (+ item 3)
                    collect (format "[00:00:%02d.000] Entry %d" i i)))
    (with-current-buffer (nskk--debug-buffer)
      (nskk--debug-trim))
    (let* ((raw (nskk-debug-test--buffer-contents))
           (lines (if (string= raw "")
                      0
                    (length (split-string (string-trim-right raw "\n") "\n")))))
      (<= lines item))))

;;; nskk--debug-append

(nskk-describe "nskk--debug-append"
  (nskk-it "appends message text to the debug buffer"
    (with-max-entries 1000
      (nskk-debug-clear)
      (nskk--debug-append "my-test-message")
      (should (string-match-p "my-test-message"
                              (nskk-debug-test--buffer-contents)))))

  (nskk-it "includes a timestamp prefix in the appended entry"
    (with-max-entries 1000
      (nskk-debug-clear)
      (nskk--debug-append "timestamped")
      (should (string-match-p "\\[[0-9]+:[0-9]+:[0-9]+\\.[0-9]+\\]"
                              (nskk-debug-test--buffer-contents)))))

  (nskk-it "appends multiple messages in order"
    (with-max-entries 1000
      (nskk-debug-clear)
      (nskk--debug-append "first")
      (nskk--debug-append "second")
      (let ((contents (nskk-debug-test--buffer-contents)))
        (should (string-match-p "first" contents))
        (should (string-match-p "second" contents))
        (should (< (string-match "first" contents)
                   (string-match "second" contents)))))))

;;; Logged text reaches the buffer, over varied input
;;
;; The timestamp half of this invariant is covered by the tables above; this
;; test exists for the input variation, so it asserts only that the generated
;; text survives the format-and-append path.

(nskk-property-test-seeded debug-pbt-logged-text-reaches-buffer
  ((msg romaji-string))
  (let ((nskk-debug-enabled t)
        (nskk-debug-max-entries 100))
    (nskk-debug-clear)
    (nskk-debug-log "%s" msg)
    (string-match-p (regexp-quote msg) (nskk-debug-test--buffer-contents)))
  50 42)

;;; Provide

(provide 'nskk-debug-test)

;;; nskk-debug-test.el ends here
