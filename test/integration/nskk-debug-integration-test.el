;;; nskk-debug-integration-test.el --- Debug module integration tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Integration tests for nskk-debug.el (Layer 0: Foundation).
;;
;; Tests verify that:
;; - `nskk-debug-message' appends text to the debug buffer.
;; - `nskk-debug-toggle' toggles the `nskk-debug-enabled' flag.
;; - `nskk-debug-clear' empties the debug buffer contents.
;; - `nskk-debug-show' makes the debug buffer visible.
;; - `nskk-debug-log' macro emits output when debug is active.

;;; Code:

(require 'ert)
(require 'nskk-debug)
(require 'nskk-test-macros)

;;;; Debug message writes to buffer

(nskk-describe "debug message writes to buffer"

  (nskk-it "message appends non-empty string to debug buffer"
    (let ((nskk-debug-enabled t))
      (nskk-debug-clear)
      (nskk-debug-message "integration-test-marker-%s" "hello")
      (with-current-buffer (nskk--debug-buffer)
        (should (string-match-p "integration-test-marker-hello" (buffer-string))))))

  (nskk-it "message with empty string does not error"
    (let ((nskk-debug-enabled t))
      (should-not (condition-case nil
                      (progn (nskk-debug-message "") nil)
                    (error t))))))

;;;; Debug toggle changes mode flag

(nskk-describe "debug toggle changes mode flag"

  (nskk-it "toggle switches from nil to t"
    (let ((nskk-debug-enabled nil))
      (nskk-debug-toggle)
      (should nskk-debug-enabled)))

  (nskk-it "toggle switches from t back to nil"
    (let ((nskk-debug-enabled t))
      (nskk-debug-toggle)
      (should-not nskk-debug-enabled)))

  (nskk-it "double toggle returns to original state"
    (let ((nskk-debug-enabled nil))
      (nskk-debug-toggle)
      (nskk-debug-toggle)
      (should-not nskk-debug-enabled))))

;;;; Debug clear empties buffer

(nskk-describe "debug clear empties buffer"

  (nskk-it "clear removes content from debug buffer"
    (let ((nskk-debug-enabled t))
      (nskk-debug-message "content-to-be-cleared")
      (nskk-debug-clear)
      (with-current-buffer (nskk--debug-buffer)
        (should (string= "" (buffer-string))))))

  (nskk-it "clear does not error when buffer does not yet exist"
    (when-let* ((buf (get-buffer nskk--debug-buffer-name)))
      (kill-buffer buf))
    (should-not (condition-case nil
                    (progn (nskk-debug-clear) nil)
                  (error t)))))

;;;; Debug-log macro behavior

(nskk-describe "debug-log macro behavior"

  (nskk-it "log emits message when debug enabled"
    (let ((nskk-debug-enabled t))
      (nskk-debug-clear)
      (nskk-debug-log "macro-test-marker-%s" "active")
      (with-current-buffer (nskk--debug-buffer)
        (should (string-match-p "macro-test-marker-active" (buffer-string))))))

  (nskk-it "log is silent when debug disabled"
    (let ((nskk-debug-enabled nil))
      (nskk-debug-clear)
      (nskk-debug-log "should-not-appear")
      (with-current-buffer (nskk--debug-buffer)
        (should (string= "" (buffer-string)))))))

;;;; Debug show displays buffer

(nskk-describe "debug show displays buffer"

  (nskk-it "nskk-debug-show does not signal"
    (should-not (condition-case nil
                    (progn (nskk-debug-show) nil)
                  (error t))))

  (nskk-it "show is safe to call consecutively"
    (should-not (condition-case nil
                    (progn (nskk-debug-show) (nskk-debug-show) nil)
                  (error t)))))

;;;; nskk-debug-message variations (parameterized)

(nskk-describe "nskk-debug-message input variations"

  ;; Cases A: various input strings — verify each appears in the debug buffer.
  ;; `input' is the format string passed directly; `expected' is the substring
  ;; that must appear in the buffer after the call.  We use a dedicated marker
  ;; suffix per case to avoid cross-contamination between cases.

  (nskk-deftest-table debug-message-inputs
    :description "nskk-debug-message handles various input string forms"
    :columns (input expected)
    :rows (("multi-line-marker\nsecond-line"             "multi-line-marker")
           ("日本語テスト-unicode-marker"                  "日本語テスト-unicode-marker")
           ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-long-marker" "long-marker")
           ("control-chars-marker\t\r"                   "control-chars-marker"))
    :body (let ((nskk-debug-enabled t))
            (nskk-debug-clear)
            (nskk-debug-message "%s" input)
            (with-current-buffer (nskk--debug-buffer)
              (should (string-match-p (regexp-quote expected) (buffer-string)))))))

;;;; nskk-debug-toggle state transitions (parameterized)

(nskk-describe "nskk-debug-toggle state transitions"

  ;; Cases B: (INITIAL-VALUE . EXPECTED-AFTER-TOGGLE)
  ;; After one `nskk-debug-toggle' call the flag must equal `expected'.

  (nskk-deftest-table debug-toggle-transitions
    :description "nskk-debug-toggle transitions between enabled states"
    :columns (input expected)
    :rows ((nil t)
           (t   nil))
    :body (let ((nskk-debug-enabled input))
            (nskk-debug-toggle)
            (should (eq nskk-debug-enabled expected)))))

(provide 'nskk-debug-integration-test)

;;; nskk-debug-integration-test.el ends here
