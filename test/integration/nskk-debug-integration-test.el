;;; nskk-debug-integration-test.el --- Debug module integration tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Integration tests for nskk-debug.el: paths that cross into `nskk-custom'
;; or drive the whole log pipeline.  Each command's behaviour in isolation is
;; covered by test/unit/nskk-debug-test.el and is not repeated here.

;;; Code:

(require 'ert)
(require 'subr-x)
(require 'nskk-custom)
(require 'nskk-debug)
(require 'nskk-test-macros)

;;; Helpers

(defun nskk-debug-integration-test--line-count ()
  "Return the number of lines currently in the debug buffer."
  (with-current-buffer (nskk--debug-buffer)
    (let ((raw (buffer-string)))
      (if (string= raw "")
          0
        (length (split-string (string-trim-right raw "\n") "\n"))))))

;;; nskk-debug-max-entries drives trimming

(nskk-describe "nskk-debug-max-entries drives trimming"
  (nskk-it "caps the buffer at the customized limit while logging"
    (let ((nskk-debug-enabled t)
          (nskk-debug-max-entries 5))
      (nskk-debug-clear)
      (dotimes (i 12)
        (nskk-debug-log "entry %d" i))
      (should (= (nskk-debug-integration-test--line-count) 5))))

  (nskk-it "keeps the newest entries and drops the oldest"
    (let ((nskk-debug-enabled t)
          (nskk-debug-max-entries 3))
      (nskk-debug-clear)
      (dotimes (i 6)
        (nskk-debug-log "entry %d" i))
      (let ((contents (with-current-buffer (nskk--debug-buffer)
                        (buffer-string))))
        (should (string-match-p "entry 5" contents))
        (should-not (string-match-p "entry 0" contents)))))

  (nskk-it "applies a limit raised after the buffer already filled"
    (let ((nskk-debug-enabled t)
          (nskk-debug-max-entries 2))
      (nskk-debug-clear)
      (dotimes (i 4)
        (nskk-debug-log "early %d" i))
      (should (= (nskk-debug-integration-test--line-count) 2))
      (setq nskk-debug-max-entries 6)
      (dotimes (i 4)
        (nskk-debug-log "late %d" i))
      (should (= (nskk-debug-integration-test--line-count) 6)))))

;;; nskk-debug-message input variations

(nskk-deftest-table debug-message-inputs
  :description "nskk-debug-message carries input forms through to the buffer"
  :columns (input expected)
  :rows (("multi-line-marker\nsecond-line" "multi-line-marker")
         ("日本語テスト-unicode-marker" "日本語テスト-unicode-marker")
         ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-long-marker" "long-marker")
         ("control-chars-marker\t\r" "control-chars-marker"))
  :body (let ((nskk-debug-enabled t)
              (nskk-debug-max-entries 100))
          (nskk-debug-clear)
          (nskk-debug-message "%s" input)
          (with-current-buffer (nskk--debug-buffer)
            (should (string-match-p (regexp-quote expected) (buffer-string))))))

;;; Provide

(provide 'nskk-debug-integration-test)

;;; nskk-debug-integration-test.el ends here
