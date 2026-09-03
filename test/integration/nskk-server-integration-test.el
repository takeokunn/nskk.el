;;; nskk-server-integration-test.el --- skkserv integration tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; skkserv integration tests.

;;; Code:

(require 'ert)
(require 'nskk-test-macros)
(require 'nskk-server)
(require 'nskk-custom)

;;;; Test utilities

(progn
  (require 'cl-lib)

  (defmacro nskk--server-with-disabled (&rest body)
    "Execute BODY with nskk-server-enable forced to nil."
    `(let ((nskk-server-enable nil)
           (nskk--server-process nil))
       ,@body))

  (defun nskk--server-delete-mock-process (process)
    "Delete mock PROCESS while treating cleanup faults as best effort."
    (let ((live-p
           (condition-case nil
               (process-live-p process)
             ((error quit) t))))
      (when live-p
        (condition-case nil
            (delete-process process)
          ((error quit) nil)))))

  (defmacro nskk--server-with-mock-cleanup (process &rest body)
    "Execute BODY and always close NSKK and delete mock PROCESS."
    (declare (indent 1) (debug t))
    (let ((process-value (cl-gensym "process-")))
      `(let ((,process-value ,process))
         (unwind-protect
             (progn ,@body)
           (unwind-protect
               (nskk-server-close)
             (nskk--server-delete-mock-process ,process-value)))))))

;;;; Disabled-mode integration tests (no network required)

(nskk-describe "server disabled mode"

  (nskk-it "returns nil from lookup for any reading"
    (nskk--server-with-disabled
     (should (null (nskk-server-lookup "かんじ")))
     (should (null (nskk-server-lookup "てすと")))
     (should (null (nskk-server-lookup "")))))

  (nskk-it "returns nil from ensure-open"
    (nskk--server-with-disabled
     (should (null (nskk-server-ensure-open)))))

  (nskk-it "returns nil from live-p when no process"
    (nskk--server-with-disabled
     (should (null (nskk-server-live-p)))))

  (nskk-it "close is safe to call when not connected"
    (nskk--server-with-disabled
     (should-not (condition-case nil
                     (progn (nskk-server-close) nil)
                   (error t))))))

;;;; Protocol tests using in-process mock skkserv

(nskk-describe "server in-process mock skkserv"

  (nskk-it "looks up Japanese over same-base EUC-JP and UTF-8 sockets"
    (dolist (coding-pair
             '((euc-jp . euc-jp-unix)
               (utf-8 . utf-8-unix)))
      (let* ((wire-coding (car coding-pair))
             (configured-coding (cdr coding-pair))
             (nskk-server-coding-system wire-coding)
             (mock
              (nskk--server-start-mock-server
               '(("あ" . "1/亜/阿/唖/\n"))))
             (server-proc (car mock))
             (port (cdr mock))
             (nskk-server-enable t)
             (nskk-server-host "127.0.0.1")
             (nskk-server-portnum port)
             (nskk--server-process nil)
             (nskk--server-kill-emacs-hook-registered nil))
        (nskk--server-with-mock-cleanup server-proc
          (should (nskk-server-open))
          (should (nskk-server-live-p))
          (should
           (coding-system-equal
            wire-coding
            (cdr (process-coding-system nskk--server-process))))
          (let ((nskk-server-coding-system configured-coding))
            (should
             (equal (nskk-server-lookup "あ")
                    '("亜" "阿" "唖"))))))))

  (nskk-it "should return nil for keys not in the dictionary"
    (let* ((mock (nskk--server-start-mock-server '(("あ" . "1/亜/阿/唖/\n"))))
           (server-proc (car mock))
           (port (cdr mock))
           (nskk-server-enable t)
           (nskk-server-host "127.0.0.1")
           (nskk-server-portnum port)
           (nskk--server-process nil)
           (nskk--server-kill-emacs-hook-registered nil))
      (nskk--server-with-mock-cleanup server-proc
        (should (nskk-server-open))
        (let ((result (nskk-server-lookup "zzzzzzzzzzzzz")))
          (should (null result))))))

  (nskk-it "deletes the mock when close signals error or quit"
    (dolist (condition '(error quit))
      (let* ((mock (nskk--server-start-mock-server nil))
             (server-proc (car mock))
             caught)
        (unwind-protect
            (progn
              (setq
               caught
               (cl-letf (((symbol-function 'nskk-server-close)
                          (lambda ()
                            (signal condition '(injected-close)))))
                 (condition-case data
                     (progn
                       (nskk--server-with-mock-cleanup server-proc)
                       nil)
                   ((error quit) data))))
              (should (eq (car caught) condition))
              (should (equal (cdr caught) '(injected-close)))
              (should-not (process-live-p server-proc)))
          (nskk--server-delete-mock-process server-proc))))))

(provide 'nskk-server-integration-test)

;;; nskk-server-integration-test.el ends here
