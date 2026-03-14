;;; nskk-server-integration-test.el --- skkserv integration tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Integration tests for nskk-server.el (skkserv TCP client).
;;
;; Most tests in this file require no live server and use mocks.
;; Network tests are guarded by the NSKK_TEST_SKKSERV environment
;; variable: set it to \"localhost:1178\" (or \"host:port\") to run
;; live skkserv tests against a running server instance.
;;
;; Run with live server:
;;   NSKK_TEST_SKKSERV=localhost:1178 make test

;;; Code:

(require 'ert)
(require 'nskk-test-macros)
(require 'nskk-server)
(require 'nskk-custom)

;;;; Test utilities

(defmacro nskk--server-with-disabled (&rest body)
  "Execute BODY with nskk-server-enable forced to nil."
  `(let ((nskk-server-enable nil)
         (nskk--server-process nil))
     ,@body))

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

  (nskk-it "should connect and look up a basic reading"
    (let* ((mock (nskk--server-start-mock-server '(("あ" . "1/亜/阿/唖/\n"))))
           (server-proc (car mock))
           (port (cdr mock))
           (nskk-server-enable t)
           (nskk-server-host "127.0.0.1")
           (nskk-server-portnum port)
           (nskk--server-process nil)
           (nskk--server-kill-emacs-hook-registered nil))
      (unwind-protect
          (progn
            (should (nskk-server-open))
            (should (nskk-server-live-p))
            (let ((result (nskk-server-lookup "あ")))
              (should (listp result))
              (should result)))
        (nskk-server-close)
        (delete-process server-proc))))

  (nskk-it "should return nil for keys not in the dictionary"
    (let* ((mock (nskk--server-start-mock-server '(("あ" . "1/亜/阿/唖/\n"))))
           (server-proc (car mock))
           (port (cdr mock))
           (nskk-server-enable t)
           (nskk-server-host "127.0.0.1")
           (nskk-server-portnum port)
           (nskk--server-process nil)
           (nskk--server-kill-emacs-hook-registered nil))
      (unwind-protect
          (progn
            (should (nskk-server-open))
            (let ((result (nskk-server-lookup "zzzzzzzzzzzzz")))
              (should (null result))))
        (nskk-server-close)
        (delete-process server-proc)))))

(provide 'nskk-server-integration-test)

;;; nskk-server-integration-test.el ends here
