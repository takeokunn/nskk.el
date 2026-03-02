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
(require 'nskk-server)
(require 'nskk-custom)

;;;; Test utilities

(defmacro nskk-server--with-disabled (&rest body)
  "Execute BODY with nskk-server-enable forced to nil."
  `(let ((nskk-server-enable nil)
         (nskk-server--process nil))
     ,@body))

(defmacro nskk-server--with-enabled (&rest body)
  "Execute BODY with nskk-server-enable forced to t."
  `(let ((nskk-server-enable t))
     ,@body))

;;;; Disabled-mode integration tests (no network required)

(ert-deftest nskk-server-integration-disabled-lookup-returns-nil ()
  "When server is disabled, nskk-server-lookup always returns nil."
  (nskk-server--with-disabled
   (should (null (nskk-server-lookup "かんじ")))
   (should (null (nskk-server-lookup "てすと")))
   (should (null (nskk-server-lookup "")))))

(ert-deftest nskk-server-integration-disabled-ensure-open-returns-nil ()
  "When server is disabled, nskk-server-ensure-open returns nil."
  (nskk-server--with-disabled
   (should (null (nskk-server-ensure-open)))))

(ert-deftest nskk-server-integration-disabled-live-p-returns-nil ()
  "When server is disabled and no process, nskk-server-live-p returns nil."
  (nskk-server--with-disabled
   (should (null (nskk-server-live-p)))))

(ert-deftest nskk-server-integration-close-when-not-connected-is-safe ()
  "nskk-server-close is safe to call when not connected."
  (nskk-server--with-disabled
   (should-not (condition-case nil
                   (progn (nskk-server-close) nil)
                 (error t)))))

;;;; Mock skkserv helper

(defun nskk-server--start-mock-server (responses)
  "Start an in-process mock skkserv and return (server-proc . port).
RESPONSES is an alist of (KEY . RESPONSE-STRING) pairs.
For keys not in RESPONSES the server sends a not-found \\='4...\\=' reply."
  (let ((srv (make-network-process
              :name " nskk-mock-skkserv"
              :buffer nil
              :service t
              :server t
              :noquery t
              :coding nskk-server-coding-system
              :filter
              (lambda (client string)
                (when (> (length string) 0)
                  (let ((cmd (aref string 0)))
                    (cond
                     ((= cmd ?0)
                      (delete-process client))
                     ((= cmd ?1)
                      (let* ((rest (substring string 1))
                             (space-pos (string-search " " rest))
                             (key (if space-pos
                                      (substring rest 0 space-pos)
                                    rest))
                             (response (or (cdr (assoc key responses))
                                          (concat "4" key " \n"))))
                        (process-send-string client response))))))))))
    (cons srv (process-contact srv :service))))

;;;; Protocol tests using in-process mock skkserv

(ert-deftest nskk-server-integration-live-connect-and-lookup ()
  "Connect to an in-process mock skkserv and look up a basic reading."
  (let* ((mock (nskk-server--start-mock-server '(("あ" . "1/亜/阿/唖/\n"))))
         (server-proc (car mock))
         (port (cdr mock))
         (nskk-server-enable t)
         (nskk-server-host "127.0.0.1")
         (nskk-server-portnum port)
         (nskk-server--process nil)
         (nskk-server--kill-emacs-hook-registered nil))
    (unwind-protect
        (progn
          (should (nskk-server-open))
          (should (nskk-server-live-p))
          (let ((result (nskk-server-lookup "あ")))
            (should (listp result))
            (should result)))
      (nskk-server-close)
      (delete-process server-proc))))

(ert-deftest nskk-server-integration-live-not-found-returns-nil ()
  "In-process mock skkserv returns nil for keys not in the dictionary."
  (let* ((mock (nskk-server--start-mock-server '(("あ" . "1/亜/阿/唖/\n"))))
         (server-proc (car mock))
         (port (cdr mock))
         (nskk-server-enable t)
         (nskk-server-host "127.0.0.1")
         (nskk-server-portnum port)
         (nskk-server--process nil)
         (nskk-server--kill-emacs-hook-registered nil))
    (unwind-protect
        (progn
          (should (nskk-server-open))
          (let ((result (nskk-server-lookup "zzzzzzzzzzzzz")))
            (should (null result))))
      (nskk-server-close)
      (delete-process server-proc))))

(provide 'nskk-server-integration-test)

;;; nskk-server-integration-test.el ends here
