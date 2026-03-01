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

;;;; Live skkserv tests (requires NSKK_TEST_SKKSERV env var)

(defun nskk-server--integration-live-p ()
  "Return non-nil if live skkserv tests should run."
  (getenv "NSKK_TEST_SKKSERV"))

(ert-deftest nskk-server-integration-live-connect-and-lookup ()
  "Connect to a live skkserv and look up a basic reading."
  (skip-unless (nskk-server--integration-live-p))
  (let* ((spec (getenv "NSKK_TEST_SKKSERV"))
         (parts (split-string spec ":"))
         (host (car parts))
         (port (if (cadr parts) (string-to-number (cadr parts)) 1178))
         (nskk-server-enable t)
         (nskk-server-host host)
         (nskk-server-portnum port)
         (nskk-server--process nil)
         (nskk-server--kill-emacs-hook-registered nil))
    (unwind-protect
        (progn
          (should (nskk-server-open))
          (should (nskk-server-live-p))
          ;; "あ" should return at least one candidate from any skkserv
          (let ((result (nskk-server-lookup "あ")))
            (should (listp result))))
      (nskk-server-close))))

(ert-deftest nskk-server-integration-live-not-found-returns-nil ()
  "skkserv returns nil for keys not in the dictionary."
  (skip-unless (nskk-server--integration-live-p))
  (let* ((spec (getenv "NSKK_TEST_SKKSERV"))
         (parts (split-string spec ":"))
         (host (car parts))
         (port (if (cadr parts) (string-to-number (cadr parts)) 1178))
         (nskk-server-enable t)
         (nskk-server-host host)
         (nskk-server-portnum port)
         (nskk-server--process nil)
         (nskk-server--kill-emacs-hook-registered nil))
    (unwind-protect
        (progn
          (should (nskk-server-open))
          ;; Nonsense key should return nil (server sends "4...")
          (let ((result (nskk-server-lookup "zzzzzzzzzzzzz")))
            (should (null result))))
      (nskk-server-close))))

(provide 'nskk-server-integration-test)

;;; nskk-server-integration-test.el ends here
