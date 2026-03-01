;;; nskk-server-test.el --- Tests for nskk-server.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;; NSKK is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; NSKK is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with NSKK.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Unit tests for nskk-server.el covering:
;; - Response parsing (pure function, no network)
;; - nskk-server-live-p guard logic
;; - nskk-server-ensure-open guard logic
;; - nskk-server-lookup guard conditions (no network)

;;; Code:

(require 'ert)
(require 'nskk-server)
(require 'nskk-test-framework)

;;;
;;; Response Parsing Tests
;;;

(nskk-deftest-unit server-parse-response-multiple-candidates
  "Test parsing a response with multiple candidates."
  (let ((result (nskk-server--parse-response "1/漢字/感じ/幹事/\n")))
    (should (equal result '("漢字" "感じ" "幹事")))))

(nskk-deftest-unit server-parse-response-two-candidates
  "Test parsing a response with two candidates."
  (let ((result (nskk-server--parse-response "1/漢字/感じ/\n")))
    (should (equal result '("漢字" "感じ")))))

(nskk-deftest-unit server-parse-response-single-candidate
  "Test parsing a response with a single candidate."
  (let ((result (nskk-server--parse-response "1/漢字/\n")))
    (should (equal result '("漢字")))))

(nskk-deftest-unit server-parse-response-not-found
  "Test parsing a not-found (command 4) response."
  (let ((result (nskk-server--parse-response "4かんじ \n")))
    (should (null result))))

(nskk-deftest-unit server-parse-response-not-found-bare
  "Test parsing a bare command-4 response."
  (let ((result (nskk-server--parse-response "4\n")))
    (should (null result))))

(nskk-deftest-unit server-parse-response-error-response
  "Test parsing an error response (command 2)."
  (let ((result (nskk-server--parse-response "2\n")))
    (should (null result))))

(nskk-deftest-unit server-parse-response-empty-string
  "Test parsing an empty string returns nil."
  (let ((result (nskk-server--parse-response "")))
    (should (null result))))

(nskk-deftest-unit server-parse-response-nil
  "Test parsing nil returns nil."
  (let ((result (nskk-server--parse-response nil)))
    (should (null result))))

(nskk-deftest-unit server-parse-response-non-string
  "Test parsing a non-string value returns nil."
  (should (null (nskk-server--parse-response 42)))
  (should (null (nskk-server--parse-response '("1/漢字/\n")))))

(nskk-deftest-unit server-parse-response-annotation-stripping
  "Test that annotations (word;note) are stripped from candidates."
  (let ((result (nskk-server--parse-response "1/漢字;注釈/感じ/\n")))
    (should (equal result '("漢字" "感じ")))))

(nskk-deftest-unit server-parse-response-annotation-stripping-multiple
  "Test that all annotated candidates are stripped correctly."
  (let ((result (nskk-server--parse-response "1/漢字;note1/感じ;note2/幹事;note3/\n")))
    (should (equal result '("漢字" "感じ" "幹事")))))

(nskk-deftest-unit server-parse-response-annotation-partial
  "Test that only annotated candidates are stripped, others left intact."
  (let ((result (nskk-server--parse-response "1/漢字;注釈/感じ/幹事;別注/\n")))
    (should (equal result '("漢字" "感じ" "幹事")))))

(nskk-deftest-unit server-parse-response-empty-candidates
  "Test parsing a response with empty candidates list returns nil or empty."
  ;; "1/\n" splits on "/" with omit-nulls=t giving no parts, so result is nil or ()
  (let ((result (nskk-server--parse-response "1/\n")))
    (should (or (null result) (equal result '())))))

(nskk-deftest-unit server-parse-response-no-trailing-slash
  "Test parsing a response without trailing slash still works."
  (let ((result (nskk-server--parse-response "1/漢字/感じ\n")))
    (should (equal result '("漢字" "感じ")))))

(nskk-deftest-unit server-parse-response-returns-list
  "Test that a successful parse always returns a list."
  (let ((result (nskk-server--parse-response "1/漢字/\n")))
    (should (listp result))))

(nskk-deftest-unit server-parse-response-strings-only
  "Test that all returned candidates are strings."
  (let ((result (nskk-server--parse-response "1/漢字/感じ/幹事/\n")))
    (should (cl-every #'stringp result))))

;;;
;;; nskk-server-live-p Tests
;;;

(nskk-deftest-unit server-live-p-nil-process
  "Test live-p returns nil when process is nil."
  (let ((nskk-server--process nil))
    (should (null (nskk-server-live-p)))))

(nskk-deftest-unit server-live-p-open-process
  "Test live-p returns non-nil when process status is open."
  (let ((nskk-server--process 'mock-proc))
    (cl-letf (((symbol-function 'process-status)
               (lambda (_proc) 'open)))
      (should (nskk-server-live-p)))))

(nskk-deftest-unit server-live-p-closed-process
  "Test live-p returns nil when process status is closed."
  (let ((nskk-server--process 'mock-proc))
    (cl-letf (((symbol-function 'process-status)
               (lambda (_proc) 'closed)))
      (should (null (nskk-server-live-p))))))

(nskk-deftest-unit server-live-p-exit-process
  "Test live-p returns nil when process status is exit."
  (let ((nskk-server--process 'mock-proc))
    (cl-letf (((symbol-function 'process-status)
               (lambda (_proc) 'exit)))
      (should (null (nskk-server-live-p))))))

(nskk-deftest-unit server-live-p-signal-process
  "Test live-p returns nil when process status is signal."
  (let ((nskk-server--process 'mock-proc))
    (cl-letf (((symbol-function 'process-status)
               (lambda (_proc) 'signal)))
      (should (null (nskk-server-live-p))))))

(nskk-deftest-unit server-live-p-checks-correct-process
  "Test live-p passes the stored process object to process-status."
  (let ((nskk-server--process 'my-proc)
        (received-proc nil))
    (cl-letf (((symbol-function 'process-status)
               (lambda (proc)
                 (setq received-proc proc)
                 'open)))
      (nskk-server-live-p)
      (should (eq received-proc 'my-proc)))))

;;;
;;; nskk-server-ensure-open Tests
;;;

(nskk-deftest-unit server-ensure-open-disabled
  "Test ensure-open returns nil without calling open when server is disabled."
  (let ((nskk-server-enable nil)
        (nskk-server--process nil)
        (open-called nil))
    (cl-letf (((symbol-function 'nskk-server-open)
               (lambda () (setq open-called t) 'mock-proc)))
      (let ((result (nskk-server-ensure-open)))
        (should (null result))
        (should (null open-called))))))

(nskk-deftest-unit server-ensure-open-already-live
  "Test ensure-open returns t without reconnecting when already live."
  (let ((nskk-server-enable t)
        (nskk-server--process 'mock-proc)
        (open-called nil))
    (cl-letf (((symbol-function 'process-status)
               (lambda (_proc) 'open))
              ((symbol-function 'nskk-server-open)
               (lambda () (setq open-called t) 'mock-proc)))
      (let ((result (nskk-server-ensure-open)))
        (should (eq result t))
        (should (null open-called))))))

(nskk-deftest-unit server-ensure-open-reconnects-when-not-live
  "Test ensure-open calls nskk-server-open when connection is not live."
  (let ((nskk-server-enable t)
        (nskk-server--process nil)
        (open-called nil))
    (cl-letf (((symbol-function 'nskk-server-open)
               (lambda ()
                 (setq open-called t)
                 'new-proc)))
      (nskk-server-ensure-open)
      (should open-called))))

(nskk-deftest-unit server-ensure-open-returns-t-on-success
  "Test ensure-open returns t when reconnect succeeds."
  (let ((nskk-server-enable t)
        (nskk-server--process nil))
    (cl-letf (((symbol-function 'nskk-server-open)
               (lambda () 'new-proc)))
      (let ((result (nskk-server-ensure-open)))
        (should (eq result t))))))

(nskk-deftest-unit server-ensure-open-returns-nil-when-open-fails
  "Test ensure-open returns nil when nskk-server-open fails."
  (let ((nskk-server-enable t)
        (nskk-server--process nil))
    (cl-letf (((symbol-function 'nskk-server-open)
               (lambda () nil)))
      (let ((result (nskk-server-ensure-open)))
        (should (null result))))))

;;;
;;; nskk-server-lookup Guard Tests (no network)
;;;

(nskk-deftest-unit server-lookup-disabled-returns-nil
  "Test lookup returns nil without sending when server is disabled."
  (let ((nskk-server-enable nil)
        (nskk-server--process nil)
        (send-called nil))
    (cl-letf (((symbol-function 'process-send-string)
               (lambda (&rest _) (setq send-called t))))
      (let ((result (nskk-server-lookup "かんじ")))
        (should (null result))
        (should (null send-called))))))

(nskk-deftest-unit server-lookup-nil-key-returns-nil
  "Test lookup returns nil for a nil key."
  (let ((nskk-server-enable t)
        (nskk-server--process 'mock-proc)
        (send-called nil))
    (cl-letf (((symbol-function 'process-status)
               (lambda (_proc) 'open))
              ((symbol-function 'process-send-string)
               (lambda (&rest _) (setq send-called t))))
      (let ((result (nskk-server-lookup nil)))
        (should (null result))
        (should (null send-called))))))

(nskk-deftest-unit server-lookup-empty-key-returns-nil
  "Test lookup returns nil for an empty string key."
  (let ((nskk-server-enable t)
        (nskk-server--process 'mock-proc)
        (send-called nil))
    (cl-letf (((symbol-function 'process-status)
               (lambda (_proc) 'open))
              ((symbol-function 'process-send-string)
               (lambda (&rest _) (setq send-called t))))
      (let ((result (nskk-server-lookup "")))
        (should (null result))
        (should (null send-called))))))

(nskk-deftest-unit server-lookup-not-live-returns-nil
  "Test lookup returns nil when connection is not live."
  (let ((nskk-server-enable t)
        (nskk-server--process nil)
        (send-called nil))
    (cl-letf (((symbol-function 'process-send-string)
               (lambda (&rest _) (setq send-called t))))
      (let ((result (nskk-server-lookup "かんじ")))
        (should (null result))
        (should (null send-called))))))

(nskk-deftest-unit server-lookup-disabled-ignores-live-process
  "Test lookup returns nil even when a process exists but server is disabled."
  (let ((nskk-server-enable nil)
        (nskk-server--process 'mock-proc)
        (send-called nil))
    (cl-letf (((symbol-function 'process-status)
               (lambda (_proc) 'open))
              ((symbol-function 'process-send-string)
               (lambda (&rest _) (setq send-called t))))
      (let ((result (nskk-server-lookup "かんじ")))
        (should (null result))
        (should (null send-called))))))

(nskk-deftest-unit server-lookup-non-string-key-returns-nil
  "Test lookup returns nil for a non-string key (integer)."
  (let ((nskk-server-enable t)
        (nskk-server--process 'mock-proc)
        (send-called nil))
    (cl-letf (((symbol-function 'process-status)
               (lambda (_proc) 'open))
              ((symbol-function 'process-send-string)
               (lambda (&rest _) (setq send-called t))))
      (let ((result (nskk-server-lookup 42)))
        (should (null result))
        (should (null send-called))))))

(provide 'nskk-server-test)

;;; nskk-server-test.el ends here
