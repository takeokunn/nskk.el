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

(nskk-describe "nskk-server--parse-response successful parsing"
  (nskk-it "parses a response with multiple candidates"
    (let ((result (nskk-server--parse-response "1/漢字/感じ/幹事/\n")))
      (should (equal result '("漢字" "感じ" "幹事")))))

  (nskk-it "parses a response with two candidates"
    (let ((result (nskk-server--parse-response "1/漢字/感じ/\n")))
      (should (equal result '("漢字" "感じ")))))

  (nskk-it "parses a response with a single candidate"
    (let ((result (nskk-server--parse-response "1/漢字/\n")))
      (should (equal result '("漢字")))))

  (nskk-it "returns a list on successful parse"
    (let ((result (nskk-server--parse-response "1/漢字/\n")))
      (should (listp result))))

  (nskk-it "returns only strings in the candidate list"
    (let ((result (nskk-server--parse-response "1/漢字/感じ/幹事/\n")))
      (should (cl-every #'stringp result))))

  (nskk-it "parses a response without trailing slash"
    (let ((result (nskk-server--parse-response "1/漢字/感じ\n")))
      (should (equal result '("漢字" "感じ"))))))

(nskk-deftest-table server-parse-response-nil-cases
  :description "nskk-server--parse-response returns nil for non-match inputs"
  :columns (input)
  :rows (("4かんじ \n")
         ("4\n")
         ("2\n")
         ("")
         (nil))
  :body (let ((result (nskk-server--parse-response input)))
          (should (null result))))

(nskk-describe "nskk-server--parse-response non-string inputs"
  (nskk-it "returns nil for a non-string integer value"
    (should (null (nskk-server--parse-response 42))))

  (nskk-it "returns nil for a non-string list value"
    (should (null (nskk-server--parse-response '("1/漢字/\n"))))))

(nskk-describe "nskk-server--parse-response annotation stripping"
  (nskk-it "strips annotations from a single annotated candidate"
    (let ((result (nskk-server--parse-response "1/漢字;注釈/感じ/\n")))
      (should (equal result '("漢字" "感じ")))))

  (nskk-it "strips annotations from all annotated candidates"
    (let ((result (nskk-server--parse-response "1/漢字;note1/感じ;note2/幹事;note3/\n")))
      (should (equal result '("漢字" "感じ" "幹事")))))

  (nskk-it "strips only annotated candidates and leaves plain ones intact"
    (let ((result (nskk-server--parse-response "1/漢字;注釈/感じ/幹事;別注/\n")))
      (should (equal result '("漢字" "感じ" "幹事"))))))

(nskk-describe "nskk-server--parse-response empty candidates list"
  (nskk-it "returns nil or empty list for a response with empty candidates"
    ;; "1/\n" splits on "/" with omit-nulls=t giving no parts, so result is nil or ()
    (let ((result (nskk-server--parse-response "1/\n")))
      (should (or (null result) (equal result '()))))))

(nskk-describe "nskk-server-live-p"
  (nskk-it "returns nil when process is nil"
    (let ((nskk-server--process nil))
      (should (null (nskk-server-live-p)))))

  (nskk-it "returns non-nil when process status is open"
    (let ((nskk-server--process 'mock-proc))
      (cl-letf (((symbol-function 'process-status)
                 (lambda (_proc) 'open)))
        (should (nskk-server-live-p)))))

  (nskk-it "returns nil when process status is closed"
    (let ((nskk-server--process 'mock-proc))
      (cl-letf (((symbol-function 'process-status)
                 (lambda (_proc) 'closed)))
        (should (null (nskk-server-live-p))))))

  (nskk-it "returns nil when process status is exit"
    (let ((nskk-server--process 'mock-proc))
      (cl-letf (((symbol-function 'process-status)
                 (lambda (_proc) 'exit)))
        (should (null (nskk-server-live-p))))))

  (nskk-it "returns nil when process status is signal"
    (let ((nskk-server--process 'mock-proc))
      (cl-letf (((symbol-function 'process-status)
                 (lambda (_proc) 'signal)))
        (should (null (nskk-server-live-p))))))

  (nskk-it "passes the stored process object to process-status"
    (let ((nskk-server--process 'my-proc)
          (received-proc nil))
      (cl-letf (((symbol-function 'process-status)
                 (lambda (proc)
                   (setq received-proc proc)
                   'open)))
        (nskk-server-live-p)
        (should (eq received-proc 'my-proc))))))

(nskk-describe "nskk-server-ensure-open"
  (nskk-it "returns nil without calling open when server is disabled"
    (let ((nskk-server-enable nil)
          (nskk-server--process nil)
          (open-called nil))
      (cl-letf (((symbol-function 'nskk-server-open)
                 (lambda () (setq open-called t) 'mock-proc)))
        (nskk-given (should (null nskk-server-enable)))
        (nskk-when  (let ((result (nskk-server-ensure-open)))
                      (nskk-then
                       (should (null result))
                       (should (null open-called))))))))

  (nskk-it "returns t without reconnecting when already live"
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

  (nskk-it "calls nskk-server-open when connection is not live"
    (let ((nskk-server-enable t)
          (nskk-server--process nil)
          (open-called nil))
      (cl-letf (((symbol-function 'nskk-server-open)
                 (lambda ()
                   (setq open-called t)
                   'new-proc)))
        (nskk-when  (nskk-server-ensure-open))
        (nskk-then  (should open-called)))))

  (nskk-it "returns t when reconnect succeeds"
    (let ((nskk-server-enable t)
          (nskk-server--process nil))
      (cl-letf (((symbol-function 'nskk-server-open)
                 (lambda () 'new-proc)))
        (let ((result (nskk-server-ensure-open)))
          (should (eq result t))))))

  (nskk-it "returns nil when nskk-server-open fails"
    (let ((nskk-server-enable t)
          (nskk-server--process nil))
      (cl-letf (((symbol-function 'nskk-server-open)
                 (lambda () nil)))
        (let ((result (nskk-server-ensure-open)))
          (should (null result)))))))

(nskk-describe "nskk-server-lookup guard conditions"
  (nskk-it "returns nil without sending when server is disabled"
    (let ((nskk-server-enable nil)
          (nskk-server--process nil)
          (send-called nil))
      (cl-letf (((symbol-function 'process-send-string)
                 (lambda (&rest _) (setq send-called t))))
        (nskk-given (should (null nskk-server-enable)))
        (nskk-when  (let ((result (nskk-server-lookup "かんじ")))
                      (nskk-then
                       (should (null result))
                       (should (null send-called))))))))

  (nskk-it "returns nil for a nil key"
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

  (nskk-it "returns nil for an empty string key"
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

  (nskk-it "returns nil when connection is not live"
    (let ((nskk-server-enable t)
          (nskk-server--process nil)
          (send-called nil))
      (cl-letf (((symbol-function 'process-send-string)
                 (lambda (&rest _) (setq send-called t))))
        (let ((result (nskk-server-lookup "かんじ")))
          (should (null result))
          (should (null send-called))))))

  (nskk-it "returns nil even when a process exists but server is disabled"
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

  (nskk-it "returns nil for a non-string integer key"
    (let ((nskk-server-enable t)
          (nskk-server--process 'mock-proc)
          (send-called nil))
      (cl-letf (((symbol-function 'process-status)
                 (lambda (_proc) 'open))
                ((symbol-function 'process-send-string)
                 (lambda (&rest _) (setq send-called t))))
        (let ((result (nskk-server-lookup 42)))
          (should (null result))
          (should (null send-called)))))))

(provide 'nskk-server-test)

;;; nskk-server-test.el ends here
