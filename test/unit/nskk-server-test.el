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
;; - nskk--server-strip-annotation: annotation removal from candidates
;; - nskk--server-parse-response: protocol response parsing (Prolog dispatch)
;; - nskk--server-lookup-guards-p: composite guard predicate (CPS)
;; - nskk-server-live-p: process status + Prolog state check (CPS)
;; - nskk-server-open: connection setup (mocked)
;; - nskk-server-close: disconnect and cleanup (mocked)
;; - nskk-server-ensure-open: reconnect logic (mocked)
;; - nskk-server-lookup: guard conditions (no network)
;; - PBT: parse invariants (all-strings, no-annotation, non-1-returns-nil)
;;
;; Prolog state setup note:
;; `nskk-server-live-p' checks server-state/1 in addition to process-status.
;; Tests that need live-p to return t must set server-state to open and
;; restore it to closed in an unwind-protect.  The helper macro
;; `nskk--server-test-with-open-state' encapsulates this pattern.

;;; Code:

(require 'ert)
(require 'nskk-server)
(require 'nskk-prolog)
(require 'nskk-test-framework)
(require 'nskk-test-macros)

;;;; Test helper: temporarily set server-state to open

(defmacro nskk--server-test-with-open-state (&rest body)
  "Execute BODY with server-state Prolog fact set to open.
Restores server-state to closed in an unwind-protect."
  `(progn
     (nskk-prolog-retract-all 'server-state 1)
     (nskk-prolog-assert '((server-state open)))
     (unwind-protect
         (progn ,@body)
       (nskk-prolog-retract-all 'server-state 1)
       (nskk-prolog-assert '((server-state closed))))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; nskk--server-strip-annotation
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-describe "nskk--server-strip-annotation"
  (nskk-context "annotated candidates"
    (nskk-it "strips annotation from a word;note pair"
      (nskk-deftest-table server-strip-annotation-cases
        :columns (input expected)
        :rows (("漢字;注釈"      "漢字")
               ("感じ;note"      "感じ")
               ("幹事;long note" "幹事")
               ("a;b"            "a"))
        :body (should (equal (nskk--server-strip-annotation input) expected))))

    (nskk-it "strips only up to the first semicolon when multiple exist"
      (should (equal (nskk--server-strip-annotation "a;b;c") "a"))))

  (nskk-context "plain candidates without annotation"
    (nskk-it "returns the string unchanged when no semicolon is present"
      (nskk-deftest-table server-strip-annotation-plain-cases
        :columns (input)
        :rows (("漢字") ("感じ") ("幹事") ("") ("a"))
        :body (should (equal (nskk--server-strip-annotation input) input))))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Prolog server-response-type/2 facts
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-describe "server-response-type/2 Prolog facts"
  (nskk-it "maps prefix \"1\" to found"
    (should (nskk-prolog-holds-p '(server-response-type "1" found))))

  (nskk-it "maps prefix \"4\" to miss"
    (should (nskk-prolog-holds-p '(server-response-type "4" miss))))

  (nskk-it "does not map prefix \"1\" to miss"
    (should (null (nskk-prolog-holds-p '(server-response-type "1" miss)))))

  (nskk-it "does not map unknown prefix \"2\" to found"
    (should (null (nskk-prolog-holds-p '(server-response-type "2" found))))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; nskk--server-parse-response: successful parsing
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-describe "nskk--server-parse-response successful parsing"
  (nskk-context "candidate count"
    (nskk-it "parses responses with one, two, and three candidates"
      (nskk-deftest-table parse-candidate-counts
        :columns (response expected)
        :rows (("1/漢字/\n"             ("漢字"))
               ("1/漢字/感じ/\n"        ("漢字" "感じ"))
               ("1/漢字/感じ/幹事/\n"   ("漢字" "感じ" "幹事")))
        :body (should (equal (nskk--server-parse-response response) expected)))))

  (nskk-context "response format variants"
    (nskk-it "parses a response without trailing slash"
      (should (equal (nskk--server-parse-response "1/漢字/感じ\n")
                     '("漢字" "感じ"))))

    (nskk-it "strips CRLF line endings correctly"
      (should (equal (nskk--server-parse-response "1/漢字/\r\n")
                     '("漢字"))))

    (nskk-it "trims whitespace around candidates"
      (should (equal (nskk--server-parse-response "1/ 漢字 /\n")
                     '("漢字")))))

  (nskk-context "return type"
    (nskk-it "returns a list on successful parse"
      (should (listp (nskk--server-parse-response "1/漢字/\n"))))

    (nskk-it "returns only strings in the candidate list"
      (let ((result (nskk--server-parse-response "1/漢字/感じ/幹事/\n")))
        (should (cl-every #'stringp result)))))

  (nskk-context "annotation stripping"
    (nskk-it "strips annotations from all annotated candidates"
      (nskk-deftest-table parse-annotation-stripping
        :columns (response expected)
        :rows (("1/漢字;注釈/感じ/\n"             ("漢字" "感じ"))
               ("1/漢字;n1/感じ;n2/幹事;n3/\n"   ("漢字" "感じ" "幹事"))
               ("1/漢字;注釈/感じ/幹事;別注/\n"   ("漢字" "感じ" "幹事")))
        :body (should (equal (nskk--server-parse-response response) expected))))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; nskk--server-parse-response: nil cases
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-describe "nskk--server-parse-response nil cases"
  (nskk-context "protocol not-found, error responses, and invalid inputs"
    (nskk-it "returns nil for all non-starting-with-1 and invalid inputs"
      (nskk-deftest-table parse-nil-all-cases
        :columns (input)
        :rows (("4かんじ \n")
               ("4\n")
               ("2\n")
               ("0")
               ("")
               (nil)
               (42)
               (("1/漢字/\n")))
        :body (should (null (nskk--server-parse-response input))))))

  (nskk-context "empty candidates body"
    (nskk-it "returns nil or empty list for a response with no candidates"
      ;; "1/\n" splits on "/" with omit-nulls=t giving no parts
      (let ((result (nskk--server-parse-response "1/\n")))
        (should (or (null result) (equal result '()))))))

  (nskk-context "boundary: leading character"
    (nskk-it "returns nil for any response not starting with the found byte"
      ;; Characters other than '1' should always yield nil
      (dolist (ch '(?0 ?2 ?3 ?4 ?5 ?9 ?a ?A))
        (let ((resp (concat (list ch) "/漢字/\n")))
          (should (null (nskk--server-parse-response resp))))))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; PBT: nskk--server-parse-response invariants
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-property-test server-candidates-are-strings
  ((word kanji-string))
  (let* ((resp   (concat "1/" word "/\n"))
         (result (nskk--server-parse-response resp)))
    (or (null result) (cl-every #'stringp result)))
  30)

(nskk-property-test server-no-semicolon-in-output
  ((word kanji-string)
   (note romaji-string))
  (let* ((resp   (concat "1/" word ";" note "/\n"))
         (result (nskk--server-parse-response resp)))
    (or (null result)
        (not (cl-some (lambda (s) (string-search ";" s)) result))))
  30)

(nskk-property-test server-non-found-byte-returns-nil
  ((word kanji-string))
  ;; Prepend '4' (not-found byte) — must always return nil
  (null (nskk--server-parse-response (concat "4" word " \n")))
  30)

;;; ─────────────────────────────────────────────────────────────────────────
;;; nskk--server-lookup-guards-p
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-describe "nskk--server-lookup-guards-p"
  (nskk-it "returns nil when server is disabled"
    (let ((nskk-server-enable nil)
          (nskk--server-process nil))
      (should (null (nskk--server-lookup-guards-p "かんじ")))))

  (nskk-it "returns nil for nil key"
    (let ((nskk-server-enable t)
          (nskk--server-process 'mock-proc))
      (nskk--server-test-with-open-state
       (nskk-with-mocks ((process-status (lambda (_) 'open)))
         (should (null (nskk--server-lookup-guards-p nil)))))))

  (nskk-it "returns nil for empty string key"
    (let ((nskk-server-enable t)
          (nskk--server-process 'mock-proc))
      (nskk--server-test-with-open-state
       (nskk-with-mocks ((process-status (lambda (_) 'open)))
         (should (null (nskk--server-lookup-guards-p "")))))))

  (nskk-it "returns nil for non-string key"
    (let ((nskk-server-enable t)
          (nskk--server-process 'mock-proc))
      (nskk--server-test-with-open-state
       (nskk-with-mocks ((process-status (lambda (_) 'open)))
         (should (null (nskk--server-lookup-guards-p 42)))))))

  (nskk-it "returns nil when connection is not live"
    (let ((nskk-server-enable t)
          (nskk--server-process nil))
      (should (null (nskk--server-lookup-guards-p "かんじ")))))

  (nskk-it "returns non-nil when all guards pass"
    (let ((nskk-server-enable t)
          (nskk--server-process 'mock-proc))
      (nskk--server-test-with-open-state
       (nskk-with-mocks ((process-status (lambda (_) 'open)))
         (should (nskk--server-lookup-guards-p "かんじ")))))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; nskk-server-live-p
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-describe "nskk-server-live-p"
  (nskk-it "returns nil when process is nil"
    (let ((nskk--server-process nil))
      (should (null (nskk-server-live-p)))))

  (nskk-it "returns non-nil when process status is open and state is open"
    (let ((nskk--server-process 'mock-proc))
      (nskk--server-test-with-open-state
       (nskk-with-mocks ((process-status (lambda (_) 'open)))
         (should (nskk-server-live-p))))))

  (nskk-it "returns nil for non-open process statuses even when state is open"
    (let ((nskk--server-process 'mock-proc))
      (nskk--server-test-with-open-state
       (dolist (status '(closed exit signal stop))
         (nskk-with-mocks ((process-status (lambda (_) status)))
           (should (null (nskk-server-live-p))))))))

  (nskk-it "returns nil when process is open but server-state is closed"
    (let ((nskk--server-process 'mock-proc))
      ;; server-state defaults to closed — no setup needed
      (nskk-with-mocks ((process-status (lambda (_) 'open)))
        (should (null (nskk-server-live-p))))))

  (nskk-it "passes the stored process object to process-status"
    (let ((nskk--server-process 'my-proc)
          (received-proc nil))
      (nskk--server-test-with-open-state
       (nskk-with-mocks ((process-status (lambda (proc) (setq received-proc proc) 'open)))
         (nskk-server-live-p)
         (should (eq received-proc 'my-proc)))))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; nskk-server-open
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-describe "nskk-server-open"
  (nskk-it "returns nil immediately when server is disabled"
    (let ((nskk-server-enable nil)
          (nskk--server-process nil)
          (open-called nil))
      (nskk-with-mocks ((open-network-stream (lambda (&rest _) (setq open-called t) 'mock-proc)))
        (nskk-given (should (null nskk-server-enable)))
        (nskk-when  (let ((result (nskk-server-open)))
                      (nskk-then
                       (should (null result))
                       (should (null open-called))))))))

  (nskk-it "calls open-network-stream with configured host and port"
    (let ((nskk-server-enable t)
          (nskk-server-host "test-host")
          (nskk-server-portnum 9999)
          (nskk--server-process nil)
          (nskk--server-kill-emacs-hook-registered t)
          (received-host nil)
          (received-port nil))
      (nskk-with-mocks ((open-network-stream
                         (lambda (_name _buf host port &rest _)
                           (setq received-host host received-port port)
                           'mock-proc))
                        (set-process-query-on-exit-flag (lambda (&rest _) nil))
                        (process-buffer (lambda (_) nil)))
        (unwind-protect
            (progn
              (nskk-server-open)
              (should (equal received-host "test-host"))
              (should (= received-port 9999)))
          (nskk-prolog-retract-all 'server-state 1)
          (nskk-prolog-assert '((server-state closed)))))))

  (nskk-it "sets nskk--server-process on successful connection"
    (let ((nskk-server-enable t)
          (nskk--server-process nil)
          (nskk--server-kill-emacs-hook-registered t))
      (nskk-with-mocks ((open-network-stream (lambda (&rest _) 'new-proc))
                        (set-process-query-on-exit-flag (lambda (&rest _) nil))
                        (process-buffer (lambda (_) nil)))
        (unwind-protect
            (progn
              (nskk-when  (nskk-server-open))
              (nskk-then  (should (eq nskk--server-process 'new-proc))))
          (setq nskk--server-process nil)
          (nskk-prolog-retract-all 'server-state 1)
          (nskk-prolog-assert '((server-state closed)))))))

  (nskk-it "sets server-state to open on successful connection"
    (let ((nskk-server-enable t)
          (nskk--server-process nil)
          (nskk--server-kill-emacs-hook-registered t))
      (nskk-with-mocks ((open-network-stream (lambda (&rest _) 'mock-proc))
                        (set-process-query-on-exit-flag (lambda (&rest _) nil))
                        (process-buffer (lambda (_) nil)))
        (unwind-protect
            (progn
              (nskk-server-open)
              (should (nskk-prolog-holds-p '(server-state open))))
          (setq nskk--server-process nil)
          (nskk-prolog-retract-all 'server-state 1)
          (nskk-prolog-assert '((server-state closed)))))))

  (nskk-it "returns nil and clears process on connection failure"
    (let ((nskk-server-enable t)
          (nskk--server-process nil)
          (nskk--server-kill-emacs-hook-registered t))
      (nskk-with-mocks ((open-network-stream (lambda (&rest _) (error "connection refused"))))
        (let ((result (nskk-server-open)))
          (should (null result))
          (should (null nskk--server-process))))))

  (nskk-it "registers kill-emacs-hook only once (idempotent)"
    (let ((nskk-server-enable t)
          (nskk--server-process nil)
          (nskk--server-kill-emacs-hook-registered nil)
          (add-hook-count 0))
      (nskk-with-mocks ((open-network-stream (lambda (&rest _) 'mock-proc))
                        (set-process-query-on-exit-flag (lambda (&rest _) nil))
                        (add-hook (lambda (&rest _) (cl-incf add-hook-count)))
                        (process-buffer (lambda (_) nil)))
        (unwind-protect
            (progn
              (nskk-server-open)
              (setq nskk--server-process nil)
              (nskk-server-open)
              (should (= add-hook-count 1)))
          (setq nskk--server-process nil)
          (nskk-prolog-retract-all 'server-state 1)
          (nskk-prolog-assert '((server-state closed))))))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; nskk-server-close
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-describe "nskk-server-close"
  (nskk-it "is a no-op when already disconnected (idempotent)"
    (let ((nskk--server-process nil)
          (send-called nil))
      (nskk-with-mocks ((process-send-string (lambda (&rest _) (setq send-called t))))
        (nskk-server-close)
        (should (null send-called))
        (should (null nskk--server-process)))))

  (nskk-it "sends '0' disconnect command when connection is live"
    (let ((nskk--server-process 'mock-proc)
          (sent-string nil))
      (nskk--server-test-with-open-state
       (nskk-with-mocks ((process-status (lambda (_) 'open))
                         (process-send-string (lambda (_ s) (setq sent-string s)))
                         (delete-process (lambda (_) nil))
                         (get-buffer (lambda (_) nil)))
         (nskk-server-close)
         (should (equal sent-string "0"))))))

  (nskk-it "does not send '0' when process is not live"
    (let ((nskk--server-process 'mock-proc)
          (send-called nil))
      (nskk-with-mocks ((process-status (lambda (_) 'closed))
                        (process-send-string (lambda (&rest _) (setq send-called t)))
                        (delete-process (lambda (_) nil))
                        (get-buffer (lambda (_) nil)))
        (nskk-server-close)
        (should (null send-called)))))

  (nskk-it "calls delete-process to tear down the process"
    (let ((nskk--server-process 'mock-proc)
          (delete-called nil))
      (nskk-with-mocks ((process-status (lambda (_) 'closed))
                        (process-send-string (lambda (&rest _) nil))
                        (delete-process (lambda (_) (setq delete-called t)))
                        (get-buffer (lambda (_) nil)))
        (nskk-server-close)
        (should delete-called))))

  (nskk-it "sets nskk--server-process to nil after closing"
    (let ((nskk--server-process 'mock-proc))
      (nskk-with-mocks ((process-status (lambda (_) 'closed))
                        (process-send-string (lambda (&rest _) nil))
                        (delete-process (lambda (_) nil))
                        (get-buffer (lambda (_) nil)))
        (nskk-when  (nskk-server-close))
        (nskk-then  (should (null nskk--server-process))))))

  (nskk-it "sets server-state to closed after closing"
    (let ((nskk--server-process 'mock-proc))
      (nskk--server-test-with-open-state
       (nskk-with-mocks ((process-status (lambda (_) 'open))
                         (process-send-string (lambda (&rest _) nil))
                         (delete-process (lambda (_) nil))
                         (get-buffer (lambda (_) nil)))
         (nskk-server-close)
         (should (nskk-prolog-holds-p '(server-state closed)))
         (should (null (nskk-prolog-holds-p '(server-state open))))))))

  (nskk-it "kills the working buffer when it exists"
    (let ((nskk--server-process nil)
          (fake-buf (list 'fake-buffer))
          (killed-buf nil))
      (nskk-with-mocks ((get-buffer (lambda (_) fake-buf))
                        (kill-buffer (lambda (b) (setq killed-buf b))))
        (nskk-server-close)
        (should (eq killed-buf fake-buf))))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; nskk-server-ensure-open
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-describe "nskk-server-ensure-open"
  (nskk-it "returns nil without calling open when server is disabled"
    ;; ensure-open/k calls (fail) immediately on disabled -- open/k is never reached.
    (let ((nskk-server-enable nil)
          (nskk--server-process nil)
          (open-called nil))
      (nskk-with-mocks ((nskk-server-open/k
                         (lambda (on-f _nf) (setq open-called t) (funcall on-f 'mock-proc))))
        (nskk-given (should (null nskk-server-enable)))
        (nskk-when  (let ((result (nskk-server-ensure-open)))
                      (nskk-then
                       (should (null result))
                       (should (null open-called))))))))

  (nskk-it "returns t without reconnecting when already live"
    ;; ensure-open/k takes the (succeed t) short-circuit -- open/k is never reached.
    (let ((nskk-server-enable t)
          (nskk--server-process 'mock-proc)
          (open-called nil))
      (nskk--server-test-with-open-state
       (nskk-with-mocks ((process-status (lambda (_) 'open))
                         (nskk-server-open/k
                          (lambda (on-f _nf) (setq open-called t) (funcall on-f 'mock-proc))))
         (let ((result (nskk-server-ensure-open)))
           (should (eq result t))
           (should (null open-called)))))))

  (nskk-it "calls nskk-server-open/k when connection is not live"
    ;; Mock the /k variant since <- chains call nskk-server-open/k directly.
    (let ((nskk-server-enable t)
          (nskk--server-process nil)
          (open-called nil))
      (nskk-with-mocks ((nskk-server-open/k
                         (lambda (on-f _nf) (setq open-called t) (funcall on-f 'new-proc))))
        (nskk-when  (nskk-server-ensure-open))
        (nskk-then  (should open-called)))))

  (nskk-it "returns t when reconnect succeeds"
    (let ((nskk-server-enable t)
          (nskk--server-process nil))
      (nskk-with-mocks ((nskk-server-open/k (lambda (on-f _nf) (funcall on-f 'new-proc))))
        (should (eq (nskk-server-ensure-open) t)))))

  (nskk-it "returns nil when nskk-server-open fails"
    (let ((nskk-server-enable t)
          (nskk--server-process nil))
      (nskk-with-mocks ((nskk-server-open/k (lambda (_on-f nf) (funcall nf))))
        (should (null (nskk-server-ensure-open)))))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; nskk-server-lookup guard conditions
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-describe "nskk-server-lookup guard conditions"
  (nskk-it "returns nil without sending when any guard fails"
    (nskk-deftest-table lookup-guard-failures
      :columns (enable process key)
      :rows ((nil  nil        "かんじ")   ; disabled, no process
             (t    nil        "かんじ")   ; enabled, no connection
             (t    mock-proc  nil)         ; enabled, live, nil key
             (t    mock-proc  "")          ; enabled, live, empty key
             (t    mock-proc  42))         ; enabled, live, non-string key
      :body
      (let ((nskk-server-enable enable)
            (nskk--server-process process)
            (send-called nil))
        (nskk-with-mocks ((process-status (lambda (_) 'open))
                          (process-send-string (lambda (&rest _) (setq send-called t))))
          (let ((result (nskk-server-lookup key)))
            (should (null result))
            (should (null send-called)))))))

  (nskk-it "returns nil even when a process exists but server is disabled"
    (let ((nskk-server-enable nil)
          (nskk--server-process 'mock-proc)
          (send-called nil))
      (nskk-with-mocks ((process-status (lambda (_) 'open))
                        (process-send-string (lambda (&rest _) (setq send-called t))))
        (should (null (nskk-server-lookup "かんじ")))
        (should (null send-called))))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; nskk-server-lookup/k: CPS variant
;;; Mock at the /k level since <- chains call the /k functions directly.
;;; nskk--server-parse-response runs for real (tests Prolog integration).
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-describe "nskk-server-lookup/k"
  (nskk-context "on-found branch"
    (nskk-it "calls on-found with the candidate list when lookup returns results"
      (let* ((found-arg nil)
             (not-found-called nil))
        (nskk-with-mocks
            ((nskk--server-lookup-guards-p/k
              (lambda (_key on-f _nf) (funcall on-f t)))
             (nskk--server-with-response/k
              (lambda (_key on-f _nf) (funcall on-f "1/漢字/感じ/\n"))))
          (nskk-server-lookup/k "かんじ"
            (lambda (cands) (setq found-arg cands))
            (lambda () (setq not-found-called t)))
          (should (equal found-arg '("漢字" "感じ")))
          (should (null not-found-called)))))

    (nskk-it "does NOT call on-not-found when lookup returns results"
      (let ((not-found-called nil))
        (nskk-with-mocks
            ((nskk--server-lookup-guards-p/k
              (lambda (_key on-f _nf) (funcall on-f t)))
             (nskk--server-with-response/k
              (lambda (_key on-f _nf) (funcall on-f "1/結果/\n"))))
          (nskk-server-lookup/k "てすと"
            (lambda (_cands) nil)
            (lambda () (setq not-found-called t)))
          (should (null not-found-called)))))

    (nskk-it "passes the full candidates list (not just first element) to on-found"
      (let ((found-arg nil))
        (nskk-with-mocks
            ((nskk--server-lookup-guards-p/k
              (lambda (_key on-f _nf) (funcall on-f t)))
             (nskk--server-with-response/k
              (lambda (_key on-f _nf) (funcall on-f "1/A/B/C/\n"))))
          (nskk-server-lookup/k "key"
            (lambda (cands) (setq found-arg cands))
            #'ignore)
          (should (equal found-arg '("A" "B" "C")))))))

  (nskk-context "on-not-found branch"
    (nskk-it "calls on-not-found when server returns not-found response"
      (let ((not-found-called nil)
            (found-called nil))
        (nskk-with-mocks
            ((nskk--server-lookup-guards-p/k
              (lambda (_key on-f _nf) (funcall on-f t)))
             (nskk--server-with-response/k
              (lambda (_key on-f _nf) (funcall on-f "4みつからない \n"))))
          (nskk-server-lookup/k "みつからない"
            (lambda (_cands) (setq found-called t))
            (lambda () (setq not-found-called t)))
          (should not-found-called)
          (should (null found-called)))))

    (nskk-it "calls on-not-found when I/O fails (with-response fails)"
      (let ((not-found-called nil)
            (found-called nil))
        (nskk-with-mocks
            ((nskk--server-lookup-guards-p/k
              (lambda (_key on-f _nf) (funcall on-f t)))
             (nskk--server-with-response/k
              (lambda (_key _on-f nf) (funcall nf))))
          (nskk-server-lookup/k "nonexistent"
            (lambda (_cands) (setq found-called t))
            (lambda () (setq not-found-called t)))
          (should not-found-called)
          (should (null found-called)))))

    (nskk-it "calls on-not-found when guard fails"
      (let ((not-found-called nil))
        (nskk-with-mocks
            ((nskk--server-lookup-guards-p/k
              (lambda (_key _on-f nf) (funcall nf))))
          (nskk-server-lookup/k "key"
            (lambda (_cands) nil)
            (lambda () (setq not-found-called t)))
          (should not-found-called)))))

  (nskk-context "exactly-one-continuation invariant"
    (nskk-it "calls exactly one continuation when lookup succeeds"
      (let ((call-count 0))
        (nskk-with-mocks
            ((nskk--server-lookup-guards-p/k
              (lambda (_key on-f _nf) (funcall on-f t)))
             (nskk--server-with-response/k
              (lambda (_key on-f _nf) (funcall on-f "1/候補/\n"))))
          (nskk-server-lookup/k "key"
            (lambda (_cands) (cl-incf call-count))
            (lambda () (cl-incf call-count)))
          (should (= call-count 1)))))

    (nskk-it "calls exactly one continuation when guard fails"
      (let ((call-count 0))
        (nskk-with-mocks
            ((nskk--server-lookup-guards-p/k
              (lambda (_key _on-f nf) (funcall nf))))
          (nskk-server-lookup/k "key"
            (lambda (_cands) (cl-incf call-count))
            (lambda () (cl-incf call-count)))
          (should (= call-count 1)))))))

;;;
;;; nskk--server-await-response
;;;

(nskk-describe "nskk--server-await-response"
  (nskk-it "returns the buffer contents when they contain a newline"
    (with-temp-buffer
      (let* ((buf (current-buffer))
             (proc 'mock-proc)
             (deadline (+ (float-time) 10)))
        (nskk-with-mocks
            ((nskk-server-live-p (lambda () t))
             (accept-process-output (lambda (_p _t)
                                      (with-current-buffer buf
                                        (insert "4/候補/\n")))))
          (let ((result (nskk--server-await-response proc buf deadline)))
            (should (stringp result))
            (should (string-match-p "\n" result)))))))

  (nskk-it "returns nil when deadline has already passed"
    (with-temp-buffer
      (let* ((buf (current-buffer))
             (proc 'mock-proc)
             (deadline (- (float-time) 1)))  ; already past
        (nskk-with-mocks
            ((nskk-server-live-p (lambda () t))
             (accept-process-output (lambda (&rest _) nil)))
          (should (null (nskk--server-await-response proc buf deadline)))))))

  (nskk-it "returns nil immediately when server is not live"
    (with-temp-buffer
      (let* ((buf (current-buffer))
             (proc 'mock-proc)
             (deadline (+ (float-time) 10)))
        (nskk-with-mocks
            ((nskk-server-live-p (lambda () nil))
             (accept-process-output (lambda (&rest _) nil)))
          (should (null (nskk--server-await-response proc buf deadline))))))))

;;;
;;; nskk--server-with-response
;;; After defun/k conversion: sync wrapper (key) -> response-string or nil.
;;; The cont argument is gone; parse is a separate step.
;;;

(nskk-describe "nskk--server-with-response"
  (nskk-it "sends command 1 + key and returns the raw response string"
    (let ((sent-cmd nil)
          result)
      (with-temp-buffer
        (let* ((buf (current-buffer))
               (proc 'mock-proc))
          (nskk-with-mocks
              ((process-buffer (lambda (_) buf))
               (process-send-string (lambda (_p cmd) (setq sent-cmd cmd)))
               (nskk--server-await-response (lambda (_p _b _d) "4/漢字/\n")))
            (let ((nskk--server-process proc)
                  (nskk-server-timeout 5.0)
                  (nskk-server-report-response nil))
              (setq result (nskk--server-with-response "かんじ"))))))
      (should (equal sent-cmd "1かんじ "))
      (should (equal result "4/漢字/\n"))))

  (nskk-it "returns nil when await-response returns nil (timeout)"
    (let (result)
      (with-temp-buffer
        (let* ((buf (current-buffer))
               (proc 'mock-proc))
          (nskk-with-mocks
              ((process-buffer (lambda (_) buf))
               (process-send-string (lambda (&rest _) nil))
               (nskk--server-await-response (lambda (&rest _) nil)))
            (let ((nskk--server-process proc)
                  (nskk-server-timeout 5.0)
                  (nskk-server-report-response nil))
              (setq result (nskk--server-with-response "key"))))))
      (should (null result)))))

;;;
;;; safe-local-variable policy
;;;

(nskk-describe "nskk-server-* risky-local-variable policy"
  (nskk-it "nskk-server-enable is marked risky-local-variable"
    (should (get 'nskk-server-enable 'risky-local-variable)))

  (nskk-it "nskk-server-host is marked risky-local-variable"
    (should (get 'nskk-server-host 'risky-local-variable)))

  (nskk-it "nskk-server-portnum is marked risky-local-variable"
    (should (get 'nskk-server-portnum 'risky-local-variable))))

(nskk-describe "nskk-server-* safe-local-variable policy"
  (nskk-it "no nskk-server-* variable has a safe-local-variable predicate"
    (dolist (sym '(nskk-server-enable nskk-server-host nskk-server-portnum
                   nskk-server-coding-system nskk-server-timeout
                   nskk-server-report-response))
      (should-not (get sym 'safe-local-variable)))))

(provide 'nskk-server-test)

;;; nskk-server-test.el ends here
