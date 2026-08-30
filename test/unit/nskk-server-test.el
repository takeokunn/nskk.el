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

  (nskk-it "rejects Unicode delimiters controls and whitespace before liveness"
    (let ((nskk-server-enable t)
          (nskk-server-coding-system 'utf-8)
          (nskk--server-process 'mock-proc))
      (nskk--server-test-with-open-state
       (nskk-with-mocks
           ((process-status
             (lambda (_) (error "unsafe key reached liveness check"))))
         (dolist (code '(#x1f #x20 #x7f #x80 #x9f #xa0 #x1680 #x2028 #x2029))
           (should-not
            (nskk--server-lookup-guards-p
             (concat "a" (string code) "b"))))))))

  (nskk-it "returns nil without signaling for invalid coding systems"
    (let ((nskk-server-enable t)
          (nskk--server-process 'mock-proc))
      (dolist (coding '(nil not-a-coding-system))
        (let ((nskk-server-coding-system coding))
          (should-not (nskk--server-lookup-guards-p "かんじ"))))))

  (nskk-it "rejects lossy EUC-JP keys before liveness"
    (let ((nskk-server-enable t)
          (nskk-server-coding-system 'euc-jp)
          (nskk--server-process 'mock-proc))
      (nskk--server-test-with-open-state
       (nskk-with-mocks
           ((process-status
             (lambda (_) (error "lossy key reached liveness check"))))
         (should-not (nskk--server-lookup-guards-p "emoji😀"))))))

  (nskk-it "returns nil when connection is not live"
    (let ((nskk-server-enable t)
          (nskk--server-process nil))
      (should (null (nskk--server-lookup-guards-p "かんじ")))))

  (nskk-it "accepts Japanese keys with matching EUC-JP and UTF-8 processes"
    (dolist (coding '(euc-jp utf-8))
      (let* ((nskk-server-enable t)
             (nskk-server-coding-system coding)
             (proc (make-pipe-process
                    :name (format "nskk-server-lookup-guards-%s" coding)
                    :buffer nil
                    :coding (cons coding coding)
                    :noquery t))
             (nskk--server-process proc))
        (unwind-protect
            (nskk--server-test-with-open-state
             (should
              (coding-system-equal
               coding (cdr (process-coding-system proc))))
             (should (nskk--server-lookup-guards-p "かんじ")))
          (when (process-live-p proc)
            (delete-process proc)))))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; nskk-server-live-p
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-describe "nskk-server-live-p"
  (nskk-it "returns nil when process is nil"
    (let ((nskk--server-process nil))
      (should (null (nskk-server-live-p)))))

  (nskk-it "returns non-nil when process status is open and state is open"
    (let* ((proc (make-pipe-process
                  :name "nskk-server-live-open"
                  :buffer nil
                  :noquery t))
           (nskk--server-process proc))
      (unwind-protect
          (nskk--server-test-with-open-state
           (should (nskk-server-live-p)))
        (when (process-live-p proc)
          (delete-process proc)))))

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
    (let* ((proc (make-pipe-process
                  :name "nskk-server-live-status-argument"
                  :buffer nil
                  :noquery t))
           (nskk--server-process proc)
           (received-proc nil))
      (unwind-protect
          (nskk--server-test-with-open-state
           (nskk-with-mocks
               ((process-status
                 (lambda (candidate)
                   (setq received-proc candidate)
                   'open)))
             (nskk-server-live-p)
             (should (eq received-proc proc))))
        (when (process-live-p proc)
          (delete-process proc))))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; nskk-server-open
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-describe "nskk-server-open"
  (nskk-it "returns nil immediately when server is disabled"
    (let ((nskk-server-enable nil)
          (nskk--server-process nil)
          (live-called nil)
          (open-called nil))
      (nskk-with-mocks ((nskk-server-live-p
                         (lambda () (setq live-called t) t))
                        (open-network-stream
                         (lambda (&rest _)
                           (setq open-called t)
                           'mock-proc)))
        (nskk-given (should (null nskk-server-enable)))
        (nskk-when  (let ((result (nskk-server-open)))
                      (nskk-then
                       (should (null result))
                       (should (null live-called))
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
                        (process-status (lambda (_) 'open))
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
                        (process-status (lambda (_) 'open))
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
                        (process-status (lambda (_) 'open))
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

  (nskk-context "connection lifecycle"
    (nskk-it "registers kill-emacs-hook only once (idempotent)"
      (let ((nskk-server-enable t)
            (nskk--server-process nil)
            (nskk--server-kill-emacs-hook-registered nil)
            (add-hook-count 0))
        (nskk-with-mocks ((open-network-stream (lambda (&rest _) 'mock-proc))
                          (process-status (lambda (_) 'open))
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
            (nskk-prolog-assert '((server-state closed)))))))

    (nskk-it "propagates live predicate errors and quits without connecting"
      (dolist (kind '(error quit))
        (let ((nskk-server-enable t)
              (nskk--server-process 'existing-process)
              (make-count 0)
              (configure-count 0)
              (data (list "live predicate" kind))
              (caught nil))
          (cl-letf (((symbol-function 'nskk-server-live-p)
                     (lambda () (signal kind data)))
                    ((symbol-function 'nskk--server-make-connection)
                     (lambda ()
                       (cl-incf make-count)
                       'new-process))
                    ((symbol-function 'nskk--server-configure-process)
                     (lambda (_proc) (cl-incf configure-count))))
            (condition-case condition
                (nskk-server-open)
              ((error quit) (setq caught condition))))
          (should (eq (car caught) kind))
          (should (equal (cdr caught) data))
          (should (= make-count 0))
          (should (= configure-count 0)))))

    (nskk-it "cleans retained process ownership before reconnecting regardless of state"
      (let ((real-configure
             (symbol-function 'nskk--server-configure-process))
            (real-delete (symbol-function 'delete-process))
            (real-retract (symbol-function 'nskk-prolog-retract-all))
            (real-assert (symbol-function 'nskk-prolog-assert)))
        (dolist (state '(legacy missing corrupt dead))
          (let* ((old-buffer
                  (generate-new-buffer
                   (format " *nskk-retained-old-%s*" state)))
                 (new-buffer
                  (generate-new-buffer
                   (format " *nskk-retained-new-%s*" state)))
                 (old-process
                  (make-pipe-process
                   :name (format "nskk-retained-old-%s" state)
                   :buffer old-buffer
                   :noquery t))
                 (new-process
                  (make-pipe-process
                   :name (format "nskk-retained-new-%s" state)
                   :buffer new-buffer
                   :noquery t))
                 (nskk-server-enable t)
                 (nskk--server-process old-process)
                 (nskk--server-kill-emacs-hook-registered t)
                 (make-count 0)
                 (configure-count 0))
            (process-put old-process 'nskk-server-owned-buffer t)
            (process-put new-process 'nskk-server-owned-buffer t)
            (unwind-protect
                (progn
                  (funcall real-retract 'server-state 1)
                  (pcase state
                    ('missing nil)
                    ('dead
                     (funcall real-assert '((server-state closed)))
                     (funcall real-delete old-process))
                    (_
                     (funcall real-assert
                              `((server-state ,state)))))
                  (cl-letf
                      (((symbol-function 'nskk--server-make-connection)
                        (lambda ()
                          (cl-incf make-count)
                          new-process))
                       ((symbol-function 'nskk--server-configure-process)
                        (lambda (proc)
                          (cl-incf configure-count)
                          (funcall real-configure proc))))
                    (should (eq (nskk-server-open) new-process)))
                  (should (= make-count 1))
                  (should (= configure-count 1))
                  (should-not (process-live-p old-process))
                  (should-not (buffer-live-p old-buffer))
                  (should (process-live-p new-process))
                  (should (buffer-live-p new-buffer))
                  (should (eq nskk--server-process new-process))
                  (should (nskk-prolog-holds-p '(server-state open)))
                  (nskk-server-close)
                  (should-not nskk--server-process)
                  (should-not (process-live-p new-process))
                  (should-not (buffer-live-p new-buffer)))
              (when (process-live-p old-process)
                (funcall real-delete old-process))
              (when (process-live-p new-process)
                (funcall real-delete new-process))
              (when (buffer-live-p old-buffer)
                (kill-buffer old-buffer))
              (when (buffer-live-p new-buffer)
                (kill-buffer new-buffer))
              (setq nskk--server-process nil)
              (funcall real-retract 'server-state 1)
              (funcall real-assert '((server-state closed))))))))

    (nskk-it "reuses one live process and one owned buffer across repeated opens"
      (let ((real-configure
             (symbol-function 'nskk--server-configure-process))
            (real-delete (symbol-function 'delete-process))
            (real-add-hook (symbol-function 'add-hook))
            (real-retract (symbol-function 'nskk-prolog-retract-all))
            (real-assert (symbol-function 'nskk-prolog-assert))
            (nskk-server-enable t)
            (nskk--server-process nil)
            (nskk--server-kill-emacs-hook-registered nil)
            (kill-emacs-hook nil)
            (nskk--server-buffer-name " *nskk-idempotent-open*")
            (make-count 0)
            (configure-count 0)
            (delete-count 0)
            (add-hook-count 0)
            (retract-count 0)
            (assert-count 0)
            (created-processes nil)
            (owned-buffer nil))
        (unwind-protect
            (cl-letf
                (((symbol-function 'nskk--server-make-connection)
                  (lambda ()
                    (cl-incf make-count)
                    (let* ((owned
                            (not (get-buffer nskk--server-buffer-name)))
                           (buffer
                            (get-buffer-create nskk--server-buffer-name))
                           (proc
                            (make-pipe-process
                             :name (format "nskk-idempotent-open-%d"
                                           make-count)
                             :buffer buffer
                             :noquery t)))
                      (process-put proc 'nskk-server-owned-buffer owned)
                      (when owned (setq owned-buffer buffer))
                      (push proc created-processes)
                      proc)))
                 ((symbol-function 'nskk--server-configure-process)
                  (lambda (proc)
                    (cl-incf configure-count)
                    (funcall real-configure proc)))
                 ((symbol-function 'delete-process)
                  (lambda (proc)
                    (cl-incf delete-count)
                    (funcall real-delete proc)))
                 ((symbol-function 'add-hook)
                  (lambda (&rest args)
                    (cl-incf add-hook-count)
                    (apply real-add-hook args)))
                 ((symbol-function 'nskk-prolog-retract-all)
                  (lambda (&rest args)
                    (cl-incf retract-count)
                    (apply real-retract args)))
                 ((symbol-function 'nskk-prolog-assert)
                  (lambda (&rest args)
                    (cl-incf assert-count)
                    (apply real-assert args))))
              (let ((first (nskk-server-open))
                    second
                    add-hook-count-after-first
                    retract-count-after-first
                    assert-count-after-first)
                (setq add-hook-count-after-first add-hook-count
                      retract-count-after-first retract-count
                      assert-count-after-first assert-count)
                (setq second (nskk-server-open))
                (should (processp first))
                (should (eq second first))
                (should (eq nskk--server-process first))
                (should (= make-count 1))
                (should (= configure-count 1))
                (should (= delete-count 0))
                (should (= add-hook-count add-hook-count-after-first))
                (should (= retract-count retract-count-after-first))
                (should (= assert-count assert-count-after-first))
                (should (memq #'nskk-server-close kill-emacs-hook))
                (should (nskk-prolog-holds-p '(server-state open)))
                (should (= (length created-processes) 1))
                (should (eq (process-buffer first) owned-buffer))
                (should (eq (get-buffer-process owned-buffer) first))
                (should (process-live-p first))
                (nskk-server-close)
                (should (> delete-count 0))
                (should-not (process-live-p first))
                (should-not nskk--server-process)
                (should-not (buffer-live-p owned-buffer))))
          (dolist (proc created-processes)
            (when (process-live-p proc)
              (funcall real-delete proc)))
          (when-let* ((buffer (get-buffer nskk--server-buffer-name)))
            (kill-buffer buffer))
          (setq nskk--server-process nil)
          (funcall real-retract 'server-state 1)
          (funcall real-assert '((server-state closed))))))

    (nskk-it "finishes cleanup-pending resources before reconnecting"
      (let* ((real-configure
              (symbol-function 'nskk--server-configure-process))
             (real-delete (symbol-function 'delete-process))
             (real-retract (symbol-function 'nskk-prolog-retract-all))
             (real-assert (symbol-function 'nskk-prolog-assert))
             (nskk-server-enable t)
             (nskk--server-buffer-name " *nskk-cleanup-before-reopen*")
             (old-buffer (get-buffer-create nskk--server-buffer-name))
             (old-process
              (make-pipe-process
               :name "nskk-cleanup-before-reopen-old"
               :buffer old-buffer
               :noquery t))
             (nskk--server-process old-process)
             (nskk--server-kill-emacs-hook-registered t)
             (make-count 0)
             (configure-count 0)
             (delete-count 0)
             (new-process nil)
             (new-buffer nil))
        (process-put old-process 'nskk-server-owned-buffer t)
        (unwind-protect
            (progn
              (funcall real-retract 'server-state 1)
              (funcall real-assert '((server-state closed)))
              (cl-letf
                  (((symbol-function 'nskk--server-make-connection)
                    (lambda ()
                      (cl-incf make-count)
                      (setq new-buffer
                            (get-buffer-create nskk--server-buffer-name))
                      (setq new-process
                            (make-pipe-process
                             :name "nskk-cleanup-before-reopen-new"
                             :buffer new-buffer
                             :noquery t))
                      (process-put new-process
                                   'nskk-server-owned-buffer t)
                      new-process))
                   ((symbol-function 'nskk--server-configure-process)
                    (lambda (proc)
                      (cl-incf configure-count)
                      (funcall real-configure proc)))
                   ((symbol-function 'delete-process)
                    (lambda (proc)
                      (cl-incf delete-count)
                      (funcall real-delete proc))))
                (should (eq (nskk-server-open) new-process))
                (should (= make-count 1))
                (should (= configure-count 1))
                (should (= delete-count 1))
                (should-not (process-live-p old-process))
                (should-not (buffer-live-p old-buffer))
                (should (process-live-p new-process))
                (should (buffer-live-p new-buffer))
                (should-not (eq old-buffer new-buffer))
                (should (eq nskk--server-process new-process))
                (should (nskk-prolog-holds-p '(server-state open)))
                (nskk-server-close)
                (should-not (process-live-p new-process))
                (should-not (buffer-live-p new-buffer))
                (should-not nskk--server-process)))
          (when (process-live-p old-process)
            (funcall real-delete old-process))
          (when (and new-process (process-live-p new-process))
            (funcall real-delete new-process))
          (when (buffer-live-p old-buffer)
            (kill-buffer old-buffer))
          (when (and new-buffer (buffer-live-p new-buffer))
            (kill-buffer new-buffer))
          (setq nskk--server-process nil)
          (funcall real-retract 'server-state 1)
          (funcall real-assert '((server-state closed))))))

    (nskk-it "blocks reconnect until cleanup-pending resources are released"
      (let* ((real-configure
              (symbol-function 'nskk--server-configure-process))
             (real-delete (symbol-function 'delete-process))
             (real-retract (symbol-function 'nskk-prolog-retract-all))
             (real-assert (symbol-function 'nskk-prolog-assert))
             (nskk-server-enable t)
             (nskk--server-buffer-name " *nskk-blocked-cleanup-reopen*")
             (old-buffer (get-buffer-create nskk--server-buffer-name))
             (old-process
              (make-pipe-process
               :name "nskk-blocked-cleanup-reopen-old"
               :buffer old-buffer
               :noquery t))
             (nskk--server-process old-process)
 (nskk--server-pending-cleanups nil)
             (nskk--server-kill-emacs-hook-registered t)
             (make-count 0)
             (configure-count 0)
             (delete-count 0)
             (kill-count 0)
             new-process
             new-buffer)
        (process-put old-process 'nskk-server-owned-buffer t)
        (unwind-protect
            (progn
              (funcall real-retract 'server-state 1)
              (funcall real-assert '((server-state closed)))
              (cl-letf (((symbol-function 'nskk--server-make-connection)
                         (lambda ()
                           (cl-incf make-count)
                           'unexpected-process))
                        ((symbol-function 'nskk--server-configure-process)
                         (lambda (_proc) (cl-incf configure-count)))
                        ((symbol-function 'delete-process)
                         (lambda (_proc) (cl-incf delete-count)))
                        ((symbol-function 'kill-buffer)
                         (lambda (&optional _buffer)
                           (cl-incf kill-count)
                           nil)))
                (should-not (nskk-server-open))
                (should (= make-count 0))
                (should (= configure-count 0))
                (should (> delete-count 0))
                (should (> kill-count 0))
                (progn
  (should-not nskk--server-process)
  (should (= (length nskk--server-pending-cleanups) 1))
  (should (eq (caar nskk--server-pending-cleanups) old-process))
  (should (eq (cdar nskk--server-pending-cleanups) old-buffer)))
                (should (process-live-p old-process))
                (should (buffer-live-p old-buffer)))
              (cl-letf (((symbol-function 'nskk--server-make-connection)
                         (lambda ()
                           (cl-incf make-count)
                           (setq new-buffer
                                 (get-buffer-create
                                  nskk--server-buffer-name))
                           (setq new-process
                                 (make-pipe-process
                                  :name "nskk-blocked-cleanup-reopen-new"
                                  :buffer new-buffer
                                  :noquery t))
                           (process-put
                            new-process 'nskk-server-owned-buffer t)
                           new-process))
                        ((symbol-function 'nskk--server-configure-process)
                         (lambda (proc)
                           (cl-incf configure-count)
                           (funcall real-configure proc))))
                (should (eq (nskk-server-open) new-process))
                (should (= make-count 1))
                (should (= configure-count 1))
                (should-not (process-live-p old-process))
                (should-not (buffer-live-p old-buffer))
                (should (eq nskk--server-process new-process))
                (should (process-live-p new-process))
                (should (buffer-live-p new-buffer))
                (should (nskk-prolog-holds-p '(server-state open)))
                (nskk-server-close)
                (progn
  (should-not nskk--server-process)
  (should-not nskk--server-pending-cleanups))
                (should-not (process-live-p new-process))
                (should-not (buffer-live-p new-buffer))))
          (when (process-live-p old-process)
            (funcall real-delete old-process))
          (when (and new-process (process-live-p new-process))
            (funcall real-delete new-process))
          (when (buffer-live-p old-buffer)
            (kill-buffer old-buffer))
          (when (buffer-live-p new-buffer)
            (kill-buffer new-buffer))
          (setq nskk--server-process nil)
          (funcall real-retract 'server-state 1)
          (funcall real-assert '((server-state closed))))))
  ))

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
    (let* ((proc (make-pipe-process
                  :name "nskk-server-close-live"
                  :buffer nil
                  :noquery t))
           (real-delete (symbol-function 'delete-process))
           (nskk--server-process proc)
           (sent-string nil))
      (unwind-protect
          (nskk--server-test-with-open-state
           (nskk-with-mocks
               ((process-send-string
                 (lambda (_ string)
                   (setq sent-string string)))
                (delete-process (lambda (_) nil))
                (get-buffer (lambda (_) nil)))
             (nskk-server-close)
             (should (equal sent-string "0"))))
        (when (process-live-p proc)
          (funcall real-delete proc)))))

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

  (nskk-it "preserves a pre-existing same-name working buffer"
    (let* ((nskk--server-buffer-name " *nskk-server-close-existing*")
           (buffer (get-buffer-create nskk--server-buffer-name))
           (proc (make-pipe-process
                  :name "nskk-server-close-existing"
                  :buffer buffer
                  :noquery nil))
           (nskk--server-process proc))
      (process-put proc 'nskk-server-owned-buffer nil)
      (unwind-protect
          (progn
            (nskk-server-close)
            (should (buffer-live-p buffer)))
        (when (process-live-p proc)
          (set-process-query-on-exit-flag proc nil)
          (delete-process proc))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (let ((kill-buffer-hook nil)
                  (kill-buffer-query-functions nil)
                  (buffer-offer-save nil))
              (kill-buffer buffer)))))))

  (nskk-it "kills an owned working buffer without hooks or queries"
    (let* ((nskk--server-buffer-name " *nskk-server-close-owned*")
           (buffer (get-buffer-create nskk--server-buffer-name))
           (proc (make-pipe-process
                  :name "nskk-server-close-owned"
                  :buffer buffer
                  :noquery nil))
           (nskk--server-process proc)
           (hook-called nil)
           (query-called nil))
      (process-put proc 'nskk-server-owned-buffer t)
      (set-process-query-on-exit-flag proc t)
      (with-current-buffer buffer
        (setq-local buffer-offer-save t)
        (set-buffer-modified-p t)
        (add-hook 'kill-buffer-hook
                  (lambda () (setq hook-called t))
                  nil
                  t)
        (add-hook 'kill-buffer-query-functions
                  (lambda ()
                    (setq query-called t)
                    nil)
                  nil
                  t))
      (unwind-protect
          (progn
            (nskk-server-close)
            (should-not (buffer-live-p buffer))
            (should-not hook-called)
            (should-not query-called)
            (should-not (process-query-on-exit-flag proc)))
        (when (process-live-p proc)
          (set-process-query-on-exit-flag proc nil)
          (delete-process proc))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (let ((kill-buffer-hook nil)
                  (kill-buffer-query-functions nil)
                  (buffer-offer-save nil))
              (set-buffer-modified-p nil)
              (kill-buffer buffer))))))))

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
    (let* ((nskk-server-enable t)
           (proc (make-pipe-process
                  :name "nskk-server-ensure-open-live"
                  :buffer nil
                  :noquery t))
           (nskk--server-process proc)
           (open-called nil))
      (unwind-protect
          (nskk--server-test-with-open-state
           (nskk-with-mocks
               ((nskk-server-open/k
                 (lambda (on-f _nf)
                   (setq open-called t)
                   (funcall on-f proc))))
             (let ((result (nskk-server-ensure-open)))
               (should (eq result t))
               (should (null open-called)))))
        (when (process-live-p proc)
          (delete-process proc)))))

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
        (should (null send-called)))))

  (nskk-it "rejects nil configured coding before liveness or mutation"
    (with-temp-buffer
      (insert "sentinel")
      (let* ((buf (current-buffer))
             (proc
              (make-pipe-process
               :name "nskk-server-nil-coding-guard"
               :buffer buf
               :coding '(utf-8 . utf-8)
               :noquery t))
             (called nil)
             (result 'uninitialized))
        (process-put proc 'nskk-response-bytes 17)
        (process-put proc 'nskk-response-overflow 'overflow)
        (process-put proc 'nskk-response-complete 'complete)
        (unwind-protect
            (progn
              (let ((nskk-server-enable t)
                    (nskk-server-coding-system nil)
                    (nskk--server-process proc))
                (nskk-with-mocks
                    ((nskk-server-live-p/k
                      (lambda (&rest _)
                        (push 'liveness called)
                        (error "liveness must not be checked")))
                     (process-buffer
                      (lambda (&rest _)
                        (push 'buffer called)
                        (error "buffer must not be read")))
                     (process-get
                      (lambda (&rest _)
                        (push 'property-read called)
                        (error "properties must not be read")))
                     (process-put
                      (lambda (&rest _)
                        (push 'property-write called)
                        (error "properties must not be written")))
                     (process-send-string
                      (lambda (&rest _)
                        (push 'send called)
                        (error "request must not be sent")))
                     (nskk-server-close
                      (lambda ()
                        (push 'cleanup called)
                        (error "cleanup must not run"))))
                  (setq result (nskk-server-lookup "abc"))))
              (should-not result)
              (should-not called)
              (should (equal (buffer-string) "sentinel"))
              (should (= (process-get proc 'nskk-response-bytes) 17))
              (should
               (eq (process-get proc 'nskk-response-overflow) 'overflow))
              (should
               (eq (process-get proc 'nskk-response-complete) 'complete)))
          (when (process-live-p proc)
            (delete-process proc))))))

  (nskk-it "requires explicit coding and rejects control bytes on the wire"
    (should (nskk--server-key-safe-for-coding-p "abc" 'utf-8))
    (should-not (nskk--server-key-safe-for-coding-p "abc" nil))
    (should-not (nskk--server-key-safe-for-coding-p "abc" 'utf-16)))

  (nskk-it "rejects invalid configured coding before internal process observation"
    (dolist (coding-system '(nil nskk-test-invalid-coding))
      (let ((nskk-server-coding-system coding-system)
            (calls nil))
        (nskk-with-mocks
            ((process-live-p
              (lambda (_)
                (push 'liveness calls)
                (error "liveness must not be checked")))
             (process-coding-system
              (lambda (_)
                (push 'coding calls)
                (error "actual coding must not be read"))))
          (should-not
           (nskk--server-process-safe-for-coding-p
            'not-a-process "abc")))
        (should-not calls))))

  (nskk-it "accepts same-base variants and rejects nil actual coding"
    (let ((proc
           (make-pipe-process
            :name "nskk-server-coding-variant"
            :coding '(utf-8 . utf-8)
            :noquery t)))
      (unwind-protect
          (progn
            (let ((nskk-server-coding-system 'utf-8-unix))
              (should
               (nskk--server-process-safe-for-coding-p proc "あ")))
            (let ((nskk-server-coding-system nil))
              (should-not
               (nskk--server-process-safe-for-coding-p proc "abc")))
            (let ((nskk-server-coding-system 'utf-8))
              (nskk-with-mocks
                  ((process-coding-system
                    (lambda (_) (cons 'utf-8 nil))))
                (should-not
                 (nskk--server-process-safe-for-coding-p proc "abc")))))
        (when (process-live-p proc)
          (delete-process proc)))))

  (nskk-it "rejects dead and invalid processes before reading coding"
    (let ((proc
           (make-pipe-process
            :name "nskk-server-dead-coding-guard"
            :coding '(utf-8 . utf-8)
            :noquery t))
          (coding-read nil)
          (nskk-server-coding-system 'utf-8))
      (delete-process proc)
      (nskk-with-mocks
          ((process-coding-system
            (lambda (_)
              (setq coding-read t)
              (error "coding must not be read"))))
        (should-not
         (nskk--server-process-safe-for-coding-p proc "abc"))
        (should-not
         (nskk--server-process-safe-for-coding-p 'not-a-process "abc")))
      (should-not coding-read)))

  (nskk-it "fails closed on quit at every coding preflight stage"
    (dolist (case '((live (configured live))
                    (actual (configured live actual))
                    (base (configured live actual base))
                    (key (configured live actual base base key))))
      (let ((quit-stage (car case))
            (expected (cadr case))
            (observed nil)
            (nskk-server-coding-system 'utf-8))
        (nskk-with-mocks
            ((coding-system-p
              (lambda (_)
                (push 'configured observed)
                t))
             (process-live-p
              (lambda (_)
                (push 'live observed)
                (if (eq quit-stage 'live)
                    (signal 'quit nil)
                  t)))
             (process-coding-system
              (lambda (_)
                (push 'actual observed)
                (if (eq quit-stage 'actual)
                    (signal 'quit nil)
                  (cons 'utf-8 'utf-8))))
             (coding-system-base
              (lambda (_)
                (push 'base observed)
                (if (eq quit-stage 'base)
                    (signal 'quit nil)
                  'utf-8)))
             (nskk--server-key-safe-for-coding-p
              (lambda (&rest _)
                (push 'key observed)
                (if (eq quit-stage 'key)
                    (signal 'quit nil)
                  t))))
          (should-not
           (nskk--server-process-safe-for-coding-p
            'mock-process "abc")))
        (should (equal (nreverse observed) expected)))))

  (nskk-it "keeps public lookup state unchanged when coding preflight quits"
    (dolist (case '((live (configured live))
                    (actual (configured live actual))
                    (base (configured live actual base))
                    (key (configured live actual base base key))))
      (with-temp-buffer
        (insert "sentinel")
        (let* ((buf (current-buffer))
               (proc
                (make-pipe-process
                 :name (format "nskk-server-quit-preflight-%s" (car case))
                 :buffer buf
                 :coding '(utf-8 . utf-8)
                 :noquery t))
               (quit-stage (car case))
               (expected (cadr case))
               (observed nil)
               (io-observed nil)
               (result 'uninitialized)
               (nskk-server-enable t)
               (nskk-server-coding-system 'utf-8)
               (nskk--server-process proc))
          (process-put proc 'nskk-response-bytes 17)
          (process-put proc 'nskk-response-overflow 'overflow)
          (process-put proc 'nskk-response-complete 'complete)
          (unwind-protect
              (progn
                (nskk-with-mocks
                    ((nskk--server-lookup-guards-p/k
                      (lambda (_key on-found _not-found)
                        (funcall on-found t)))
                     (coding-system-p
                      (lambda (_)
                        (push 'configured observed)
                        t))
                     (process-live-p
                      (lambda (_)
                        (push 'live observed)
                        (if (eq quit-stage 'live)
                            (signal 'quit nil)
                          t)))
                     (process-coding-system
                      (lambda (_)
                        (push 'actual observed)
                        (if (eq quit-stage 'actual)
                            (signal 'quit nil)
                          (cons 'utf-8 'utf-8))))
                     (coding-system-base
                      (lambda (_)
                        (push 'base observed)
                        (if (eq quit-stage 'base)
                            (signal 'quit nil)
                          'utf-8)))
                     (nskk--server-key-safe-for-coding-p
                      (lambda (&rest _)
                        (push 'key observed)
                        (if (eq quit-stage 'key)
                            (signal 'quit nil)
                          t)))
                     (process-buffer
                      (lambda (&rest _)
                        (push 'buffer io-observed)
                        (error "buffer must not be read")))
                     (process-get
                      (lambda (&rest _)
                        (push 'property-read io-observed)
                        (error "properties must not be read")))
                     (process-put
                      (lambda (&rest _)
                        (push 'property-write io-observed)
                        (error "properties must not be written")))
                     (erase-buffer
                      (lambda ()
                        (push 'erase io-observed)
                        (error "buffer must not be erased")))
                     (process-send-string
                      (lambda (&rest _)
                        (push 'send io-observed)
                        (error "request must not be sent")))
                     (nskk-server-close
                      (lambda ()
                        (push 'cleanup io-observed)
                        (error "cleanup must not run"))))
                  (setq result (nskk-server-lookup "abc")))
                (should-not result)
                (should (equal (nreverse observed) expected))
                (should-not io-observed)
                (should (equal (buffer-string) "sentinel"))
                (should (= (process-get proc 'nskk-response-bytes) 17))
                (should
                 (eq (process-get proc 'nskk-response-overflow) 'overflow))
                (should
                 (eq (process-get proc 'nskk-response-complete) 'complete)))
            (when (process-live-p proc)
              (delete-process proc))))))))

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
  (nskk-it "returns only the first line when one chunk contains multiple lines"
    (with-temp-buffer
      (let* ((buf (current-buffer))
             (proc 'mock-proc)
             (wait-budget 10))
        (nskk-with-mocks
            ((nskk-server-live-p (lambda () t))
             (accept-process-output
              (lambda (wait-proc _timeout &optional _millisec just-this-one)
                (should (eq wait-proc proc))
                (should just-this-one)
                (with-current-buffer buf
                  (insert "4/候補/\n4/余分/\n")))))
          (should (equal (nskk--server-await-response
                          proc buf wait-budget)
                         "4/候補/\n"))))))

  (nskk-context
  "finite response wait budget"
  (nskk-it
    "returns nil and resets the connection after the budget is exhausted"
    (with-temp-buffer
      (let ((buf (current-buffer))
            (proc 'mock-proc)
            (close-called nil)
            (polled nil))
        (nskk-with-mocks
          ((nskk-server-live-p
              (lambda ()
                t))
            (nskk-server-close
              (lambda ()
                (setq close-called t)))
            (accept-process-output
              (lambda (&rest _)
                (setq polled t))))
          (should-not (nskk--server-await-response proc buf 0))
          (should close-called)
          (should-not polled)))))
  (nskk-it
    "treats non-finite numeric response budgets as exhausted"
    (dolist (wait-budget '(0.0e+NaN 1.0e+INF -1.0e+INF))
      (with-temp-buffer
        (let ((buf (current-buffer))
              (proc 'mock-proc)
              (close-called nil)
              (polled nil))
          (nskk-with-mocks
            ((nskk-server-live-p
                (lambda ()
                  t))
              (nskk-server-close
                (lambda ()
                  (setq close-called t)))
              (accept-process-output
                (lambda (&rest _)
                  (setq polled t))))
            (should-not
             (nskk--server-await-response proc buf wait-budget))
            (should close-called)
            (should-not polled))))))
  (nskk-it
    "signals before polling when the response budget is not a number"
    (with-temp-buffer
      (let ((buf (current-buffer))
            (proc 'mock-proc)
            (polled nil))
        (nskk-with-mocks
          ((accept-process-output
              (lambda (&rest _)
                (setq polled t))))
          (should-error
           (nskk--server-await-response proc buf 'invalid)
           :type 'wrong-type-argument)
          (should-not polled)))))
  (nskk-it
    "never polls longer than the remaining response wait budget"
    (with-temp-buffer
      (let ((buf (current-buffer))
            (proc 'mock-proc)
            wait
            close-called)
        (nskk-with-mocks
          ((nskk-server-live-p
              (lambda ()
                t))
            (nskk-server-close
              (lambda ()
                (setq close-called t)))
            (accept-process-output
              (lambda (_proc timeout &optional _millisec _just-this-one)
                (setq wait timeout))))
          (should-not (nskk--server-await-response proc buf 0.025))
          (should close-called)
          (should (> wait 0))
          (should (<= wait 0.025001))))))
  (nskk-it
    "keeps response timeout finite when the wall clock jumps"
    (dolist (clock-values '((100.0 99.0 98.0 101.0)
                            (100.0 101.0 102.0 103.0)))
      (with-temp-buffer
        (let ((buf (current-buffer))
              (proc 'mock-proc)
              (readings (copy-sequence clock-values))
              (clock-calls 0)
              (waits nil)
              (close-called nil))
          (nskk-with-mocks
            ((float-time
                (lambda (&optional _)
                  (cl-incf clock-calls)
                  (or (pop readings) 1000.0)))
              (nskk-server-live-p
                (lambda ()
                  t))
              (nskk-server-close
                (lambda ()
                  (setq close-called t)))
              (accept-process-output
                (lambda (wait-proc timeout
                         &optional _millisec just-this-one)
                  (should (eq wait-proc proc))
                  (should just-this-one)
                  (push timeout waits))))
            (should-not (nskk--server-await-response proc buf 0.25))
            (should close-called)
            (should (= clock-calls 0))
            (should (= (length waits) 3))
            (should (cl-every (lambda (wait)
                                (and (> wait 0)
                                     (<= wait 0.1)))
                              waits))
            (should (< (abs (- (apply #'+ waits) 0.25))
                       0.000001))))))))

  (nskk-it "returns nil and resets the connection when the server disconnects"
    (with-temp-buffer
      (let* ((buf (current-buffer))
             (proc 'mock-proc)
             (wait-budget 10)
             (close-called nil))
        (nskk-with-mocks
            ((nskk-server-live-p (lambda () nil))
             (nskk-server-close (lambda () (setq close-called t)))
             (accept-process-output (lambda (&rest _) nil)))
          (should-not (nskk--server-await-response proc buf wait-budget))
          (should close-called)))))

  ;; Regression: a server that streams data without ever sending a newline
  ;; must not accumulate unbounded memory.  Once the buffer exceeds the cap,
  ;; await-response fails and resets the connection rather than looping until
  ;; the wait budget expires.
  (nskk-it "rejects and resets an over-cap response even when it contains a newline"
    (with-temp-buffer
      (let* ((buf (current-buffer))
             (proc 'mock-proc)
             (wait-budget 10)
             (close-called nil))
        (nskk-with-mocks
            ((nskk-server-live-p (lambda () t))
             (nskk-server-close (lambda () (setq close-called t)))
             (accept-process-output
              (lambda (_p _t &optional _millisec _just-this-one)
                (with-current-buffer buf
                  (insert (make-string nskk--server-max-response-size ?x))
                  (insert "\n")))))
          (should-not (nskk--server-await-response proc buf wait-budget))
          (should close-called)))))

  (nskk-context "response byte accounting"
  (nskk-it "still succeeds for a normal-sized response under the cap"
    (with-temp-buffer
      (let* ((buf (current-buffer))
             (proc 'mock-proc)
             (wait-budget 10)
             (close-called nil))
        (nskk-with-mocks
            ((nskk-server-live-p (lambda () t))
             (nskk-server-close (lambda () (setq close-called t)))
             (accept-process-output
              (lambda (_p _t &optional _millisec _just-this-one)
                (with-current-buffer buf (insert "1/漢字/\n")))))
          (should (stringp
                   (nskk--server-await-response proc buf wait-budget)))
          (should-not close-called)))))
  (nskk-it "accepts a multibyte response exactly at the byte cap"
    (with-temp-buffer
      (let ((buf (current-buffer))
            (proc 'mock-proc)
            (wait-budget 10)
            (close-called nil)
            (nskk--server-max-response-size 7))
        (nskk-with-mocks
            ((nskk-server-live-p (lambda () t))
             (nskk-server-close (lambda () (setq close-called t)))
             (accept-process-output
              (lambda (_p _t &optional _millisec _just-this-one)
                (with-current-buffer buf (insert "漢字\n")))))
          (should (equal (nskk--server-await-response
                          proc buf wait-budget)
                         "漢字\n"))
          (should-not close-called)))))
  (nskk-it "rejects a multibyte response one byte above the cap"
    (with-temp-buffer
      (let ((buf (current-buffer))
            (proc 'mock-proc)
            (wait-budget 10)
            (close-called nil)
            (nskk--server-max-response-size 6))
        (nskk-with-mocks
            ((nskk-server-live-p (lambda () t))
             (nskk-server-close (lambda () (setq close-called t)))
             (accept-process-output
              (lambda (_p _t &optional _millisec _just-this-one)
                (with-current-buffer buf (insert "漢字\n")))))
          (should-not (nskk--server-await-response
                       proc buf wait-budget))
          (should close-called)))))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; nskk--server-make-connection: async (:nowait) connect + timeout
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-describe "nskk--server-make-connection async connect"
  (nskk-it "returns the process object once the connection reaches \\='open"
    (let ((nskk-server-timeout 5))
      (nskk-with-mocks
          ((open-network-stream (lambda (&rest _) 'mock-proc))
           (get-buffer-create (lambda (_) nil))
           (process-status (lambda (_) 'open))
           (accept-process-output (lambda (&rest _) nil)))
        (should (eq (nskk--server-make-connection) 'mock-proc)))))

  ;; A blackholed host leaves the process stuck in \\='connect;
  ;; make-connection must exhaust its wait budget and delete the process.
  (nskk-context
  "bounded connection initialization"
  (nskk-it
    "gives up at the timeout and deletes the process when connect never completes"
    (let ((nskk-server-timeout 0.3)
          (deleted-proc nil))
      (nskk-with-mocks
        ((open-network-stream
            (lambda (&rest _)
              'stuck-proc))
          (get-buffer-create
            (lambda (_)
              nil))
          (process-status
            (lambda (_)
              'connect))
          (accept-process-output
            (lambda (&rest _)
              nil))
          (delete-process
            (lambda (proc)
              (setq deleted-proc proc))))
        (should-not (nskk--server-make-connection))
        (should (eq deleted-proc 'stuck-proc)))))
  (nskk-it
    "does not poll for non-finite connection budgets"
    (dolist (wait-budget '(0.0e+NaN 1.0e+INF -1.0e+INF))
      (let ((nskk-server-timeout wait-budget)
            (poll-count 0)
            (deleted-proc nil))
        (nskk-with-mocks
          ((open-network-stream
              (lambda (&rest _)
                'stuck-proc))
            (get-buffer-create
              (lambda (_)
                nil))
            (process-status
              (lambda (_)
                'connect))
            (accept-process-output
              (lambda (&rest _)
                (cl-incf poll-count)))
            (delete-process
              (lambda (proc)
                (setq deleted-proc proc))))
          (should-not (nskk--server-make-connection))
          (should (= poll-count 0))
          (should (eq deleted-proc 'stuck-proc))))))
  (nskk-it
    "handles non-numeric connection timeout as a type error with cleanup"
    (let ((nskk-server-timeout 'invalid)
          (poll-count 0)
          (deleted-proc nil)
          (failure-message nil))
      (nskk-with-mocks
        ((open-network-stream
            (lambda (&rest _)
              'stuck-proc))
          (get-buffer-create
            (lambda (_)
              nil))
          (process-status
            (lambda (_)
              'connect))
          (accept-process-output
            (lambda (&rest _)
              (cl-incf poll-count)))
          (delete-process
            (lambda (proc)
              (setq deleted-proc proc)))
          (nskk-debug-message
            (lambda (_format message)
              (setq failure-message message))))
        (should-not (nskk--server-make-connection))
        (should (= poll-count 0))
        (should (eq deleted-proc 'stuck-proc))
        (should (string-match-p "numberp" failure-message)))))
  (nskk-it
    "never polls longer than the remaining connection wait budget"
    (let ((nskk-server-timeout 0.025)
          (status 'connect)
          wait)
      (nskk-with-mocks
        ((open-network-stream
            (lambda (&rest _)
              'stuck-proc))
          (get-buffer-create
            (lambda (_)
              nil))
          (process-status
            (lambda (_)
              status))
          (accept-process-output
            (lambda (_proc timeout &optional _millisec _just-this-one)
              (setq wait timeout
                    status 'failed)))
          (delete-process
            (lambda (_)
              nil)))
        (should-not (nskk--server-make-connection))
        (should (> wait 0))
        (should (<= wait 0.025001)))))
  (nskk-it
    "keeps connection timeout finite when the wall clock jumps"
    (dolist (clock-values '((100.0 99.0 98.0 101.0)
                            (100.0 101.0 102.0 103.0)))
      (let ((nskk-server-timeout 0.25)
            (readings (copy-sequence clock-values))
            (clock-calls 0)
            (waits nil)
            (deleted-proc nil))
        (nskk-with-mocks
          ((open-network-stream
              (lambda (&rest _)
                'stuck-proc))
            (get-buffer-create
              (lambda (_)
                nil))
            (process-status
              (lambda (_)
                'connect))
            (float-time
              (lambda (&optional _)
                (cl-incf clock-calls)
                (or (pop readings) 1000.0)))
            (accept-process-output
              (lambda (proc timeout &optional _millisec _just-this-one)
                (should (eq proc 'stuck-proc))
                (push timeout waits)))
            (delete-process
              (lambda (proc)
                (setq deleted-proc proc))))
          (should-not (nskk--server-make-connection))
          (should (eq deleted-proc 'stuck-proc))
          (should (= clock-calls 0))
          (should (= (length waits) 3))
          (should (cl-every (lambda (wait)
                              (and (> wait 0)
                                   (<= wait 0.1)))
                            waits))
          (should (< (abs (- (apply #'+ waits) 0.25))
                     0.000001))))))
  (nskk-it
    "cleans up the process and owned buffer when initialization signals"
    (let* ((nskk--server-buffer-name " *nskk-server-init-error-test*")
           (proc
            (make-process
             :name
             "nskk-server-init-error-test"
             :command
             (list (or (executable-find "cat") "/bin/cat"))
             :connection-type
             'pipe
             :noquery
             t)))
      (unwind-protect
          (nskk-with-mocks
            ((open-network-stream
                (lambda (&rest _)
                  proc))
              (set-process-filter
                (lambda (&rest _)
                  (error "filter setup failed"))))
            (should-not (nskk--server-make-connection))
            (should-not (process-live-p proc))
            (should-not (get-buffer nskk--server-buffer-name)))
        (when (process-live-p proc)
          (delete-process proc))
        (when-let* ((buffer (get-buffer nskk--server-buffer-name)))
          (kill-buffer buffer))))))

  (nskk-it "returns nil and deletes the process when connect fails"
    (let ((nskk-server-timeout 5)
          (deleted-proc nil))
      (nskk-with-mocks
          ((open-network-stream (lambda (&rest _) 'failed-proc))
           (get-buffer-create (lambda (_) nil))
           (process-status (lambda (_) 'failed))
           (accept-process-output (lambda (&rest _) nil))
           (delete-process (lambda (proc) (setq deleted-proc proc))))
        (should-not (nskk--server-make-connection))
        (should (eq deleted-proc 'failed-proc))))))

;;;
;;; nskk--server-with-response
;;; After defun/k conversion: sync wrapper (key) -> response-string or nil.
;;; The cont argument is gone; parse is a separate step.
;;;

(nskk-describe "nskk--server-with-response"
  (nskk-it "sends command 1 + key and passes the finite wait budget"
    (let ((sent-cmd nil)
          (wait-budget nil)
          result)
      (with-temp-buffer
        (let ((buf (current-buffer))
              (proc 'mock-proc))
          (nskk-with-mocks
              ((process-live-p
              (lambda (_) t))
             (process-coding-system
                (lambda (_) '(euc-jp . euc-jp)))
               (process-buffer (lambda (_) buf))
               (process-send-string
                (lambda (_proc command)
                  (setq sent-cmd command)))
               (nskk--server-await-response
                (lambda (_proc _buf budget)
                  (setq wait-budget budget)
                  "4/漢字/\n")))
            (let ((nskk--server-process proc)
                  (nskk-server-timeout 5.0)
                  (nskk-server-report-response nil))
              (setq result (nskk--server-with-response "かんじ"))))))
      (should (equal sent-cmd "1かんじ "))
      (should (= wait-budget 5.0))
      (should (equal result "4/漢字/\n"))))

  (nskk-it "returns nil when await-response returns nil (timeout)"
    (let (result)
      (with-temp-buffer
        (let ((buf (current-buffer))
              (proc 'mock-proc))
          (nskk-with-mocks
              ((process-live-p
              (lambda (_) t))
             (process-coding-system
                (lambda (_) '(euc-jp . euc-jp)))
               (process-buffer (lambda (_) buf))
               (process-send-string (lambda (&rest _) nil))
               (nskk--server-await-response (lambda (&rest _) nil)))
            (let ((nskk--server-process proc)
                  (nskk-server-timeout 5.0)
                  (nskk-server-report-response nil))
              (setq result (nskk--server-with-response "key"))))))
      (should-not result)))

  (nskk-it "cleans up without polling for a non-numeric owned response budget"
    (let ((close-called nil)
          (polled nil)
          result)
      (with-temp-buffer
        (let ((buf (current-buffer))
              (proc 'mock-proc))
          (nskk-with-mocks
            ((process-live-p
              (lambda (_) t))
             (process-coding-system
              (lambda (_) '(euc-jp . euc-jp)))
             (process-buffer
              (lambda (_)
                buf))
             (process-send-string
              (lambda (&rest _)
                nil))
             (nskk-server-close
              (lambda ()
                (setq close-called t)))
             (accept-process-output
              (lambda (&rest _)
                (setq polled t))))
            (let ((nskk--server-process proc)
                  (nskk-server-timeout 'invalid)
                  (nskk-server-report-response nil))
              (setq result (nskk--server-with-response "key"))))))
      (should-not result)
      (should close-called)
      (should-not polled))))

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

(nskk-describe "nskk server bounded process filter"
  (nskk-it "retains only a complete first line within the byte cap"
    (with-temp-buffer
      (let ((buf (current-buffer))
            (props (make-hash-table :test #'eq))
            (nskk--server-max-response-size 7))
        (nskk-with-mocks
            ((process-get (lambda (_proc key) (gethash key props)))
             (process-put
              (lambda (_proc key value) (puthash key value props)))
             (process-buffer (lambda (_proc) buf)))
          (nskk--server-process-filter 'mock-proc "漢字\nignored")
          (nskk--server-process-filter 'mock-proc "also ignored")
          (should (equal (buffer-string) "漢字\n"))
          (should (= (string-bytes (buffer-string)) 7))
          (should (eq (gethash 'nskk-response-complete props) t))
          (should-not (gethash 'nskk-response-overflow props))))))

  (nskk-it "rejects an over-cap decoded chunk before retaining it"
    (with-temp-buffer
      (let ((buf (current-buffer))
            (props (make-hash-table :test #'eq))
            (nskk--server-max-response-size 6))
        (nskk-with-mocks
            ((process-get (lambda (_proc key) (gethash key props)))
             (process-put
              (lambda (_proc key value) (puthash key value props)))
             (process-buffer (lambda (_proc) buf)))
          (nskk--server-process-filter 'mock-proc "漢字\ntrailing")
          (should (string-empty-p (buffer-string)))
          (should (<= (string-bytes (buffer-string))
                      nskk--server-max-response-size))
          (should (eq (gethash 'nskk-response-overflow props) t))))))

  (nskk-it "counts decoded UTF-8 chunks across a character boundary"
    (with-temp-buffer
      (let ((buf (current-buffer))
            (props (make-hash-table :test #'eq))
            (nskk--server-max-response-size 7))
        (nskk-with-mocks
            ((process-get (lambda (_proc key) (gethash key props)))
             (process-put
              (lambda (_proc key value) (puthash key value props)))
             (process-buffer (lambda (_proc) buf)))
          (nskk--server-process-filter 'mock-proc "漢")
          (nskk--server-process-filter 'mock-proc "字\nextra")
          (should (equal (buffer-string) "漢字\n"))
          (should (= (gethash 'nskk-response-bytes props) 7)))))))

(nskk-describe "nskk server response validation and cleanup"
  (nskk-it "rejects C0 DEL and C1 controls in candidates"
    (dolist (control '(0 1 9 31 127 128 159))
      (should
       (equal (nskk--server-parse-response
               (concat "1/safe/" (string control) "bad/safe2/\n"))
              '("safe" "safe2")))
      (should-not
       (nskk--server-parse-response
        (concat "1/" (string control) "bad/\n")))))

  (nskk-it "rejects reused processes with changed coding before any mutation"
    (with-temp-buffer
      (insert "sentinel")
      (let* ((buf (current-buffer))
             (proc
              (make-pipe-process
               :name "nskk-server-coding-mismatch"
               :buffer buf
               :coding '(euc-jp . euc-jp)
               :noquery t))
             (sent nil)
             (close-called nil))
        (process-put proc 'nskk-response-bytes 17)
        (process-put proc 'nskk-response-overflow 'overflow)
        (process-put proc 'nskk-response-complete 'complete)
        (unwind-protect
            (let ((nskk--server-process proc)
                  (nskk-server-coding-system 'utf-8))
              (nskk-with-mocks
                  ((process-send-string
                    (lambda (&rest _) (setq sent t)))
                   (nskk-server-close
                    (lambda () (setq close-called t))))
                (should-not (nskk--server-with-response "かんじ"))
                (should-not sent)
                (should-not close-called)
                (should (equal (buffer-string) "sentinel"))
                (should (= (process-get proc 'nskk-response-bytes) 17))
                (should
                 (eq (process-get proc 'nskk-response-overflow) 'overflow))
                (should
                 (eq (process-get proc 'nskk-response-complete) 'complete))))
          (when (process-live-p proc)
            (delete-process proc))))))

  (nskk-it "does not close when setup fails before a send attempt"
    (let ((nskk--server-process 'mock-proc)
          (close-called nil))
      (nskk-with-mocks
          ((process-live-p
              (lambda (_) t))
             (process-coding-system
            (lambda (_) '(euc-jp . euc-jp)))
           (process-buffer (lambda (_proc) (error "setup failed")))
           (nskk-server-close (lambda () (setq close-called t))))
        (should-not (nskk--server-with-response "key"))
        (should-not close-called))))

  (nskk-it "closes after a send error without leaking cleanup errors"
    (with-temp-buffer
      (let ((buf (current-buffer))
            (nskk--server-process 'mock-proc)
            (close-called nil))
        (nskk-with-mocks
            ((process-live-p
              (lambda (_) t))
             (process-coding-system
              (lambda (_) '(euc-jp . euc-jp)))
             (process-buffer (lambda (_proc) buf))
             (process-send-string (lambda (&rest _) (error "send failed")))
             (nskk-server-close
              (lambda ()
                (setq close-called t)
                (error "close failed"))))
          (should-not (nskk--server-with-response "key"))
          (should close-called)))))

  (nskk-it "closes after an await error without leaking cleanup quits"
    (with-temp-buffer
      (let ((buf (current-buffer))
            (nskk--server-process 'mock-proc)
            (close-called nil))
        (nskk-with-mocks
            ((process-live-p
              (lambda (_) t))
             (process-coding-system
              (lambda (_) '(euc-jp . euc-jp)))
             (process-buffer (lambda (_proc) buf))
             (process-send-string (lambda (&rest _) nil))
             (nskk--server-await-response
              (lambda (&rest _) (error "await failed")))
             (nskk-server-close
              (lambda ()
                (setq close-called t)
                (signal 'quit '("close quit")))))
          (should-not (nskk--server-with-response "key"))
          (should close-called)))))

  (nskk-it "closes after an await quit and preserves the original quit"
    (with-temp-buffer
      (let ((buf (current-buffer))
            (nskk--server-process 'mock-proc)
            (close-called nil)
            caught)
        (nskk-with-mocks
            ((process-live-p
              (lambda (_) t))
             (process-coding-system
              (lambda (_) '(euc-jp . euc-jp)))
             (process-buffer (lambda (_proc) buf))
             (process-send-string (lambda (&rest _) nil))
             (nskk--server-await-response
              (lambda (&rest _) (signal 'quit '("await quit"))))
             (nskk-server-close
              (lambda ()
                (setq close-called t)
                (error "close failed"))))
          (condition-case err
              (nskk--server-with-response "key")
            (quit (setq caught err)))
          (should (equal caught '(quit "await quit")))
          (should close-called)))))

  (nskk-it "keeps the connection after a completed response"
    (with-temp-buffer
      (let ((buf (current-buffer))
            (nskk--server-process 'mock-proc)
            (close-called nil))
        (nskk-with-mocks
            ((process-live-p
              (lambda (_) t))
             (process-coding-system
              (lambda (_) '(euc-jp . euc-jp)))
             (process-buffer (lambda (_proc) buf))
             (process-send-string (lambda (&rest _) nil))
             (nskk--server-await-response
              (lambda (&rest _) "1/value/\n"))
             (nskk-server-close (lambda () (setq close-called t))))
          (should (equal (nskk--server-with-response "key") "1/value/\n"))
          (should-not close-called))))))

(nskk-describe "nskk-server-open configure rollback"
    (nskk-it "rolls back every configure error and quit without changing CPS failure"
      (let ((real-set-process-query-on-exit-flag
             (symbol-function 'set-process-query-on-exit-flag))
            (real-add-hook (symbol-function 'add-hook))
            (real-retract (symbol-function 'nskk-prolog-retract-all))
            (real-assert (symbol-function 'nskk-prolog-assert)))
        (dolist (kind '(error quit))
          (dolist (stage '(query hook retract assert debug))
            (let* ((name (format "nskk-configure-%s-%s" stage kind))
                   (nskk--server-buffer-name
                    (format " *%s*" name))
                   (buffer (get-buffer-create nskk--server-buffer-name))
                   (proc
                    (make-process
                     :name name
                     :buffer buffer
                     :command
                     (list (or (executable-find "cat") "/bin/cat"))
                     :connection-type 'pipe
                     :noquery t))
                   (nskk-server-enable t)
                   (nskk--server-process 'previous-process)
                   (nskk--server-kill-emacs-hook-registered nil)
                   (kill-emacs-hook nil)
                   (data (list "injected" stage kind))
                   (caught nil)
                   (retract-injected nil))
              (unwind-protect
                  (cl-letf
                      (((symbol-function 'nskk--server-make-connection)
                        (lambda ()
                          (process-put proc 'nskk-server-owned-buffer t)
                          proc))
                       ((symbol-function 'set-process-query-on-exit-flag)
                        (lambda (&rest args)
                          (if (eq stage 'query)
                              (signal kind data)
                            (apply real-set-process-query-on-exit-flag args))))
                       ((symbol-function 'add-hook)
                        (lambda (&rest args)
                          (apply real-add-hook args)
                          (when (eq stage 'hook)
                            (signal kind data))))
                       ((symbol-function 'nskk-prolog-retract-all)
                        (lambda (&rest args)
                          (if (and (eq stage 'retract)
                                   (not retract-injected))
                              (progn
                                (setq retract-injected t)
                                (signal kind data))
                            (apply real-retract args))))
                       ((symbol-function 'nskk-prolog-assert)
                        (lambda (facts)
                          (if (and (eq stage 'assert)
                                   (equal facts '((server-state open))))
                              (signal kind data)
                            (funcall real-assert facts))))
                       ((symbol-function 'nskk-debug-message)
                        (lambda (&rest _)
                          (when (eq stage 'debug)
                            (signal kind data)))))
                    (condition-case condition
                        (nskk-server-open)
                      ((error quit)
                       (setq caught condition)))
                    (should (eq (car caught) kind))
                    (should (equal (cdr caught) data))
                    (should
                     (eq nskk--server-process 'previous-process))
                    (should-not
                     nskk--server-kill-emacs-hook-registered)
                    (should-not
                     (memq #'nskk-server-close kill-emacs-hook))
                    (should-not (process-live-p proc))
                    (should-not (buffer-live-p buffer))
                    (should
                     (nskk-prolog-holds-p
                      '(server-state closed))))
                (when (process-live-p proc)
                  (delete-process proc))
                (when (buffer-live-p buffer)
                  (kill-buffer buffer))
                (funcall real-retract 'server-state 1)
                (funcall real-assert
                         '((server-state closed))))))))))

  (nskk-describe "nskk server connection ownership boundaries"
    (nskk-it "cleans up an owned process and buffer when initialization quits"
      (let* ((name "nskk-make-connection-quit")
             (nskk--server-buffer-name " *nskk-make-connection-quit*")
             (proc (make-process
                    :name name
                    :buffer nil
                    :command (list (or (executable-find "cat") "/bin/cat"))
                    :connection-type 'pipe
                    :noquery t))
             (caught nil))
        (unwind-protect
            (cl-letf (((symbol-function 'open-network-stream)
                       (lambda (&rest _) proc))
                      ((symbol-function 'set-process-filter)
                       (lambda (&rest _)
                         (signal 'quit '("filter quit")))))
              (condition-case condition
                  (nskk--server-make-connection)
                (quit (setq caught condition)))
              (should (eq (car caught) 'quit))
              (should-not (process-live-p proc))
              (should-not (get-buffer nskk--server-buffer-name)))
          (when (process-live-p proc)
            (delete-process proc))
          (when-let* ((buffer (get-buffer nskk--server-buffer-name)))
            (kill-buffer buffer)))))

    (nskk-it "keeps the target as the wait condition while allowing connection events"
      (let* ((nskk--server-buffer-name " *nskk-target-only-wait*")
             (nskk-server-timeout 0.1)
             (noise-buffer (generate-new-buffer " *nskk-unrelated-noise*"))
             (noise-process
              (make-process
               :name "nskk-unrelated-noise"
               :buffer noise-buffer
               :command (list (or (executable-find "cat") "/bin/cat"))
               :connection-type 'pipe
               :noquery t))
             (target-status 'connect)
             (wait-calls nil))
        (unwind-protect
            (progn
              (process-send-string noise-process "noise\n")
              (cl-letf (((symbol-function 'open-network-stream)
                         (lambda (&rest _) 'target-process))
                        ((symbol-function 'process-status)
                         (lambda (proc)
                           (if (eq proc 'target-process)
                               target-status
                             'run)))
                        ((symbol-function 'accept-process-output)
                         (lambda (proc timeout &optional millisec just-this-one)
                           (push (list proc timeout millisec just-this-one)
                                 wait-calls)
                           (setq target-status 'failed)
                           nil)))
                (should-not (nskk--server-make-connection)))
              (should wait-calls)
              (dolist (call wait-calls)
                (should (eq (nth 0 call) 'target-process))
                (should (numberp (nth 1 call)))
                (should-not (nth 2 call))
                (should-not (nth 3 call)))
              (should (process-live-p noise-process)))
          (when (process-live-p noise-process)
            (delete-process noise-process))
          (when (buffer-live-p noise-buffer)
            (kill-buffer noise-buffer))
          (when-let* ((buffer (get-buffer nskk--server-buffer-name)))
            (kill-buffer buffer)))))

    (nskk-it "retains configure rollback ownership until cleanup faults recover"
      (let ((real-configure
             (symbol-function 'nskk--server-configure-process))
            (real-delete (symbol-function 'delete-process))
            (real-kill (symbol-function 'kill-buffer))
            (real-retract (symbol-function 'nskk-prolog-retract-all))
            (real-assert (symbol-function 'nskk-prolog-assert)))
        (dolist (phase '(delete kill))
          (dolist (kind '(error quit))
            (let ((nskk-server-enable t)
                  (nskk--server-process nil)
 (nskk--server-pending-cleanups nil)
                  (nskk--server-kill-emacs-hook-registered nil)
                  (kill-emacs-hook nil)
                  (make-count 0)
                  (configure-count 0)
                  (faulting t)
                  (processes nil)
                  (buffers nil)
                  (configure-data
                   (list "configure rollback" phase kind))
                  (cleanup-data
                   (list "cleanup rollback" phase kind))
                  caught
                  before
                  failed-process
                  failed-buffer)
              (unwind-protect
                  (progn
                    (funcall real-retract 'server-state 1)
                    (funcall real-assert '((server-state legacy)))
                    (setq before (nskk--server-prolog-state-snapshot))
                    (cl-letf
                        (((symbol-function 'nskk--server-make-connection)
                          (lambda ()
                            (cl-incf make-count)
                            (let* ((buffer
                                    (generate-new-buffer
                                     (format
                                      " *nskk-configure-ownership-%s-%s-%d*"
                                      phase kind make-count)))
                                   (proc
                                    (make-pipe-process
                                     :name
                                     (format
                                      "nskk-configure-ownership-%s-%s-%d"
                                      phase kind make-count)
                                     :buffer buffer
                                     :noquery t)))
                              (process-put
                               proc 'nskk-server-owned-buffer t)
                              (push buffer buffers)
                              (push proc processes)
                              proc)))
                         ((symbol-function 'nskk--server-configure-process)
                          (lambda (proc)
                            (cl-incf configure-count)
                            (if (= configure-count 1)
                                (progn
                                  (setq nskk--server-process proc)
                                  (set-process-buffer proc nil)
                                  (add-hook
                                   'kill-emacs-hook #'nskk-server-close)
                                  (setq
                                   nskk--server-kill-emacs-hook-registered
                                   t)
                                  (funcall real-retract 'server-state 1)
                                  (funcall real-assert
                                           '((server-state corrupt)))
                                  (signal kind configure-data))
                              (funcall real-configure proc))))
                         ((symbol-function 'delete-process)
                          (lambda (proc)
                            (if (and faulting (eq phase 'delete))
                                (signal kind cleanup-data)
                              (funcall real-delete proc))))
                         ((symbol-function 'kill-buffer)
                          (lambda (&optional buffer-or-name)
                            (if (and faulting (eq phase 'kill))
                                (signal kind cleanup-data)
                              (funcall real-kill buffer-or-name)))))
                      (condition-case condition
                          (nskk-server-open)
                        ((error quit) (setq caught condition)))
                      (setq failed-process (car processes)
                            failed-buffer (car buffers))
                      (should (eq (car caught) kind))
                      (should (equal (cdr caught) configure-data))
                      (progn
  (should-not nskk--server-process)
  (should (= (length nskk--server-pending-cleanups) 1))
  (if (eq phase 'delete)
      (progn
        (should (eq (caar nskk--server-pending-cleanups)
                    failed-process))
        (should-not (cdar nskk--server-pending-cleanups)))
    (should-not (caar nskk--server-pending-cleanups))
    (should (eq (cdar nskk--server-pending-cleanups)
                failed-buffer))))
                      (should nskk--server-kill-emacs-hook-registered)
                      (should (memq #'nskk-server-close kill-emacs-hook))
                      (if (eq phase 'delete)
                          (progn
                            (should (process-live-p failed-process))
                            (should-not
                             (buffer-live-p failed-buffer)))
                        (should-not (process-live-p failed-process))
                        (should (buffer-live-p failed-buffer)))
                      (let* ((after
                              (nskk--server-prolog-state-snapshot))
                             (before-missing (aref before 0))
                             (after-missing (aref after 0)))
                        (dotimes (index (length after))
                          (when (eq (aref after index) after-missing)
                            (aset after index before-missing)))
                        (should (equal before after)))
                      (let ((make-before make-count)
                            (configure-before configure-count))
                        (should-not (nskk-server-open))
                        (should (= make-count make-before))
                        (should (= configure-count configure-before))
                        (progn
  (should-not nskk--server-process)
  (should (= (length nskk--server-pending-cleanups) 1))
  (if (eq phase 'delete)
      (progn
        (should (eq (caar nskk--server-pending-cleanups)
                    failed-process))
        (should-not (cdar nskk--server-pending-cleanups)))
    (should-not (caar nskk--server-pending-cleanups))
    (should (eq (cdar nskk--server-pending-cleanups)
                failed-buffer))))
                        (should
                         (nskk-prolog-holds-p
                          '(server-state legacy))))
                      (setq faulting nil)
                      (let ((replacement (nskk-server-open)))
                        (should (processp replacement))
                        (should (eq nskk--server-process replacement))
                        (should-not (eq replacement failed-process))
                        (should (= make-count 2))
                        (should (= configure-count 2))
                        (should-not
                         (process-live-p failed-process))
                        (should-not
                         (buffer-live-p failed-buffer))
                        (should (process-live-p replacement))
                        (should
                         (buffer-live-p
                          (process-buffer replacement)))
                        (should
                         (nskk-prolog-holds-p
                          '(server-state open)))
                        (nskk-server-close)
                        (progn
  (should-not nskk--server-process)
  (should-not nskk--server-pending-cleanups))
                        (should-not (process-live-p replacement))
                        (should-not
                         (buffer-live-p
                          (process-buffer replacement))))))
                (dolist (proc processes)
                  (when (process-live-p proc)
                    (funcall real-delete proc)))
                (dolist (buffer buffers)
                  (when (buffer-live-p buffer)
                    (funcall real-kill buffer)))
                (setq nskk--server-process nil)
                (funcall real-retract 'server-state 1)
                (funcall real-assert
                         '((server-state closed))))))))))

  (nskk-describe "nskk-server-close atomic publication"
    (nskk-it "restores a warm indexed state and resources on publish error and quit"
      (let ((real-retract (symbol-function 'nskk-prolog-retract-all))
            (real-assert (symbol-function 'nskk-prolog-assert)))
        (dolist (phase '(retract assert))
          (dolist (kind '(error quit))
            (nskk-prolog-test-with-isolated-db
              (nskk-prolog-with-database-fields
                  ((index-bucket-tail-cache
                    (make-hash-table :test 'equal)))
              (let* ((buffer
                      (generate-new-buffer
                       (format " *nskk-close-publish-%s-%s*" phase kind)))
                     (proc
                      (make-pipe-process
                       :name (format "nskk-close-publish-%s-%s" phase kind)
                       :buffer buffer
                       :noquery nil))
                     (nskk--server-process proc)
                     (kill-emacs-hook (list #'nskk-server-close))
                     (send-count 0)
                     (delete-count 0)
                     (data (list "close publish" phase kind)))
                (process-put proc 'nskk-server-owned-buffer t)
                (set-process-query-on-exit-flag proc t)
                (unwind-protect
                    (progn
                      (nskk-prolog-clear-database)
                      (nskk-prolog-set-index 'server-state 1 :hash)
                      (funcall real-assert '((server-state open)))
                      (funcall real-assert '((server-state standby)))
                      (funcall real-assert '((server-state open)))
                      (let* ((key "server-state/1")
                             (database-head
                              (gethash key (nskk-prolog-database)))
                             (database-tail
                              (gethash key (nskk-prolog-database-tails)))
                             (index
                              (gethash key (nskk-prolog-hash-indices)))
                             (bucket (gethash 'open index))
                             (bucket-tail (last bucket))
                             (cache-entry
                              (gethash
                               key (nskk-prolog-index-bucket-tail-cache)))
                             (cache-buckets (aref cache-entry 2))
                             (cache-info (gethash 'open cache-buckets))
                             (database-copy (copy-tree database-head))
                             (bucket-copy (copy-tree bucket))
                             (lookup-copy
                              (copy-tree
                               (nskk-prolog-get-clauses
                                'server-state '(open) nil)))
                             (before
                              (nskk--server-prolog-state-snapshot)))
                        (let ((caught
                               (cl-letf
                                   (((symbol-function 'process-send-string)
                                     (lambda (&rest _)
                                       (cl-incf send-count)))
                                    ((symbol-function 'delete-process)
                                     (lambda (&rest _)
                                       (cl-incf delete-count)))
                                    ((symbol-function
                                      'nskk-prolog-retract-all)
                                     (lambda (&rest args)
                                       (prog1 (apply real-retract args)
                                         (when (eq phase 'retract)
                                           (signal kind data)))))
                                    ((symbol-function 'nskk-prolog-assert)
                                     (lambda (&rest args)
                                       (prog1 (apply real-assert args)
                                         (when (eq phase 'assert)
                                           (signal kind data))))))
                                 (condition-case condition
                                     (progn (nskk-server-close) nil)
                                   ((error quit) condition)))))
                          (should (eq (car caught) kind))
                          (should (equal (cdr caught) data)))
                        (should (eq nskk--server-process proc))
                        (should (process-live-p proc))
                        (should (buffer-live-p buffer))
                        (should (process-query-on-exit-flag proc))
                        (should
                         (equal kill-emacs-hook
                                (list #'nskk-server-close)))
                        (should (= send-count 0))
                        (should (= delete-count 0))
                        (let* ((after
                                (nskk--server-prolog-state-snapshot))
                               (before-missing (aref before 0))
                               (after-missing (aref after 0)))
                          (dotimes (position (length after))
                            (when (eq (aref after position) after-missing)
                              (aset after position before-missing)))
                          (should (= (length after) 8))
                          (should (equal before after))
                          (dotimes (position 6)
                            (should
                             (eq (aref before (+ position 2))
                                 (aref after (+ position 2))))))
                        (should
                         (eq database-head
                             (gethash key (nskk-prolog-database))))
                        (should
                         (eq database-tail
                             (gethash key (nskk-prolog-database-tails))))
                        (should
                         (eq index
                             (gethash key (nskk-prolog-hash-indices))))
                        (should (eq bucket (gethash 'open index)))
                        (should
                         (eq cache-entry
                             (gethash
                              key (nskk-prolog-index-bucket-tail-cache))))
                        (should (eq cache-buckets (aref cache-entry 2)))
                        (should (eq cache-info
                                    (gethash 'open cache-buckets)))
                        (should (eq bucket (aref cache-info 0)))
                        (should (eq bucket-tail (aref cache-info 1)))
                        (should (equal database-copy database-head))
                        (should (equal bucket-copy bucket))
                        (should
                         (equal lookup-copy
                                (nskk-prolog-get-clauses
                                 'server-state '(open) nil)))
                        (funcall real-assert '((server-state open)))
                        (let* ((new-database-tail
                                (gethash key (nskk-prolog-database-tails)))
                               (new-bucket-tail (last bucket))
                               (new-cache-info
                                (gethash 'open cache-buckets)))
                          (should
                           (eq database-head
                               (gethash key (nskk-prolog-database))))
                          (should (eq (cdr database-tail)
                                      new-database-tail))
                          (should (eq bucket (gethash 'open index)))
                          (should (eq (cdr bucket-tail) new-bucket-tail))
                          (should
                           (eq cache-entry
                               (gethash
                                key (nskk-prolog-index-bucket-tail-cache))))
                          (should (eq cache-buckets (aref cache-entry 2)))
                          (should (eq bucket (aref new-cache-info 0)))
                          (should
                           (eq new-bucket-tail (aref new-cache-info 1)))
                          (should
                           (= (length
                               (nskk-prolog-get-clauses
                                'server-state '(open) nil))
                              3)))))
                  (when (process-live-p proc)
                    (set-process-query-on-exit-flag proc nil)
                    (delete-process proc))
                  (when (buffer-live-p buffer)
                    (let ((kill-buffer-hook nil)
                          (kill-buffer-query-functions nil))
                      (kill-buffer buffer)))))))))))

    (nskk-it "retries pre-effect teardown faults without repeating completed effects"
      (let ((real-send (symbol-function 'process-send-string))
            (real-delete (symbol-function 'delete-process))
            (real-kill (symbol-function 'kill-buffer)))
        (dolist (case '((send before)
                        (delete before)
                        (delete after)
                        (kill before)
                        (kill after)))
          (dolist (kind '(error quit))
            (let* ((phase (car case))
                   (timing (cadr case))
                   (owned-buffer
                    (not (and (eq phase 'delete)
                              (eq timing 'before))))
                   (buffer
                    (generate-new-buffer
                     (format " *nskk-close-cleanup-%s-%s-%s*"
                             phase timing kind)))
                   (proc
                    (make-pipe-process
                     :name (format "nskk-close-cleanup-%s-%s-%s"
                                   phase timing kind)
                     :buffer buffer
                     :noquery nil))
                   (nskk--server-process proc)
 (nskk--server-pending-cleanups nil)
                   (data (list "close cleanup" phase timing kind))
                   (hook-called nil)
                   (query-called nil)
                   (send-count 0)
                   (delete-count 0)
                   (kill-count 0))
              (process-put proc 'nskk-server-owned-buffer owned-buffer)
              (with-current-buffer buffer
                (add-hook 'kill-buffer-hook
                          (lambda () (setq hook-called t)) nil t)
                (add-hook 'kill-buffer-query-functions
                          (lambda () (setq query-called t) nil) nil t))
              (unwind-protect
                  (progn
                    (nskk-prolog-retract-all 'server-state 1)
                    (nskk-prolog-assert '((server-state open)))
                    (let ((caught
                           (condition-case condition
                               (cl-letf
                                   (((symbol-function 'process-send-string)
                                     (lambda (&rest args)
                                       (cl-incf send-count)
                                       (if (eq phase 'send)
                                           (signal kind data)
                                         (apply real-send args))))
                                    ((symbol-function 'delete-process)
                                     (lambda (&rest args)
                                       (cl-incf delete-count)
                                       (cond
                                        ((and (eq phase 'delete)
                                              (eq timing 'before)
                                              (= delete-count 1))
                                         (signal kind data))
                                        ((and (eq phase 'delete)
                                              (eq timing 'after)
                                              (= delete-count 1))
                                         (prog1 (apply real-delete args)
                                           (signal kind data)))
                                        (t (apply real-delete args)))))
                                    ((symbol-function 'kill-buffer)
                                     (lambda (&rest args)
                                       (cl-incf kill-count)
                                       (cond
                                        ((and (eq phase 'kill)
                                              (eq timing 'before)
                                              (= kill-count 1))
                                         (signal kind data))
                                        ((and (eq phase 'kill)
                                              (eq timing 'after)
                                              (= kill-count 1))
                                         (prog1 (apply real-kill args)
                                           (signal kind data)))
                                        (t (apply real-kill args))))))
                                 (nskk-server-close)
                                 nil)
                             ((error quit) condition))))
                      (should-not caught))
                    (progn
  (should-not nskk--server-process)
  (should-not nskk--server-pending-cleanups))
                    (should-not (process-live-p proc))
                    (if owned-buffer
                        (should-not (buffer-live-p buffer))
                      (should (buffer-live-p buffer)))
                    (should-not (process-query-on-exit-flag proc))
                    (should-not hook-called)
                    (should-not query-called)
                    (should (nskk-prolog-holds-p '(server-state closed)))
                    (should (= send-count 1))
                    (should
                     (= delete-count
                        (if (and (eq phase 'delete)
                                 (eq timing 'before))
                            2
                          1)))
                    (should
                     (= kill-count
                        (cond
                         ((not owned-buffer) 0)
                         ((and (eq phase 'kill)
                               (eq timing 'before))
                          2)
                         (t 1)))))
                (when (process-live-p proc)
                  (set-process-query-on-exit-flag proc nil)
                  (funcall real-delete proc))
                (when (buffer-live-p buffer)
                  (let ((kill-buffer-hook nil)
                        (kill-buffer-query-functions nil))
                    (funcall real-kill buffer)))))))))

    (nskk-it "retains cleanup ownership across persistent pre-effect teardown faults"
	     (let ((real-send (symbol-function 'process-send-string))
		   (real-delete (symbol-function 'delete-process))
		   (real-kill (symbol-function 'kill-buffer))
		   (real-retract (symbol-function 'nskk-prolog-retract-all))
		   (real-assert (symbol-function 'nskk-prolog-assert)))
	       (dolist (phase '(delete kill))
		 (dolist (kind '(error quit))
		   (let* ((owned-buffer (eq phase 'kill))
			  (buffer
			   (generate-new-buffer
			    (format " *nskk-close-persistent-%s-%s*" phase kind)))
			  (proc
			   (make-pipe-process
			    :name (format "nskk-close-persistent-%s-%s" phase kind)
			    :buffer buffer
			    :noquery nil))
			  (nskk--server-process proc)
 (nskk--server-pending-cleanups nil)
			  (data (list "persistent cleanup" phase kind))
			  (faulting t)
			  (send-count 0)
			  (delete-count 0)
			  (kill-count 0)
			  (retract-count 0)
			  (assert-count 0))
		     (process-put proc 'nskk-server-owned-buffer owned-buffer)
		     (unwind-protect
			 (progn
			   (nskk-prolog-retract-all 'server-state 1)
			   (nskk-prolog-assert '((server-state open)))
			   (cl-letf
			       (((symbol-function 'process-send-string)
				 (lambda (&rest args)
				   (cl-incf send-count)
				   (apply real-send args)))
				((symbol-function 'nskk-prolog-retract-all)
				 (lambda (&rest args)
				   (cl-incf retract-count)
				   (apply real-retract args)))
				((symbol-function 'nskk-prolog-assert)
				 (lambda (&rest args)
				   (cl-incf assert-count)
				   (apply real-assert args)))
				((symbol-function 'delete-process)
				 (lambda (&rest args)
				   (cl-incf delete-count)
				   (if (and faulting (eq phase 'delete))
				       (signal kind data)
				     (apply real-delete args))))
				((symbol-function 'kill-buffer)
				 (lambda (&rest args)
				   (cl-incf kill-count)
				   (if (and faulting (eq phase 'kill))
				       (signal kind data)
				     (apply real-kill args)))))
			     (should-not
			      (condition-case condition
				  (progn (nskk-server-close) nil)
				((error quit) condition)))
			     (progn
  (should-not nskk--server-process)
  (should (= (length nskk--server-pending-cleanups) 1))
  (if (eq phase 'delete)
      (progn
        (should (eq (caar nskk--server-pending-cleanups) proc))
        (should-not (cdar nskk--server-pending-cleanups)))
    (should-not (caar nskk--server-pending-cleanups))
    (should (eq (cdar nskk--server-pending-cleanups) buffer))))
			     (should (nskk-prolog-holds-p '(server-state closed)))
			     (should (= send-count 1))
			     (should (= retract-count 1))
			     (should (= assert-count 1))
			     (if (eq phase 'delete)
				 (progn
				   (should (process-live-p proc))
				   (should (= delete-count 2))
				   (should (= kill-count 0)))
			       (should-not (process-live-p proc))
			       (should (buffer-live-p buffer))
			       (should (= delete-count 1))
			       (should (= kill-count 2)))
			     (progn
  (should-not
   (condition-case condition
       (progn (nskk-server-close) nil)
     ((error quit) condition)))
  (should-not nskk--server-process)
  (should (= (length nskk--server-pending-cleanups) 1))
  (if (eq phase 'delete)
      (progn
        (should (eq (caar nskk--server-pending-cleanups) proc))
        (should-not (cdar nskk--server-pending-cleanups)))
    (should-not (caar nskk--server-pending-cleanups))
    (should (eq (cdar nskk--server-pending-cleanups) buffer)))
  (should (= send-count 1))
  (should (= retract-count 1))
  (should (= assert-count 1))
  (should (= delete-count (if (eq phase 'delete) 4 1)))
  (should (= kill-count (if (eq phase 'kill) 4 0)))
  (setq faulting nil))
			     (should-not
			      (condition-case condition
				  (progn (nskk-server-close) nil)
				((error quit) condition)))
			     (progn
  (should-not nskk--server-process)
  (should-not nskk--server-pending-cleanups))
			     (should-not (process-live-p proc))
			     (if owned-buffer
				 (should-not (buffer-live-p buffer))
			       (should (buffer-live-p buffer)))
			     (should (= send-count 1))
			     (should (= retract-count 1))
			     (should (= assert-count 1))
			     (should (= delete-count (if (eq phase 'delete) 5 1)))
			     (should (= kill-count (if (eq phase 'kill) 5 0)))))
		       (when (process-live-p proc)
			 (set-process-query-on-exit-flag proc nil)
			 (funcall real-delete proc))
		       (when (buffer-live-p buffer)
			 (let ((kill-buffer-hook nil)
			       (kill-buffer-query-functions nil))
			   (funcall real-kill buffer))))))))))
  (nskk-describe "nskk-server pending cleanup with real processes"
    (nskk-it "blocks duplicate opens until failed setup resources are reclaimed"
      (let ((real-filter (symbol-function 'set-process-filter))
            (real-delete (symbol-function 'delete-process))
            (real-kill (symbol-function 'kill-buffer))
            (real-retract (symbol-function 'nskk-prolog-retract-all))
            (real-assert (symbol-function 'nskk-prolog-assert)))
        (dolist (scenario '((process delete)
                            (process kill)
                            (buffer-only kill)))
          (dolist (kind '(error quit))
            (let* ((ownership (car scenario))
                   (phase (cadr scenario))
                   (nskk-server-enable t)
                   (nskk-server-timeout 0)
                   (nskk--server-buffer-name
                    (format " *nskk-real-pending-%s-%s-%s*"
                            ownership phase kind))
                   (nskk--server-process nil)
                   (nskk--server-pending-cleanups nil)
                   (nskk--server-kill-emacs-hook-registered nil)
                   (kill-emacs-hook nil)
                   (faulting t)
                   (make-count 0)
                   (processes nil)
                   (buffers nil)
                   (setup-data (list "setup fault" ownership phase kind))
                   (cleanup-data (list "cleanup fault" ownership phase kind))
                   caught
                   failed-process
                   failed-buffer)
              (unwind-protect
                  (progn
                    (funcall real-retract 'server-state 1)
                    (funcall real-assert '((server-state closed)))
                    (cl-letf
                        (((symbol-function 'open-network-stream)
                          (lambda (_name buffer-or-name &rest _args)
                            (cl-incf make-count)
                            (let ((buffer
                                   (if (bufferp buffer-or-name)
                                       buffer-or-name
                                     (get-buffer-create buffer-or-name))))
                              (cl-pushnew buffer buffers)
                              (if (and faulting
                                       (eq ownership 'buffer-only))
                                  (signal kind setup-data)
                                (let ((proc
                                       (make-pipe-process
                                        :name
                                        (format "nskk-real-pending-%s-%s-%s-%d"
                                                ownership phase kind make-count)
                                        :buffer (if (and faulting (eq phase 'delete))
    nil
  buffer)
                                        :noquery t)))
                                  (push proc processes)
                                  proc)))))
                         ((symbol-function 'set-process-filter)
                          (lambda (proc filter)
                            (if (and faulting (eq ownership 'process))
                                (signal kind setup-data)
                              (funcall real-filter proc filter))))
                         ((symbol-function 'delete-process)
                          (lambda (proc)
                            (if (and faulting (eq phase 'delete))
                                (signal kind cleanup-data)
                              (funcall real-delete proc))))
                         ((symbol-function 'kill-buffer)
                          (lambda (&optional buffer)
                            (if (and faulting (eq phase 'kill))
                                (signal kind cleanup-data)
                              (funcall real-kill buffer)))))
                      (setq caught
                            (condition-case condition
                                (progn (nskk-server-open) nil)
                              ((error quit) condition)))
                      (if (eq kind 'error)
    (should-not caught)
  (should (eq (car caught) kind))
  (should (equal (cdr caught) setup-data)))
                      (setq failed-process (car processes))
                      (setq failed-buffer (car buffers))
                      (should (= make-count 1))
                      (should-not nskk--server-process)
                      (should (= (length nskk--server-pending-cleanups) 1))
                      (if (eq phase 'delete)
                          (progn
                            (should
                             (eq (caar nskk--server-pending-cleanups)
                                 failed-process))
                            (should-not
                             (cdar nskk--server-pending-cleanups)))
                        (should-not
                         (caar nskk--server-pending-cleanups))
                        (should
                         (eq (cdar nskk--server-pending-cleanups)
                             failed-buffer)))
                      (should nskk--server-kill-emacs-hook-registered)
                      (should (memq #'nskk-server-close kill-emacs-hook))
                      (let ((live-process-count
                             (cl-count-if #'process-live-p processes))
                            (live-buffer-count
                             (cl-count-if #'buffer-live-p buffers)))
                        (setq caught
                              (condition-case condition
                                  (progn (nskk-server-open) nil)
                                ((error quit) condition)))
                        (should-not caught)
                        (should (= make-count 1))
                        (should
                         (= (cl-count-if #'process-live-p processes)
                            live-process-count))
                        (should
                         (= (cl-count-if #'buffer-live-p buffers)
                            live-buffer-count))
                        (should
                         (= (length nskk--server-pending-cleanups) 1)))
                      (setq faulting nil)
                      (let* ((replacement (nskk-server-open))
                             (replacement-buffer
                              (and replacement
                                   (process-buffer replacement))))
                        (should (processp replacement))
                        (should (process-live-p replacement))
                        (should (= make-count 2))
                        (should-not nskk--server-pending-cleanups)
                        (when failed-process
                          (should-not (process-live-p failed-process)))
                        (should-not (buffer-live-p failed-buffer))
                        (should
                         (= (cl-count-if #'process-live-p processes) 1))
                        (should
                         (= (cl-count-if #'buffer-live-p buffers) 1))
                        (nskk-server-close)
                        (should-not nskk--server-process)
                        (should-not nskk--server-pending-cleanups)
                        (should-not (process-live-p replacement))
                        (should-not (buffer-live-p replacement-buffer))
                        (should
                         (= (cl-count-if #'process-live-p processes) 0))
                        (should
                         (= (cl-count-if #'buffer-live-p buffers) 0)))))
                (dolist (proc processes)
                  (when (process-live-p proc)
                    (set-process-query-on-exit-flag proc nil)
                    (funcall real-delete proc)))
                (dolist (buffer buffers)
                  (when (buffer-live-p buffer)
                    (let ((kill-buffer-hook nil)
                          (kill-buffer-query-functions nil))
                      (funcall real-kill buffer))))
                (setq nskk--server-process nil)
                (setq nskk--server-pending-cleanups nil)
                (funcall real-retract 'server-state 1)
                (funcall real-assert '((server-state closed))))))))))
  (provide 'nskk-server-test)

;;; nskk-server-test.el ends here
