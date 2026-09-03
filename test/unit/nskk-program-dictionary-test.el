;;; nskk-program-dictionary-test.el --- Tests for nskk-program-dictionary.el -*- lexical-binding: t; -*-

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

;; Tests for nskk-program-dictionary.el.

;;; Code:

(require 'ert)
(require 'nskk-program-dictionary)
(require 'nskk-prolog)
(require 'nskk-test-framework)
(require 'nskk-test-macros)

;;; ─────────────────────────────────────────────────────────────────────────
;;; Test helpers
;;; ─────────────────────────────────────────────────────────────────────────

(defmacro nskk--prog-dict-test-with-env (enable dicts &rest body)
  "Execute BODY with isolated program dictionary configuration and cache state."
  (declare (indent 2))
  `(let ((nskk-program-dict-enable ,enable)
         (nskk-program-dicts ,dicts)
         (nskk--program-dict-cache nil)
         (nskk--program-dict-config-snapshot
          nskk--program-dict-no-config-snapshot))
     ,@body))


;;; ─────────────────────────────────────────────────────────────────────────
;;; Prolog fact tables
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-describe "program-dict-entry-type/2 Prolog facts"
  (nskk-it "maps function to call-function"
    (should (nskk-prolog-holds-p '(program-dict-entry-type function call-function))))

  (nskk-it "maps command to call-command"
    (should (nskk-prolog-holds-p '(program-dict-entry-type command call-command))))

  (nskk-it "does not accept unknown kinds"
    (nskk-deftest-table entry-type-unknown
      :columns (kind)
      :rows    ((unknown) (elisp) (shell) (nil-kind))
      :body    (should (null (nskk-prolog-holds-p
                              `(program-dict-entry-type ,kind \?_))))))

  (nskk-it "resolves function handler via nskk-prolog-query-value"
    (should (eq (nskk-prolog-query-value
                 '(program-dict-entry-type function \?a) '\?a)
                'call-function)))

  (nskk-it "resolves command handler via nskk-prolog-query-value"
    (should (eq (nskk-prolog-query-value
                 '(program-dict-entry-type command \?a) '\?a)
                'call-command))))

(nskk-describe "program-dict-output-prefix/3 Prolog facts"
  (nskk-it "maps / to skk format with / delimiter"
    (should (nskk-prolog-holds-p '(program-dict-output-prefix "/" skk "/"))))

  (nskk-it "maps 1 to skkserv format with / delimiter"
    (should (nskk-prolog-holds-p '(program-dict-output-prefix "1" skkserv "/"))))

  (nskk-it "does not match unknown prefix characters"
    (nskk-deftest-table prefix-unknown
      :columns (ch)
      :rows    (("4") ("0") ("a") ("あ") (""))
      :body    (should (null (nskk-prolog-holds-p
                              `(program-dict-output-prefix ,ch \?_ \?_))))))

  (nskk-it "resolves delimiter for / via nskk-prolog-query-value"
    (should (equal (nskk-prolog-query-value
                    '(program-dict-output-prefix "/" \?_ \?d) '\?d)
                   "/")))

  (nskk-it "resolves delimiter for 1 via nskk-prolog-query-value"
    (should (equal (nskk-prolog-query-value
                    '(program-dict-output-prefix "1" \?_ \?d) '\?d)
                   "/")))

  (nskk-it "returns nil for unknown prefix via nskk-prolog-query-value"
    (should (null (nskk-prolog-query-value
                   '(program-dict-output-prefix "4" \?_ \?d) '\?d)))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; nskk--program-dict-strip-annotation
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-describe "nskk--program-dict-strip-annotation"
  (nskk-context "annotated candidates"
    (nskk-it "strips annotation from word;note pairs"
      (nskk-deftest-table strip-annotation-cases
        :columns (input expected)
        :rows    (("漢字;注釈"       "漢字")
                  ("感じ;note"       "感じ")
                  ("幹事;long note"  "幹事")
                  ("abc;xyz"         "abc"))
        :body    (should (equal (nskk--program-dict-strip-annotation input)
                                expected))))

    (nskk-it "strips only up to the first semicolon when multiple exist"
      (should (equal (nskk--program-dict-strip-annotation "a;b;c") "a"))))

  (nskk-context "plain candidates without annotation"
    (nskk-it "returns the string unchanged when no semicolon is present"
      (nskk-deftest-table strip-annotation-plain
        :columns (input)
        :rows    (("漢字") ("感じ") ("幹事") ("") ("abc"))
        :body    (should (equal (nskk--program-dict-strip-annotation input)
                                input))))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; nskk--program-dict-build-call
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-describe "nskk--program-dict-build-call"
  (nskk-context "%s placeholder replacement"
    (nskk-it "replaces %s with the key as a separate argument"
      (let ((result (nskk--program-dict-build-call "my-dict %s" "かんじ")))
        (should (equal (car result)  "my-dict"))
        (should (null  (cadr result)))          ; stdin-p = nil
        (should (equal (cddr result) '("かんじ")))))

    (nskk-it "replaces %s when it appears after fixed arguments"
      (let ((result (nskk--program-dict-build-call "dict --exact %s" "てすと")))
        (should (equal (car result)  "dict"))
        (should (null  (cadr result)))
        (should (equal (cddr result) '("--exact" "てすと")))))

    (nskk-it "replaces only the exact %s token, not substrings"
      (let ((result (nskk--program-dict-build-call "cmd %s-flag" "key")))
        ;; "%s-flag" is not the exact token "%s", so stdin mode is used
        (should (eq (cadr result) t))))

    (nskk-it "key with special characters is passed as-is without shell escaping"
      (let ((result (nskk--program-dict-build-call "dict %s" "a b;c")))
        (should (member "a b;c" (cddr result))))))

  (nskk-context "stdin mode (no %s)"
    (nskk-it "sets stdin-p to t when no %s token is present"
      (let ((result (nskk--program-dict-build-call "my-stdin-dict" "かんじ")))
        (should (equal (car result)  "my-stdin-dict"))
        (should (eq    (cadr result) t))
        (should (null  (cddr result)))))

    (nskk-it "includes fixed args after program name in stdin mode"
      (let ((result (nskk--program-dict-build-call "dict --server localhost" "key")))
        (should (equal (car result)  "dict"))
        (should (eq    (cadr result) t))
        (should (equal (cddr result) '("--server" "localhost"))))))

  (nskk-context "return structure"
    (nskk-it "always returns a cons cell"
      (should (consp (nskk--program-dict-build-call "prog %s" "k")))
      (should (consp (nskk--program-dict-build-call "prog" "k"))))

    (nskk-it "program name is always a string"
      (nskk-deftest-table build-call-program-name
        :columns (cmd)
        :rows    (("prog %s") ("prog") ("my-tool --flag %s"))
        :body
        (should (stringp (car (nskk--program-dict-build-call cmd "key"))))))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; nskk--program-dict-parse-output: on-found cases
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-describe "nskk--program-dict-parse-output on-found"
  (nskk-context "SKK format (/ prefix)"
    (nskk-it "parses one or more candidates"
      (nskk-deftest-table parse-skk-counts
        :columns (input expected)
        :rows    (("/漢字/"           '("漢字"))
                  ("/漢字/感じ/"      '("漢字" "感じ"))
                  ("/漢字/感じ/幹事/" '("漢字" "感じ" "幹事")))
        :body    (should (equal (nskk--program-dict-parse-output input) expected))))

    (nskk-it "strips trailing newline before parsing"
      (should (equal (nskk--program-dict-parse-output "/漢字/\n")
                     '("漢字"))))

    (nskk-it "strips CRLF endings correctly"
      (should (equal (nskk--program-dict-parse-output "/漢字/\r\n")
                     '("漢字"))))

    (nskk-it "trims whitespace around each candidate"
      (should (equal (nskk--program-dict-parse-output "/ 漢字 /感じ/")
                     '("漢字" "感じ")))))

  (nskk-context "skkserv format (1 prefix)"
    (nskk-it "parses one or more candidates"
      (nskk-deftest-table parse-skkserv-counts
        :columns (input expected)
        :rows    (("1/漢字/"      '("漢字"))
                  ("1/漢字/感じ/" '("漢字" "感じ")))
        :body    (should (equal (nskk--program-dict-parse-output input) expected))))

    (nskk-it "strips trailing newline"
      (should (equal (nskk--program-dict-parse-output "1/漢字/\n")
                     '("漢字")))))

  (nskk-context "one-per-line fallback (unknown prefix)"
    (nskk-it "splits on newlines for unrecognized first character"
      (nskk-deftest-table parse-linefeed-counts
        :columns (input expected)
        :rows    (("漢字\n感じ\n幹事" '("漢字" "感じ" "幹事"))
                  ("漢字"             '("漢字")))
        :body    (should (equal (nskk--program-dict-parse-output input) expected)))))

  (nskk-context "annotation stripping"
    (nskk-it "strips annotations in SKK format"
      (nskk-deftest-table parse-annotation-skk
        :columns (input expected)
        :rows    (("/漢字;注釈/"              '("漢字"))
                  ("/漢字;n1/感じ;n2/"        '("漢字" "感じ"))
                  ("/漢字;注/感じ/幹事;別/"   '("漢字" "感じ" "幹事")))
        :body    (should (equal (nskk--program-dict-parse-output input) expected))))

    (nskk-it "strips annotations in one-per-line format"
      (should (equal (nskk--program-dict-parse-output "漢字;注釈\n感じ")
                     '("漢字" "感じ"))))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; nskk--program-dict-parse-output: on-not-found cases
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-describe "nskk--program-dict-parse-output on-not-found"
  (nskk-it "returns nil for nil, non-string, and empty string inputs"
    (nskk-deftest-table parse-nil-inputs
      :columns (input)
      :rows ((nil) ("") (42) (("list")))
      :body (should (null (nskk--program-dict-parse-output input)))))

  (nskk-it "returns nil for whitespace-only output"
    (nskk-deftest-table parse-whitespace
      :columns (input)
      :rows (("   ") ("\n") ("\r\n") ("\t"))
      :body (should (null (nskk--program-dict-parse-output input)))))

  (nskk-it "returns nil for SKK with empty candidate body"
    (should (null (nskk--program-dict-parse-output "//"))))

  (nskk-it "rejects candidates containing NUL or control characters"
    (dolist (control (list 0 1 9 10 13 31 127 128 159))
      (should
       (null
        (nskk--program-dict-parse-output
         (concat "/bad" (string control) "value/"))))))

  (nskk-it "keeps safe candidates while dropping controlled candidates"
    (should
     (equal
      (nskk--program-dict-parse-output
       (concat "/safe/bad" (string 0) "value/also-safe/"))
      (quote ("safe" "also-safe")))))

  (nskk-it "rejects control characters hidden in annotations"
    (should
     (null
      (nskk--program-dict-parse-output
       (concat "/candidate;bad" (string 0) "annotation/"))))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; nskk--program-dict-parse-output: CPS invariants
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-describe "nskk--program-dict-parse-output CPS contract"
  (nskk-it "calls exactly on-found (not on-not-found) for valid output"
    (let ((found-count 0) (not-found-count 0))
      (nskk--program-dict-parse-output/k "/漢字/"
        (lambda (_v) (cl-incf found-count))
        (lambda ()   (cl-incf not-found-count)))
      (should (= found-count 1))
      (should (= not-found-count 0))))

  (nskk-it "calls exactly on-not-found (not on-found) for nil input"
    (let ((found-count 0) (not-found-count 0))
      (nskk--program-dict-parse-output/k nil
        (lambda (_v) (cl-incf found-count))
        (lambda ()   (cl-incf not-found-count)))
      (should (= found-count 0))
      (should (= not-found-count 1))))

  (nskk-it "passes a proper list of strings to on-found"
    (nskk--program-dict-parse-output/k "/漢字/感じ/幹事/"
      (lambda (cands)
        (should (listp cands))
        (should (cl-every #'stringp cands))
        (should (= (length cands) 3)))
      #'ignore)))

;;; ─────────────────────────────────────────────────────────────────────────
;;; nskk--program-dict-exec-command
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-describe "nskk--program-dict-exec-command"
  (nskk-it "passes argv without shell interpolation"
    (let (found)
      (nskk--program-dict-exec-command/k
        (or (executable-find "sh") shell-file-name) nil
        (list "-c" "printf %s \"$1\"" "nskk-test"
              "argv value;$(ignored)")
        (lambda (value) (setq found value))
        (function ignore))
      (should (equal found "argv value;$(ignored)"))))

  (nskk-it "sends the key on stdin followed by newline and EOF"
    (let (found)
      (nskk--program-dict-exec-command/k
        (or (executable-find "sh") shell-file-name) "stdin value" (list "-c" "cat")
        (lambda (value) (setq found value))
        (function ignore))
      (should (equal found "stdin value\n"))))

  (nskk-it "does not mix standard error into successful output"
    (let (found)
      (nskk--program-dict-exec-command/k
        (or (executable-find "sh") shell-file-name) nil
        (list "-c" "printf stdout; printf stderr >&2; exit 0")
        (lambda (value) (setq found value))
        (function ignore))
      (should (equal found "stdout"))))

  (nskk-it "discards large standard error without affecting stdout"
    (let (found)
      (nskk--program-dict-exec-command/k
        (or (executable-find "sh") shell-file-name) nil
        (list "-c"
              "dd if=/dev/zero bs=1048576 count=2 2>/dev/null >&2; printf stdout")
        (lambda (value) (setq found value))
        (function ignore))
      (should (equal found "stdout"))))

  (nskk-it "treats a nonzero exit as a miss"
    (let ((found-called nil) (miss-called nil))
      (nskk--program-dict-exec-command/k
        (or (executable-find "sh") shell-file-name) nil (list "-c" "printf ignored; exit 7")
        (lambda (_value) (setq found-called t))
        (lambda () (setq miss-called t)))
      (should-not found-called)
      (should miss-called)))

  (nskk-context
  "process tree termination"
  (nskk-it
    "terminates a command at the configured deadline"
    (let ((nskk-program-dict-timeout 0.05)
          (start (float-time))
          (miss-called nil))
      (nskk--program-dict-exec-command/k
        (or (executable-find "sh") shell-file-name)
        nil
        (list "-c" "while :; do :; done")
        (function ignore)
        (lambda ()
          (setq miss-called t)))
      (should miss-called)
      (should (< (- (float-time) start) 1.0))))
  (nskk-it
    "signals a process group only after validating group leadership"
    (let (signals
          deleted)
      (nskk-with-mocks
        ((processp
            (lambda (_)
              t))
          (process-id
            (lambda (_)
              42))
          (process-attributes
            (lambda (_)
              (list (cons (quote pgrp) 42))))
          (process-live-p
            (lambda (_)
              nil))
          (signal-process
            (lambda (pid signal)
              (push (list pid signal) signals)))
          (delete-process
            (lambda (process)
              (setq deleted process))))
        (nskk--program-dict-stop-process-group (quote mock-proc))
        (should (equal signals (list (list -42 (quote SIGKILL)))))
        (should-not deleted))))
  (nskk-it
    "falls back to deleting only the direct process when group leadership is unknown"
    (let (signals
          deleted)
      (nskk-with-mocks
        ((processp
            (lambda (_)
              t))
          (process-id
            (lambda (_)
              42))
          (process-attributes
            (lambda (_)
              (list (cons (quote pgrp) 7))))
          (process-live-p
            (lambda (_)
              t))
          (signal-process
            (lambda (pid signal)
              (push (list pid signal) signals)))
          (delete-process
            (lambda (process)
              (setq deleted process))))
        (nskk--program-dict-stop-process-group (quote mock-proc))
        (should-not signals)
        (should (eq deleted (quote mock-proc))))))
  (nskk-it
   "terminates descendants when a command times out"
   (let ((pid-file (make-temp-file "nskk-program-dict-child-"))
         (nskk-program-dict-timeout 0.2)
         child-pid)
     (unwind-protect (progn (nskk--program-dict-exec-command/k
			     (or (executable-find "sh") shell-file-name)
			     nil
			     (list
			      "-c"
			      (format "sleep 30 & echo $! > %s; wait" (shell-quote-argument pid-file)))
			     (function ignore)
			     (function ignore)) (with-temp-buffer
			     (insert-file-contents pid-file)
			     (setq child-pid (string-to-number (buffer-string)))) (should (> child-pid 0)) (let ((deadline (+ (float-time) 1)))
			     (while
				 (and (process-attributes child-pid) (< (float-time) deadline))
			       (sleep-for 0.01))) (should-not (process-attributes child-pid)))
       (when (and child-pid (process-attributes child-pid))
         (ignore-errors (signal-process child-pid (quote SIGKILL))))
       (delete-file pid-file)))))

  (nskk-it "times out a command producing unbounded standard error"
	   (let ((nskk-program-dict-timeout 0.05)
		 (start (float-time))
		 (miss-called nil))
	     (nskk--program-dict-exec-command/k
        (or (executable-find "sh") shell-file-name) nil
        (list "-c" "while :; do printf 1234567890 >&2; done")
        (function ignore)
        (lambda () (setq miss-called t)))
      (should miss-called)
      (should (< (- (float-time) start) 1.0))))

  (nskk-it "accepts output at the byte limit"
    (let ((nskk--program-dict-max-output-size 32) found)
      (nskk--program-dict-exec-command/k
        (or (executable-find "sh") shell-file-name) nil
        (list "-c" "printf %s 12345678901234567890123456789012")
        (lambda (value) (setq found value))
        (function ignore))
      (should (= (string-bytes found) 32))))

  (nskk-it "rejects output above the byte limit"
    (let ((nskk--program-dict-max-output-size 32)
          (found-called nil)
          (miss-called nil))
      (nskk--program-dict-exec-command/k
        (or (executable-find "sh") shell-file-name) nil
        (list "-c" "printf %s 123456789012345678901234567890123")
        (lambda (_value) (setq found-called t))
        (lambda () (setq miss-called t)))
      (should-not found-called)
      (should miss-called)))

  (nskk-it "preserves NUL for parser rejection rather than truncating stdout"
  (let (found)
    (nskk--program-dict-exec-command/k
      (or (executable-find "sh") shell-file-name) nil
      (list "-c" "printf /bad\\\\000value/")
      (lambda (value) (setq found value))
      (function ignore))
    (should (string-search (string 0) found))
    (should-not (nskk--program-dict-parse-output found))))

  (nskk-it "does not leak processes or output buffers after stderr timeout"
    (let ((nskk-program-dict-timeout 0.05))
      (nskk--program-dict-exec-command/k
        (or (executable-find "sh") shell-file-name) nil
        (list "-c" "while :; do printf 1234567890 >&2; done")
        (function ignore) (function ignore))
      (should-not
       (cl-find-if
        (lambda (process)
          (string-prefix-p "nskk-program-dict" (process-name process)))
        (process-list)))
      (should-not
       (cl-find-if
        (lambda (buffer)
          (string-prefix-p " *nskk-program-dict-" (buffer-name buffer)))
        (buffer-list)))))

  (progn
  (nskk-it "calls exactly one continuation"
    (let ((count 0))
      (nskk--program-dict-exec-command/k
        (or (executable-find "sh") shell-file-name) nil (list "-c" "printf ok")
        (lambda (_value) (cl-incf count))
        (lambda () (cl-incf count)))
      (should (= count 1))))

  (nskk-it "disables polling and cleans up for invalid wait budgets"
    (let ((invalid-timeouts
           (list "invalid"
                 0
                 -0.5
                 (read "0.0e+NaN")
                 (read "1.0e+INF")
                 (read "-1.0e+INF")))
          wait-calls
          deleted
          (miss-count 0))
      (dolist (timeout invalid-timeouts)
        (let ((nskk-program-dict-timeout timeout))
          (cl-letf (((symbol-function 'make-pipe-process)
                     (lambda (&rest _args) 'owned-stderr))
                    ((symbol-function 'make-process)
                     (lambda (&rest _args) 'owned-stdout))
                    ((symbol-function 'process-status)
                     (lambda (_process) 'run))
                    ((symbol-function 'processp)
                     (lambda (process)
                       (memq process '(owned-stdout owned-stderr))))
                    ((symbol-function 'process-id)
                     (lambda (_process) nil))
                    ((symbol-function 'process-live-p)
                     (lambda (_process) t))
                    ((symbol-function 'process-send-eof)
                     (lambda (_process) nil))
                    ((symbol-function 'delete-process)
                     (lambda (process) (push process deleted)))
                    ((symbol-function 'accept-process-output)
                     (lambda (&rest args) (push args wait-calls))))
            (nskk--program-dict-exec-command/k
              "unused"
              nil
              nil
              (lambda (_value)
                (ert-fail "invalid timeout unexpectedly succeeded"))
              (lambda () (cl-incf miss-count))))))
      (should-not wait-calls)
      (should (= miss-count (length invalid-timeouts)))
      (should
       (= (cl-count 'owned-stdout deleted) (length invalid-timeouts)))
      (should
       (= (cl-count 'owned-stderr deleted) (length invalid-timeouts)))
      (should-not (get-buffer " *nskk-program-dict-output*"))
      (should-not (get-buffer " *nskk-program-dict-stderr*"))))

  (nskk-it "releases a partially allocated output buffer"
    (let ((real-generate (symbol-function (quote generate-new-buffer)))
          (allocations 0)
          created
          result)
      (cl-letf (((symbol-function (quote generate-new-buffer))
                 (lambda (name)
                   (setq allocations (1+ allocations))
                   (if (= allocations 2)
                       (error "simulated allocation failure")
                     (setq created (funcall real-generate name))))))
        (nskk--program-dict-exec-command/k
          "unused" nil nil
          (lambda (_value) (setq result :found))
          (lambda () (setq result :not-found))))
      (should (= allocations 2))
      (should (eq result :not-found))
      (should-not (buffer-live-p created))))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; nskk--program-dict-call-function
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-describe "nskk--program-dict-call-function"
  (nskk-context "on-found branch"
    (nskk-it "calls on-found for a proper list of safe non-empty strings"
      (nskk-deftest-table call-function-found
        :columns (return-val)
        :rows    (((quote ("candidate")))
                  ((quote ("first" "second")))
                  ((quote ("a" "b" "c"))))
        :body
        (let (found)
          (nskk--program-dict-call-function/k
            (lambda (_key) return-val) "key"
            (lambda (value) (setq found value))
            (function ignore))
          (should (equal found return-val)))))

    (nskk-it "passes the key to the function"
      (let (received-key)
        (nskk--program-dict-call-function/k
          (lambda (key)
            (setq received-key key)
            (list "result"))
          "reading" (function ignore) (function ignore))
        (should (equal received-key "reading")))))

  (nskk-context "on-not-found branch"
    (nskk-it "rejects malformed or unsafe function results"
      (nskk-deftest-table call-function-not-found
        :columns (return-val)
        :rows    ((nil)
                  ("string")
                  (42)
                  (t)
                  (:keyword)
                  ((quote ("")))
                  ((quote ("ok" 1)))
                  ((quote ("ok" . "tail")))
                  ((list "bad\nvalue"))
                  ((list "bad\0value"))
                  ((list "bad\tvalue"))
                  ((list (concat "bad" (string 127) "value")))
                  ((list (concat "bad" (string 128) "value"))))
        :body
        (let ((found-called nil)
              (miss-called nil))
          (nskk--program-dict-call-function/k
            (lambda (_key) return-val) "key"
            (lambda (_value) (setq found-called t))
            (lambda () (setq miss-called t)))
          (should-not found-called)
          (should miss-called))))

    (nskk-it "rejects a circular candidate list"
      (let ((result (list "ok"))
            (miss-called nil))
        (setcdr result result)
        (nskk--program-dict-call-function/k
          (lambda (_key) result) "key"
          (function ignore)
          (lambda () (setq miss-called t)))
        (should miss-called)))

    (nskk-it "calls on-not-found when the function signals an error"
      (let ((miss-called nil))
        (nskk--program-dict-call-function/k
          (lambda (_key) (error "boom")) "key"
          (function ignore)
          (lambda () (setq miss-called t)))
        (should miss-called))))

  (nskk-context "CPS invariant"
    (nskk-it "calls exactly one continuation"
      (let ((count 0))
        (nskk--program-dict-call-function/k
          (lambda (_key) (list "x")) "key"
          (lambda (_value) (cl-incf count))
          (lambda () (cl-incf count)))
        (should (= count 1))))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; nskk--program-dict-call-command
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-describe "nskk--program-dict-call-command"
  (nskk-context "on-found branch"
    (nskk-it "calls on-found with candidates from SKK output"
      (let ((found-arg nil))
        (nskk-with-mocks
            ((nskk--program-dict-exec-command/k
              (lambda (_prog _stdin _args on-f _nf) (funcall on-f "/漢字/感じ/"))))
          (nskk--program-dict-call-command/k "prog %s" "かんじ"
            (lambda (v) (setq found-arg v))
            #'ignore)
          (should (equal found-arg '("漢字" "感じ"))))))

    (nskk-it "calls on-found with candidates from skkserv output"
      (let ((found-arg nil))
        (nskk-with-mocks
            ((nskk--program-dict-exec-command/k
              (lambda (_prog _stdin _args on-f _nf) (funcall on-f "1/漢字/"))))
          (nskk--program-dict-call-command/k "prog %s" "key"
            (lambda (v) (setq found-arg v))
            #'ignore)
          (should (equal found-arg '("漢字"))))))

    (nskk-it "calls on-found with candidates from line-delimited output"
      (let ((found-arg nil))
        (nskk-with-mocks
            ((nskk--program-dict-exec-command/k
              (lambda (_prog _stdin _args on-f _nf) (funcall on-f "漢字\n感じ"))))
          (nskk--program-dict-call-command/k "prog" "key"
            (lambda (v) (setq found-arg v))
            #'ignore)
          (should (equal found-arg '("漢字" "感じ")))))))

  (nskk-context "on-not-found branch"
    (nskk-it "calls on-not-found when exec-command fails"
      (let ((not-found-called nil))
        (nskk-with-mocks
            ((nskk--program-dict-exec-command/k
              (lambda (_prog _stdin _args _on-f nf) (funcall nf))))
          (nskk--program-dict-call-command/k "prog %s" "key"
            #'ignore
            (lambda () (setq not-found-called t)))
          (should not-found-called))))

    (nskk-it "calls on-not-found when output has no parseable candidates"
      (let ((not-found-called nil))
        (nskk-with-mocks
            ((nskk--program-dict-exec-command/k
              (lambda (_prog _stdin _args on-f _nf) (funcall on-f ""))))
          (nskk--program-dict-call-command/k "prog %s" "key"
            #'ignore
            (lambda () (setq not-found-called t)))
          (should not-found-called)))))

  (nskk-context "argument routing"
    (nskk-it "passes nil stdin-key when %s is present (argv mode)"
      (let ((received-stdin :unset))
        (nskk-with-mocks
            ((nskk--program-dict-exec-command/k
              (lambda (_prog stdin-key _args on-f _nf)
                (setq received-stdin stdin-key)
                (funcall on-f "/x/"))))
          (nskk--program-dict-call-command/k "prog %s" "かんじ"
            #'ignore #'ignore)
          (should (null received-stdin)))))    ; %s mode: no stdin

    (nskk-it "passes the key as stdin-key when no %s is present (stdin mode)"
      (let ((received-stdin nil))
        (nskk-with-mocks
            ((nskk--program-dict-exec-command/k
              (lambda (_prog stdin-key _args on-f _nf)
                (setq received-stdin stdin-key)
                (funcall on-f "/x/"))))
          (nskk--program-dict-call-command/k "prog" "かんじ"
            #'ignore #'ignore)
          (should (equal received-stdin "かんじ")))))))  ; stdin mode

;;; ─────────────────────────────────────────────────────────────────────────
;;; nskk--program-dict-invoke-entry (Prolog dispatch)
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-describe "nskk--program-dict-invoke-entry"
  (nskk-context "function entries"
    (nskk-it "dispatches to call-function for a lambda"
      (let ((fn-called nil))
        (nskk-with-mocks
            ((nskk--program-dict-call-function/k
              (lambda (fn key on-f _nf)
                (setq fn-called fn)
                (funcall on-f (funcall fn key)))))
          (let* ((my-fn (lambda (_k) '("漢字")))
                 (result (nskk--program-dict-invoke-entry my-fn "key")))
            (should (eq fn-called my-fn))
            (should (equal result '("漢字")))))))

    (nskk-it "calls on-found with the function's results"
      (let ((found-arg nil))
        (nskk--program-dict-invoke-entry/k (lambda (_k) '("漢字" "感じ")) "かんじ"
          (lambda (v) (setq found-arg v))
          #'ignore)
        (should (equal found-arg '("漢字" "感じ")))))

    (nskk-it "calls on-not-found when function returns nil"
      (let ((not-found-called nil))
        (nskk--program-dict-invoke-entry/k (lambda (_k) nil) "key"
          #'ignore
          (lambda () (setq not-found-called t)))
        (should not-found-called))))

  (nskk-context "command entries (strings)"
    (nskk-it "dispatches to call-command for a string entry"
      (let ((cmd-received nil))
        (nskk-with-mocks
            ((nskk--program-dict-call-command/k
              (lambda (cmd _key on-f _nf)
                (setq cmd-received cmd)
                (funcall on-f '("候補")))))
          (nskk--program-dict-invoke-entry "my-cmd %s" "かんじ")
          (should (equal cmd-received "my-cmd %s")))))

    (nskk-it "calls on-found for a command that returns candidates"
      (let ((found-arg nil))
        (nskk-with-mocks
            ((nskk--program-dict-exec-command/k
              (lambda (_p _s _a on-f _nf) (funcall on-f "/漢字/"))))
          (nskk--program-dict-invoke-entry/k "my-cmd %s" "かんじ"
            (lambda (v) (setq found-arg v))
            #'ignore)
          (should (equal found-arg '("漢字")))))))

  (nskk-context "CPS invariant"
    (nskk-it "calls exactly one continuation"
      (let ((count 0))
        (nskk--program-dict-invoke-entry/k (lambda (_k) '("x")) "key"
          (lambda (_v) (cl-incf count))
          (lambda ()   (cl-incf count)))
        (should (= count 1))))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; nskk--program-dict-collect-all
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-describe "nskk--program-dict-collect-all"
  (nskk-context "single entry"
    (nskk-it "calls on-found when the single entry succeeds"
      (let ((found-arg nil))
        (nskk--program-dict-collect-all/k
          (list (lambda (_key) '("漢字"))) "かんじ"
          (lambda (value) (setq found-arg value))
          #'ignore)
        (should (equal found-arg '("漢字")))))

    (nskk-it "calls on-not-found when the single entry misses"
      (let ((not-found-called nil))
        (nskk--program-dict-collect-all/k
          (list (lambda (_key) nil)) "key"
          #'ignore
          (lambda () (setq not-found-called t)))
        (should not-found-called))))

  (nskk-context "multiple entries"
    (nskk-it "merges results in entry order"
      (let ((found-arg nil))
        (nskk--program-dict-collect-all/k
          (list
            (lambda (_key) '("漢字"))
            (lambda (_key) '("感じ")))
          "かんじ"
          (lambda (value) (setq found-arg value))
          #'ignore)
        (should (equal found-arg '("漢字" "感じ")))))

    (nskk-it "skips entries that return nil and collects from the rest"
      (let ((found-arg nil))
        (nskk--program-dict-collect-all/k
          (list
            (lambda (_key) nil)
            (lambda (_key) '("幹事"))
            (lambda (_key) nil))
          "key"
          (lambda (value) (setq found-arg value))
          #'ignore)
        (should (equal found-arg '("幹事")))))

    (nskk-it "calls on-not-found when all entries miss"
      (let ((not-found-called nil))
        (nskk--program-dict-collect-all/k
          (list
            (lambda (_key) nil)
            (lambda (_key) nil))
          "key"
          #'ignore
          (lambda () (setq not-found-called t)))
        (should not-found-called))))

  (nskk-context "stable deduplication"
    (nskk-it "preserves order, first equal candidate, and input lists"
      (let* ((first-shared
              (propertize (copy-sequence "shared") 'origin 'first))
             (second-shared
              (propertize (copy-sequence "shared") 'origin 'second))
             (first-candidates (list "result1" first-shared "middle"))
             (second-candidates (list second-shared "result2"))
             (first-before (mapcar #'copy-sequence first-candidates))
             (second-before (mapcar #'copy-sequence second-candidates))
             (first-tail (cdr first-candidates))
             (second-tail (cdr second-candidates))
             (result
              (nskk--program-dict-collect-all
                (list
                  (lambda (_key) first-candidates)
                  (lambda (_key) second-candidates))
                "key")))
        (should
          (equal
            (mapcar #'substring-no-properties result)
            '("result1" "shared" "middle" "result2")))
        (should (eq (nth 1 result) first-shared))
        (should (eq (get-text-property 0 'origin (nth 1 result)) 'first))
        (should (equal-including-properties first-candidates first-before))
        (should (equal-including-properties second-candidates second-before))
        (should (eq (cdr first-candidates) first-tail))
        (should (eq (cdr second-candidates) second-tail)))))

  (nskk-context "empty entries list"
    (nskk-it "calls on-not-found for an empty entries list"
      (let ((not-found-called nil))
        (nskk--program-dict-collect-all/k
          nil "key"
          #'ignore
          (lambda () (setq not-found-called t)))
        (should not-found-called)))))

(nskk-describe "nskk--program-dict-collect-all scaling"
  (nskk-context "large-input scaling"
    (nskk-it "performs one hash lookup per candidate"
      (let* ((candidate-count 5000)
             (shared-candidates (list "shared"))
             (entries (make-list candidate-count shared-candidates))
             (real-gethash (symbol-function 'gethash))
             (real-puthash (symbol-function 'puthash))
             (real-merge
              (symbol-function 'nskk--program-dict-merge-candidate-lists))
             (invocations 0)
             (hash-lookups 0)
             (hash-inserts 0)
             (merged-list-count 0)
             result)
        (cl-letf
            (((symbol-function 'nskk--program-dict-invoke-entry)
              (lambda (entry _key)
                (cl-incf invocations)
                entry))
             ((symbol-function 'nskk--program-dict-merge-candidate-lists)
              (lambda (candidate-lists)
                (setq merged-list-count (length candidate-lists))
                (cl-letf
                    (((symbol-function 'gethash)
                      (lambda (key table &optional default)
                        (cl-incf hash-lookups)
                        (funcall real-gethash key table default)))
                     ((symbol-function 'puthash)
                      (lambda (key value table)
                        (cl-incf hash-inserts)
                        (funcall real-puthash key value table))))
                  (funcall real-merge candidate-lists)))))
          (setq result (nskk--program-dict-collect-all entries "key")))
        (should (equal result '("shared")))
        (should (= invocations candidate-count))
        (should (= merged-list-count candidate-count))
        (should (= hash-lookups candidate-count))
        (should (= hash-inserts 1))))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; nskk-program-dict-lookup: enable guard and empty-dict guard
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-describe "nskk-program-dict-lookup enable/dict guard"
  (nskk-it "returns nil when nskk-program-dict-enable is nil"
    (nskk--prog-dict-test-with-env nil (list (lambda (_k) '("x")))
      (should (null (nskk-program-dict-lookup "key")))))

  (nskk-it "returns nil when nskk-program-dicts is nil"
    (nskk--prog-dict-test-with-env t nil
      (should (null (nskk-program-dict-lookup "key")))))

  (nskk-it "returns nil when both enable is nil and dicts is nil"
    (nskk--prog-dict-test-with-env nil nil
      (should (null (nskk-program-dict-lookup "key")))))

  (nskk-it "calls on-not-found (not on-found) when disabled"
    (let ((found-called nil) (not-found-called nil))
      (nskk--prog-dict-test-with-env nil (list (lambda (_k) '("x")))
        (nskk-program-dict-lookup/k "key"
          (lambda (_v) (setq found-called t))
          (lambda ()   (setq not-found-called t))))
      (should (null found-called))
      (should not-found-called))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; nskk-program-dict-lookup: cache behaviour
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-describe "nskk-program-dict-lookup cache"
  (nskk-it "returns cached result on the second call without re-invoking entries"
    (let ((call-count 0))
      (nskk--prog-dict-test-with-env t (list (lambda (_k) (cl-incf call-count) '("漢字")))
        (nskk-program-dict-lookup "かんじ")
        (nskk-program-dict-lookup "かんじ")
        (should (= call-count 1)))))

  (nskk-it "stores and retrieves the correct candidate list from cache"
    (nskk--prog-dict-test-with-env t (list (lambda (_k) '("漢字" "感じ")))
      (nskk-program-dict-lookup "かんじ")
      (should (equal (nskk-program-dict-lookup "かんじ") '("漢字" "感じ")))))

  (nskk-it "caches per-key independently"
    (let ((store '(("かんじ" . ("漢字")) ("てすと" . ("手隅")))))
      (nskk--prog-dict-test-with-env
          t
          (list (lambda (k) (cdr (assoc k store))))
        (should (equal (nskk-program-dict-lookup "かんじ") '("漢字")))
        (should (equal (nskk-program-dict-lookup "てすと") '("手隅")))
        ;; Second calls come from cache
        (should (equal (nskk-program-dict-lookup "かんじ") '("漢字")))
        (should (equal (nskk-program-dict-lookup "てすと") '("手隅"))))))

  (nskk-it "adversarial: miss canonical and every public result own detached cyclic graphs"
  (let* ((metadata (vector nil))
         (first (propertize "first" 'metadata metadata))
         (second (propertize "second" 'metadata metadata))
         (original (list first second)))
    (aset metadata 0 metadata)
    (nskk--prog-dict-test-with-env t (list (lambda (_key) original))
      (cl-letf (((symbol-function 'nskk--program-dict-merge-candidate-lists)
                 (lambda (_candidate-lists) original)))
        (let* ((miss (nskk-program-dict-lookup "graph"))
               (cache nskk--program-dict-cache)
               (node (gethash "graph" (nskk-cache-lru-hash cache)))
               (canonical (nskk-cache-lru-node-value node))
               (hit-one (nskk-program-dict-lookup "graph"))
               (hit-two (nskk-program-dict-lookup "graph"))
               (canonical-metadata
                (get-text-property 0 'metadata (car canonical)))
               (miss-metadata
                (get-text-property 0 'metadata (car miss)))
               (hit-one-metadata
                (get-text-property 0 'metadata (car hit-one)))
               (hit-two-metadata
                (get-text-property 0 'metadata (car hit-two))))
          (dolist (graph (list canonical miss hit-one hit-two))
            (dolist (candidate graph)
              (should (eq (get-text-property 0 'nskk-no-learn candidate) t)))
            (let ((graph-metadata
                   (get-text-property 0 'metadata (car graph))))
              (should (eq graph-metadata
                          (get-text-property 0 'metadata (cadr graph))))
              (should (eq (aref graph-metadata 0) graph-metadata))))
          (dolist (candidate original)
            (should-not (get-text-property 0 'nskk-no-learn candidate)))
          (should-not (eq canonical original))
          (should-not (eq miss original))
          (should-not (eq miss canonical))
          (should-not (eq hit-one canonical))
          (should-not (eq hit-two canonical))
          (should-not (eq hit-one hit-two))
          (should-not (eq (car canonical) first))
          (should-not (eq (car miss) first))
          (should-not (eq (car miss) (car canonical)))
          (should-not (eq (car hit-one) (car canonical)))
          (should-not (eq (car hit-one) (car hit-two)))
          (should-not (eq canonical-metadata metadata))
          (should-not (eq miss-metadata metadata))
          (should-not (eq miss-metadata canonical-metadata))
          (should-not (eq hit-one-metadata canonical-metadata))
          (should-not (eq hit-two-metadata canonical-metadata))
          (should-not (eq hit-one-metadata hit-two-metadata))
          (aset first 0 ?X)
          (aset metadata 0 nil)
          (aset (car miss) 0 ?M)
          (aset miss-metadata 0 nil)
          (aset (car hit-one) 0 ?H)
          (aset hit-one-metadata 0 nil)
          (should (equal (substring-no-properties (car canonical)) "first"))
          (should (eq (aref canonical-metadata 0) canonical-metadata))
          (should (equal (substring-no-properties (car hit-two)) "first"))
          (should (eq (aref hit-two-metadata 0) hit-two-metadata))
          (let* ((hit-three (nskk-program-dict-lookup "graph"))
                 (hit-three-metadata
                  (get-text-property 0 'metadata (car hit-three))))
            (should-not (eq hit-three canonical))
            (should-not (eq hit-three hit-two))
            (dolist (candidate hit-three)
              (should (eq (get-text-property 0 'nskk-no-learn candidate) t)))
            (should (equal (substring-no-properties (car hit-three)) "first"))
            (should (eq hit-three-metadata
                        (get-text-property 0 'metadata (cadr hit-three))))
            (should (eq (aref hit-three-metadata 0) hit-three-metadata))))))))

  (nskk-it "adversarial: setq setcar and setcdr config mutations invalidate by value"
    (let* ((first-count 0)
           (second-count 0)
           (first-function (lambda (_key) (cl-incf first-count) '("first")))
           (second-function (lambda (_key) (cl-incf second-count) '("second"))))
      (nskk--prog-dict-test-with-env t (list first-function)
        (nskk--program-dict-sync-config)
        (let ((cache (nskk--program-dict-ensure-cache)))
          (nskk-cache-put cache "marker" '("cached"))
          (setq nskk-program-dicts (list first-function))
          (nskk--program-dict-sync-config)
          (should (= (nskk-cache-size cache) 1))
          (setq nskk-program-dicts (list second-function))
          (nskk--program-dict-sync-config)
          (should (= (nskk-cache-size cache) 0))
          (nskk-cache-put cache "marker" '("cached"))
          (setcar nskk-program-dicts first-function)
          (nskk--program-dict-sync-config)
          (should (= (nskk-cache-size cache) 0))
          (nskk-cache-put cache "marker" '("cached"))
          (setcdr nskk-program-dicts (list second-function))
          (nskk--program-dict-sync-config)
          (should (= (nskk-cache-size cache) 0))
          (should-not (eq nskk--program-dict-config-snapshot
                          nskk-program-dicts))
          (should (eq (car nskk--program-dict-config-snapshot)
                      first-function))
          (should (eq (cadr nskk--program-dict-config-snapshot)
                      second-function))
          (should (= first-count 0))
          (should (= second-count 0))))))

  (nskk-it "adversarial: in-place command string mutation invalidates a detached snapshot"
    (let ((command (copy-sequence "dictionary %s")))
      (nskk--prog-dict-test-with-env t (list command)
        (nskk--program-dict-sync-config)
        (let ((cache (nskk--program-dict-ensure-cache))
              (snapshot-command
               (car nskk--program-dict-config-snapshot)))
          (nskk-cache-put cache "marker" '("cached"))
          (should-not (eq snapshot-command command))
          (aset command 0 ?D)
          (should (equal snapshot-command "dictionary %s"))
          (nskk--program-dict-sync-config)
          (should (= (nskk-cache-size cache) 0))
          (should-not (eq (car nskk--program-dict-config-snapshot)
                          command))
          (should (equal (car nskk--program-dict-config-snapshot)
                         "Dictionary %s"))))))

  (nskk-it "adversarial: cyclic and dotted configurations fail finitely without invocation"
    (let* ((call-count 0)
           (entry (lambda (_key) (cl-incf call-count) '("unexpected")))
           (cyclic (list entry))
           (dotted (cons entry 'invalid-tail)))
      (setcdr cyclic cyclic)
      (nskk--prog-dict-test-with-env t cyclic
        (should-not (nskk-program-dict-lookup "cyclic"))
        (should-not (nskk-program-dict-lookup "cyclic"))
        (should-not nskk--program-dict-cache))
      (nskk--prog-dict-test-with-env t dotted
        (should-not (nskk-program-dict-lookup "dotted"))
        (should-not (nskk-program-dict-lookup "dotted"))
        (should-not nskk--program-dict-cache))
      (should (= call-count 0))))

  (nskk-it "adversarial: mutable string and cons keys are detached before publication"
    (let ((call-count 0))
      (nskk--prog-dict-test-with-env
          t
          (list (lambda (_key) (cl-incf call-count) '("candidate")))
        (let ((string-key (copy-sequence "mutable")))
          (should (equal (nskk-program-dict-lookup string-key)
                         '("candidate")))
          (aset string-key 0 ?X)
          (should (equal (nskk-program-dict-lookup "mutable")
                         '("candidate")))
          (should (= call-count 1))
          (should (= (nskk-cache-size nskk--program-dict-cache) 1)))
        (let ((cons-key (list (copy-sequence "compound")
                              (vector (copy-sequence "node")))))
          (should (equal (nskk-program-dict-lookup cons-key)
                         '("candidate")))
          (aset (car cons-key) 0 ?X)
          (aset (aref (cadr cons-key) 0) 0 ?X)
          (setcdr cons-key '(mutated-tail))
          (should (equal
                   (nskk-program-dict-lookup
                    (list "compound" (vector "node")))
                   '("candidate")))
          (should (= call-count 2))
          (should (= (nskk-cache-size nskk--program-dict-cache) 2)))))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; nskk-program-dict-lookup: CPS contract
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-describe "nskk-program-dict-lookup CPS contract"
  (nskk-it "calls on-found with the candidate list when lookup succeeds"
    (let ((found-arg nil))
      (nskk--prog-dict-test-with-env t (list (lambda (_k) '("漢字")))
        (nskk-program-dict-lookup/k "かんじ"
          (lambda (v) (setq found-arg v))
          #'ignore))
      (should (equal found-arg '("漢字")))))

  (nskk-it "calls on-not-found when all entries miss"
    (let ((not-found-called nil))
      (nskk--prog-dict-test-with-env t (list (lambda (_k) nil))
        (nskk-program-dict-lookup/k "key"
          #'ignore
          (lambda () (setq not-found-called t))))
      (should not-found-called)))

  (nskk-it "calls exactly one continuation on success"
    (let ((count 0))
      (nskk--prog-dict-test-with-env t (list (lambda (_k) '("x")))
        (nskk-program-dict-lookup/k "key"
          (lambda (_v) (cl-incf count))
          (lambda ()   (cl-incf count))))
      (should (= count 1))))

  (nskk-it "calls exactly one continuation on failure"
    (let ((count 0))
      (nskk--prog-dict-test-with-env t (list (lambda (_k) nil))
        (nskk-program-dict-lookup/k "key"
          (lambda (_v) (cl-incf count))
          (lambda ()   (cl-incf count))))
      (should (= count 1)))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; PBT: output parsing invariants
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-property-test program-dict-parse-output-returns-strings
  ((word kanji-string))
  (let ((result (nskk--program-dict-parse-output (concat "/" word "/"))))
    (or (null result) (cl-every #'stringp result)))
  30)

(nskk-property-test program-dict-parse-output-strips-semicolons
  ((word kanji-string)
   (note romaji-string))
  (let ((result (nskk--program-dict-parse-output
                 (concat "/" word ";" note "/"))))
    (or (null result)
        (not (cl-some (lambda (s) (string-search ";" s)) result))))
  30)

(nskk-property-test program-dict-parse-skkserv-prefix-matches-skk
  ((word kanji-string))
  ;; SKK format (/word/) and skkserv format (1/word/) must yield the same candidates
  (let ((skk-result     (nskk--program-dict-parse-output (concat "/" word "/")))
        (skkserv-result (nskk--program-dict-parse-output (concat "1/" word "/"))))
    (equal skk-result skkserv-result))
  30)

(nskk-property-test program-dict-build-call-program-is-string
  ((cmd romaji-string))
  ;; build-call must always return a string as the program name
  (stringp (car (nskk--program-dict-build-call (concat cmd " %s") "key")))
  30)

(nskk-property-test program-dict-strip-annotation-idempotent
  ((word kanji-string))
  ;; strip-annotation applied twice must equal strip-annotation applied once
  (equal (nskk--program-dict-strip-annotation
          (nskk--program-dict-strip-annotation word))
         (nskk--program-dict-strip-annotation word))
  30)

(nskk-property-test program-dict-strip-annotation-no-semicolon-in-result
  ((word kanji-string)
   (note romaji-string))
  ;; Result of stripping must never contain a semicolon
  (not (string-search ";"
                      (nskk--program-dict-strip-annotation
                       (concat word ";" note))))
  30)

;;; ─────────────────────────────────────────────────────────────────────────
;;; nskk-program-dict-dispatch-table (Section 13)
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-describe "nskk-program-dict-dispatch-table defaults"
  (nskk-it "contains entries for today, now, and ="
    (let ((prefixes (mapcar #'car nskk-program-dict-dispatch-table)))
      (should (member "today" prefixes))
      (should (member "now"   prefixes))
      (should (member "="     prefixes))))

  (nskk-it "all entries are (string . function) cons cells"
    (dolist (entry nskk-program-dict-dispatch-table)
      (should (consp entry))
      (should (stringp (car entry)))
      (should (functionp (cdr entry)))))

  (nskk-it "today entry maps to nskk--program-dict-today"
    (let ((entry (assoc "today" nskk-program-dict-dispatch-table)))
      (should entry)
      (should (eq (cdr entry) #'nskk--program-dict-today))))

  (nskk-it "now entry maps to nskk--program-dict-now"
    (let ((entry (assoc "now" nskk-program-dict-dispatch-table)))
      (should entry)
      (should (eq (cdr entry) #'nskk--program-dict-now))))

  (nskk-it "= entry maps to nskk--program-dict-calculate"
    (let ((entry (assoc "=" nskk-program-dict-dispatch-table)))
      (should entry)
      (should (eq (cdr entry) #'nskk--program-dict-calculate)))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; nskk--program-dict-today (Section 14)
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-describe "nskk--program-dict-today"
  (nskk-context "return structure"
    (nskk-it "returns a list of exactly 2 strings"
      (let ((result (nskk--program-dict-today "today")))
        (should (listp result))
        (should (= (length result) 2))
        (should (cl-every #'stringp result))))

    (nskk-it "ignores the key argument (today prefix triggers it)"
      ;; Even with an extended key, it still returns 2 candidates
      (let ((result (nskk--program-dict-today "today-extra")))
        (should (= (length result) 2)))))

  (nskk-context "format validation"
    (nskk-it "first candidate matches YYYY/MM/DD(WeekAbbrev) pattern"
      (let ((cand1 (car (nskk--program-dict-today "today"))))
        (should (string-match-p "\\`[0-9]\\{4\\}/[0-9]\\{2\\}/[0-9]\\{2\\}(\\(?:Sun\\|Mon\\|Tue\\|Wed\\|Thu\\|Fri\\|Sat\\))\\'" cand1))))

    (nskk-it "second candidate matches YYYY年MM月DD日(WeekKanji) pattern"
      (let ((cand2 (cadr (nskk--program-dict-today "today"))))
        (should (string-match-p "\\`[0-9]\\{4\\}年[0-9]\\{2\\}月[0-9]\\{2\\}日([日月火水木金土])\\'" cand2))))

    (nskk-it "year in first candidate is a 4-digit number > 2000"
      (let ((cand1 (car (nskk--program-dict-today "today"))))
        (string-match "\\`\\([0-9]\\{4\\}\\)/" cand1)
        (should (> (string-to-number (match-string 1 cand1)) 2000))))

    (nskk-it "month in first candidate is in range 01-12"
      (let* ((cand1  (car (nskk--program-dict-today "today")))
             (month  (string-to-number (substring cand1 5 7))))
        (should (<= 1 month 12))))

    (nskk-it "day in first candidate is in range 01-31"
      (let* ((cand1 (car (nskk--program-dict-today "today")))
             (day   (string-to-number (substring cand1 8 10))))
        (should (<= 1 day 31))))

    (nskk-it "both candidates represent the same year/month/day"
      (let* ((result (nskk--program-dict-today "today"))
             (cand1  (car result))
             (cand2  (cadr result)))
        (string-match "\\`\\([0-9]\\{4\\}\\)/\\([0-9]\\{2\\}\\)/\\([0-9]\\{2\\}\\)" cand1)
        (let ((y (match-string 1 cand1))
              (m (match-string 2 cand1))
              (d (match-string 3 cand1)))
          (should (string-match-p (regexp-quote (format "%s年%s月%s日" y m d)) cand2)))))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; nskk--program-dict-now (Section 14)
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-describe "nskk--program-dict-now"
  (nskk-context "return structure"
    (nskk-it "returns a list of exactly 2 strings"
      (let ((result (nskk--program-dict-now "now")))
        (should (listp result))
        (should (= (length result) 2))
        (should (cl-every #'stringp result)))))

  (nskk-context "format validation"
    (nskk-it "first candidate matches HH:MM:SS pattern"
      (let ((cand1 (car (nskk--program-dict-now "now"))))
        (should (string-match-p "\\`[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\'" cand1))))

    (nskk-it "second candidate matches HH時MM分SS秒 pattern"
      (let ((cand2 (cadr (nskk--program-dict-now "now"))))
        (should (string-match-p "\\`[0-9]\\{2\\}時[0-9]\\{2\\}分[0-9]\\{2\\}秒\\'" cand2))))

    (nskk-it "hour in first candidate is in range 00-23"
      (let* ((cand1 (car (nskk--program-dict-now "now")))
             (hour  (string-to-number (substring cand1 0 2))))
        (should (<= 0 hour 23))))

    (nskk-it "minute in first candidate is in range 00-59"
      (let* ((cand1   (car (nskk--program-dict-now "now")))
             (minute  (string-to-number (substring cand1 3 5))))
        (should (<= 0 minute 59))))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; nskk--program-dict-calculate (Section 14)
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-describe "nskk--program-dict-calculate"
  (nskk-context "arithmetic evaluation"
    (nskk-it "evaluates simple addition"
      (let ((result (nskk--program-dict-calculate "=1+2")))
        (should (listp result))
        (should (= (length result) 1))
        (should (equal (car result) "3"))))

    (nskk-it "evaluates multiplication with parentheses"
      (let ((result (nskk--program-dict-calculate "=(3+4)*2")))
        (should (equal (car result) "14"))))

    (nskk-it "evaluates power operator"
      (let ((result (nskk--program-dict-calculate "=2^10")))
        (should (equal (car result) "1024"))))

    (nskk-it "returns a single-element list on success"
      (nskk-deftest-table pd-builtin-calc-single-result
        :columns (expr)
        :rows    (("=1+1") ("=100-50") ("=6*7"))
        :body    (let ((result (nskk--program-dict-calculate expr)))
                   (should (= (length result) 1))
                   (should (stringp (car result)))))))

  (nskk-context "error handling"
    (nskk-it "returns a non-nil list for expressions calc-eval can represent symbolically"
      ;; calc-eval returns a string (symbolic expression) for some non-numeric inputs
      (let ((result (nskk--program-dict-calculate "=not-a-number")))
        ;; Should return a list with a string, not signal an error
        (when result
          (should (listp result))
          (should (stringp (car result))))))

    (nskk-it "does not signal an error for any expression"
      ;; Must not propagate errors to callers regardless of input
      (should-not (condition-case _
                      (progn (nskk--program-dict-calculate "=???") nil)
                    (error t))))

    (nskk-it "returns error message for empty expression (= with no operand)"
      ;; calc-eval \"\" returns (0 . \"Expected a number\") (cons, not string/signal);
      ;; the (consp result) branch extracts (cdr result) as the error message.
      (let ((result (nskk--program-dict-calculate "=")))
        (should (listp result))
        (should (= (length result) 1))
        (should (stringp (car result))))))

  (nskk-context "key parsing"
    (nskk-it "strips the leading = from key before evaluating"
      ;; "=5+5" -> evaluates "5+5" -> "10"
      (let ((result (nskk--program-dict-calculate "=5+5")))
        (should (equal (car result) "10"))))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; nskk-program-dict-builtin-lookup (Section 15)
;;; ─────────────────────────────────────────────────────────────────────────

(defmacro nskk--pd-builtin-test-with-env (enable &rest body)
  "Execute BODY with `nskk-program-dict-enable' bound to ENABLE."
  (declare (indent 1))
  `(let ((nskk-program-dict-enable ,enable))
     ,@body))

(nskk-describe "nskk-program-dict-builtin-lookup enable guard"
  (nskk-it "returns nil (sync) when nskk-program-dict-enable is nil"
    (nskk--pd-builtin-test-with-env nil
      (should (null (nskk-program-dict-builtin-lookup "today")))))

  (nskk-it "calls on-not-found when nskk-program-dict-enable is nil"
    (let ((not-found-called nil))
      (nskk--pd-builtin-test-with-env nil
        (nskk-program-dict-builtin-lookup/k "today"
          #'ignore
          (lambda () (setq not-found-called t))))
      (should not-found-called)))

  (nskk-it "calls on-not-found for an unrecognized key (no prefix match)"
    (let ((not-found-called nil))
      (nskk--pd-builtin-test-with-env t
        (nskk-program-dict-builtin-lookup/k "zzz-no-match"
          #'ignore
          (lambda () (setq not-found-called t))))
      (should not-found-called))))

(nskk-describe "nskk-program-dict-builtin-lookup prefix dispatch"
  (nskk-it "calls on-found for 'today'"
    (let ((found-arg nil))
      (nskk--pd-builtin-test-with-env t
        (nskk-program-dict-builtin-lookup/k "today"
          (lambda (v) (setq found-arg v))
          #'ignore))
      (should (listp found-arg))
      (should found-arg)))

  (nskk-it "calls on-found for 'now'"
    (let ((found-arg nil))
      (nskk--pd-builtin-test-with-env t
        (nskk-program-dict-builtin-lookup/k "now"
          (lambda (v) (setq found-arg v))
          #'ignore))
      (should (listp found-arg))
      (should found-arg)))

  (nskk-it "calls on-found for '=1+1'"
    (let ((found-arg nil))
      (nskk--pd-builtin-test-with-env t
        (nskk-program-dict-builtin-lookup/k "=1+1"
          (lambda (v) (setq found-arg v))
          #'ignore))
      (should (listp found-arg))
      (should found-arg)))

  (nskk-it "returns candidates from today handler"
    (nskk--pd-builtin-test-with-env t
      (let ((result (nskk-program-dict-builtin-lookup "today")))
        (should (= (length result) 2))
        (should (cl-every #'stringp result)))))

  (nskk-it "returns candidates from now handler"
    (nskk--pd-builtin-test-with-env t
      (let ((result (nskk-program-dict-builtin-lookup "now")))
        (should (= (length result) 2))
        (should (cl-every #'stringp result))))))

(nskk-describe "nskk-program-dict-builtin-lookup no-learn property"
  (nskk-it "all candidates have nskk-no-learn property set to t"
    (nskk--pd-builtin-test-with-env t
      (let ((result (nskk-program-dict-builtin-lookup "today")))
        (should result)
        (dolist (cand result)
          (should (eq (get-text-property 0 'nskk-no-learn cand) t))))))

  (nskk-it "no-learn property is set to t (not just truthy) for 'now' candidates"
    ;; Normalize: use (eq ... t) like the today test for consistency.
    (nskk--pd-builtin-test-with-env t
      (let ((result (nskk-program-dict-builtin-lookup "now")))
        (should result)
        (dolist (cand result)
          (should (eq (get-text-property 0 'nskk-no-learn cand) t))))))

  (nskk-it "no-learn property is set to t for '=' calculator candidates"
    (nskk--pd-builtin-test-with-env t
      (let ((result (nskk-program-dict-builtin-lookup "=1+1")))
        (should result)
        (dolist (cand result)
          (should (eq (get-text-property 0 'nskk-no-learn cand) t)))))))

(nskk-describe "nskk-program-dict-builtin-lookup CPS contract"
  (nskk-it "calls exactly one continuation on found"
    (let ((count 0))
      (nskk--pd-builtin-test-with-env t
        (nskk-program-dict-builtin-lookup/k "today"
          (lambda (_v) (cl-incf count))
          (lambda ()   (cl-incf count))))
      (should (= count 1))))

  (nskk-it "calls exactly one continuation on not-found"
    (let ((count 0))
      (nskk--pd-builtin-test-with-env t
        (nskk-program-dict-builtin-lookup/k "zzz-no-match"
          (lambda (_v) (cl-incf count))
          (lambda ()   (cl-incf count))))
      (should (= count 1))))

  (nskk-it "candidates are a proper list of strings"
    (nskk--pd-builtin-test-with-env t
      (nskk-program-dict-builtin-lookup/k "today"
        (lambda (cands)
          (should (listp cands))
          (should (cl-every #'stringp cands)))
        #'ignore)))

  (nskk-it "handler error is caught and treated as miss for that handler"
    (let* ((called nil)
           (broken-handler (lambda (_k) (error "boom")))
           (nskk-program-dict-dispatch-table
            (append (list (cons "boom" broken-handler))
                    nskk-program-dict-dispatch-table)))
      (nskk--pd-builtin-test-with-env t
        (let ((result (nskk-program-dict-builtin-lookup "today")))
          (should (= (length result) 2)))
        (setq called (nskk-program-dict-builtin-lookup "boom"))
        (should (null called)))))

  (nskk-it "rejects malformed handler candidates and keeps valid handlers"
    (let ((circular (list "loop")))
      (setcdr circular circular)
      (dolist (invalid
               (list 42
                     (cons "head" "tail")
                     circular
                     (list 42)
                     (list "")
                     (list (concat "bad" (string 0)))))
        (let ((logged nil)
              (nskk-program-dict-dispatch-table
               (list
                (cons "test" (lambda (_key) invalid))
                (cons "test" (lambda (_key) '("valid"))))))
          (nskk--pd-builtin-test-with-env t
            (nskk-with-mocks
                ((nskk-debug-message
                  (lambda (&rest _) (setq logged t))))
              (let ((result (nskk-program-dict-builtin-lookup "test")))
                (should
                 (equal (mapcar #'substring-no-properties result)
                        '("valid")))
                (should logged)))))))))

(nskk-describe "nskk-program-dict-builtin-lookup deduplication"
  (nskk-it "preserves order, first equal candidate, properties, and inputs"
    (let* ((first-shared
            (propertize (copy-sequence "shared") 'origin 'first))
           (second-shared
            (propertize (copy-sequence "shared") 'origin 'second))
           (first-candidates (list "result1" first-shared "middle"))
           (second-candidates (list second-shared "result2"))
           (first-before (mapcar #'copy-sequence first-candidates))
           (second-before (mapcar #'copy-sequence second-candidates))
           (first-tail (cdr first-candidates))
           (second-tail (cdr second-candidates))
           (nskk-program-dict-dispatch-table
            (list
              (cons "test" (lambda (_key) first-candidates))
              (cons "test" (lambda (_key) second-candidates))))
           result)
      (nskk--pd-builtin-test-with-env t
        (setq result (nskk-program-dict-builtin-lookup "test")))
      (should
        (equal
          (mapcar #'substring-no-properties result)
          '("result1" "shared" "middle" "result2")))
      (should (eq (get-text-property 0 'origin (nth 1 result)) 'first))
      (should
        (cl-every
          (lambda (candidate)
            (get-text-property 0 'nskk-no-learn candidate))
          result))
      (should (equal-including-properties first-candidates first-before))
      (should (equal-including-properties second-candidates second-before))
      (should (eq (cdr first-candidates) first-tail))
      (should (eq (cdr second-candidates) second-tail))
      (should-not (get-text-property 0 'nskk-no-learn first-shared))
      (should-not (get-text-property 0 'nskk-no-learn second-shared))))

  (nskk-it "performs one hash lookup per candidate for large dispatch tables"
    (let* ((handler-count 5000)
           (shared-candidates (list "shared"))
           (handler-calls 0)
           (handler
            (lambda (_key)
              (cl-incf handler-calls)
              shared-candidates))
           (nskk-program-dict-dispatch-table
            (make-list handler-count (cons "test" handler)))
           (real-gethash (symbol-function 'gethash))
           (real-puthash (symbol-function 'puthash))
           (real-merge
            (symbol-function 'nskk--program-dict-merge-candidate-lists))
           (hash-lookups 0)
           (hash-inserts 0)
           (merged-list-count 0)
           result)
      (cl-letf
          (((symbol-function 'nskk--program-dict-merge-candidate-lists)
            (lambda (candidate-lists)
              (setq merged-list-count (length candidate-lists))
              (cl-letf
                  (((symbol-function 'gethash)
                    (lambda (key table &optional default)
                      (cl-incf hash-lookups)
                      (funcall real-gethash key table default)))
                   ((symbol-function 'puthash)
                    (lambda (key value table)
                      (cl-incf hash-inserts)
                      (funcall real-puthash key value table))))
                (funcall real-merge candidate-lists)))))
        (nskk--pd-builtin-test-with-env t
          (setq result (nskk-program-dict-builtin-lookup "test"))))
      (should
        (equal (mapcar #'substring-no-properties result) '("shared")))
      (should (= handler-calls handler-count))
      (should (= merged-list-count handler-count))
      (should (= hash-lookups handler-count))
      (should (= hash-inserts 1)))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; nskk--program-dict-now: second range (S-1)
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-describe "nskk--program-dict-now second range"
  (nskk-it "second in first candidate is in range 00-59"
    (let* ((cand1  (car (nskk--program-dict-now "now")))
           (second (string-to-number (substring cand1 6 8))))
      (should (<= 0 second 59))))

  (nskk-it "both candidates contain the same second value"
    (let* ((result (nskk--program-dict-now "now"))
           (cand1  (car result))
           (cand2  (cadr result))
           (sec1   (string-to-number (substring cand1 6 8))))
      ;; HH:MM:SS and HH時MM分SS秒 share the same second
      (should (string-match-p (format "%02d秒" sec1) cand2)))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; nskk--program-dict-calculate: floating point and custom (S-3/S-4/S-5)
;;; ─────────────────────────────────────────────────────────────────────────

;;; ─────────────────────────────────────────────────────────────────────────
;;; dispatch-table: custom entry priority (S-4)
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-describe "nskk-program-dict-dispatch-table custom entry"
  (nskk-it "custom entry prepended to dispatch table is invoked (S-4)"
    (let* ((custom-called nil)
           (custom-result '("custom-today"))
           (nskk-program-dict-dispatch-table
            (cons (cons "today" (lambda (_k)
                                  (setq custom-called t)
                                  custom-result))
                  nskk-program-dict-dispatch-table)))
      (nskk--pd-builtin-test-with-env t
        (let ((result (nskk-program-dict-builtin-lookup "today")))
          (should custom-called)
          (should (member "custom-today" (mapcar #'substring-no-properties result)))))))

  (nskk-it "custom entry with unique prefix works independently"
    (let* ((nskk-program-dict-dispatch-table
            (cons (cons "myprefix" (lambda (_k) '("my-result")))
                  nskk-program-dict-dispatch-table)))
      (nskk--pd-builtin-test-with-env t
        (let ((result (nskk-program-dict-builtin-lookup "myprefix-query")))
          (should result)
          (should (member "my-result" (mapcar #'substring-no-properties result))))))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; PBT: nskk-no-learn invariant on all builtin-lookup results (S-6)
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-property-test pd-builtin-no-learn-invariant-today
  ((word romaji-string))
  ;; Any key with "today" prefix must yield only nskk-no-learn-t candidates
  (let ((nskk-program-dict-enable t)
        (key (concat "today" word)))
    (let ((result (nskk-program-dict-builtin-lookup key)))
      (or (null result)
          (cl-every (lambda (c) (eq (get-text-property 0 'nskk-no-learn c) t))
                    result))))
  20)

(nskk-property-test pd-builtin-no-learn-invariant-now
  ((word romaji-string))
  ;; Any key with "now" prefix must yield only nskk-no-learn-t candidates
  (let ((nskk-program-dict-enable t)
        (key (concat "now" word)))
    (let ((result (nskk-program-dict-builtin-lookup key)))
      (or (null result)
          (cl-every (lambda (c) (eq (get-text-property 0 'nskk-no-learn c) t))
                    result))))
  20)

;;;
;;; safe-local-variable policy
;;;

(nskk-describe "nskk-program-dict-enable safe-local-variable policy"
  (nskk-it "is marked as risky-local-variable"
    (should (get 'nskk-program-dict-enable 'risky-local-variable)))

  (nskk-it "is classified as risky by risky-local-variable-p"
    (should (risky-local-variable-p 'nskk-program-dict-enable)))

  (nskk-it "has no safe-local-variable predicate"
    (should-not (get 'nskk-program-dict-enable 'safe-local-variable))))

(nskk-describe "nskk--program-dict-calculate additional cases"
  (nskk-context "floating point results (S-3)"
    (nskk-it "returns a string result for floating point addition"
      (let ((result (nskk--program-dict-calculate "=1.5+2.5")))
        (should (listp result))
        (should (stringp (car result)))))
    (nskk-it "returns a string result for expressions producing decimals"
      (let ((result (nskk--program-dict-calculate "=3.0+1.0")))
        (should (listp result))
        (should (stringp (car result))))))
  (nskk-context "isolated process boundary"
    (nskk-context
  "calculation process tree termination"
  (nskk-it
    "returns before the deadline for a high-cost expression"
    (let ((nskk-program-dict-timeout 0.05)
          (started (float-time)))
      (should-not (nskk--program-dict-calculate "=100000!"))
      (should (< (- (float-time) started) 1.0))))
  (nskk-it
    "terminates descendants of a timed-out calculation process"
    (let ((script (make-temp-file "nskk-calc-tree-" nil ".sh"))
          (pid-file (make-temp-file "nskk-calc-child-"))
          (nskk-program-dict-timeout 0.2)
          child-pid)
      (unwind-protect (progn
          (with-temp-file
            script
            (insert
              "#!/bin/sh\nsleep 30 &\necho $! > "
              (shell-quote-argument pid-file)
              "\nwait\n"))
          (set-file-modes script #o700)
          (let ((invocation-directory (file-name-directory script))
                (invocation-name (file-name-nondirectory script)))
            (should-not (nskk--program-dict-run-calculation "1+1")))
          (with-temp-buffer
            (insert-file-contents pid-file)
            (setq child-pid (string-to-number (buffer-string))))
          (should (> child-pid 0))
          (let ((deadline (+ (float-time) 1)))
            (while
              (and (process-attributes child-pid) (< (float-time) deadline))
              (sleep-for 0.01)))
          (should-not (process-attributes child-pid)))
        (when (and child-pid (process-attributes child-pid))
          (ignore-errors (signal-process child-pid (quote SIGKILL))))
        (delete-file script)
        (delete-file pid-file)))))
    (nskk-it "rejects stdout that exceeds the byte cap"
      (let ((nskk--program-dict-max-calculation-size 8))
        (should-not (nskk--program-dict-calculate "=2^100"))))
    (progn
      (nskk-it "drains exact-cap stdout from short-lived calculations"
  (cl-letf (((symbol-function (quote sleep-for))
             (lambda (&rest _args)
               (ert-fail "successful calculation drain must not sleep"))))
    (let ((nskk--program-dict-max-calculation-size 3))
      (dotimes (_ 10)
        (should
         (equal (nskk--program-dict-run-calculation "1+1") "2"))))))
      (nskk-context "finite wait budget"
		    (nskk-it
		     "terminates continuous stdout near the absolute deadline"
		     (let ((script (make-temp-file "nskk-calc-continuous-" nil ".sh"))
			   (nskk-program-dict-timeout 0.1)
			   (nskk--program-dict-max-calculation-size most-positive-fixnum)
			   (clock-hz 1000000000))
		       (unwind-protect (progn
					 (with-temp-file
					     script
					   (insert
					    "#!/bin/sh\ni=0\nwhile [ \"$i\" -lt 100 ]; do\n  printf x\n  i=$((i + 1))\n  sleep 0.01\ndone\nsleep 2\n"))
					 (set-file-modes script #o700)
					 (let ((invocation-directory (file-name-directory script))
					       (invocation-name (file-name-nondirectory script))
					       (started-at (car (time-convert nil clock-hz))))
					   (should-not (nskk--program-dict-run-calculation "1+1"))
					   (should
					    (< (/ (- (car (time-convert nil clock-hz)) started-at) (float clock-hz)) 0.6))))
			 (delete-file script))))
		    (nskk-it "stops draining after delayed stderr becomes quiet"
			     (let ((nskk-program-dict-timeout 1.0)
				   (clock-hz 1000000000)
				   (ticks 0)
				   stdout-filter stderr-filter sentinel started stderr-delivered
				   (drain-calls 0))
			       (cl-letf (((symbol-function (quote time-convert))
					  (lambda (&rest _args)
					    (prog1 (cons ticks clock-hz)
					      (setq ticks (+ ticks 1000000)))))
					 ((symbol-function (quote make-pipe-process))
					  (lambda (&rest args)
					    (setq stderr-filter (plist-get args :filter))
					    (quote owned-stderr)))
					 ((symbol-function (quote make-process))
					  (lambda (&rest args)
					    (setq stdout-filter (plist-get args :filter)
						  sentinel (plist-get args :sentinel))
					    (quote owned-stdout)))
					 ((symbol-function (quote accept-process-output))
					  (lambda (wait-process &rest _args)
					    (cond
					     ((and (eq wait-process (quote owned-stdout)) (not started))
					      (setq started t)
					      (funcall stdout-filter wait-process "\"2\"")
					      (funcall sentinel wait-process "finished\n"))
					     ((eq wait-process (quote owned-stdout))
					      (setq drain-calls (1+ drain-calls)))
					     ((and (null wait-process) (not stderr-delivered))
					      (setq stderr-delivered t)
					      (funcall stderr-filter (quote owned-stderr) "late")))))
					 ((symbol-function (quote process-status))
					  (lambda (_process) (quote exit)))
					 ((symbol-function (quote process-exit-status)) (lambda (_process) 0))
					 ((symbol-function (quote processp))
					  (lambda (process) (eq process (quote owned-stderr))))
					 ((symbol-function (quote delete-process)) (lambda (_process)))
					 ((symbol-function (quote nskk--program-dict-stop-process-group))
					  (lambda (_process))))
				 (should (equal (nskk--program-dict-run-calculation "1+1") "2"))
				 (should stderr-delivered)
				 (should (= drain-calls 3)))))

		    (nskk-it "disables calculation polling for invalid wait budgets"
			     (let ((invalid-timeouts
				    (list "invalid"
					  0
					  -0.5
					  (read "0.0e+NaN")
                       (read "1.0e+INF")
                       (read "-1.0e+INF")))
                wait-calls
                deleted)
            (dolist (timeout invalid-timeouts)
              (let ((nskk-program-dict-timeout timeout))
                (cl-letf (((symbol-function 'make-pipe-process)
                           (lambda (&rest _args) 'owned-stderr))
                          ((symbol-function 'make-process)
                           (lambda (&rest _args) 'owned-stdout))
                          ((symbol-function 'process-status)
                           (lambda (_process) 'run))
                          ((symbol-function 'processp)
                           (lambda (process)
                             (memq process '(owned-stdout owned-stderr))))
                          ((symbol-function 'process-id)
                           (lambda (_process) nil))
                          ((symbol-function 'process-live-p)
                           (lambda (_process) t))
                          ((symbol-function 'delete-process)
                           (lambda (process) (push process deleted)))
                          ((symbol-function 'accept-process-output)
                           (lambda (&rest args) (push args wait-calls))))
                  (should-not (nskk--program-dict-run-calculation "1+1")))))
            (should-not wait-calls)
            (should
             (= (cl-count 'owned-stdout deleted)
                (length invalid-timeouts)))
            (should
             (= (cl-count 'owned-stderr deleted)
                (length invalid-timeouts)))
            (should-not (get-buffer " *nskk-program-dict-calc-output*"))
            (should-not (get-buffer " *nskk-program-dict-calc-stderr*"))))))
    (nskk-it "rejects stderr that exceeds the byte cap"
      (let ((script (make-temp-file "nskk-calc-stderr-" nil ".sh"))
            (nskk--program-dict-max-calculation-size 4096))
        (unwind-protect
            (progn
              (with-temp-file script
                (insert "#!/bin/sh\nprintf '\"2\"'\nprintf '%4097s' x >&2\n"))
              (set-file-modes script #o700)
              (let ((invocation-directory (file-name-directory script))
                    (invocation-name (file-name-nondirectory script)))
                (should-not
                 (nskk--program-dict-run-calculation "1+1"))))
          (delete-file script)))))
  (nskk-context "calculation result decoding"
    (nskk-it "accepts exactly one safe string"
      (should (equal
               (nskk--program-dict-read-calculation-result "\"ok\"\n")
               "ok")))
    (nskk-it "rejects reader evaluation even when enabled ambiently"
      (let ((read-eval t))
        (cl-progv (list (quote side-effect)) (list nil)
          (should-not
           (nskk--program-dict-read-calculation-result
            "#.(progn (setq side-effect t) \"ok\")"))
          (should-not (symbol-value (quote side-effect))))))
    (nskk-it "rejects circular reader syntax even when enabled ambiently"
      (let ((read-circle t))
        (should-not
         (nskk--program-dict-read-calculation-result "#1=\"ok\""))))
    (nskk-it "rejects a non-string result"
      (should-not (nskk--program-dict-read-calculation-result "42")))
    (nskk-it "rejects trailing Lisp objects"
      (should-not
       (nskk--program-dict-read-calculation-result "\"ok\" \"extra\"")))
    (nskk-it "rejects unsafe string contents"
      (should-not
       (nskk--program-dict-read-calculation-result "\"bad\\nvalue\""))))
  (nskk-context "unexpected subprocess results (S-5)"
    (nskk-it "returns nil gracefully for nil"
      (nskk-with-mocks
          ((nskk--program-dict-run-calculation (lambda (_expr) nil)))
        (should-not (nskk--program-dict-calculate "=1+1"))))
    (nskk-it "returns nil gracefully for a non-string"
      (nskk-with-mocks
          ((nskk--program-dict-run-calculation (lambda (_expr) 42)))
        (should-not (nskk--program-dict-calculate "=1+1")))))
  (nskk-context "calculation input byte limit"
    (nskk-it "accepts an expression exactly at the byte limit"
      (let ((nskk--program-dict-max-calculation-size 4) called)
        (nskk-with-mocks
            ((nskk--program-dict-run-calculation
              (lambda (expr) (setq called expr) "ok")))
          (should (equal (nskk--program-dict-calculate "=1234")
                         (list "ok")))
          (should (equal called "1234")))))
    (nskk-it "rejects one byte over without starting a subprocess"
      (let ((nskk--program-dict-max-calculation-size 4) called)
        (nskk-with-mocks
            ((nskk--program-dict-run-calculation
              (lambda (_expr) (setq called t) "bad")))
          (should-not (nskk--program-dict-calculate "=12345"))
          (should-not called))))
    (nskk-it "counts multibyte expressions in bytes"
      (let ((nskk--program-dict-max-calculation-size 5) called)
        (nskk-with-mocks
            ((nskk--program-dict-run-calculation
              (lambda (_expr) (setq called t) "bad")))
          (should-not (nskk--program-dict-calculate "=漢字"))
          (should-not called))))))

(nskk-describe "nskk-program-dicts malicious dir-locals scenario"
  (nskk-it "lambda injection is rejected by safe-local-variable-p"
    (should-not (safe-local-variable-p 'nskk-program-dicts
                                       (list (lambda (k) (shell-command k))))))

  (nskk-it "shell command string is rejected by safe-local-variable-p"
    (should-not (safe-local-variable-p 'nskk-program-dicts '("rm -rf ~ %s"))))

  (nskk-it "enable flag injection is rejected by safe-local-variable-p"
    (should-not (safe-local-variable-p 'nskk-program-dict-enable t))))

(nskk-describe "program dictionary stdout drain regression"
  (nskk-it "never loses exact-cap stdout from a short-lived command"
  (cl-letf (((symbol-function (quote sleep-for))
             (lambda (&rest _args)
               (ert-fail "successful command drain must not sleep"))))
    (let ((nskk--program-dict-max-output-size 32)
          (expected "12345678901234567890123456789012")
          (misses 0))
      (dotimes (_ 100)
        (let (found)
          (nskk--program-dict-exec-command/k
            (or (executable-find "sh") shell-file-name) nil
            (list "-c" "printf %s 12345678901234567890123456789012")
            (lambda (value) (setq found value))
            (lambda () (cl-incf misses)))
          (should (equal found expected))))
      (should (zerop misses))))))

(nskk-describe "nskk calculation unwind cleanup"
  (nskk-it "releases the first buffer when the second allocation errors"
    (let ((original-generate-new-buffer
           (symbol-function 'generate-new-buffer))
          (allocation-count 0)
          (process-started nil))
      (should-not (get-buffer " *nskk-program-dict-calc-output*"))
      (should-not (get-buffer " *nskk-program-dict-calc-stderr*"))
      (cl-letf (((symbol-function 'generate-new-buffer)
                 (lambda (name &rest args)
                   (cl-incf allocation-count)
                   (if (= allocation-count 2)
                       (error "injected second buffer failure")
                     (apply original-generate-new-buffer name args))))
                ((symbol-function 'make-pipe-process)
                 (lambda (&rest _args)
                   (setq process-started t)))
                ((symbol-function 'make-process)
                 (lambda (&rest _args)
                   (setq process-started t))))
        (should-not (nskk--program-dict-run-calculation "1+1")))
      (should (= allocation-count 2))
      (should-not process-started)
      (should-not (get-buffer " *nskk-program-dict-calc-output*"))
      (should-not (get-buffer " *nskk-program-dict-calc-stderr*"))))

  (nskk-it "releases both buffers when serialization errors"
    (let ((process-started nil))
      (cl-letf (((symbol-function 'prin1-to-string)
                 (lambda (&rest _args)
                   (error "injected serialization failure")))
                ((symbol-function 'make-pipe-process)
                 (lambda (&rest _args)
                   (setq process-started t)))
                ((symbol-function 'make-process)
                 (lambda (&rest _args)
                   (setq process-started t))))
        (should-not (nskk--program-dict-run-calculation "1+1")))
      (should-not process-started)
      (should-not (get-buffer " *nskk-program-dict-calc-output*"))
      (should-not (get-buffer " *nskk-program-dict-calc-stderr*"))))

  (nskk-it "releases both buffers when serialization quits"
    (let ((process-started nil))
      (cl-letf (((symbol-function 'prin1-to-string)
                 (lambda (&rest _args)
                   (signal 'quit nil)))
                ((symbol-function 'make-pipe-process)
                 (lambda (&rest _args)
                   (setq process-started t)))
                ((symbol-function 'make-process)
                 (lambda (&rest _args)
                   (setq process-started t))))
        (should (eq 'quit (condition-case nil (progn (nskk--program-dict-run-calculation "1+1") 'no-quit) (quit 'quit)))))
      (should-not process-started)
      (should-not (get-buffer " *nskk-program-dict-calc-output*"))
      (should-not (get-buffer " *nskk-program-dict-calc-stderr*"))))

  (nskk-it "stops the process tree when process waiting quits"
    (let ((script (make-temp-file "nskk-calc-quit-tree-" nil ".sh"))
          (pid-file (make-temp-file "nskk-calc-quit-child-"))
          (original-make-process (symbol-function 'make-process))
          (original-make-pipe-process
           (symbol-function 'make-pipe-process))
          main-process
          stderr-process
          main-pid
          child-pid)
      (unwind-protect
          (progn
            (with-temp-file script
              (insert
               "#!/bin/sh\nsleep 30 &\necho $! > "
               (shell-quote-argument pid-file)
               "\nwait\n"))
            (set-file-modes script #o700)
            (let ((invocation-directory (file-name-directory script))
                  (invocation-name (file-name-nondirectory script)))
              (cl-letf
                  (((symbol-function 'make-process)
                    (lambda (&rest args)
                      (setq main-process
                            (apply original-make-process args)
                            main-pid
                            (process-id main-process))
                      main-process))
                   ((symbol-function 'make-pipe-process)
                    (lambda (&rest args)
                      (setq stderr-process
                            (apply original-make-pipe-process args))))
                   ((symbol-function 'accept-process-output)
                    (lambda (&rest _args)
                      (let ((deadline (+ (float-time) 1.0)))
                        (while
                            (and
                             (zerop
                              (file-attribute-size
                               (file-attributes pid-file)))
                             (< (float-time) deadline))
                          (sleep-for 0.01)))
                      (with-temp-buffer
                        (insert-file-contents pid-file)
                        (setq child-pid
                              (string-to-number (buffer-string))))
                      (signal 'quit nil))))
                (should (eq 'quit (condition-case nil (progn (nskk--program-dict-run-calculation "1+1") 'no-quit) (quit 'quit))))))
            (should (> main-pid 0))
            (should (> child-pid 0))
            (should-not (process-live-p main-process))
            (should-not (process-live-p stderr-process))
            (let ((deadline (+ (float-time) 1.0)))
              (while
                  (and (or (process-attributes main-pid)
                           (process-attributes child-pid))
                       (< (float-time) deadline))
                (sleep-for 0.01)))
            (should-not (process-attributes main-pid))
            (should-not (process-attributes child-pid))
            (should-not (get-buffer " *nskk-program-dict-calc-output*"))
            (should-not (get-buffer " *nskk-program-dict-calc-stderr*")))
        (when (and child-pid (process-attributes child-pid))
          (ignore-errors (signal-process child-pid 'SIGKILL)))
        (when (and main-pid (process-attributes main-pid))
          (ignore-errors (signal-process main-pid 'SIGKILL)))
        (when (and (processp main-process) (process-live-p main-process)) (ignore-errors (delete-process main-process)))
        (when (and (processp stderr-process) (process-live-p stderr-process)) (ignore-errors (delete-process stderr-process)))
        (delete-file script)
        (delete-file pid-file)))))

(progn (progn
  (defun nskk--program-dict-test-cache-snapshot (cache)
    "Return an identity-sensitive snapshot of CACHE."
    (if (nskk-cache-lru-p cache)
        (let (nodes)
          (maphash
           (lambda (lookup-key node)
             (push (vector lookup-key
                           node
                           (nskk-cache-lru-node-key node)
                           (nskk-cache-lru-node-value node)
                           (nskk-cache-lru-node-prev node)
                           (nskk-cache-lru-node-next node))
                   nodes))
           (nskk-cache-lru-hash cache))
          (list :kind 'lru
                :capacity (nskk-cache-lru-capacity cache)
                :size (nskk-cache-lru-size cache)
                :hash (nskk-cache-lru-hash cache)
                :head (nskk-cache-lru-head cache)
                :tail (nskk-cache-lru-tail cache)
                :head-next (nskk-cache-lru-node-next
                            (nskk-cache-lru-head cache))
                :tail-prev (nskk-cache-lru-node-prev
                            (nskk-cache-lru-tail cache))
                :hits (nskk-cache-lru-hits cache)
                :misses (nskk-cache-lru-misses cache)
                :nodes nodes))
      (let (entries buckets)
        (maphash
         (lambda (lookup-key entry)
           (push (vector lookup-key
                         entry
                         (nskk-cache-lfu-entry-key entry)
                         (nskk-cache-lfu-entry-value entry)
                         (nskk-cache-lfu-entry-frequency entry))
                 entries))
         (nskk-cache-lfu-hash cache))
        (maphash (lambda (frequency bucket)
                   (push (vector frequency bucket) buckets))
                 (nskk-cache-lfu-freq cache))
        (list :kind 'lfu
              :capacity (nskk-cache-lfu-capacity cache)
              :size (nskk-cache-lfu-size cache)
              :hash (nskk-cache-lfu-hash cache)
              :freq (nskk-cache-lfu-freq cache)
              :min-freq (nskk-cache-lfu-min-freq cache)
              :hits (nskk-cache-lfu-hits cache)
              :misses (nskk-cache-lfu-misses cache)
              :entries entries
              :buckets buckets))))

  (defun nskk--program-dict-test-should-match-cache-snapshot
      (cache snapshot)
    "Assert that CACHE still has every identity recorded in SNAPSHOT."
    (if (eq (plist-get snapshot :kind) 'lru)
        (progn
          (should (eq (nskk-cache-lru-capacity cache)
                      (plist-get snapshot :capacity)))
          (should (eq (nskk-cache-lru-size cache)
                      (plist-get snapshot :size)))
          (should (eq (nskk-cache-lru-hash cache)
                      (plist-get snapshot :hash)))
          (should (eq (nskk-cache-lru-head cache)
                      (plist-get snapshot :head)))
          (should (eq (nskk-cache-lru-tail cache)
                      (plist-get snapshot :tail)))
          (should (eq (nskk-cache-lru-node-next
                       (nskk-cache-lru-head cache))
                      (plist-get snapshot :head-next)))
          (should (eq (nskk-cache-lru-node-prev
                       (nskk-cache-lru-tail cache))
                      (plist-get snapshot :tail-prev)))
          (should (eq (nskk-cache-lru-hits cache)
                      (plist-get snapshot :hits)))
          (should (eq (nskk-cache-lru-misses cache)
                      (plist-get snapshot :misses)))
          (dolist (record (plist-get snapshot :nodes))
            (let ((node (aref record 1)))
              (should (eq (gethash (aref record 0)
                                   (nskk-cache-lru-hash cache))
                          node))
              (should (eq (nskk-cache-lru-node-key node)
                          (aref record 2)))
              (should (eq (nskk-cache-lru-node-value node)
                          (aref record 3)))
              (should (eq (nskk-cache-lru-node-prev node)
                          (aref record 4)))
              (should (eq (nskk-cache-lru-node-next node)
                          (aref record 5))))))
      (progn
        (should (eq (nskk-cache-lfu-capacity cache)
                    (plist-get snapshot :capacity)))
        (should (eq (nskk-cache-lfu-size cache)
                    (plist-get snapshot :size)))
        (should (eq (nskk-cache-lfu-hash cache)
                    (plist-get snapshot :hash)))
        (should (eq (nskk-cache-lfu-freq cache)
                    (plist-get snapshot :freq)))
        (should (eq (nskk-cache-lfu-min-freq cache)
                    (plist-get snapshot :min-freq)))
        (should (eq (nskk-cache-lfu-hits cache)
                    (plist-get snapshot :hits)))
        (should (eq (nskk-cache-lfu-misses cache)
                    (plist-get snapshot :misses)))
        (dolist (record (plist-get snapshot :entries))
          (let ((entry (aref record 1)))
            (should (eq (gethash (aref record 0)
                                 (nskk-cache-lfu-hash cache))
                        entry))
            (should (eq (nskk-cache-lfu-entry-key entry)
                        (aref record 2)))
            (should (eq (nskk-cache-lfu-entry-value entry)
                        (aref record 3)))
            (should (eq (nskk-cache-lfu-entry-frequency entry)
                        (aref record 4)))))
        (dolist (record (plist-get snapshot :buckets))
          (should (eq (gethash (aref record 0)
                               (nskk-cache-lfu-freq cache))
                      (aref record 1)))))))

  (defun nskk--program-dict-test-install-cache (strategy capacity)
    "Install an empty program dictionary cache using STRATEGY and CAPACITY."
    (nskk--program-dict-sync-config)
    (setq nskk--program-dict-cache
          (nskk-cache-create :type strategy :capacity capacity)))

  (progn
  (nskk-describe "nskk program dictionary dedicated object graph"
    (nskk-it "copies and marks cyclic shared cons vector hash and properties"
      (let* ((text (copy-sequence "leaf"))
             (shared (vector text text #'ignore 'atom))
             (root (cons shared nil))
             (table (make-hash-table :test 'eq))
             (hash-key (copy-sequence "hash-key")))
        (setcdr root table)
        (puthash root shared table)
        (puthash hash-key root table)
        (add-text-properties
         0 (length text)
         (list 'legacy 'kept
               'nskk-no-learn 'old
               'backlink root
               'shared-value shared)
         text)
        (let* ((copied (nskk--program-dict-copy-graph root))
               (copied-shared (car copied))
               (copied-table (cdr copied))
               (copied-text (aref copied-shared 0))
               copied-hash-key)
          (maphash
           (lambda (key _value)
             (when (and (stringp key) (equal key "hash-key"))
               (setq copied-hash-key key)))
           copied-table)
          (should-not (eq copied root))
          (should-not (eq copied-shared shared))
          (should-not (eq copied-table table))
          (should-not (eq copied-text text))
          (should (eq copied-text (aref copied-shared 1)))
          (should (eq (aref copied-shared 2) #'ignore))
          (should (eq (aref copied-shared 3) 'atom))
          (should (eq (gethash copied copied-table) copied-shared))
          (should copied-hash-key)
          (should-not (eq copied-hash-key hash-key))
          (should (eq (gethash copied-hash-key copied-table) copied))
          (should (eq (get-text-property 0 'legacy copied-text) 'kept))
          (should (eq (get-text-property 0 'nskk-no-learn copied-text)
                      'old))
          (should (eq (get-text-property 0 'backlink copied-text) copied))
          (should (eq (get-text-property 0 'shared-value copied-text)
                      copied-shared))
          (should (eq (nskk--program-dict-mark-no-learn copied) copied))
          (should (eq (get-text-property 0 'legacy copied-text) 'kept))
          (should (eq (get-text-property 0 'nskk-no-learn copied-text) t))
          (should (eq (get-text-property 0 'nskk-no-learn copied-hash-key)
                      t))
          (should-not (eq (get-text-property 0 'nskk-no-learn text) t))
          (aset copied-text 0 ?X)
          (should (equal text "leaf"))))
      (let ((deep (copy-sequence "bottom")))
        (dotimes (_index 20000)
          (setq deep (cons deep nil)))
        (let ((copied (nskk--program-dict-copy-graph deep)))
          (nskk--program-dict-mark-no-learn copied)
          (dotimes (_index 20000)
            (setq copied (car copied)))
          (should (equal copied "bottom"))
          (should (eq (get-text-property 0 'nskk-no-learn copied) t))))))

  (progn
  (nskk-describe "nskk program dictionary cache strategy graph behavior"
    (nskk-it "supports equal detached keys hits and eviction in LRU and LFU"
      (dolist (strategy (list (quote lru) (quote lfu)))
        (let ((entry-count 0)
              sources)
          (nskk--prog-dict-test-with-env
              t
              (list (function ignore))
            (nskk--program-dict-test-install-cache strategy 2)
            (cl-letf
                (((symbol-function
                   (quote nskk--program-dict-collect-all/k))
                  (lambda (_entries key on-found _on-not-found)
                    (cl-incf entry-count)
                    (let* ((text
                            (copy-sequence
                             (format "candidate-%s" (aref (car key) 1))))
                           (shared (vector text text))
                           (table (make-hash-table :test (quote eq)))
                           (candidate (cons shared table)))
                      (add-text-properties
                       0 (length text)
                       (list (quote legacy) (quote kept)
                             (quote nskk-no-learn) (quote old))
                       text)
                      (puthash text shared table)
                      (push candidate sources)
                      (funcall on-found (list candidate))))))
              (let* ((key-a (list (vector (quote key) (copy-sequence "a"))))
                     (equal-key-a
                      (list (vector (quote key) (copy-sequence "a"))))
                     (key-b (list (vector (quote key) (copy-sequence "b"))))
                     (key-c (list (vector (quote key) (copy-sequence "c"))))
                     (public-a (nskk-program-dict-lookup key-a))
                     (public-b (nskk-program-dict-lookup key-b))
                     (hit-a (nskk-program-dict-lookup equal-key-a))
                     (public-c (nskk-program-dict-lookup key-c))
                     (cache nskk--program-dict-cache)
                     (table
                      (if (eq strategy (quote lru))
                          (nskk-cache-lru-hash cache)
                        (nskk-cache-lfu-hash cache)))
                     (record-a (gethash key-a table))
                     (source-a (car (last sources))))
                (should-not (eq key-a equal-key-a))
                (should (equal key-a equal-key-a))
                (should (= entry-count 3))
                (should public-a)
                (should public-b)
                (should public-c)
                (should record-a)
                (should (gethash key-c table))
                (should-not (gethash key-b table))
                (should (= (nskk-cache-size cache) 2))
                (let ((canonical-a
                       (if (eq strategy (quote lru))
                           (nskk-cache-lru-node-value record-a)
                         (nskk-cache-lfu-entry-value record-a))))
                  (should-not (eq public-a canonical-a))
                  (should-not (eq hit-a canonical-a))
                  (should-not (eq (car hit-a) (car canonical-a)))
                  (should-not (eq (car public-a) source-a))
                  (should (eq (aref (car (car hit-a)) 0)
                              (aref (car (car hit-a)) 1)))
                  (should
                   (eq (gethash
                        (aref (car (car hit-a)) 0)
                        (cdr (car hit-a)))
                       (car (car hit-a))))
                  (should
                   (eq (get-text-property
                        0 (quote legacy)
                        (aref (car (car hit-a)) 0))
                       (quote kept)))
                  (should
                   (eq (get-text-property
                        0 (quote nskk-no-learn)
                        (aref (car (car hit-a)) 0))
                       t))
                  (should
                   (eq (get-text-property
                        0 (quote nskk-no-learn)
                        (aref (car source-a) 0))
                       (quote old)))
                  (aset (aref (car (car hit-a)) 0) 0 ?X)
                  (should-not
                   (eq (aref (aref (car (car canonical-a)) 0) 0) ?X)))
                (if (eq strategy (quote lru))
                    (progn
                      (should (= (nskk-cache-lru-hits cache) 1))
                      (should (= (nskk-cache-lru-misses cache) 3)))
                  (should (= (nskk-cache-lfu-hits cache) 1))
                  (should (= (nskk-cache-lfu-misses cache) 3))))))))))
  (progn
  (nskk-describe "nskk program dictionary publication transaction"
    (nskk-it "publishes canonical public and owned-key graphs in order"
      (nskk--prog-dict-test-with-env
          t
          (list (function ignore))
        (nskk--program-dict-test-install-cache (quote lru) 4)
        (let* ((source-text (copy-sequence "source"))
               (source-shared (vector source-text source-text))
               (source-table (make-hash-table :test (quote eq)))
               (source (cons source-shared source-table))
               (results (list source))
               (key-text (copy-sequence "key"))
               (key-shared (vector key-text key-text))
               (key (list key-shared))
               (real-copy
                (symbol-function (quote nskk--program-dict-copy-graph)))
               (real-mark
                (symbol-function (quote nskk--program-dict-mark-no-learn)))
               (real-put (symbol-function (quote nskk-cache-put)))
               (copy-count 0)
               events canonical marked-input public owned-key put-key put-value
               callback-value stored-key stored-value)
          (add-text-properties
           0 (length source-text)
           (list (quote legacy) (quote kept)
                 (quote nskk-no-learn) (quote old))
           source-text)
          (add-text-properties
           0 (length key-text)
           (list (quote key-prop) (quote kept))
           key-text)
          (puthash source-text source-shared source-table)
          (cl-letf
              (((symbol-function (quote nskk--program-dict-collect-all/k))
                (lambda (_entries _key on-found _on-not-found)
                  (funcall on-found results)))
               ((symbol-function (quote nskk--program-dict-copy-graph))
                (lambda (object)
                  (cl-incf copy-count)
                  (push (pcase copy-count
                          (1 (quote canonical-copy))
                          (2 (quote public-copy))
                          (3 (quote key-copy)))
                        events)
                  (let ((copied (funcall real-copy object)))
                    (pcase copy-count
                      (1 (setq canonical copied))
                      (2 (setq public copied))
                      (3 (setq owned-key copied)))
                    copied)))
               ((symbol-function (quote nskk--program-dict-mark-no-learn))
                (lambda (object)
                  (push (quote mark) events)
                  (setq marked-input object)
                  (funcall real-mark object)))
               ((symbol-function (quote nskk-cache-put))
                (lambda (cache cache-key value)
                  (push (quote put) events)
                  (setq put-key cache-key
                        put-value value)
                  (funcall real-put cache cache-key value))))
            (nskk-program-dict-lookup/k
             key
             (lambda (value)
               (push (quote callback) events)
               (setq callback-value value))
             (lambda () (should nil))))
          (should
           (equal (nreverse events)
                  (quote
                   (canonical-copy mark public-copy key-copy put callback))))
          (should (eq marked-input canonical))
          (should (eq put-key owned-key))
          (should (eq put-value canonical))
          (should (eq callback-value public))
          (should-not (eq canonical results))
          (should-not (eq public canonical))
          (should-not (eq owned-key key))
          (should-not (eq (car canonical) source))
          (should-not (eq (car public) (car canonical)))
          (should-not (eq (car owned-key) key-shared))
          (should-not (eq (aref (car owned-key) 0) key-text))
          (should (eq (aref (car owned-key) 0)
                      (aref (car owned-key) 1)))
          (let* ((canonical-candidate (car canonical))
                 (canonical-shared (car canonical-candidate))
                 (canonical-table (cdr canonical-candidate))
                 (canonical-text (aref canonical-shared 0))
                 (public-candidate (car public))
                 (public-shared (car public-candidate))
                 (public-table (cdr public-candidate))
                 (public-text (aref public-shared 0)))
            (should (eq canonical-text (aref canonical-shared 1)))
            (should (eq (gethash canonical-text canonical-table)
                        canonical-shared))
            (should (eq public-text (aref public-shared 1)))
            (should (eq (gethash public-text public-table) public-shared))
            (should (eq (get-text-property
                         0 (quote legacy) canonical-text)
                        (quote kept)))
            (should (eq (get-text-property
                         0 (quote nskk-no-learn) canonical-text)
                        t))
            (should (eq (get-text-property
                         0 (quote nskk-no-learn) public-text)
                        t)))
          (maphash
           (lambda (cache-key record)
             (setq stored-key cache-key
                   stored-value (nskk-cache-lru-node-value record)))
           (nskk-cache-lru-hash nskk--program-dict-cache))
          (should-not (eq stored-key owned-key))
          (should-not (eq (car stored-key) (car owned-key)))
          (should (equal stored-key key))
          (should (eq stored-value canonical))
          (should (eq (get-text-property
                       0 (quote nskk-no-learn) source-text)
                      (quote old)))
          (should (eq (get-text-property 0 (quote legacy) source-text)
                      (quote kept)))
          (should (eq (aref source-shared 0) source-text))
          (should (eq (aref source-shared 1) source-text))
          (should (eq (gethash source-text source-table) source-shared))
          (should-not
           (get-text-property 0 (quote nskk-no-learn) key-text))))))

  (nskk-describe "nskk program dictionary persistent hit fault matrix"
    (nskk-it "restores exact LRU and LFU state for copy faults before callback"
      (let ((hit-case-count 0))
        (dolist (strategy (quote (lru lfu)))
          (dolist (timing (quote (before after)))
            (dolist (fault (quote (error quit)))
              (progn
                (cl-incf hit-case-count)
                (nskk--prog-dict-test-with-env
                  t
                  (list (function ignore))
                  (nskk--program-dict-test-install-cache strategy 3)
                  (let* ((key (list (vector (quote hit) (copy-sequence "key"))))
                         (canonical-text (copy-sequence "cached"))
                         (canonical (list (vector canonical-text canonical-text)))
                         (cache nskk--program-dict-cache)
                         (real-copy (symbol-function (quote nskk--program-dict-copy-graph)))
                         snapshot
                         caught
                         retry
                         (copy-count 0)
                         (callback-count 0))
                    (add-text-properties
                      0
                      (length canonical-text)
                      (list (quote nskk-no-learn) t (quote legacy) (quote kept))
                      canonical-text)
                    (nskk-cache-put cache key canonical)
                    (setq snapshot (nskk--program-dict-test-cache-snapshot cache))
                    (cl-letf
                      (((symbol-function (quote nskk--program-dict-copy-graph))
                          (lambda (object)
                            (cl-incf copy-count)
                            (when (eq timing (quote before))
                              (signal fault (list (quote injected-hit) timing)))
                            (let ((copied (funcall real-copy object)))
                              (when (eq timing (quote after))
                                (signal fault (list (quote injected-hit) timing)))
                              copied))))
                      (condition-case
                        condition
                        (nskk-program-dict-lookup/k
                          key
                          (lambda (_value)
                            (cl-incf callback-count))
                          (lambda ()
                            (should nil)))
                        ((error quit)
                          (setq caught condition))))
                    (should (eq (car caught) fault))
                    (should (= copy-count 1))
                    (should (= callback-count 0))
                    (nskk--program-dict-test-should-match-cache-snapshot cache snapshot)
                    (should (equal canonical-text "cached"))
                    (should (eq (get-text-property 0 (quote legacy) canonical-text) (quote kept)))
                    (setq retry (nskk-program-dict-lookup key))
                    (should retry)
                    (should-not (eq retry canonical))
                    (if (eq strategy (quote lru)) (progn
                        (should (= (nskk-cache-lru-hits cache) 1))
                        (should (= (nskk-cache-lru-misses cache) 0)))
                      (should (= (nskk-cache-lfu-hits cache) 1))
                      (should (= (nskk-cache-lfu-misses cache) 0)))))))))
        (should (= hit-case-count 8)))
      ))

  (nskk-describe "nskk program dictionary persistent miss fault matrix"
    (nskk-it "rolls back every pre-publication boundary and retries"
      (let ((miss-case-count 0)
            (publication-copy-fault-case-count 0)
            (publication-copy-fault-cases (make-hash-table :test (function equal)))
            (miss-strategy-counts (make-hash-table :test (function eq)))
            (miss-boundary-counts (make-hash-table :test (function eq)))
            (miss-strategy-boundary-counts (make-hash-table :test (function equal))))
        (dolist (strategy (quote (lru lfu)))
          (dolist (boundary (quote (canonical-copy mark public-copy key-copy)))
            (dolist (timing (quote (before after)))
              (dolist (fault (quote (error quit)))
                (progn
                  (cl-incf miss-case-count)
                  (cl-incf (gethash strategy miss-strategy-counts 0))
                  (cl-incf (gethash boundary miss-boundary-counts 0))
                  (cl-incf (gethash (cons strategy boundary) miss-strategy-boundary-counts 0))
                  (let ((publication-case (list boundary timing fault)))
                    (unless (gethash publication-case publication-copy-fault-cases)
                      (puthash publication-case t publication-copy-fault-cases)
                      (cl-incf publication-copy-fault-case-count)))
                  (nskk--prog-dict-test-with-env
                    t
                    (list (function ignore))
                    (nskk--program-dict-test-install-cache strategy 4)
                    (let* ((cache nskk--program-dict-cache)
                           (source-text (copy-sequence "candidate"))
                           (source-shared (vector source-text source-text))
                           (source-table (make-hash-table :test (quote eq)))
                           (source (cons source-shared source-table))
                           (results (list source))
                           (key
                          (list
                            (vector (quote miss) (copy-sequence (format "%s-%s-%s" boundary timing fault)))))
                           (real-copy (symbol-function (quote nskk--program-dict-copy-graph)))
                           (real-mark (symbol-function (quote nskk--program-dict-mark-no-learn)))
                           (target-copy
                          (pcase
                            boundary
                            ((quote canonical-copy) 1)
                            ((quote public-copy) 2)
                            ((quote key-copy) 3)
                            (_ nil)))
                           snapshot
                           caught
                           retry
                           (copy-count 0)
                           (mark-count 0)
                           (collector-count 0)
                           (callback-count 0))
                      (add-text-properties
                        0
                        (length source-text)
                        (list (quote nskk-no-learn) (quote old) (quote legacy) (quote kept))
                        source-text)
                      (puthash source-text source-shared source-table)
                      (nskk-cache-put cache (list (quote seed-a)) (list (copy-sequence "a")))
                      (nskk-cache-put cache (list (quote seed-b)) (list (copy-sequence "b")))
                      (setq snapshot (nskk--program-dict-test-cache-snapshot cache))
                      (cl-letf
                        (((symbol-function (quote nskk--program-dict-collect-all/k))
                            (lambda (_entries _key on-found _on-not-found)
                              (cl-incf collector-count)
                              (funcall on-found results))))
                        (cl-letf
                          (((symbol-function (quote nskk--program-dict-copy-graph))
                              (lambda (object)
                                (cl-incf copy-count)
                                (when (and target-copy (= copy-count target-copy) (eq timing (quote before)))
                                  (signal fault (list (quote injected-miss-copy) boundary timing)))
                                (let ((copied (funcall real-copy object)))
                                  (when (and target-copy (= copy-count target-copy) (eq timing (quote after)))
                                    (signal fault (list (quote injected-miss-copy) boundary timing)))
                                  copied)))
                            ((symbol-function (quote nskk--program-dict-mark-no-learn))
                              (lambda (object)
                                (cl-incf mark-count)
                                (when (and (eq boundary (quote mark)) (eq timing (quote before)))
                                  (signal fault (list (quote injected-miss-mark) timing)))
                                (let ((marked (funcall real-mark object)))
                                  (when (and (eq boundary (quote mark)) (eq timing (quote after)))
                                    (signal fault (list (quote injected-miss-mark) timing)))
                                  marked))))
                          (condition-case
                            condition
                            (nskk-program-dict-lookup/k
                              key
                              (lambda (_value)
                                (cl-incf callback-count))
                              (lambda ()
                                (should nil)))
                            ((error quit)
                              (setq caught condition))))
                        (should (eq (car caught) fault))
                        (should (= callback-count 0))
                        (should
                          (=
                            copy-count
                            (pcase
                              boundary
                              ((quote canonical-copy) 1)
                              ((quote mark) 1)
                              ((quote public-copy) 2)
                              ((quote key-copy) 3))))
                        (should
                          (=
                            mark-count
                            (if (eq boundary (quote canonical-copy)) 0
                              1)))
                        (nskk--program-dict-test-should-match-cache-snapshot cache snapshot)
                        (should (eq (aref source-shared 0) source-text))
                        (should (eq (aref source-shared 1) source-text))
                        (should (eq (gethash source-text source-table) source-shared))
                        (should
                          (eq (get-text-property 0 (quote nskk-no-learn) source-text) (quote old)))
                        (should (eq (get-text-property 0 (quote legacy) source-text) (quote kept)))
                        (setq retry (nskk-program-dict-lookup key)))
                      (should (= collector-count 2))
                      (should (= (nskk-cache-size cache) 3))
                      (should retry)
                      (let* ((candidate (car retry))
                             (shared (car candidate))
                             (table (cdr candidate))
                             (text (aref shared 0)))
                        (should (eq text (aref shared 1)))
                        (should (eq (gethash text table) shared))
                        (should (eq (get-text-property 0 (quote nskk-no-learn) text) t))
                        (should (eq (get-text-property 0 (quote legacy) text) (quote kept))))
                      (if (eq strategy (quote lru)) (progn
                          (should (= (nskk-cache-lru-hits cache) 0))
                          (should (= (nskk-cache-lru-misses cache) 1)))
                        (should (= (nskk-cache-lfu-hits cache) 0))
                        (should (= (nskk-cache-lfu-misses cache) 1))))))))))
        (should (= miss-case-count 32))
        (should (= publication-copy-fault-case-count 16))
        (should (= (hash-table-count publication-copy-fault-cases) 16))
        (dolist (strategy (quote (lru lfu)))
          (should (= (gethash strategy miss-strategy-counts 0) 16)))
        (dolist (boundary (quote (canonical-copy mark public-copy key-copy)))
          (should (= (gethash boundary miss-boundary-counts 0) 8)))
        (dolist (strategy (quote (lru lfu)))
          (dolist (boundary (quote (canonical-copy mark public-copy key-copy)))
            (should
              (= (gethash (cons strategy boundary) miss-strategy-boundary-counts 0) 4)))))
      ))

  (nskk-describe "nskk program dictionary persistent callback commit"
    (nskk-it "keeps committed cache state when callback errors or quits"
      (let ((callback-fault-case-count 0))
        (dolist (strategy (quote (lru lfu)))
          (dolist (fault (quote (error quit)))
            (progn
              (cl-incf callback-fault-case-count)
              (nskk--prog-dict-test-with-env
                t
                (list (function ignore))
                (nskk--program-dict-test-install-cache strategy 2)
                (let* ((cache nskk--program-dict-cache)
                       (source-text (copy-sequence "committed"))
                       (source (list source-text))
                       (key (list (vector (quote callback) (copy-sequence (symbol-name fault)))))
                       (collector-count 0)
                       (callback-count 0)
                       caught
                       record
                       canonical
                       retry)
                  (add-text-properties
                    0
                    (length source-text)
                    (list (quote nskk-no-learn) (quote old) (quote legacy) (quote kept))
                    source-text)
                  (cl-letf
                    (((symbol-function (quote nskk--program-dict-collect-all/k))
                        (lambda (_entries _key on-found _on-not-found)
                          (cl-incf collector-count)
                          (funcall on-found (list source)))))
                    (condition-case
                      condition
                      (nskk-program-dict-lookup/k
                        key
                        (lambda (_value)
                          (cl-incf callback-count)
                          (signal fault (list (quote injected-callback))))
                        (lambda ()
                          (should nil)))
                      ((error quit)
                        (setq caught condition)))
                    (should (eq (car caught) fault))
                    (should (= callback-count 1))
                    (should (= collector-count 1))
                    (should (= (nskk-cache-size cache) 1))
                    (setq record (gethash
                        key
                        (if (eq strategy (quote lru)) (nskk-cache-lru-hash cache)
                          (nskk-cache-lfu-hash cache))))
                    (should record)
                    (setq canonical (if (eq strategy (quote lru)) (nskk-cache-lru-node-value record)
                        (nskk-cache-lfu-entry-value record)))
                    (should
                      (eq (get-text-property 0 (quote nskk-no-learn) (car (car canonical))) t))
                    (should
                      (eq (get-text-property 0 (quote legacy) (car (car canonical))) (quote kept)))
                    (should
                      (eq (get-text-property 0 (quote nskk-no-learn) source-text) (quote old)))
                    (nskk-program-dict-lookup/k
                      key
                      (lambda (value)
                        (cl-incf callback-count)
                        (setq retry value))
                      (lambda ()
                        (should nil))))
                  (should (= collector-count 1))
                  (should (= callback-count 2))
                  (should retry)
                  (should-not (eq retry canonical))
                  (if (eq strategy (quote lru)) (progn
                      (should (= (nskk-cache-lru-hits cache) 1))
                      (should (= (nskk-cache-lru-misses cache) 1)))
                    (should (= (nskk-cache-lfu-hits cache) 1))
                    (should (= (nskk-cache-lfu-misses cache) 1))))))))
        (should (= callback-fault-case-count 4)))
      ))

  (provide (quote nskk-program-dictionary-test)))))))

;;; nskk-program-dictionary-test.el ends here
