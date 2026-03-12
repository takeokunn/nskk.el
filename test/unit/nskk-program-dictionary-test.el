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

;; Unit tests for nskk-program-dictionary.el covering:
;;
;; - Prolog fact tables:
;;     program-dict-entry-type/2 (function/command dispatch)
;;     program-dict-output-prefix/3 (format detection + delimiter)
;; - nskk--program-dict-strip-annotation: annotation removal
;; - nskk--program-dict-build-call: command tokenization and %s replacement
;; - nskk--program-dict-parse-output: SKK/skkserv/line output parsing (CPS)
;; - nskk--program-dict-exec-command: unified process execution (CPS)
;; - nskk--program-dict-call-function: Elisp function entry (CPS)
;; - nskk--program-dict-call-command: shell command entry (CPS)
;; - nskk--program-dict-invoke-entry: Prolog-driven dispatch (CPS)
;; - nskk--program-dict-collect-all: multi-entry aggregation (CPS)
;; - nskk-program-dict-lookup: public API with enable guard and cache (CPS)
;; - PBT: output parsing invariants, build-call invariants

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
  "Execute BODY with `nskk-program-dict-enable' and `nskk-program-dicts' bound.
Resets `nskk--program-dict-cache' to nil so each test starts cache-free."
  (declare (indent 2))
  `(let ((nskk-program-dict-enable ,enable)
         (nskk-program-dicts ,dicts)
         (nskk--program-dict-cache nil))
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
      :rows    ((nil) ("") (42) (("list")))
      :body    (should (null (nskk--program-dict-parse-output input)))))

  (nskk-it "returns nil for whitespace-only output"
    (nskk-deftest-table parse-whitespace
      :columns (input)
      :rows    (("   ") ("\n") ("\r\n") ("\t"))
      :body    (should (null (nskk--program-dict-parse-output input)))))

  (nskk-it "returns nil for SKK with empty candidate body"
    ;; "//" splits to zero parts with omit-nulls=t
    (should (null (nskk--program-dict-parse-output "//")))))

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
  (nskk-context "argv mode (stdin-key = nil)"
    (nskk-it "calls on-found with stdout string on success"
      (let ((found-arg nil))
        (nskk-with-mocks
            ((call-process (lambda (_prog _in _out _disp &rest _args)
                             (insert "/候補/\n")
                             0)))
          (nskk--program-dict-exec-command/k "my-dict" nil '("かんじ")
            (lambda (v) (setq found-arg v))
            #'ignore)
          (should (stringp found-arg))
          (should (string-prefix-p "/" found-arg)))))

    (nskk-it "passes args to call-process"
      (let ((received-args nil))
        (nskk-with-mocks
            ((call-process (lambda (_prog _in _out _disp &rest args)
                             (setq received-args args)
                             0)))
          (nskk--program-dict-exec-command/k "prog" nil '("--flag" "かんじ")
            #'ignore #'ignore)
          (should (equal received-args '("--flag" "かんじ"))))))

    (nskk-it "calls on-not-found when call-process signals an error"
      (let ((not-found-called nil))
        (nskk-with-mocks
            ((call-process (lambda (&rest _) (error "not found"))))
          (nskk--program-dict-exec-command/k "bad" nil nil
            #'ignore
            (lambda () (setq not-found-called t)))
          (should not-found-called)))))

  (nskk-context "stdin mode (stdin-key non-nil)"
    (nskk-it "calls on-found with stdout string on success"
      (let ((found-arg nil))
        (nskk-with-mocks
            ((call-process-region
              (lambda (beg end _prog _del _out _disp &rest _args)
                (delete-region beg end)
                (insert "/漢字/\n")
                0)))
          (nskk--program-dict-exec-command/k "my-dict" "かんじ" nil
            (lambda (v) (setq found-arg v))
            #'ignore)
          (should (stringp found-arg))
          (should (string-prefix-p "/" found-arg)))))

    (nskk-it "writes stdin-key followed by a newline before the call"
      (let ((inserted-content nil))
        (nskk-with-mocks
            ((call-process-region
              (lambda (beg end _prog _del _out _disp &rest _args)
                (setq inserted-content (buffer-substring beg end))
                0)))
          (nskk--program-dict-exec-command/k "prog" "てすと" nil
            #'ignore #'ignore)
          (should (equal inserted-content "てすと\n")))))

    (nskk-it "passes args to call-process-region"
      (let ((received-args nil))
        (nskk-with-mocks
            ((call-process-region
              (lambda (_beg _end _prog _del _out _disp &rest args)
                (setq received-args args)
                0)))
          (nskk--program-dict-exec-command/k "prog" "key" '("--flag")
            #'ignore #'ignore)
          (should (equal received-args '("--flag")))))  )

    (nskk-it "calls on-not-found when call-process-region signals an error"
      (let ((not-found-called nil))
        (nskk-with-mocks
            ((call-process-region (lambda (&rest _) (error "no such file"))))
          (nskk--program-dict-exec-command/k "bad" "key" nil
            #'ignore
            (lambda () (setq not-found-called t)))
          (should not-found-called)))))

  (nskk-context "CPS contract"
    (nskk-it "calls exactly one continuation on argv success"
      (let ((count 0))
        (nskk-with-mocks
            ((call-process (lambda (&rest _) (insert "/x/") 0)))
          (nskk--program-dict-exec-command/k "p" nil nil
            (lambda (_v) (cl-incf count))
            (lambda ()   (cl-incf count)))
          (should (= count 1)))))

    (nskk-it "calls exactly one continuation on stdin failure"
      (let ((count 0))
        (nskk-with-mocks
            ((call-process-region (lambda (&rest _) (error "fail"))))
          (nskk--program-dict-exec-command/k "p" "key" nil
            (lambda (_v) (cl-incf count))
            (lambda ()   (cl-incf count)))
          (should (= count 1)))))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; nskk--program-dict-call-function
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-describe "nskk--program-dict-call-function"
  (nskk-context "on-found branch"
    (nskk-it "calls on-found when fn returns a non-empty list"
      (nskk-deftest-table call-function-found
        :columns (return-val)
        :rows    (('("漢字"))
                  ('("漢字" "感じ"))
                  ('("a" "b" "c")))
        :body
        (let ((found-arg nil))
          (nskk--program-dict-call-function/k (lambda (_k) return-val) "key"
            (lambda (v) (setq found-arg v))
            #'ignore)
          (should (equal found-arg return-val)))))

    (nskk-it "passes the key to the function"
      (let ((received-key nil))
        (nskk--program-dict-call-function/k
          (lambda (k) (setq received-key k) '("result")) "かんじ"
          #'ignore #'ignore)
        (should (equal received-key "かんじ")))))

  (nskk-context "on-not-found branch"
    (nskk-it "calls on-not-found for nil, string, integer, boolean, and keyword"
      (nskk-deftest-table call-function-not-found
        :columns (return-val)
        :rows    ((nil) ("string") (42) (t) (:keyword))
        :body
        (let ((not-found-called nil))
          (nskk--program-dict-call-function/k (lambda (_k) return-val) "key"
            #'ignore
            (lambda () (setq not-found-called t)))
          (should not-found-called))))

    (nskk-it "calls on-not-found when fn signals an error"
      (let ((not-found-called nil))
        (nskk--program-dict-call-function/k
          (lambda (_k) (error "boom")) "key"
          #'ignore
          (lambda () (setq not-found-called t)))
        (should not-found-called))))

  (nskk-context "CPS invariant"
    (nskk-it "calls exactly one continuation"
      (let ((count 0))
        (nskk--program-dict-call-function/k (lambda (_k) '("x")) "key"
          (lambda (_v) (cl-incf count))
          (lambda ()   (cl-incf count)))
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
          (list (lambda (_k) '("漢字"))) "かんじ"
          (lambda (v) (setq found-arg v))
          #'ignore)
        (should (equal found-arg '("漢字")))))

    (nskk-it "calls on-not-found when the single entry misses"
      (let ((not-found-called nil))
        (nskk--program-dict-collect-all/k
          (list (lambda (_k) nil)) "key"
          #'ignore
          (lambda () (setq not-found-called t)))
        (should not-found-called))))

  (nskk-context "multiple entries"
    (nskk-it "merges results from all succeeding entries"
      (let ((found-arg nil))
        (nskk--program-dict-collect-all/k
          (list (lambda (_k) '("漢字"))
                (lambda (_k) '("感じ")))
          "かんじ"
          (lambda (v) (setq found-arg v))
          #'ignore)
        (should (member "漢字" found-arg))
        (should (member "感じ" found-arg))))

    (nskk-it "skips entries that return nil and collects from the rest"
      (let ((found-arg nil))
        (nskk--program-dict-collect-all/k
          (list (lambda (_k) nil)
                (lambda (_k) '("幹事"))
                (lambda (_k) nil))
          "key"
          (lambda (v) (setq found-arg v))
          #'ignore)
        (should (equal found-arg '("幹事")))))

    (nskk-it "calls on-not-found when all entries miss"
      (let ((not-found-called nil))
        (nskk--program-dict-collect-all/k
          (list (lambda (_k) nil)
                (lambda (_k) nil))
          "key"
          #'ignore
          (lambda () (setq not-found-called t)))
        (should not-found-called))))

  (nskk-context "deduplication"
    (nskk-it "deduplicates candidates across entries"
      (let ((found-arg nil))
        (nskk--program-dict-collect-all/k
          (list (lambda (_k) '("漢字" "感じ"))
                (lambda (_k) '("感じ" "幹事")))
          "key"
          (lambda (v) (setq found-arg v))
          #'ignore)
        (should (= (length found-arg)
                   (length (delete-dups (copy-sequence found-arg))))))))

  (nskk-context "empty entries list"
    (nskk-it "calls on-not-found for an empty entries list"
      (let ((not-found-called nil))
        (nskk--program-dict-collect-all/k
          nil "key"
          #'ignore
          (lambda () (setq not-found-called t)))
        (should not-found-called)))))

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
        (should (equal (nskk-program-dict-lookup "てすと") '("手隅")))))))

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

(provide 'nskk-program-dictionary-test)

;;; nskk-program-dictionary-test.el ends here
