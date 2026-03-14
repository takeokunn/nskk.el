;;; nskk-program-dictionary.el --- Program dictionary support for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

;; Author: takeokunn <bararararatty@gmail.com>
;; Maintainer: takeokunn <bararararatty@gmail.com>
;; URL: https://github.com/takeokunn/nskk.el
;; Version: 0.1.0
;; Keywords: i18n
;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Program dictionary (プログラム辞書) support for NSKK (Layer 2: Domain).
;;
;; Layer position: L2 (Domain) -- depends on nskk-prolog, nskk-cps-macros,
;;   nskk-cache, and optionally nskk-debug.
;;
;; Supports two types of program dictionary entries:
;;
;; 1. Emacs Lisp functions -- called as (funcall fn reading); must return
;;    a list of candidate strings or nil.
;;
;; 2. Shell commands (strings) -- executed via `call-process'.  The literal
;;    token "%s" (whitespace-delimited) in the command is replaced by the
;;    reading as a separate argument.  When no "%s" token is present, the
;;    reading is sent to the command via stdin.  stdout is parsed as SKK or
;;    skkserv format.
;;
;; Supported output formats from external commands:
;;   SKK format:       /候補1/候補2/...     (delimiter: "/")
;;   skkserv format:   1/候補1/候補2/...    (delimiter: "/")
;;   one-per-line:     候補1\n候補2\n...    (delimiter: "\n", fallback)
;;
;; Annotations ("word;note") are stripped to "word" in all formats.
;;
;; Prolog predicates maintained by this module:
;; - `program-dict-entry-type/2'    -- (kind handler-sym) entry dispatch
;; - `program-dict-output-prefix/3' -- (prefix-char format-sym delimiter)
;;                                     output format detection table
;;
;; Key public API:
;; - `nskk-program-dict-lookup' -- unified CPS lookup across all entries
;;
;; Integration:
;;   Called from `nskk-core-search/k' in nskk-henkan.el as the final
;;   fallback after skkserv: dict-lookup -> skkserv -> program-dict -> fail.
;;
;; Configuration example:
;;   (setq nskk-program-dict-enable t)
;;   (setq nskk-program-dicts
;;     (list
;;       (lambda (reading) (my-lisp-lookup reading))  ; Elisp function
;;       "/usr/local/bin/my-dict %s"                  ; %s -> reading as arg
;;       "my-stdin-dict"))                             ; reading via stdin

;;; Code:

(require 'cl-lib)
(require 'subr-x)   ; string-empty-p, string-trim, string-trim-right, thread-last
(require 'nskk-prolog)
(require 'nskk-cps-macros)
(require 'nskk-cache)
(require 'nskk-debug nil t)

(declare-function nskk-debug-message "nskk-debug" (fmt &rest args))

;;; Section 1: Customization

(defgroup nskk-program-dict nil
  "Program dictionary (プログラム辞書) settings for NSKK."
  :prefix "nskk-program-dict-"
  :group 'nskk)

(defcustom nskk-program-dict-enable nil
  "When non-nil, enable program dictionaries as a dictionary fallback.
When nil (default), `nskk-program-dict-lookup' fails immediately with zero
overhead regardless of the value of `nskk-program-dicts'.

To enable program dictionary lookup:
  (setq nskk-program-dict-enable t)
  (setq nskk-program-dicts
    (list (lambda (r) (my-lookup r))
          \"/usr/local/bin/my-dict %s\"))"
  :type 'boolean
  :safe #'booleanp
  :group 'nskk-program-dict)

(defcustom nskk-program-dicts nil
  "List of program dictionary entries for NSKK.
Each entry is either:

- A function: called as (funcall fn reading).  Must return a list of
  candidate strings or nil.  Errors signalled by the function are caught
  and treated as a miss.

- A string: a shell command template executed via `call-process'.  The
  literal token \"%s\" (as a separate whitespace-delimited word) is
  replaced by the reading as a distinct command-line argument; this avoids
  shell injection even when the reading contains special characters.  When
  no \"%s\" token is present, the reading is sent to the command via stdin.

  stdout is parsed as SKK format (/候補1/候補2/...), skkserv format
  (1/候補1/候補2/...), or one-per-line as a fallback.

All entries are tried in list order.  Results are collected and deduplicated
via `delete-dups'.  The session-scoped LRU cache keyed on the reading is
shared across all entries to avoid repeated process invocations.

Example:
  (setq nskk-program-dicts
    (list
      (lambda (r) (my-lisp-lookup r))   ; Emacs Lisp function
      \"/usr/local/bin/my-dict %s\"       ; %s replaced by reading (as arg)
      \"my-stdin-dict\"))                  ; reading sent via stdin"
  :type '(repeat (choice function string))
  :safe (lambda (v)
          (and (listp v)
               (cl-every (lambda (e) (or (functionp e) (stringp e))) v)))
  :group 'nskk-program-dict)

(defcustom nskk-program-dict-timeout 1.0
  "Timeout in seconds for each external program dictionary command.
When a command exceeds this duration it is abandoned and the entry is
skipped.

Note: timeout interruption requires the Emacs event loop to be active;
it may not fire during `call-process' in batch/non-interactive contexts
because `call-process' is synchronous and does not yield to the event loop.
Has no effect on Emacs Lisp function entries."
  :type 'number
  :safe #'numberp
  :group 'nskk-program-dict)

;;; Section 2: Prolog infrastructure

;; Entry type dispatch table.
;; Maps the kind of a program dictionary entry (function or command) to the
;; handler atom used in `nskk--program-dict-invoke-entry/k'.
(nskk-prolog-define-fact-table program-dict-entry-type (:arity 2 :index :hash)
  (function call-function)
  (command  call-command))

;; Output format detection table.
;; Maps the first character of external command stdout to a format symbol
;; and the delimiter used to split the candidate body.
;;   "/"  -> skk:     /候補1/候補2/...   delimiter "/"
;;   "1"  -> skkserv: 1/候補1/候補2/...  delimiter "/"
;; When the first character is not in this table the fallback is "\n".
(nskk-prolog-define-fact-table program-dict-output-prefix (:arity 3 :index :hash)
  ("/" skk     "/")
  ("1" skkserv "/"))

;;; Section 3: Internal state

(defvar nskk--program-dict-cache nil
  "Session-scoped LRU cache for program dictionary results.
Initialized lazily on first call to `nskk-program-dict-lookup'.
Keyed by reading string; values are deduplicated candidate lists.")

(defconst nskk--program-dict-cache-capacity 256
  "Maximum number of entries kept in `nskk--program-dict-cache'.")

(defun nskk--program-dict-ensure-cache ()
  "Return the program dict LRU cache, creating it lazily when needed."
  (unless nskk--program-dict-cache
    (setq nskk--program-dict-cache
          (nskk-cache-create :type 'lru :capacity nskk--program-dict-cache-capacity)))
  nskk--program-dict-cache)

;;; Section 4: Command string parsing

(defun nskk--program-dict-build-call (cmd key)
  "Derive (PROGRAM STDIN-P . ARGS) from command template CMD and KEY.

CMD is tokenized by `split-string-and-unquote'.  The literal token \"%s\"
is replaced by KEY as a separate list element -- safe for readings that
contain spaces or shell metacharacters because KEY is never interpolated
into a shell string; it is passed directly to `call-process' as an
argument element.

Returns (PROGRAM STDIN-P . ARGS) where:
  PROGRAM  -- executable name (first token)
  STDIN-P  -- t when no \"%s\" token was found (reading via stdin)
  ARGS     -- remaining argument strings (possibly empty)"
  (let* ((tokens          (split-string-and-unquote cmd))
         (has-placeholder (member "%s" tokens)))
    (if has-placeholder
        (let ((subst (mapcar (lambda (tok) (if (string= tok "%s") key tok))
                             tokens)))
          (cons (car subst) (cons nil (cdr subst))))
      (cons (car tokens) (cons t (cdr tokens))))))

;;; Section 5: Annotation stripping

(defun nskk--program-dict-strip-annotation (candidate)
  "Strip the annotation from CANDIDATE, returning the bare word.
Annotations have the form \"word;note\"; this function returns \"word\".
When no semicolon is present CANDIDATE is returned unchanged."
  (let ((semi (string-search ";" candidate)))
    (if semi (substring candidate 0 semi) candidate)))

;;; Section 6: External command execution

(defun/k nskk--program-dict-exec-command (program stdin-key args)
  "Execute PROGRAM, piping STDIN-KEY via stdin when non-nil, else using ARGS.

When STDIN-KEY is non-nil, inserts STDIN-KEY followed by a newline into a
temporary buffer and pipes it to PROGRAM via `call-process-region' (the
input region is deleted so only stdout remains).  When STDIN-KEY is nil,
PROGRAM is invoked directly via `call-process' with ARGS as argv.

The call is guarded by `with-timeout' using `nskk-program-dict-timeout'.
Any error or timeout calls on-not-found.  Calls on-found with stdout string."
  (let ((output
         (condition-case err
             (with-timeout (nskk-program-dict-timeout nil)
               (with-temp-buffer
                 (if stdin-key
                     (progn
                       (insert stdin-key "\n")
                       (apply #'call-process-region
                              (point-min) (point-max)
                              program
                              t    ; delete input region (stdin piped)
                              t    ; stdout -> current buffer
                              nil  ; no display
                              args))
                   (apply #'call-process program nil t nil args))
                 (buffer-string)))
           (error
            (nskk-debug-message "nskk-program-dict: command %s error: %s"
                                program (error-message-string err))
            nil))))
    (if output (succeed output) (fail))))

;;; Section 7: Output parsing

(defun/k nskk--program-dict-parse-output (output)
  "Parse stdout OUTPUT from a program dictionary into a candidate list.

Queries `program-dict-output-prefix/3' (Prolog) with the first character
of OUTPUT to detect the output format and its split delimiter:
  \"/\"  (skk)      -> /候補1/候補2/...   delimiter \"/\"
  \"1\"  (skkserv)  -> 1/候補1/候補2/...  delimiter \"/\"
  other            -> one-per-line fallback, delimiter \"\\n\"

For recognized formats the leading prefix character is stripped before
splitting.  Annotations (\"word;note\") are stripped via
`nskk--program-dict-strip-annotation'.  Empty and whitespace-only parts
are discarded.

Calls on-found with the candidate list; on-not-found when OUTPUT is nil,
empty, or yields no valid candidates."
  (if (or (not (stringp output)) (string-empty-p output))
      (fail)
    (let* ((trimmed    (string-trim-right output))
           (first-char (if (string-empty-p trimmed) "" (substring trimmed 0 1)))
           ;; Prolog resolves the split delimiter; nil means unknown format.
           (delimiter  (nskk-prolog-query-value
                        `(program-dict-output-prefix ,first-char \?_ \?d) '\?d))
           ;; Known formats: strip the leading prefix char before splitting.
           (body       (if delimiter (substring trimmed 1) trimmed))
           (candidates (thread-last
                         (split-string body (or delimiter "\n") t)
                         (mapcar #'string-trim)
                         (mapcar #'nskk--program-dict-strip-annotation)
                         (delq nil))))
      (if candidates (succeed candidates) (fail)))))

;;; Section 8: Elisp function entry

(defun/k nskk--program-dict-call-function (fn key)
  "Call Emacs Lisp function FN with KEY as the reading argument.
FN must return a list of candidate strings or nil.  Any error signalled
by FN is caught, logged via `nskk-debug-message', and treated as a miss.

Calls on-found with the non-empty candidate list; on-not-found when FN
returns nil, a non-list value, an empty list, or signals an error."
  (let ((result (condition-case err
                    (funcall fn key)
                  (error
                   (nskk-debug-message "nskk-program-dict: function error: %s"
                                       (error-message-string err))
                   nil))))
    (if (consp result) (succeed result) (fail))))

;;; Section 9: Shell command entry

(defun/k nskk--program-dict-call-command (cmd key)
  "Execute shell command CMD looking up reading KEY.
Parses CMD via `nskk--program-dict-build-call', runs the command via
`nskk--program-dict-exec-command/k', then passes stdout to
`nskk--program-dict-parse-output/k' for SKK/skkserv/line parsing.

Calls on-found with candidates; on-not-found on timeout, error, or when
the command produces no parseable candidates."
  (pcase-let* ((`(,program ,stdin-p . ,args) (nskk--program-dict-build-call cmd key)))
    (nskk-debug-message "nskk-program-dict: cmd=%s key=%s stdin=%s" cmd key stdin-p)
    (<- output nskk--program-dict-exec-command program (when stdin-p key) args)
    (<- cands  nskk--program-dict-parse-output output)
    (succeed cands)))

;;; Section 10: Entry dispatch (Prolog-driven)

(defun/k nskk--program-dict-invoke-entry (entry key)
  "Dispatch a single program dictionary ENTRY for reading KEY.
Classifies ENTRY as \\='function (via `functionp') or \\='command (string),
queries the `program-dict-entry-type/2' Prolog table for the handler atom,
and dispatches to the appropriate handler.

Calls on-found with candidates; on-not-found when the entry is
unrecognized or the handler returns no results."
  (pcase (nskk-prolog-query-value
          `(program-dict-entry-type
            ,(if (functionp entry) 'function 'command) \?a)
          '\?a)
    ('call-function (<- cands nskk--program-dict-call-function entry key))
    ('call-command  (<- cands nskk--program-dict-call-command  entry key))
    (_ (fail))))

;;; Section 11: Multi-entry collector

(defun/k nskk--program-dict-collect-all (entries key)
  "Collect and merge candidates from all ENTRIES for reading KEY.
Iterates ENTRIES in list order using the sync wrapper of
`nskk--program-dict-invoke-entry/k'.  Entries that miss are skipped.
Results are merged with `append' and globally deduplicated via `delete-dups'.

Calls on-found with the deduplicated candidate list; on-not-found when all
entries return no candidates."
  (let ((acc (cl-loop for entry in entries
                      for result = (nskk--program-dict-invoke-entry entry key)
                      when result append result into acc
                      finally return acc)))
    (if acc
        (succeed (delete-dups (copy-sequence acc)))
      (fail))))

;;; Section 12: Public API

;;;###autoload
(defun/k nskk-program-dict-lookup (key)
  "Look up KEY across all entries in `nskk-program-dicts'.
Fails immediately when `nskk-program-dict-enable' is nil or
`nskk-program-dicts' is empty.

Cache hit (session-scoped LRU): calls on-found immediately.
Cache miss: delegates to `nskk--program-dict-collect-all/k', stores the
result in the cache (capacity `nskk--program-dict-cache-capacity'), then
calls on-found.

Calls on-found with the merged, deduplicated candidate list; on-not-found
when disabled, `nskk-program-dicts' is nil, or all entries miss."
  (if (not (and nskk-program-dict-enable nskk-program-dicts))
      (fail)
    (let ((cache (nskk--program-dict-ensure-cache)))
      (<-or cached nskk-cache-get cache key
        :found (succeed cached)
        :fail  (progn
                 (<- results nskk--program-dict-collect-all nskk-program-dicts key)
                 (nskk-cache-put cache key results)
                 (succeed results))))))

(provide 'nskk-program-dictionary)

;;; nskk-program-dictionary.el ends here
