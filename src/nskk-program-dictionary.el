;;; nskk-program-dictionary.el --- Program dictionary support for NSKK -*- lexical-binding: t; -*-
;; Copyright (C) 2026 NSKK Contributors
;; Author: takeokunn <bararararatty@gmail.com>
;; Maintainer: takeokunn <bararararatty@gmail.com>
;; URL: https://github.com/takeokunn/nskk.el
;; Keywords: i18n convenience
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
;; Supports three types of program dictionary entries:
;;
;; 1. Built-in dispatch handlers -- selected by AquaSKK-compatible prefix
;;    matching against `nskk-program-dict-dispatch-table'.  Built-in entries:
;;      "today"  -- current date in two formats (YYYY/MM/DD and 年月日)
;;      "now"    -- current time in two formats (HH:MM:SS and 時分秒)
;;      "="      -- arithmetic via `calc-eval' (e.g. "=(32768+64)*1024" → 33619968)
;;    Candidates produced by built-in handlers are marked with the
;;    `nskk-no-learn' text property so they are never persisted to the
;;    personal dictionary (equivalent to AquaSKK SetAvoidStudy).
;;
;; 2. Emacs Lisp functions -- called as (funcall fn reading); must return
;;    a list of candidate strings or nil.
;;
;; 3. Shell commands (strings) -- executed asynchronously via `make-process'.
;;    stdout and stderr use separate pipes; stderr is discarded.  The literal
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
;; - `nskk-program-dict-builtin-lookup' -- CPS lookup against built-in dispatch table
;; - `nskk-program-dict-lookup'         -- CPS lookup across user-defined entries
;;
;; Integration:
;;   Both are called from `nskk-core-search/k' in nskk-henkan.el as fallbacks
;;   after skkserv: dict-lookup → skkserv → builtin-handlers → user-program-dict → fail.
;;
;; Configuration example:
;;   (setq nskk-program-dict-enable t)       ; enables both built-in and user entries
;;   (setq nskk-program-dicts
;;     (list
;;       (lambda (reading) (my-lisp-lookup reading))  ; Elisp function
;;       "/usr/local/bin/my-dict %s"                  ; %s -> reading as arg
;;       "my-stdin-dict"))                             ; reading via stdin
;;; Code:
(require 'cl-lib)

(require 'subr-x) ; string-empty-p, string-trim, string-trim-right, thread-last

(require 'nskk-prolog)

(require 'nskk-cps-macros)

(require 'nskk-cache)

(require 'nskk-debug nil
  t)

(declare-function nskk-debug-message "nskk-debug" (fmt &rest args))

;;; Section 1: Customization
(defgroup
  nskk-program-dict
  nil
  "Program dictionary (プログラム辞書) settings for NSKK."
  :prefix
  "nskk-program-dict-"
  :group
  'nskk)

(defcustom
  nskk-program-dict-enable
  nil
  "When non-nil, enable program dictionaries as a dictionary fallback.
When nil (default), `nskk-program-dict-lookup' fails immediately with zero
overhead regardless of the value of `nskk-program-dicts'.

To enable program dictionary lookup:
  (setq nskk-program-dict-enable t)
  (setq nskk-program-dicts
    (list (lambda (r) (my-lookup r))
          \"/usr/local/bin/my-dict %s\"))"
  :type
  'boolean
  :risky
  t
  :group
  'nskk-program-dict)

(defcustom
  nskk-program-dicts
  nil
  "List of program dictionary entries for NSKK.
Each entry is either a function or a command-template string.  Functions are
called with the reading and must return nil or a proper list of non-empty safe
candidate strings.  Commands run asynchronously with `make-process`; the
whitespace-delimited token \"%s\" becomes a distinct argv element, otherwise
the reading is sent through stdin.  No shell interpolation is performed.

Command stdout is limited to 1 MiB and parsed as SKK, skkserv, or one-candidate-
per-line output.  stderr is discarded separately and never parsed.  A command
that times out, exceeds the output limit, or exits nonzero is treated as a miss.
All entries are tried in order and their results are deduplicated."
  :type
  (quote (repeat (choice function string)))
  :risky
  t
  :group
  (quote nskk-program-dict))

(defcustom
  nskk-program-dict-timeout
  1.0
  "Finite wait budget in seconds for external commands and isolated calculations.
Owned stdout and stderr polls consume this budget in slices of at most 0.1
seconds.  Non-positive, non-numeric, and non-finite values disable polling."
  :type
  (quote number)
  :safe
  (function numberp)
  :group
  (quote nskk-program-dict))

;;; Section 2: Prolog infrastructure
;; Entry type dispatch table.
;; Maps the kind of a program dictionary entry (function or command) to the
;; handler atom used in `nskk--program-dict-invoke-entry/k'.
(nskk-prolog-define-fact-table
  program-dict-entry-type
  (:arity 2 :index :hash)
  (function call-function)
  (command call-command))

;; Output format detection table.
;; Maps the first character of external command stdout to a format symbol
;; and the delimiter used to split the candidate body.
;;   "/"  -> skk:     /候補1/候補2/...   delimiter "/"
;;   "1"  -> skkserv: 1/候補1/候補2/...  delimiter "/"
;; When the first character is not in this table the fallback is "\n".
(nskk-prolog-define-fact-table
  program-dict-output-prefix
  (:arity 3 :index :hash)
  ("/" skk "/")
  ("1" skkserv "/"))

;;; Section 3: Internal state
(defvar nskk--program-dict-cache nil
  "Session-scoped LRU cache for program dictionary results.
Initialized lazily on first call to `nskk-program-dict-lookup'.
Keyed by reading string; values are canonical candidate graphs.")

(defconst
  nskk--program-dict-no-config-snapshot
  (make-symbol "nskk-program-dict-no-config-snapshot")
  "Sentinel denoting that no program dictionary config was snapshotted yet.")

(defvar nskk--program-dict-config-snapshot nskk--program-dict-no-config-snapshot
  "Detached snapshot of the program dictionary configuration.")

(defun nskk--program-dict-config-equal-p (left right)
  "Return non-nil when valid config values LEFT and RIGHT are equivalent.
Comparison is cycle-safe.  List spines and string contents use value
semantics, while function objects retain identity semantics."
  (let ((pending (list (cons left right)))
        (seen (make-hash-table :test #'eq)))
    (catch 'different
      (while
        pending
        (let* ((pair (pop pending))
               (left-value (car pair))
               (right-value (cdr pair)))
          (cond
            ((eq left-value right-value))
            ((and (stringp left-value) (stringp right-value))
              (unless (equal left-value right-value)
                (throw 'different nil)))
            ((or (functionp left-value) (functionp right-value)) (throw 'different nil))
            ((and (consp left-value) (consp right-value))
              (let ((right-values (gethash left-value seen)))
                (unless (and right-values (gethash right-value right-values))
                  (unless right-values
                    (setq right-values (make-hash-table :test #'eq))
                    (puthash left-value right-values seen))
                  (puthash right-value t right-values)
                  (push (cons (car left-value) (car right-value)) pending)
                  (push (cons (cdr left-value) (cdr right-value)) pending))))
            (t (throw 'different nil)))))
      t)))

(defun nskk--program-dict-config-valid-p (config)
  "Return non-nil when CONFIG is a finite proper list of valid entries."
  (let ((cursor config)
        (seen (make-hash-table :test #'eq)))
    (catch 'invalid
      (while
        (consp cursor)
        (when (gethash cursor seen)
          (throw 'invalid nil))
        (puthash cursor t seen)
        (unless (or (functionp (car cursor)) (stringp (car cursor)))
          (throw 'invalid nil))
        (setq cursor (cdr cursor)))
      (if (null cursor) t
        nil))))

(defun nskk--program-dict-sync-config ()
  "Invalidate the cache when `nskk-program-dicts' changed by value.
The replacement snapshot is fully copied before either cache or snapshot
state is modified, so copy errors and quits preserve the old state."
  (unless (and
      (not
        (eq nskk--program-dict-config-snapshot nskk--program-dict-no-config-snapshot))
      (nskk--program-dict-config-equal-p
        nskk-program-dicts
        nskk--program-dict-config-snapshot))
    (let ((snapshot (nskk-prolog-copy-term nskk-program-dicts)))
      (when nskk--program-dict-cache
        (nskk-cache-clear nskk--program-dict-cache))
      (setq nskk--program-dict-config-snapshot snapshot))))

(defconst
  nskk--program-dict-cache-capacity
  256
  "Maximum number of entries kept in nskk--program-dict-cache.")

(defconst
  nskk--program-dict-max-output-size
  (* 1024 1024)
  "Maximum number of output bytes accepted from a program dictionary.")

(defconst
  nskk--program-dict-max-calculation-size
  4096
  "Maximum bytes accepted for a calculation expression or output stream.")

(defun nskk--program-dict-ensure-cache ()
  "Return the program dict LRU cache, creating it lazily when needed."
  (unless nskk--program-dict-cache
    (setq nskk--program-dict-cache (nskk-cache-create :type 'lru :capacity nskk--program-dict-cache-capacity)))
  nskk--program-dict-cache)

;;; Section 4: Command string parsing
(defun nskk--program-dict-build-call (cmd key)
  "Derive (PROGRAM STDIN-P . ARGS) from command template CMD and KEY.

CMD is tokenized by `split-string-and-unquote`.  The literal token \"%s\"
is replaced by KEY as a separate list element -- safe for readings that
contain spaces or shell metacharacters because KEY is never interpolated
into a shell string; it is passed directly to `make-process` as an argument
element.

Returns (PROGRAM STDIN-P . ARGS) where:
  PROGRAM  -- executable name (first token)
  STDIN-P  -- t when no \"%s\" token was found (reading via stdin)
  ARGS     -- remaining argument strings (possibly empty)"
  (let* ((tokens (split-string-and-unquote cmd))
         (has-placeholder (member "%s" tokens)))
    (if has-placeholder (let ((subst
            (mapcar
              (lambda (tok)
                (if (string= tok "%s") key
                  tok))
              tokens)))
        (cons (car subst) (cons nil (cdr subst))))
      (cons (car tokens) (cons t (cdr tokens))))))

;;; Section 5: Annotation stripping
(defun nskk--program-dict-strip-annotation (candidate)
  "Strip the annotation from CANDIDATE, returning the bare word.
Annotations have the form \"word;note\"; this function returns \"word\".
When no semicolon is present CANDIDATE is returned unchanged."
  (let ((semi (string-search ";" candidate)))
    (if semi (substring candidate 0 semi)
      candidate)))

;;; Section 6: External command execution
(defun nskk--program-dict-stop-process-group (process)
  "Stop PROCESS and, when safely identifiable, its descendant process group.
Local pipe subprocesses normally lead a process group whose ID equals their
PID.  Signal that group only after verifying this invariant from OS process
attributes.  Platforms without process-group information or signalling fall
back to deleting only the direct process."
  (when (processp process)
    (let* ((pid (process-id process))
           (attributes
          (and (integerp pid) (> pid 0) (ignore-errors (process-attributes pid))))
           (process-group (alist-get (quote pgrp) attributes)))
      (when (and (integerp process-group) (= process-group pid))
        (ignore-errors (signal-process (- process-group) (quote SIGKILL))))
      (when (process-live-p process)
        (ignore-errors (delete-process process))))))

(defun/k
  nskk--program-dict-exec-command
  (program stdin-key args)
  "Execute PROGRAM asynchronously with ARGS and optional STDIN-KEY.

STDIN-KEY, when non-nil, is sent followed by a newline.  Standard input is
then closed so commands waiting for EOF can terminate.  Execution is limited
by `nskk-program-dict-timeout', and stdout and stderr are each limited to
`nskk--program-dict-max-output-size' bytes.  Standard error is discarded.
Calls on-found with stdout only when PROGRAM exits successfully; otherwise
calls on-not-found."
  (let ((output-buffer nil)
        (stderr-buffer nil)
        (stderr-process nil)
        (process nil)
        (output-bytes 0)
        (stderr-bytes 0)
        (overflow nil)
        (finished nil)
        (timed-out nil)
        (output nil))
    (unwind-protect (condition-case
        err
        (progn
          (setq output-buffer (generate-new-buffer " *nskk-program-dict-output*")
                stderr-buffer (generate-new-buffer " *nskk-program-dict-stderr*"))
          (let ((remaining-wait nskk-program-dict-timeout))
            (setq stderr-process (make-pipe-process
                :name
                "nskk-program-dict-stderr"
                :buffer
                stderr-buffer
                :coding
                'utf-8-unix
                :noquery
                t
                :filter
                (lambda (_stderr chunk)
                  (setq stderr-bytes (+ stderr-bytes (string-bytes chunk)))
                  (when (> stderr-bytes nskk--program-dict-max-output-size)
                    (setq overflow t)
                    (nskk--program-dict-stop-process-group process)))
                :sentinel
                #'ignore))
            (setq process (make-process
                :name
                "nskk-program-dict"
                :buffer
                output-buffer
                :stderr
                stderr-process
                :command
                (cons program args)
                :connection-type
                'pipe
                :coding
                'utf-8-unix
                :noquery
                t
                :sentinel
                (lambda (_process _event)
                  (setq finished t))
                :filter
                (lambda (_process chunk)
                  (setq output-bytes (+ output-bytes (string-bytes chunk)))
                  (if (> output-bytes nskk--program-dict-max-output-size) (progn
                      (setq overflow t)
                      (nskk--program-dict-stop-process-group process))
                    (when (buffer-live-p output-buffer)
                      (with-current-buffer output-buffer (insert chunk)))))))
            (when (process-live-p process)
              (when stdin-key
                (process-send-string process (concat stdin-key "\n")))
              (process-send-eof process))
            (while
              (and
                (numberp remaining-wait)
                (> remaining-wait 0)
                (= (- remaining-wait remaining-wait) 0)
                (not finished)
                (not (memq (process-status process) '(exit signal closed failed)))
                (not overflow))
              (dolist (wait-process (list process stderr-process))
                (when (and (> remaining-wait 0) (not finished) (not overflow))
                  (let ((slice (min 0.1 remaining-wait)))
                    (setq remaining-wait (max 0 (- remaining-wait slice)))
                    (accept-process-output wait-process slice nil t)))))
            (when (memq (process-status process) '(exit signal closed failed))
              (setq finished t))
            (when (and
                finished
                (not overflow)
                (numberp remaining-wait)
                (> remaining-wait 0)
                (= (- remaining-wait remaining-wait) 0))
              (let ((previous-output-bytes output-bytes)
                    (previous-stderr-bytes stderr-bytes)
                    (quiet-rounds 0))
                (while
                  (and (not overflow) (> remaining-wait 0) (< quiet-rounds 2))
                  (let ((slice (min 0.1 remaining-wait)))
                    (setq remaining-wait (max 0 (- remaining-wait slice)))
                    (accept-process-output process slice nil t)
                    (accept-process-output nil 0)
                    (if (and
                        (= previous-output-bytes output-bytes)
                        (= previous-stderr-bytes stderr-bytes)) (setq quiet-rounds (1+ quiet-rounds))
                      (setq quiet-rounds 0))
                    (setq previous-output-bytes output-bytes
                          previous-stderr-bytes stderr-bytes)))))
            (setq timed-out (and (not finished) (not overflow)))
            (cond
              (overflow
                (nskk-debug-message
                  "nskk-program-dict: command %s exceeded output limit"
                  program))
              (timed-out
                (nskk-debug-message "nskk-program-dict: command %s timed out" program))
              ((not
                  (and (eq (process-status process) 'exit) (zerop (process-exit-status process))))
                (nskk-debug-message
                  "nskk-program-dict: command %s exited unsuccessfully"
                  program))
              (t
                (setq output (with-current-buffer output-buffer (buffer-string)))))))
        (error
          (nskk-debug-message
            "nskk-program-dict: command %s error: %s"
            program
            (error-message-string err))))
      (condition-case
        nil
        (nskk--program-dict-stop-process-group process)
        ((error quit) nil))
      (condition-case
        nil
        (when (processp stderr-process)
          (delete-process stderr-process))
        ((error quit) nil))
      (condition-case
        nil
        (when (buffer-live-p output-buffer)
          (kill-buffer output-buffer))
        ((error quit) nil))
      (condition-case
        nil
        (when (buffer-live-p stderr-buffer)
          (kill-buffer stderr-buffer))
        ((error quit) nil)))
    (if output (succeed output)
      (fail))))

;;; Section 7: Output parsing
(defun/k
  nskk--program-dict-parse-output
  (output)
  "Parse stdout OUTPUT from a program dictionary into a candidate list.

Queries `program-dict-output-prefix/3` (Prolog) with the first character
of OUTPUT to detect the output format and its split delimiter:
  \"/\"  (skk)      -> /候補1/候補2/...   delimiter \"/\"
  \"1\"  (skkserv)  -> 1/候補1/候補2/...  delimiter \"/\"
  other            -> one-per-line fallback, delimiter \"\\n\"

For recognized formats the leading prefix character is stripped before
splitting.  Annotations (\"word;note\") are stripped via
`nskk--program-dict-strip-annotation`.  Empty parts and candidates containing
control characters are discarded before and after annotation stripping.

Calls on-found with the proper, non-empty candidate list; on-not-found when
OUTPUT is nil, empty, or yields no valid candidates."
  (if (or (not (stringp output)) (string-empty-p output)) (fail)
    (let* ((trimmed (string-trim-right output))
           (first-char
          (if (string-empty-p trimmed) ""
            (substring trimmed 0 1)))
           (delimiter
          (nskk-prolog-query-value
            `(program-dict-output-prefix ,first-char \?_ \?d)
            (quote \?d)))
           (body
          (if delimiter (substring trimmed 1)
            trimmed))
           (candidates
          (thread-last
            (split-string body (or delimiter "\n") t)
            (mapcar (function string-trim))
            (cl-remove-if-not (function nskk--program-dict-valid-function-candidate-p))
            (mapcar (function nskk--program-dict-strip-annotation))
            (cl-remove-if-not (function nskk--program-dict-valid-function-candidate-p)))))
      (if (and (consp candidates) (proper-list-p candidates)) (succeed candidates)
        (fail)))))

;;; Section 8: Elisp function entry
(defun nskk--program-dict-valid-function-candidate-p (candidate)
  "Return non-nil when CANDIDATE is safe program dictionary output."
  (and
    (stringp candidate)
    (not (string-empty-p candidate))
    (cl-every
      (lambda (char)
        (or (and (>= char 32) (< char 127)) (> char 159)))
      candidate)))

(defun/k
  nskk--program-dict-call-function
  (fn key)
  "Call Emacs Lisp function FN with KEY as the reading argument.
FN must return a proper, non-empty list of non-empty candidate strings.
Candidates containing control characters are rejected.  Any error signalled
by FN is caught, logged via `nskk-debug-message', and treated as a miss.

Calls on-found with a validated candidate list; otherwise calls on-not-found."
  (let ((result
        (condition-case
          err
          (funcall fn key)
          (error
            (nskk-debug-message
              "nskk-program-dict: function error: %s"
              (error-message-string err))
            nil))))
    (if (and
        (consp result)
        (proper-list-p result)
        (cl-every (function nskk--program-dict-valid-function-candidate-p) result)) (succeed result)
      (fail))))

;;; Section 9: Shell command entry
(defun/k
  nskk--program-dict-call-command
  (cmd key)
  "Execute shell command CMD looking up reading KEY.
Parses CMD via `nskk--program-dict-build-call', runs the command via
`nskk--program-dict-exec-command/k', then passes stdout to
`nskk--program-dict-parse-output/k' for SKK/skkserv/line parsing.

Calls on-found with candidates; on-not-found on timeout, error, or when
the command produces no parseable candidates."
  (pcase-let*
    ((`(,program ,stdin-p . ,args) (nskk--program-dict-build-call cmd key)))
    (nskk-debug-message "nskk-program-dict: cmd=%s key=%s stdin=%s" cmd key stdin-p)
    (<-
      output
      nskk--program-dict-exec-command
      program
      (when stdin-p
        key)
      args)
    (<- cands nskk--program-dict-parse-output output)
    (succeed cands)))

;;; Section 10: Entry dispatch (Prolog-driven)
(defun/k
  nskk--program-dict-invoke-entry
  (entry key)
  "Dispatch a single program dictionary ENTRY for reading KEY.
Classifies ENTRY as \\='function (via `functionp') or \\='command (string),
queries the `program-dict-entry-type/2' Prolog table for the handler atom,
and dispatches to the appropriate handler.

Calls on-found with candidates; on-not-found when the entry is
unrecognized or the handler returns no results."
  (pcase
    (nskk-prolog-query-value
      `(program-dict-entry-type
        ,(if (functionp entry) 'function
          'command)
        \?a)
      '\?a)
    ('call-function (<- cands nskk--program-dict-call-function entry key))
    ('call-command (<- cands nskk--program-dict-call-command entry key))
    (_ (fail))))

;;; Section 11: Multi-entry collector
(defun nskk--program-dict-merge-candidate-lists (candidate-lists)
  "Merge CANDIDATE-LISTS, preserving first-seen order and uniqueness.
Candidates are compared with `equal' in expected linear time.  The returned
list has a fresh spine; CANDIDATE-LISTS and its member lists are not modified."
  (let ((seen (make-hash-table :test #'equal))
        unique)
    (dolist (candidates candidate-lists (nreverse unique))
      (dolist (candidate candidates)
        (unless (gethash candidate seen)
          (puthash candidate t seen)
          (push candidate unique))))))

(defun/k
  nskk--program-dict-collect-all
  (entries key)
  "Collect and merge candidates from all ENTRIES for reading KEY.
Iterates ENTRIES in list order using the sync wrapper of
`nskk--program-dict-invoke-entry/k'.  Entries that miss are skipped.  Results
are stably deduplicated with `equal' without modifying entry-owned lists.

Calls on-found with the deduplicated candidate list; on-not-found when all
entries return no candidates."
  (let (candidate-lists)
    (dolist (entry entries)
      (let ((result (nskk--program-dict-invoke-entry entry key)))
        (when result
          (push result candidate-lists))))
    (if candidate-lists (succeed (nskk--program-dict-merge-candidate-lists (nreverse candidate-lists)))
      (fail))))

;;; Section 12: Public API
(progn
  (defun nskk--program-dict-copy-graph (object)
    "Return a detached copy of OBJECT suitable for program dictionary caches.
Conses, vectors, hash-table keys and values, strings, and string text-property
values are copied recursively.  Cycles and shared subgraphs are preserved.
Functions and other atoms are retained as leaves."
    (let ((missing (make-symbol "nskk-program-dict-copy-missing"))
          (memo (make-hash-table :test (function eq)))
          (pending (list object))
          non-hash
          hashes)
      (while
        pending
        (let ((current (pop pending)))
          (when (eq (gethash current memo missing) missing)
            (cond
              ((functionp current))
              ((consp current)
                (puthash current (cons nil nil) memo)
                (push current non-hash)
                (push (car current) pending)
                (push (cdr current) pending))
              ((hash-table-p current)
                (puthash
                  current
                  (make-hash-table
                    :test
                    (hash-table-test current)
                    :size
                    (max 1 (hash-table-size current))
                    :rehash-size
                    (hash-table-rehash-size current)
                    :rehash-threshold
                    (hash-table-rehash-threshold current)
                    :weakness
                    (hash-table-weakness current))
                  memo)
                (push current hashes)
                (maphash
                  (lambda (key value)
                    (push key pending)
                    (push value pending))
                  current))
              ((stringp current)
                (puthash current (substring-no-properties current) memo)
                (push current non-hash)
                (let ((position 0)
                      (limit (length current)))
                  (while
                    (< position limit)
                    (let ((properties (text-properties-at position current)))
                      (while properties (pop properties) (push (pop properties) pending)))
                    (setq position (next-property-change position current limit)))))
              ((vectorp current)
                (puthash current (make-vector (length current) nil) memo)
                (push current non-hash)
                (dotimes (index (length current))
                  (push (aref current index) pending)))))))
      (let ((copy-of
            (lambda (value)
              (gethash value memo value))))
        (dolist (current non-hash)
          (let ((copy (gethash current memo)))
            (cond
              ((consp current)
                (setcar copy (funcall copy-of (car current)))
                (setcdr copy (funcall copy-of (cdr current))))
              ((stringp current)
                (let ((position 0)
                      (limit (length current)))
                  (while
                    (< position limit)
                    (let ((next (next-property-change position current limit))
                          (properties (text-properties-at position current))
                          copied-properties)
                      (while
                        properties
                        (let ((property (pop properties))
                              (value (pop properties)))
                          (setq copied-properties (nconc copied-properties (list property (funcall copy-of value))))))
                      (add-text-properties position next copied-properties copy)
                      (setq position next)))))
              ((vectorp current)
                (dotimes (index (length current))
                  (aset copy index (funcall copy-of (aref current index))))))))
        (dolist (current hashes)
          (let ((copy (gethash current memo)))
            (maphash
              (lambda (key value)
                (puthash (funcall copy-of key) (funcall copy-of value) copy))
              current)))
        (funcall copy-of object))))
  (defun nskk--program-dict-mark-no-learn (candidates)
    "Mark all strings reachable from CANDIDATES as non-persistable.
Conses, vectors, hash-table keys and values, and string text-property values
are traversed without looping on cyclic or shared graphs.  Existing text
properties are retained and `nskk-no-learn' is overwritten with exactly t."
    (let ((seen (make-hash-table :test (function eq)))
          (pending (list candidates)))
      (while
        pending
        (let ((current (pop pending)))
          (unless (or (functionp current) (gethash current seen))
            (cond
              ((stringp current)
                (puthash current t seen)
                (add-text-properties 0 (length current) (list 'nskk-no-learn t) current)
                (let ((position 0)
                      (limit (length current)))
                  (while
                    (< position limit)
                    (let ((properties (text-properties-at position current)))
                      (while properties (pop properties) (push (pop properties) pending)))
                    (setq position (next-property-change position current limit)))))
              ((consp current)
                (puthash current t seen)
                (push (car current) pending)
                (push (cdr current) pending))
              ((hash-table-p current)
                (puthash current t seen)
                (maphash
                  (lambda (key value)
                    (push key pending)
                    (push value pending))
                  current))
              ((vectorp current)
                (puthash current t seen)
                (dotimes (index (length current))
                  (push (aref current index) pending)))))))
      candidates)))

(progn
  (defun nskk--program-dict-cache-observation-state (cache)
    "Return CACHE metadata needed to undo a failed miss observation."
    (cond
      ((nskk-cache-lru-p cache)
        (vector
          'lru
          (nskk-cache-lru-capacity cache)
          (nskk-cache-lru-size cache)
          (nskk-cache-lru-hash cache)
          (nskk-cache-lru-head cache)
          (nskk-cache-lru-tail cache)
          (nskk-cache-lru-hits cache)
          (nskk-cache-lru-misses cache)))
      ((nskk-cache-lfu-p cache)
        (vector
          'lfu
          (nskk-cache-lfu-capacity cache)
          (nskk-cache-lfu-size cache)
          (nskk-cache-lfu-hash cache)
          (nskk-cache-lfu-freq cache)
          (nskk-cache-lfu-min-freq cache)
          (nskk-cache-lfu-hits cache)
          (nskk-cache-lfu-misses cache)))))
  (defun nskk--program-dict-restore-cache-observation-state (cache state)
    "Restore CACHE metadata from observation STATE."
    (pcase
      (aref state 0)
      ('lru
        (setf (nskk-cache-lru-capacity cache) (aref state 1)
              (nskk-cache-lru-size cache) (aref state 2)
              (nskk-cache-lru-hash cache) (aref state 3)
              (nskk-cache-lru-head cache) (aref state 4)
              (nskk-cache-lru-tail cache) (aref state 5)
              (nskk-cache-lru-hits cache) (aref state 6)
              (nskk-cache-lru-misses cache) (aref state 7)))
      ('lfu
        (setf (nskk-cache-lfu-capacity cache) (aref state 1)
              (nskk-cache-lfu-size cache) (aref state 2)
              (nskk-cache-lfu-hash cache) (aref state 3)
              (nskk-cache-lfu-freq cache) (aref state 4)
              (nskk-cache-lfu-min-freq cache) (aref state 5)
              (nskk-cache-lfu-hits cache) (aref state 6)
              (nskk-cache-lfu-misses cache) (aref state 7)))))
  (progn
    (defun nskk-program-dict-lookup/k (key on-found on-not-found)
      "Look up KEY across configured program dictionaries in CPS style.
Cache hits return detached public graphs.  Cache misses build a detached
canonical graph, mark it, prepare detached public and key graphs, publish
atomically, and only then invoke ON-FOUND.  Errors and quits before publication
restore the exact pre-observation cache state."
      (nskk--program-dict-sync-config)
      (if (and
          nskk-program-dict-enable
          nskk-program-dicts
          (nskk--program-dict-config-valid-p nskk-program-dicts)) (let* ((cache (nskk--program-dict-ensure-cache))
               (observation-state (nskk--program-dict-cache-observation-state cache)))
          (nskk-cache-get-prepared/k
            cache
            key
            (function nskk--program-dict-copy-graph)
            (lambda (public)
              (funcall on-found public))
            (lambda ()
              (let ((committed nil))
                (condition-case
                  condition
                  (nskk--program-dict-collect-all/k
                    nskk-program-dicts
                    key
                    (lambda (results)
                      (condition-case
                        condition
                        (let* ((canonical
                              (nskk--program-dict-mark-no-learn (nskk--program-dict-copy-graph results)))
                               (public (nskk--program-dict-copy-graph canonical))
                               (owned-key (nskk--program-dict-copy-graph key)))
                          (nskk-cache-put cache owned-key canonical)
                          (setq committed t)
                          (funcall on-found public))
                        ((error quit)
                          (unless committed
                            (nskk--program-dict-restore-cache-observation-state cache observation-state))
                          (signal (car condition) (cdr condition)))))
                    on-not-found)
                  ((error quit)
                    (unless committed
                      (nskk--program-dict-restore-cache-observation-state cache observation-state))
                    (signal (car condition) (cdr condition))))))))
        (funcall on-not-found)))
    (defun nskk-program-dict-lookup (key)
      "Synchronously look up KEY across configured program dictionaries."
      (nskk-program-dict-lookup/k key (function identity) (function ignore)))
    (put
      (quote nskk-program-dict-lookup/k)
      (quote nskk--cps-continuation-pattern)
      :found-not-found)))

;;; Section 13: Built-in dispatch table
(defcustom
  nskk-program-dict-dispatch-table
  (list
    (cons "today" #'nskk--program-dict-today)
    (cons "now" #'nskk--program-dict-now)
    (cons "=" #'nskk--program-dict-calculate))
  "Built-in program dictionary dispatch table (AquaSKK DispatchTable equivalent).
Each entry is (PREFIX . HANDLER-FUNCTION) where PREFIX is matched against
the reading with `string-prefix-p' and HANDLER-FUNCTION receives the full
reading string and returns a list of candidate strings, or nil.

Users may prepend custom entries:
  (push (cons \"prefix\" #\\='my-handler) nskk-program-dict-dispatch-table)

Built-in entries:
  \"today\" -- current date in two formats (AquaSKK today handler equivalent)
  \"now\"   -- current time in two formats (AquaSKK now handler equivalent)
  \"=\"     -- arithmetic via `calc-eval' (AquaSKK calculate handler equivalent)

This table is consulted only when `nskk-program-dict-enable' is non-nil.
Candidates produced by built-in handlers are marked with the `nskk-no-learn'
text property so they are never persisted to the personal dictionary."
  :type
  '(repeat (cons (string :tag "Prefix") (function :tag "Handler")))
  :group
  'nskk-program-dict)

;;; Section 14: Built-in handlers
(defun nskk--program-dict-today (_key)
  "Return current date as a candidate list.
Equivalent to the AquaSKK \\='today\\=' handler.
Returns two candidates:
  1. \"YYYY/MM/DD(WeekAbbrev)\"     e.g. \"2026/03/15(Sun)\"
  2. \"YYYY年MM月DD日(WeekKanji)\"  e.g. \"2026年03月15日(日)\"
_KEY is ignored; the current system date is always used."
  (let* ((now (decode-time))
         (year (decoded-time-year now))
         (month (decoded-time-month now))
         (day (decoded-time-day now))
         (wday (decoded-time-weekday now))
         (abbrev '("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))
         (kanji '("日" "月" "火" "水" "木" "金" "土")))
    (list
      (format "%04d/%02d/%02d(%s)" year month day (nth wday abbrev))
      (format "%04d年%02d月%02d日(%s)" year month day (nth wday kanji)))))

(defun nskk--program-dict-now (_key)
  "Return current time as a candidate list.
Equivalent to the AquaSKK \\='now\\=' handler.
Returns two candidates:
  1. \"HH:MM:SS\"      e.g. \"14:30:00\"
  2. \"HH時MM分SS秒\"   e.g. \"14時30分00秒\"
_KEY is ignored; the current system time is always used."
  (let* ((now (decode-time))
         (hour (decoded-time-hour now))
         (minute (decoded-time-minute now))
         (second (truncate (decoded-time-second now))))
    (list
      (format "%02d:%02d:%02d" hour minute second)
      (format "%02d時%02d分%02d秒" hour minute second))))

(defun nskk--program-dict-read-calculation-result (output)
  "Read one safe calculation result from subprocess OUTPUT.
Return nil unless OUTPUT contains exactly one non-empty candidate string."
  (condition-case nil
      (unless (string-match-p "\\`[[:space:]]*#[0-9]+[=#]" output)
        (let* ((parsed (read-from-string output))
               (candidate (car parsed))
               (remainder (substring output (cdr parsed))))
          (when (and (string-empty-p (string-trim remainder))
                     (nskk--program-dict-valid-function-candidate-p candidate))
            candidate)))
    (error nil)))

(defun nskk--program-dict-run-calculation (expr)
  "Evaluate EXPR in an isolated Emacs process and return one safe string.
Polling and post-exit draining share one absolute deadline.  Each stream is
capped at `nskk--program-dict-max-calculation-size' bytes."
  (let ((stdout-buffer nil)
        (stderr-buffer nil)
        (stderr-process nil)
        (process nil)
        (stdout-bytes 0)
        (stderr-bytes 0)
        (overflow nil)
        (finished nil)
        (timed-out nil)
        (output nil)
        (form nil))
    (unwind-protect (condition-case
        err
        (progn
          (setq stdout-buffer (generate-new-buffer " *nskk-program-dict-calc-output*")
                stderr-buffer (generate-new-buffer " *nskk-program-dict-calc-stderr*")
                form (prin1-to-string
              `(condition-case
                err
                (let* ((raw (calc-eval ,expr))
                       (result
                      (cond
                        ((stringp raw) raw)
                        ((and (consp raw) (stringp (cadr raw))) (cadr raw))
                        (t nil))))
                  (if result (prin1 result)
                    (kill-emacs 2)))
                (error (prin1 (error-message-string err))))))
          (let* ((clock-hz 1000000000)
                 (valid-timeout
                (and
                  (numberp nskk-program-dict-timeout)
                  (> nskk-program-dict-timeout 0)
                  (= (- nskk-program-dict-timeout nskk-program-dict-timeout) 0)))
                 (deadline
                (and
                  valid-timeout
                  (+
                    (car (time-convert nil clock-hz))
                    (truncate (* nskk-program-dict-timeout clock-hz)))))
                 (remaining-time
                (lambda ()
                  (max 0 (/ (- deadline (car (time-convert nil clock-hz))) (float clock-hz)))))
                 (remaining-wait (and valid-timeout (funcall remaining-time))))
            (setq stderr-process (make-pipe-process
                :name
                "nskk-program-dict-calc-stderr"
                :buffer
                stderr-buffer
                :coding
                'utf-8-unix
                :noquery
                t
                :sentinel
                #'ignore
                :filter
                (lambda (_stderr chunk)
                  (setq stderr-bytes (+ stderr-bytes (string-bytes chunk)))
                  (when (> stderr-bytes nskk--program-dict-max-calculation-size)
                    (setq overflow t)
                    (nskk--program-dict-stop-process-group process)))))
            (setq process (make-process
                :name
                "nskk-program-dict-calc"
                :buffer
                stdout-buffer
                :stderr
                stderr-process
                :command
                (list
                  (expand-file-name invocation-name invocation-directory)
                  "--quick"
                  "--batch"
                  "--eval"
                  form)
                :connection-type
                'pipe
                :coding
                'utf-8-unix
                :noquery
                t
                :sentinel
                (lambda (_process _event)
                  (setq finished t))
                :filter
                (lambda (_process chunk)
                  (setq stdout-bytes (+ stdout-bytes (string-bytes chunk)))
                  (if (> stdout-bytes nskk--program-dict-max-calculation-size) (progn
                      (setq overflow t)
                      (nskk--program-dict-stop-process-group process))
                    (when (buffer-live-p stdout-buffer)
                      (with-current-buffer stdout-buffer (insert chunk)))))))
            (while
              (and
                valid-timeout
                (setq remaining-wait (funcall remaining-time))
                (> remaining-wait 0)
                (not finished)
                (not overflow))
              (dolist (wait-process (list process stderr-process))
                (when (and
                    (setq remaining-wait (funcall remaining-time))
                    (> remaining-wait 0)
                    (not finished)
                    (not overflow))
                  (accept-process-output wait-process (min 0.1 remaining-wait) nil t))))
            (when (and
                finished
                (not overflow)
                valid-timeout
                (setq remaining-wait (funcall remaining-time))
                (> remaining-wait 0))
              (let ((previous-stdout-bytes stdout-bytes)
                    (previous-stderr-bytes stderr-bytes)
                    (quiet-rounds 0))
                (while
                  (and
                    (not overflow)
                    (setq remaining-wait (funcall remaining-time))
                    (> remaining-wait 0)
                    (< quiet-rounds 2))
                  (accept-process-output process (min 0.1 remaining-wait) nil t)
                  (accept-process-output nil 0)
                  (if (and
                      (= previous-stdout-bytes stdout-bytes)
                      (= previous-stderr-bytes stderr-bytes)) (setq quiet-rounds (1+ quiet-rounds))
                    (setq quiet-rounds 0))
                  (setq previous-stdout-bytes stdout-bytes
                        previous-stderr-bytes stderr-bytes))))
            (setq timed-out (and (not finished) (not overflow)))
            (when (and
                (not overflow)
                (not timed-out)
                (eq (process-status process) 'exit)
                (zerop (process-exit-status process)))
              (setq output (with-current-buffer stdout-buffer (buffer-string))))))
        (error
          (nskk-debug-message
            "nskk-program-dict: calculation process error: %s"
            (error-message-string err))))
      (condition-case
        nil
        (nskk--program-dict-stop-process-group process)
        ((error quit) nil))
      (condition-case
        nil
        (when (processp stderr-process)
          (delete-process stderr-process))
        ((error quit) nil))
      (condition-case
        nil
        (when (buffer-live-p stdout-buffer)
          (kill-buffer stdout-buffer))
        ((error quit) nil))
      (condition-case
        nil
        (when (buffer-live-p stderr-buffer)
          (kill-buffer stderr-buffer))
        ((error quit) nil)))
    (when output
      (nskk--program-dict-read-calculation-result output))))

(defun nskk--program-dict-calculate (key)
  "Evaluate arithmetic expression in KEY (prefixed with `=') via calc.
Return a single-element candidate list for a result or evaluation error.
Return nil when the isolated calculation process fails."
  (let ((expr (substring key 1)))
    (when (<= (string-bytes expr) nskk--program-dict-max-calculation-size)
      (let ((result (nskk--program-dict-run-calculation expr)))
        (when (stringp result)
          (list result))))))

;;; Section 15: Built-in lookup public API
(defun/k
  nskk-program-dict-builtin-lookup
  (key)
  "Look up KEY using built-in prefix handlers.

Only active when `nskk-program-dict-enable' is non-nil and KEY is a string.
Walks `nskk-program-dict-dispatch-table' in order.  Every matching prefix
handler is invoked under `condition-case'; valid candidate lists are collected,
stably deduplicated by `equal', and returned with the text property
`nskk-no-learn' set to t on every candidate.  Handler errors or malformed
returns are logged and skipped."
  (if (and (stringp key) nskk-program-dict-enable) (let (candidate-lists)
      (dolist (pair nskk-program-dict-dispatch-table)
        (when (string-prefix-p (car pair) key)
          (let ((cands
                (condition-case
                  err
                  (funcall (cdr pair) key)
                  (error
                    (nskk-debug-message
                      "nskk-program-dict: builtin handler [%s] error: %s"
                      (car pair)
                      (error-message-string err))
                    nil))))
            (cond
              ((and
                  (consp cands)
                  (proper-list-p cands)
                  (cl-every #'nskk--program-dict-valid-function-candidate-p cands))
                (push cands candidate-lists))
              (cands
                (nskk-debug-message
                  "nskk-program-dict: builtin handler [%s] returned invalid candidates"
                  (car pair)))))))
      (if candidate-lists (succeed
          (mapcar
            (lambda (candidate)
              (propertize candidate 'nskk-no-learn t))
            (nskk--program-dict-merge-candidate-lists (nreverse candidate-lists))))
        (fail)))
    (fail)))

(provide 'nskk-program-dictionary)

;;; nskk-program-dictionary.el ends here
