;;; nskk-test-framework.el --- NSKK Test Framework using ERT  -*- lexical-binding: t; -*-
;; Copyright (C) 2026 NSKK Authors
;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: i18n
;; Homepage: https://github.com/takeokunn/nskk.el
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
;; This file provides a comprehensive test framework for NSKK using ERT
;; (Emacs Lisp Regression Testing). It includes test helpers and utilities
;; to support TDD (Test-Driven Development) and PBT (Property-Based
;; Testing) strategies.
;;
;; Features:
;; - ERT-based test framework
;; - Test environment setup/teardown
;;; Code:
(progn
  (defvar nskk-test--persistence-directory
    (file-name-as-directory (make-temp-file "nskk-test-persistence-" t))
    "Temporary root for all persistence performed by the test process.")

  (defun nskk-test--cleanup-persistence-directory ()
    "Remove the test process persistence directory."
    (when (and
           (file-directory-p nskk-test--persistence-directory)
           (file-in-directory-p nskk-test--persistence-directory
                                temporary-file-directory))
      (delete-directory nskk-test--persistence-directory t)))

  (add-hook 'kill-emacs-hook #'nskk-test--cleanup-persistence-directory t))

(setq user-emacs-directory nskk-test--persistence-directory)

(require 'ert)

(require 'subr-x)

(require 'nskk-dictionary)

(require 'nskk-prolog)

(require 'nskk-trie)

(eval-when-compile
  (require 'cl-lib))

(require 'nskk-state)

(require 'nskk-kana)

(require 'nskk-henkan)

(require 'nskk-input)

(require 'nskk-converter)

(require 'nskk-candidate-window)

(require 'nskk-annotation)

(setq nskk-dict-use-ja-dic nil)

(setq nskk-dict-user-dictionary-file (expand-file-name "nskk/jisyo" nskk-test--persistence-directory)
      nskk-search-learning-file (expand-file-name "nskk/learning.dat" nskk-test--persistence-directory)
      nskk-study-file (expand-file-name "nskk/study.dat" nskk-test--persistence-directory))

(nskk-state-initialize-prolog)

(nskk-kana-initialize)

(nskk-henkan-initialize)

(nskk-input-initialize)

(nskk-converter-initialize)

;;;;
;;;; Test Framework Configuration
;;;;
(defgroup
  nskk-test
  nil
  "NSKK test framework configuration."
  :prefix
  "nskk-test-"
  :group
  'nskk)

(defcustom
  nskk-test-verbose
  nil
  "Enable verbose test output."
  :type
  'boolean
  :group
  'nskk-test)

(defcustom
  nskk-test-property-runs
  100
  "Default number of runs for property-based tests."
  :type
  'integer
  :group
  'nskk-test)

;;;;
;;;; Test State Management
;;;;
(defvar nskk--test-mode nil
  "Non-nil when running in test mode.")

(defvar nskk--test-state nil
  "Current test state information.")

(cl-defstruct
  nskk-test-state
  "Test state container."
  (name nil :read-only t)
  (start-time nil)
  (end-time nil))

;;;;
;;;; Test Environment Setup
;;;;
(defun nskk--test-setup ()
  "Setup test environment before each test."
  (setq nskk--test-mode t
        nskk--test-state (make-nskk-test-state
      :name
      (or (ert-running-test) 'unknown)
      :start-time
      (current-time)))
  (when nskk-test-verbose
    (message "[NSKK Test] Setup: %s" (ert-test-name (ert-running-test)))))

(defun nskk--test-teardown ()
  "Cleanup test environment after each test."
  (when nskk--test-state
    (setf (nskk-test-state-end-time nskk--test-state) (current-time)))
  (when nskk-test-verbose
    (message "[NSKK Test] Teardown: %s" (ert-test-name (ert-running-test)))))

;;;;
;;;; Test Definition Macros
;;;;
(defmacro nskk-deftest (name docstring &rest body)
  "Define an NSKK test with NAME, DOCSTRING, and BODY."
  (declare (indent 2)
           (doc-string 2))
  `(ert-deftest
    ,(intern (format "nskk-test-%s" name))
    ()
    ,docstring
    (let ((nskk--test-mode t)
          (nskk--test-state nil))
      (nskk--test-setup)
      (unwind-protect (progn
          ,@body)
        (nskk--test-teardown)))))

(defmacro nskk-deftest-unit (name docstring &rest body)
  "Define a unit test."
  (declare (indent 2)
           (doc-string 2))
  `(ert-deftest
    ,(intern (format "nskk-unit-%s" name))
    ()
    ,docstring
    (let ((nskk--test-mode t))
      (nskk--test-setup)
      (unwind-protect (progn
          ,@body)
        (nskk--test-teardown)))))

(defmacro nskk-deftest-performance (name docstring &rest body)
  "Define a performance test."
  (declare (indent 2)
           (doc-string 2))
  `(ert-deftest
    ,(intern (format "nskk-performance-%s" name))
    ()
    ,docstring
    (let ((nskk--test-mode t)
          (nskk--test-start-time (current-time)))
      (nskk--test-setup)
      (unwind-protect (progn
          ,@body
          (let ((elapsed (float-time (time-subtract (current-time) nskk--test-start-time))))
            (message "[NSKK Performance] %s: %.3fms" ',name (* 1000 elapsed))))
        (nskk--test-teardown)))))

;;;;
;;;; Test Assertions
;;;;
(defun nskk-assert-approx-equal (a b &optional epsilon)
  "Assert that A and B are approximately equal within EPSILON."
  (let ((eps (or epsilon 0.001)))
    (unless (< (abs (- a b)) eps)
      (ert-fail (format "Not approximately equal: %S vs %S (epsilon: %s)" a b eps)))))

(defun nskk-assert-strings-equal (a b)
  "Assert that strings A and B are equal, with detailed error message."
  (unless (equal a b)
    (ert-fail (format "Strings differ:\nExpected: %S\nActual:   %S" a b))))

;;;;
;;;; Test Data Generators
;;;;
(defvar nskk--test-generators nil
  "Registry of test data generators.")

(defun nskk-register-generator (name generator)
  "Register a test data generator."
  (setf (alist-get name nskk--test-generators) generator))

(defun nskk-generate (name &rest args)
  "Generate test data using generator NAME."
  (let ((generator (alist-get name nskk--test-generators)))
    (when generator
      (apply generator args))))

(nskk-register-generator
  'romaji-string
  (lambda (&optional length)
    (let ((chars
          '("a"
            "i"
            "u"
            "e"
            "o"
            "ka"
            "ki"
            "ku"
            "ke"
            "ko"
            "sa"
            "shi"
            "su"
            "se"
            "so"
            "ta"
            "chi"
            "tsu"
            "te"
            "to"
            "na"
            "ni"
            "nu"
            "ne"
            "no"))
          (len (or length (+ 1 (random 10)))))
      (mapconcat
        'identity
        (cl-loop repeat len collect (nth (random (length chars)) chars))
        ""))))

(nskk-register-generator
  'hiragana-string
  (lambda (&optional length)
    (let ((chars
          '("あ"
            "い"
            "う"
            "え"
            "お"
            "か"
            "き"
            "く"
            "け"
            "こ"
            "さ"
            "し"
            "す"
            "せ"
            "そ"
            "た"
            "ち"
            "つ"
            "て"
            "と"
            "な"
            "に"
            "ぬ"
            "ね"
            "の"))
          (len (or length (+ 1 (random 10)))))
      (string-join
        (cl-loop repeat len collect (nth (random (length chars)) chars))
        ""))))

(nskk-register-generator
  'kanji-string
  (lambda (&optional length)
    (let ((chars
          '("漢" "字" "変" "換" "日" "本" "語" "入" "力" "シ" "ス" "テ" "ム"))
          (len (or length (+ 1 (random 5)))))
      (string-join
        (cl-loop repeat len collect (nth (random (length chars)) chars))
        ""))))

;;;;
;;;; Prolog Test Isolation
;;;;
(defun nskk-prolog-test--copy-trie-node (node copies)
  "Return a graph copy of trie NODE using identity map COPIES."
  (nskk-prolog-test--copy-object node copies))

(defun nskk-prolog-test--copy-trie (trie copies)
  "Return a graph copy of TRIE using identity map COPIES."
  (nskk-prolog-test--copy-object trie copies))

(defun nskk-prolog-test--copy-object (object copies)
  "Return a nonrecursive graph copy of OBJECT using identity map COPIES.
Conses, records, vectors, strings and hash tables are allocated before their
edges are traversed.  String text-property values share the same identity map,
and hash entries are strongly snapshotted before the destination is allocated."
  (let ((missing (make-symbol "missing"))
        (pending (list object))
        composites
        hash-tables)
    (while pending
      (let ((current (pop pending)))
        (when (eq (gethash current copies missing) missing)
          (cond
           ((consp current)
            (puthash current (cons nil nil) copies)
            (push current composites)
            (push (car current) pending)
            (push (cdr current) pending))
           ((hash-table-p current)
            (let (entries)
              (let ((gc-cons-threshold most-positive-fixnum))
                (maphash
                 (lambda (key value)
                   (push (cons key value) entries))
                 current))
              (puthash
               current
               (make-hash-table
                :test (hash-table-test current)
                :size (max 1 (hash-table-size current))
                :rehash-size (hash-table-rehash-size current)
                :rehash-threshold (hash-table-rehash-threshold current)
                :weakness (hash-table-weakness current))
               copies)
              (push (cons current entries) hash-tables)
              (dolist (entry entries)
                (push (car entry) pending)
                (push (cdr entry) pending))))
           ((bool-vector-p current)
            (puthash current (copy-sequence current) copies))
           ((stringp current)
            (puthash current (substring-no-properties current) copies)
            (push current composites)
            (let ((position 0)
                  (limit (length current)))
              (while (< position limit)
                (let* ((next (next-property-change position current limit))
                       (properties (text-properties-at position current)))
                  (while properties
                    (push (cadr properties) pending)
                    (setq properties (cddr properties)))
                  (setq position next)))))
           ((recordp current)
            (puthash current (copy-sequence current) copies)
            (push current composites)
            (let ((index 1))
              (while (< index (length current))
                (push (aref current index) pending)
                (setq index (1+ index)))))
           ((vectorp current)
            (puthash current (make-vector (length current) nil) copies)
            (push current composites)
            (let ((index 0))
              (while (< index (length current))
                (push (aref current index) pending)
                (setq index (1+ index)))))
           (t
            (puthash current current copies))))))
    (cl-labels
        ((copy-of
          (value)
          (let ((copy (gethash value copies missing)))
            (if (eq copy missing) value copy))))
      (dolist (current composites)
        (let ((new (gethash current copies)))
          (cond
           ((consp current)
            (setcar new (copy-of (car current)))
            (setcdr new (copy-of (cdr current))))
           ((stringp current)
            (let ((position 0)
                  (limit (length current)))
              (while (< position limit)
                (let* ((next (next-property-change position current limit))
                       (properties (copy-sequence
                                    (text-properties-at position current)))
                       (cursor properties))
                  (while cursor
                    (setcar (cdr cursor) (copy-of (cadr cursor)))
                    (setq cursor (cddr cursor)))
                  (set-text-properties position next properties new)
                  (setq position next)))))
           ((recordp current)
            (let ((index 1))
              (while (< index (length current))
                (aset new index (copy-of (aref current index)))
                (setq index (1+ index)))))
           ((vectorp current)
            (let ((index 0))
              (while (< index (length current))
                (aset new index (copy-of (aref current index)))
                (setq index (1+ index))))))))
      (dolist (table-and-entries hash-tables)
        (let ((new (gethash (car table-and-entries) copies)))
          (dolist (entry (cdr table-and-entries))
            (puthash (copy-of (car entry))
                     (copy-of (cdr entry))
                     new))))
      (copy-of object))))

  (defun nskk-prolog-test--copy-hash-table (table copies)
    "Return a graph copy of hash TABLE using identity map COPIES."
    (nskk-prolog-test--copy-object table copies))

  (defun nskk-prolog-test--restore-state (saved-stores saved-flags)
    "Restore SAVED-STORES and SAVED-FLAGS after attempting every target."
    (let ((inhibit-quit t)
          first-condition)
      (cl-labels
          ((attempt
            (operation)
            (condition-case condition
                (funcall operation)
              (t
               (unless first-condition
                 (setq first-condition condition))))))
        (dolist (entry (append saved-stores saved-flags))
          (let ((symbol (nth 0 entry))
                (was-bound (nth 1 entry))
                (value (nth 2 entry))
                (watchers (nth 3 entry)))
            (dolist (watcher
                     (copy-sequence (get-variable-watchers symbol)))
              (attempt
               (lambda ()
                 (remove-variable-watcher symbol watcher))))
            (attempt
             (lambda ()
               (if was-bound
                   (set symbol value)
                 (makunbound symbol))))
            (dolist (watcher
                     (copy-sequence (get-variable-watchers symbol)))
              (attempt
               (lambda ()
                 (remove-variable-watcher symbol watcher))))
            (dolist (watcher (reverse (copy-sequence watchers)))
              (attempt
               (lambda ()
                 (add-variable-watcher symbol watcher)))))))
      (when first-condition
        (signal (car first-condition) (cdr first-condition)))))

(defmacro nskk-prolog-test-with-isolated-db (&rest body)
  "Execute BODY with an isolated Prolog object graph.
All identity-bearing stores are restored exactly after normal return, error,
or quit.  A single identity map keeps copied database and index clauses `eq'
while separating every mutable cons, vector, string, hash table, and trie
from the saved graph.  Database tails are rebuilt against the copied spine,
and the index bucket tail cache starts empty."
  (declare (indent 0))
  (let ((saved-stores (cl-gensym "saved-stores-"))
        (saved-flags (cl-gensym "saved-flags-"))
        (copies (cl-gensym "copies-"))
        (isolated-db (cl-gensym "isolated-db-"))
        (isolated-index (cl-gensym "isolated-index-"))
        (isolated-hash (cl-gensym "isolated-hash-"))
        (isolated-trie (cl-gensym "isolated-trie-"))
        (isolated-tails (cl-gensym "isolated-tails-"))
        (isolated-tail-cache (cl-gensym "isolated-tail-cache-"))
        (store-symbol (cl-gensym "store-symbol-"))
        (flag-symbol (cl-gensym "flag-symbol-"))
        (tail-key (cl-gensym "tail-key-"))
        (tail-facts (cl-gensym "tail-facts-"))
        (store-entry (cl-gensym "store-entry-"))
        (flag-entry (cl-gensym "flag-entry-")))
    `(let*
         ((,saved-stores
           (mapcar
            (lambda (,store-symbol)
              (list ,store-symbol
                    t
                    (symbol-value ,store-symbol)
                    (copy-sequence
                     (get-variable-watchers ,store-symbol))))
            (append
             (butlast nskk-prolog-state-variables)
             nskk-dict-index-variables)))
          (,saved-flags
           (mapcar
            (lambda (,flag-symbol)
              (list ,flag-symbol
                    (boundp ,flag-symbol)
                    (and (boundp ,flag-symbol)
                         (symbol-value ,flag-symbol))
                    (copy-sequence
                     (get-variable-watchers ,flag-symbol))))
            (nskk-prolog-query-all-values
             '(module-initialized-flag \?f) '\?f)))
          (,copies (make-hash-table :test (quote eq)))
          (,isolated-db
           (nskk-prolog-test--copy-object
            (nskk-prolog-database) ,copies))
          (,isolated-index
           (nskk-prolog-test--copy-object
            (nskk-prolog-index-config) ,copies))
          (,isolated-hash
           (nskk-prolog-test--copy-object
            (nskk-prolog-hash-indices) ,copies))
          (,isolated-trie
           (nskk-prolog-test--copy-object
            (nskk-prolog-trie-indices) ,copies))
          (,isolated-tails
           (make-hash-table
            :test (hash-table-test (nskk-prolog-database-tails))
            :size (max 1 (hash-table-size (nskk-prolog-database-tails)))))
          (,isolated-tail-cache
           (make-hash-table
            :test (hash-table-test (nskk-prolog-index-bucket-tail-cache))
            :size (max 1
                       (hash-table-size
                        (nskk-prolog-index-bucket-tail-cache))))))
       (maphash
        (lambda (,tail-key ,tail-facts)
          (when ,tail-facts
            (puthash ,tail-key (last ,tail-facts) ,isolated-tails)))
        ,isolated-db)
       (unwind-protect
           (progn
             (cl-mapc #'set
                      (butlast (butlast nskk-prolog-state-variables))
                      (list ,isolated-db ,isolated-tails ,isolated-index
                            ,isolated-hash ,isolated-trie
                            ,isolated-tail-cache))
             (dolist (,flag-entry ,saved-flags)
               (when (nth 1 ,flag-entry)
                 (set (car ,flag-entry) nil)))
             ,@body)
         (nskk-prolog-test--restore-state ,saved-stores ,saved-flags)))))

;;;;
;;;; Shared Test Fixtures
;;;;
(defconst
  nskk--test-minimal-dict
  '(("あ" . ("亜")))
  "Minimal stub dictionary for E2E tests that don't need specific readings.")

;;;;
;;;; Mock Dictionary Helpers
;;;;
(defun nskk-test-create-mock-dict (&optional entries)
  "Create a mock dictionary index with ENTRIES via Prolog facts.
ENTRIES is an alist of (key . candidates-list).
If nil, uses a default set of common Japanese words.

WARNING: This function asserts facts under the production predicate
`user-dict-entry'.  Always call within `nskk-prolog-test-with-isolated-db'
or `nskk-with-mock-dict' to prevent Prolog database pollution."
  (let ((default-entries
        '(("かんじ" . ("漢字" "感じ" "幹事"))
          ("にほんご" . ("日本語"))
          ("にほん" . ("日本" "二本"))
          ("ひらがな" . ("平仮名"))
          ("かたかな" . ("片仮名"))
          ("へんかん" . ("変換"))
          ("にゅうりょく" . ("入力"))
          ("もじ" . ("文字"))
          ("さくら" . ("桜"))
          ("やま" . ("山"))
          ("かわ" . ("川" "河"))
          ("はな" . ("花" "鼻"))
          ("あめ" . ("雨" "飴"))))
        (pred 'user-dict-entry))
    (nskk-prolog-retract-all pred 2)
    (nskk-prolog-set-index pred 2 :trie)
    (dolist (entry (or entries default-entries))
      (nskk-prolog-assert (list (list pred (car entry) (cdr entry)))))
    (make-nskk-dict-index :predicate pred)))

(gv-define-simple-setter nskk-dict-system-index nskk-dict-set-system-index)
(gv-define-simple-setter nskk-dict-user-index nskk-dict-set-user-index)

(defmacro nskk-with-mock-dict (entries &rest body)
  "Execute BODY with a mock dictionary installed.
ENTRIES is an alist of (key . candidates-list) or nil for defaults.
Restores original dictionary state after BODY completes.

Asserts \\='(dict-initialized) into the isolated Prolog database so
that guards relying on `nskk-prolog-holds-p' see the mock as
initialized."
  (declare (indent 1))
  `(nskk-prolog-test-with-isolated-db
     (cl-letf (((nskk-dict-system-index) (nskk-test-create-mock-dict ,entries))
               ((nskk-dict-user-index) nil))
       (nskk-prolog-assert '((dict-initialized)))
       ,@body)))

;;;;
;;;; Convenience Test Macros
;;;;
(defmacro nskk-with-test-buffer (mode &rest body)
  "Execute BODY in a temp buffer with `nskk-mode' enabled.
MODE is an optional initial mode symbol such as \\='hiragana, \\='katakana,
or \\='ascii.  When non-nil the corresponding `nskk-set-mode-MODE' function
is called immediately after enabling the mode.  Pass nil to keep the
default (ascii) mode that `nskk-mode' starts in.

`nskk-mode' is always disabled in an `unwind-protect' clause so that
test failures do not leave the buffer in a broken state."
  (declare (indent 1))
  `(let ((mode-value ,mode))
     (with-temp-buffer
    (nskk-mode 1)
    (when mode-value
      (let ((setter (intern (format "nskk-set-mode-%s" (symbol-name mode-value)))))
        (funcall setter)))
    (unwind-protect (progn
        ,@body)
      (nskk-mode -1)))))

(defmacro nskk-with-state (mode &rest body)
  "Execute BODY with `nskk-current-state' bound to a fresh state for MODE.
Unlike `nskk-with-test-buffer', this does not open a buffer or enable
`nskk-mode'; it is intended for pure-functional tests that only need a
state struct (e.g., modeline, cursor colour).  When MODE is nil,
`nskk-current-state' is bound to nil."
  (declare (indent 1))
  `(let* ((mode-value ,mode)
         (nskk-current-state
          (when mode-value
            (nskk-state-create mode-value))))
    ,@body))

(defmacro nskk-with-mocks (bindings &rest body)
  "Execute BODY with function mocks defined by BINDINGS.
BINDINGS is a list of (FUNCTION-SYMBOL MOCK-FORM) pairs.  Each binding
temporarily replaces the named function with MOCK-FORM (which may be a
lambda or any other callable), then automatically restores the original
definition when BODY exits — even on error.

Example:
  (nskk-with-mocks ((nskk-converting-p (lambda () t))
                    (nskk-commit-current (lambda () (insert \"確定\"))))
    (nskk-kakutei)
    (should (string= (buffer-string) \"確定\")))"
  (declare (indent 1))
  `(cl-letf
    ,(mapcar
      (lambda (b)
        `((symbol-function ',(car b)) ,(cadr b)))
      bindings)
    ,@body))

(defmacro nskk-with-prolog-entries (entries &rest body)
  "Execute BODY in an isolated Prolog database pre-loaded with ENTRIES.
ENTRIES is a list of (PREDICATE KEY VALUES) triples where PREDICATE is an
unquoted symbol, KEY is a string, and VALUES is a list.  The database is
snapshot-copied and restored after BODY, so no test facts leak.

Example:
  (nskk-with-prolog-entries ((user-dict-entry \"かんじ\" (\"漢字\" \"感じ\"))
                             (user-dict-entry \"さくら\" (\"桜\")))
    (let ((idx (make-nskk-dict-index :predicate \\='user-dict-entry)))
      (should (nskk-search-prefix idx \"かん\" nil nil))))"
  (declare (indent 1))
  `(nskk-prolog-test-with-isolated-db
    ,@(mapcar
      (lambda (e)
        `(progn
          (nskk-prolog-set-index ',(nth 0 e) 2 :trie)
          (nskk-prolog-assert (list (list ',(nth 0 e) ,(nth 1 e) ',(nth 2 e))))))
      entries)
    ,@body))

;;;;
;;;; Domain-Specific Assertions
;;;;
(defmacro nskk-should-mode (expected-mode)
  (declare (indent 1))
  "Assert that the current nskk mode equals EXPECTED-MODE.
Reads `nskk-current-state' and compares with `nskk-state-mode'."
  `(should (eq (nskk-state-mode nskk-current-state) ,expected-mode)))

(defmacro nskk-should-buffer (expected)
  (declare (indent 1))
  "Assert that the current buffer's entire content equals the EXPECTED string."
  `(should (string= (buffer-string) ,expected)))

(defmacro nskk-should-equal (expected actual)
  (declare (indent 2))
  "Assert that EXPECTED and ACTUAL are `equal'."
  `(should (equal ,expected ,actual)))

(defmacro nskk-should-candidates (expected result)
  (declare (indent 2))
  "Assert that RESULT is a `nskk-dict-entry' whose candidates list equals EXPECTED.
EXPECTED is a list of candidate strings; RESULT is the value returned by a
search function such as `nskk-search'."
  (let ((result-value (make-symbol "result-value")))
    `(let ((,result-value ,result))
       (should (nskk-dict-entry-p ,result-value))
       (should (equal (nskk-dict-entry-candidates ,result-value) ,expected)))))

;;;;
;;;; Mock skkserv Helper
;;;;
(require 'nskk-keymap)

(require 'nskk-server)

(defun nskk--server-mock-child-form (responses ready-file)
  "Return the form run by the external mock skkserv child.
RESPONSES is an alist of (KEY . RESPONSE-STRING) pairs.
READY-FILE receives the strict startup line after the listener is ready."
  `(progn
    (require 'subr-x)
    (let ((responses ',responses)
          (server nil))
      (unwind-protect (progn
          (setq server (make-network-process
              :name
              " nskk-mock-skkserv-listener"
              :buffer
              nil
              :family
              'ipv4
              :host
              "127.0.0.1"
              :service
              t
              :server
              t
              :noquery
              t
              :coding
              ',nskk-server-coding-system
              :log
              (lambda (_server client _message)
                (set-process-query-on-exit-flag client nil)
                (process-put client 'nskk--server-mock-pending ""))
              :filter
              (lambda (client string)
                (let ((pending
                      (concat (or (process-get client 'nskk--server-mock-pending) "") string))
                      (continue t))
                  (while
                    (and continue (> (length pending) 0) (process-live-p client))
                    (pcase
                      (aref pending 0)
                      (?0
                        (setq pending (substring pending 1))
                        (delete-process client))
                      (?1
                        (let ((space-pos (string-search " " pending 1)))
                          (if (not space-pos) (setq continue nil)
                            (let* ((key (substring pending 1 space-pos))
                                   (response (or (cdr (assoc key responses)) (concat "4" key " \n"))))
                              (setq pending (substring pending (1+ space-pos)))
                              (process-send-string client response)))))
                      (_
                        (setq pending "")
                        (delete-process client))))
                  (when (process-live-p client)
                    (process-put client 'nskk--server-mock-pending pending))))))
          (set-process-query-on-exit-flag server nil)
          (let ((coding-system-for-write 'utf-8-unix))
            (with-temp-buffer
              (insert (format "READY %d\n" (process-contact server :service)))
              (write-region (point-min) (point-max) ,ready-file nil 'silent)))
          (while (process-live-p server) (accept-process-output nil 1.0)))
        (when (and server (process-live-p server))
          (delete-process server))))))

(progn
  (defmacro nskk--server-mock-accept-with-budget
      (process remaining-budget-ms max-slice-ms)
    "Wait for PROCESS using and reducing REMAINING-BUDGET-MS.
The finite positive wait is capped at MAX-SLICE-MS and deducted before
calling `accept-process-output'."
    (declare (debug t))
    (let ((slice-ms (make-symbol "slice-ms"))
          (remaining-budget-value (make-symbol "remaining-budget")))
      `(let* ((,remaining-budget-value ,remaining-budget-ms)
              (,slice-ms
               (min ,remaining-budget-value ,max-slice-ms)))
         (when (> ,slice-ms 0)
           (setq ,remaining-budget-ms
                 (- ,remaining-budget-value ,slice-ms))
           (accept-process-output
            ,process
            (/ (float ,slice-ms) 1000.0)
            nil
            t)))))

  (defun nskk--server-mock-bounded-filter (process string)
    "Retain the bounded diagnostic tail from STRING for PROCESS."
    (let* ((limit (or (process-get process 'nskk--server-mock-limit) 4096))
           (output (or (process-get process 'nskk--server-mock-output) ""))
           (combined (concat output string)))
      (if (<= (length combined) limit)
          (process-put process 'nskk--server-mock-output combined)
        (process-put
         process
         'nskk--server-mock-output
         (substring combined (- (length combined) limit)))
        (process-put process 'nskk--server-mock-output-overflow t)))))

(defun nskk--server-mock-read-ready-line (ready-file limit)
  "Read at most LIMIT characters from READY-FILE.
Return :overflow when the file contains more than LIMIT characters."
  (condition-case
    nil
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8-unix))
        (insert-file-contents ready-file nil 0 (1+ limit)))
      (if (> (buffer-size) limit) :overflow
        (buffer-string)))
    (file-error nil)))

(defun nskk--server-mock-process-sentinel (process _event)
  "Release resources owned by a started external mock server PROCESS."
  (when (process-get process 'nskk--server-mock-started)
    (let ((stderr-process (process-get process 'nskk--server-mock-stderr-process)))
      (when (and stderr-process (process-live-p stderr-process))
        (delete-process stderr-process))
      (process-put process 'nskk--server-mock-stderr-process nil))))

(defun nskk--server-mock-diagnostic (process stderr-process)
  "Return bounded startup diagnostics for PROCESS and STDERR-PROCESS."
  (let ((stdout (and process (process-get process 'nskk--server-mock-output)))
        (stdout-overflow
        (and process (process-get process 'nskk--server-mock-output-overflow)))
        (stderr
        (and stderr-process (process-get stderr-process 'nskk--server-mock-output)))
        (stderr-overflow
        (and
          stderr-process
          (process-get stderr-process 'nskk--server-mock-output-overflow))))
    (format
      ": status=%S stdout-tail=%S%s stderr-tail=%S%s"
      (if process (process-status process)
        'not-started)
      (or stdout "")
      (if stdout-overflow " [truncated]"
        "")
      (or stderr "")
      (if stderr-overflow " [truncated]"
        ""))))

(defun nskk--server-start-mock-server (responses)
  "Start an external mock skkserv and return (PROCESS . PORT).
RESPONSES is an alist of (KEY . RESPONSE-STRING) pairs.
For keys not in RESPONSES the server sends a not-found \\='4...\\=' reply.

This helper is shared by both `nskk-server-integration-test' and
`nskk-server-henkan-integration-test'.  Always call in an
`unwind-protect' that deletes the returned process."
  (let ((program (expand-file-name invocation-name invocation-directory))
 (ready-file nil)
 (stderr-process nil)
 (process nil)
 (remaining-budget-ms 5000)
 (started nil)
 (startup-problem nil)
 port)
    (unwind-protect (progn
        (setq ready-file (make-temp-file "nskk-mock-ready-"))
        (set-file-modes ready-file #o600)
        (setq stderr-process (make-pipe-process
            :name
            " nskk-mock-skkserv-stderr"
            :buffer
            nil
            :noquery
            t
            :coding
            'utf-8-unix
            :filter
            #'nskk--server-mock-bounded-filter))
        (process-put stderr-process 'nskk--server-mock-limit 4096)
        (setq process (make-process
            :name
            " nskk-mock-skkserv"
            :buffer
            nil
            :stderr
            stderr-process
            :command
            (list
              program
              "--quick"
              "--batch"
              "--eval"
              (prin1-to-string (nskk--server-mock-child-form responses ready-file)))
            :connection-type
            'pipe
            :coding
            'utf-8-unix
            :noquery
            t
            :filter
            #'nskk--server-mock-bounded-filter))
        (process-put process 'nskk--server-mock-limit 256)
        (process-put process 'nskk--server-mock-stderr-process stderr-process)
        (set-process-sentinel process #'nskk--server-mock-process-sentinel)
        (while
 (and
  (process-live-p process)
  (not port)
  (not startup-problem)
  (> remaining-budget-ms 0))
 (nskk--server-mock-accept-with-budget
  process remaining-budget-ms 50)
 (let ((ready-line
        (nskk--server-mock-read-ready-line ready-file 128)))
  (cond
   ((eq ready-line :overflow)
    (setq startup-problem "ready line exceeded 128 characters"))
   ((or (null ready-line) (string-empty-p ready-line)))
   ((and
     (string-prefix-p "READY " ready-line)
     (string-suffix-p "\n" ready-line)
     (> (length ready-line) 7)
     (cl-every
      (lambda (char)
       (<= ?0 char ?9))
      (substring ready-line 6 -1)))
    (let ((candidate
           (string-to-number (substring ready-line 6 -1))))
     (if (<= 1 candidate 65535)
         (setq port candidate)
       (setq startup-problem
             "ready port was outside 1..65535"))))
   ((string-suffix-p "\n" ready-line)
    (setq startup-problem "ready line was malformed")))))
        (when (process-live-p stderr-process) (nskk--server-mock-accept-with-budget stderr-process remaining-budget-ms 50))
        (unless (and port (process-live-p process))
          (error
            "Mock skkserv failed to start%s%s"
            (if startup-problem (concat ": " startup-problem)
              "")
            (nskk--server-mock-diagnostic process stderr-process)))
        (process-put process 'nskk--server-mock-started t)
        (delete-file ready-file)
        (setq ready-file nil)
        (setq started t)
        (cons process port))
      (unless started
        (when (and process (process-live-p process))
          (ignore-errors (delete-process process)))
        (when (and stderr-process (process-live-p stderr-process))
          (ignore-errors (delete-process stderr-process))))
      (when (and ready-file (file-exists-p ready-file))
        (ignore-errors (delete-file ready-file))))))

;;;;
;;;; Integration Session Helpers
;;;;
(defmacro nskk-integration-with-session (mode &rest body)
  "Execute BODY in a full NSKK session initialized to MODE.
Sets up a temporary buffer with a fresh state struct, empty romaji buffer,
and initialized romaji table.  Suitable for integration tests that exercise
the full input pipeline without enabling `nskk-mode'."
  (declare (indent 1))
  `(with-temp-buffer
    (let ((nskk-current-state (nskk-state-create ,mode))
          (nskk-converter-auto-start-henkan t))
      (nskk-state-set-conversion-overlay nil)
      (nskk-state-set-romaji-buffer "")
      (nskk-initialize-romaji-table)
      ,@body)))

(defun nskk--integration-type-char (char)
  "Simulate typing CHAR via `nskk-self-insert' in an integration session."
  (let ((last-command-event char))
    (nskk-self-insert 1)))

(defmacro nskk-azik-with-session (mode &rest body)
  "Like `nskk-integration-with-session' but with AZIK style loaded.
Restores the standard romaji table after BODY completes so that
subsequent non-AZIK tests are not affected."
  (declare (indent 1))
  `(nskk-integration-with-session
    ,mode
    (nskk-converter-load-style 'azik)
    (unwind-protect (progn
        ,@body)
      (nskk-converter-load-style 'standard))))

(provide 'nskk-test-framework)

;;; nskk-test-framework.el ends here
