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
;; - Property-based testing support

;;; Code:

(require 'ert)
(require 'nskk-dictionary)
(require 'nskk-prolog)
(require 'nskk-trie)
(eval-when-compile (require 'cl-lib))

;; Load all NSKK modules and initialize their Prolog predicates once for the
;; test session.  This mirrors what `nskk--enable' does at mode activation
;; time, but without dictionary loading or buffer-local setup.
(require 'nskk-state)
(require 'nskk-kana)
(require 'nskk-henkan)
(require 'nskk-input)
(require 'nskk-converter)

;; NOTE: The initialization calls below are guarded by idempotency flags
;; (e.g., `nskk--state-prolog-initialized').  If you `eval-buffer' this
;; file after editing, those flags will prevent re-initialization unless
;; you first reset them manually.  See `nskk-prolog-test-with-isolated-db'
;; in this file for the complete list of flags to reset.
(nskk-state-initialize-prolog)
(nskk-kana-initialize)
(nskk-henkan-initialize)
(nskk-input-initialize)
(nskk-converter-initialize)


;;;;
;;;; Test Framework Configuration
;;;;

(defgroup nskk-test nil
  "NSKK test framework configuration."
  :prefix "nskk-test-"
  :group 'nskk)

(defcustom nskk-test-verbose nil
  "Enable verbose test output."
  :type 'boolean
  :group 'nskk-test)

(defcustom nskk-test-property-runs 100
  "Default number of runs for property-based tests."
  :type 'integer
  :group 'nskk-test)


;;;;
;;;; Test State Management
;;;;

(defvar nskk--test-mode nil
  "Non-nil when running in test mode.")

(defvar nskk--test-state nil
  "Current test state information.")

(cl-defstruct nskk-test-state
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
                         :name (or (ert-running-test) 'unknown)
                         :start-time (current-time)))
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
  (declare (indent 2) (doc-string 2))
  `(ert-deftest ,(intern (format "nskk-test-%s" name)) ()
     ,docstring
     (let ((nskk--test-mode t)
           (nskk--test-state nil))
       (nskk--test-setup)
       (unwind-protect
           (progn
             ,@body)
         (nskk--test-teardown)))))

(defmacro nskk-deftest-unit (name docstring &rest body)
  "Define a unit test."
  (declare (indent 2) (doc-string 2))
  `(ert-deftest ,(intern (format "nskk-unit-%s" name)) ()
     ,docstring
     (let ((nskk--test-mode t))
       (nskk--test-setup)
       (unwind-protect
           (progn
             ,@body)
         (nskk--test-teardown)))))

(defmacro nskk-deftest-performance (name docstring &rest body)
  "Define a performance test."
  (declare (indent 2) (doc-string 2))
  `(ert-deftest ,(intern (format "nskk-performance-%s" name)) ()
     ,docstring
     (let ((nskk--test-mode t)
           (nskk--test-start-time (current-time)))
       (nskk--test-setup)
       (unwind-protect
           (progn
             ,@body
             (let ((elapsed (float-time
                             (time-subtract (current-time)
                                           nskk--test-start-time))))
               (message "[NSKK Performance] %s: %.3fms" ',name (* 1000 elapsed))))
         (nskk--test-teardown)))))


;;;;
;;;; Test Assertions
;;;;

(defun nskk-assert-approx-equal (a b &optional epsilon)
  "Assert that A and B are approximately equal within EPSILON."
  (let ((eps (or epsilon 0.001)))
    (unless (< (abs (- a b)) eps)
      (ert-fail (format "Not approximately equal: %S vs %S (epsilon: %s)"
                        a b eps)))))

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

;; Predefined generators
(nskk-register-generator 'romaji-string
  (lambda (&optional length)
    (let ((chars '("a" "i" "u" "e" "o"
                   "ka" "ki" "ku" "ke" "ko"
                   "sa" "shi" "su" "se" "so"
                   "ta" "chi" "tsu" "te" "to"
                   "na" "ni" "nu" "ne" "no"))
          (len (or length (+ 1 (random 10)))))
      (mapconcat 'identity
                 (cl-loop repeat len
                          collect (nth (random (length chars)) chars))
                 ""))))

(nskk-register-generator 'hiragana-string
  (lambda (&optional length)
    (let ((chars '("あ" "い" "う" "え" "お"
                   "か" "き" "く" "け" "こ"
                   "さ" "し" "す" "せ" "そ"
                   "た" "ち" "つ" "て" "と"
                   "な" "に" "ぬ" "ね" "の"))
          (len (or length (+ 1 (random 10)))))
      (mapconcat #'identity
                 (cl-loop repeat len
                          collect (nth (random (length chars)) chars))
                 ""))))

(nskk-register-generator 'kanji-string
  (lambda (&optional length)
    (let ((chars '("漢" "字" "変" "換" "日" "本" "語"
                   "入" "力" "シ" "ス" "テ" "ム"))
          (len (or length (+ 1 (random 5)))))
      (mapconcat #'identity
                 (cl-loop repeat len
                          collect (nth (random (length chars)) chars))
                 ""))))




;;;;
;;;; Prolog Test Isolation
;;;;

(defun nskk-prolog-test--copy-trie-node (node)
  "Return a deep copy of trie NODE and all its descendants.
Recursively copies the children hash-table so mutations in one test
do not bleed into the saved copy."
  (let ((new (nskk-trie-node--create
              :is-end  (nskk-trie-node-is-end node)
              :value   (nskk-trie-node-value node)
              :count   (nskk-trie-node-count node))))
    (when-let* ((children (nskk-trie-node-children node)))
      (let ((new-children (make-hash-table :test 'eq
                                           :size (hash-table-count children))))
        (maphash (lambda (ch child)
                   (puthash ch (nskk-prolog-test--copy-trie-node child)
                            new-children))
                 children)
        (setf (nskk-trie-node-children new) new-children)))
    new))

(defun nskk-prolog-test--copy-trie (trie)
  "Return a deep copy of TRIE.
The root node and all descendant nodes are recursively duplicated so
that `nskk-trie-insert' and `nskk-trie-delete' in the
test body cannot mutate the saved snapshot."
  (nskk-trie--create-internal
   :root (nskk-prolog-test--copy-trie-node (nskk-trie-root trie))
   :size (nskk-trie-size trie)))

(defun nskk-prolog-test--copy-hash-table (ht)
  "Deep-copy hash table HT for test isolation.
Lists are shallow-copied with `copy-sequence'.  Trie structures are
recursively deep-copied so that in-place mutations in the test body
do not persist through restoration.  All other values are shared
by reference."
  (let ((new (make-hash-table :test (hash-table-test ht)
                              :size (hash-table-size ht))))
    (maphash (lambda (k v)
               (puthash k (cond
                           ((nskk-trie-p v)
                            (nskk-prolog-test--copy-trie v))
                           ((sequencep v)
                            (copy-sequence v))
                           (t v))
                        new))
             ht)
    new))

(defmacro nskk-prolog-test-with-isolated-db (&rest body)
  "Execute BODY with an isolated Prolog database.
Saves and restores the global database so tests don't leak.

After restoration `nskk--prolog-database-tails' is re-derived from
the actual last cons cells of the restored database rather than from
a saved shallow copy.  This maintains the O(1)-append head/tail
invariant: `copy-sequence' creates a new list spine, so a separately
saved tail copy would point to cons cells that are no longer the
true end of the chain, causing silent append failures.

`nskk--user-dict-index' and `nskk--system-dict-index' are also saved
and restored because `nskk-dict-register-word' sets them as a side
effect via `setq', and the change would otherwise persist globally
after the test ends."
  (declare (indent 0))
  `(let ((saved-db              (nskk-prolog-test--copy-hash-table nskk--prolog-database))
         (saved-idx             (nskk-prolog-test--copy-hash-table nskk--prolog-index-config))
         (saved-hash            (nskk-prolog-test--copy-hash-table nskk--prolog-hash-indices))
         (saved-trie            (nskk-prolog-test--copy-hash-table nskk--prolog-trie-indices))
         (saved-counter         nskk--prolog-var-counter)
         (saved-user-dict       nskk--user-dict-index)
         (saved-system-dict     nskk--system-dict-index)
         ;; Save all *-initialized flags so each isolated DB gets fresh Prolog
         ;; fact table initialization.  Without this, after the first test
         ;; restores the Prolog DB all fact-populating functions are skipped
         ;; because their idempotency guard remains t, leaving the fresh DB
         ;; empty of those facts (e.g. semicolon-key-action, japanese-mode).
         (saved-input-init      (and (boundp 'nskk--input-initialized)
                                     nskk--input-initialized))
         (saved-state-init      (and (boundp 'nskk--state-prolog-initialized)
                                     nskk--state-prolog-initialized))
         (saved-henkan-init     (and (boundp 'nskk--henkan-initialized)
                                     nskk--henkan-initialized))
         (saved-kana-init       (and (boundp 'nskk--kana-initialized)
                                     nskk--kana-initialized))
         (saved-converter-init  (and (boundp 'nskk--converter-initialized)
                                     nskk--converter-initialized))
         (saved-cand-init       (and (boundp 'nskk--candidate-key-facts-initialized)
                                     nskk--candidate-key-facts-initialized)))
     (when (boundp 'nskk--input-initialized)
       (setq nskk--input-initialized nil))
     (when (boundp 'nskk--state-prolog-initialized)
       (setq nskk--state-prolog-initialized nil))
     (when (boundp 'nskk--henkan-initialized)
       (setq nskk--henkan-initialized nil))
     (when (boundp 'nskk--kana-initialized)
       (setq nskk--kana-initialized nil))
     (when (boundp 'nskk--converter-initialized)
       (setq nskk--converter-initialized nil))
     (when (boundp 'nskk--candidate-key-facts-initialized)
       (setq nskk--candidate-key-facts-initialized nil))
     (unwind-protect
         (progn ,@body)
       (setq nskk--prolog-database    saved-db
             nskk--prolog-index-config saved-idx
             nskk--prolog-hash-indices saved-hash
             nskk--prolog-trie-indices saved-trie
             nskk--prolog-var-counter  saved-counter
             nskk--user-dict-index     saved-user-dict
             nskk--system-dict-index   saved-system-dict)
       (when (boundp 'nskk--input-initialized)
         (setq nskk--input-initialized saved-input-init))
       (when (boundp 'nskk--state-prolog-initialized)
         (setq nskk--state-prolog-initialized saved-state-init))
       (when (boundp 'nskk--henkan-initialized)
         (setq nskk--henkan-initialized saved-henkan-init))
       (when (boundp 'nskk--kana-initialized)
         (setq nskk--kana-initialized saved-kana-init))
       (when (boundp 'nskk--converter-initialized)
         (setq nskk--converter-initialized saved-converter-init))
       (when (boundp 'nskk--candidate-key-facts-initialized)
         (setq nskk--candidate-key-facts-initialized saved-cand-init))
       ;; Re-derive database-tails from the now-restored database.
       ;; `copy-sequence' on a list creates a new cons-cell spine, so any
       ;; separately saved tail would point to cells that are not the actual
       ;; end of the restored chain.  Walking to (last v) guarantees the
       ;; tail pointer stays consistent with the head pointer.
       (let ((new-tails (make-hash-table :test 'equal :size 128)))
         (maphash (lambda (k v)
                    (when v (puthash k (last v) new-tails)))
                  nskk--prolog-database)
         (setq nskk--prolog-database-tails new-tails)))))

;;;;
;;;; Shared Test Fixtures
;;;;

(defconst nskk--test-minimal-dict '(("あ" . ("亜")))
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
    (make-nskk-dict-index
     :predicate pred)))

(defmacro nskk-with-mock-dict (entries &rest body)
  "Execute BODY with a mock dictionary installed.
ENTRIES is an alist of (key . candidates-list) or nil for defaults.
Restores original dictionary state after BODY completes.

Asserts \\='(dict-initialized) into the isolated Prolog database so
that guards relying on `nskk-prolog-holds-p' see the mock as
initialized."
  (declare (indent 1))
  `(nskk-prolog-test-with-isolated-db
     (let ((nskk--system-dict-index (nskk-test-create-mock-dict ,entries))
           (nskk--user-dict-index nil))
       ;; Assert dict-initialized so Prolog-based init guards pass
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
  `(with-temp-buffer
     (nskk-mode 1)
     (when ,mode
       (let ((setter (intern (format "nskk-set-mode-%s" (symbol-name ,mode)))))
         (funcall setter)))
     (unwind-protect
         (progn ,@body)
       (nskk-mode -1))))

(defmacro nskk-with-state (mode &rest body)
  "Execute BODY with `nskk-current-state' bound to a fresh state for MODE.
Unlike `nskk-with-test-buffer', this does not open a buffer or enable
`nskk-mode'; it is intended for pure-functional tests that only need a
state struct (e.g., modeline, cursor colour).  When MODE is nil,
`nskk-current-state' is bound to nil."
  (declare (indent 1))
  `(let ((nskk-current-state (when ,mode (nskk-state-create ,mode))))
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
  `(cl-letf ,(mapcar (lambda (b)
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
      (should (nskk-search idx \"かんじ\" \\='exact))))"
  (declare (indent 1))
  `(nskk-prolog-test-with-isolated-db
     ,@(mapcar (lambda (e)
                 `(progn
                    (nskk-prolog-set-index ',(nth 0 e) 2 :trie)
                    (nskk-prolog-assert (list (list ',(nth 0 e)
                                                   ,(nth 1 e)
                                                   ',(nth 2 e))))))
               entries)
     ,@body))


;;;;
;;;; Domain-Specific Assertions
;;;;

(defmacro nskk-should-mode (expected-mode)
  "Assert that the current nskk mode equals EXPECTED-MODE.
Reads `nskk-current-state' and compares with `nskk-state-mode'."
  `(should (eq (nskk-state-mode nskk-current-state) ,expected-mode)))

(defmacro nskk-should-buffer (expected)
  "Assert that the current buffer's entire content equals the EXPECTED string."
  `(should (string= (buffer-string) ,expected)))

(defmacro nskk-should-equal (expected actual)
  "Assert that EXPECTED and ACTUAL are `equal'."
  `(should (equal ,expected ,actual)))

(defmacro nskk-should-candidates (expected result)
  "Assert that RESULT is a `nskk-dict-entry' whose candidates list equals EXPECTED.
EXPECTED is a list of candidate strings; RESULT is the value returned by a
search function such as `nskk-search'."
  `(progn
     (should (nskk-dict-entry-p ,result))
     (should (equal (nskk-dict-entry-candidates ,result) ,expected))))


;;;;
;;;; Mock skkserv Helper
;;;;

;; `nskk-keymap' is needed for `nskk-self-insert', used by the
;; integration session helpers below.
(require 'nskk-keymap)

;; `nskk-server-coding-system' is defined in nskk-server.el.
;; Load it here so the mock helper below can reference that variable.
(require 'nskk-server)

(defun nskk--server-start-mock-server (responses)
  "Start an in-process mock skkserv and return (server-proc . port).
RESPONSES is an alist of (KEY . RESPONSE-STRING) pairs.
For keys not in RESPONSES the server sends a not-found \\='4...\\=' reply.

This helper is shared by both `nskk-server-integration-test' and
`nskk-server-henkan-integration-test'.  Always call in an
`unwind-protect' that deletes the returned server process."
  (let ((srv (make-network-process
              :name " nskk-mock-skkserv"
              :buffer nil
              :family 'ipv4
              :host "127.0.0.1"
              :service t
              :server t
              :noquery t
              :coding nskk-server-coding-system
              :filter
              (lambda (client string)
                (when (> (length string) 0)
                  (let ((cmd (aref string 0)))
                    (cond
                     ((= cmd ?0)
                      (delete-process client))
                     ((= cmd ?1)
                      (let* ((rest (substring string 1))
                             (space-pos (string-search " " rest))
                             (key (if space-pos
                                      (substring rest 0 space-pos)
                                    rest))
                             (response (or (cdr (assoc key responses))
                                          (concat "4" key " \n"))))
                        (process-send-string client response))))))))))
    (cons srv (process-contact srv :service))))


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
           (nskk--conversion-overlay nil)
           (nskk--romaji-buffer "")
           (nskk-converter-auto-start-henkan t))
       (nskk--initialize-romaji-table)
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
  `(nskk-integration-with-session ,mode
     (nskk-converter-load-style 'azik)
     (unwind-protect
         (progn ,@body)
       (nskk-converter-load-style 'standard))))


(provide 'nskk-test-framework)

;;; nskk-test-framework.el ends here
