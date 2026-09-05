;;; nskk-e2e-helpers.el --- E2E test helpers for NSKK  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

;; Author: takeokunn <bararararatty@gmail.com>
;; Maintainer: takeokunn <bararararatty@gmail.com>
;; URL: https://github.com/takeokunn/nskk.el
;; Keywords: i18n, testing

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:


;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nskk)
(require 'nskk-test-framework)
(require 'nskk-modeline)
(require 'nskk-state)
(require 'nskk-henkan)
(require 'nskk-input)
(require 'nskk-prolog)

(declare-function nskk-candidate-show-list "nskk-candidate-window")
(declare-function nskk-candidate-hide-list "nskk-candidate-window")

;;;;
;;;; Default Mock Dictionary
;;;;

(defconst nskk-e2e--default-dict
  '(("あ"     . ("亜" "阿"))
    ("い"     . ("意" "位"))
    ("かんじ" . ("漢字" "感じ" "幹事"))
    ("へんかん" . ("変換"))
    ("にほん"  . ("日本" "二本"))
    ("にほんご" . ("日本語"))
    ("ひらがな" . ("平仮名"))
    ("かたかな" . ("片仮名"))
    ("さくら"  . ("桜"))
    ("やま"    . ("山"))
    ("かわ"    . ("川" "河"))
    ("はな"    . ("花" "鼻"))
    ("てすと"  . ("テスト"))
    ("あいて"  . ("相手" "愛手"))
    ("おくり"  . ("送り"))
    ("かき"    . ("柿" "牡蠣"))
    ("か"      . ("蚊" "課" "下"))
    ("き"      . ("木" "気" "季"))
    ("く"      . ("句" "区" "苦"))
    ("け"      . ("毛" "家" "気"))
    ("こ"      . ("子" "故" "湖"))
    ("さ"      . ("差" "左" "詐"))
    ("し"      . ("詩" "市" "死"))
    ("す"      . ("酢" "巣" "須"))
    ("せ"      . ("背" "世" "瀬"))
    ("そ"      . ("素" "組" "曽"))
    ("な"      . ("名" "奈"))
    ("に"      . ("荷" "二" "似"))
    ("ぬ"      . ("ぬ"))
    ("ね"      . ("根" "値" "寝"))
    ("の"      . ("野" "乃" "農"))
    ("できr"  . ("出来"))
    ("みr"    . ("見" "診"))
    ("かk"    . ("書" "掛" "欠"))
    ("おくr"  . ("送" "贈" "遅"))
    ("はしr"  . ("走"))
    ("よm"    . ("読"))
    ("きk"    . ("聞"))
    ("いk"    . ("行"))
    ("くr"    . ("来"))
    ("おもw"  . ("思"))
    ("はなs"  . ("話"))
    ("とr"    . ("取"))
    ("たべr"  . ("食" "喰"))
    ("はびこr" . ("蔓延")))
  "Default mock dictionary for E2E tests.
Uses alist of (reading . candidates-list).
Entries are asserted as user-dict-entry facts, which are visible to
nskk-dict-lookup via the dict-entry/2 bridge rule.")

;;;;
;;;; Core E2E Test Macro
;;;;

(defmacro nskk-e2e-with-buffer (initial-mode dict-entries &rest body)
  "Execute BODY in a buffer with nskk-mode enabled.

INITIAL-MODE is the starting mode symbol (e.g., \\='hiragana) or nil for ascii.
DICT-ENTRIES is an alist of (reading . candidates-list) or nil for defaults.

Initialization order:
  1. Isolate Prolog DB with nskk-prolog-test-with-isolated-db
  2. Assert (dict-initialized) BEFORE enabling nskk-mode
  3. Assert mock entries as user-dict-entry facts
  4. Enable nskk-mode in a temp buffer
  5. Set INITIAL-MODE if specified
  6. Run BODY
  7. Teardown: disable nskk-mode, reset global state"
  (declare (indent 2) (debug t))
  `(nskk-prolog-test-with-isolated-db
     (nskk-e2e--with-buffer-body ,initial-mode ,dict-entries ,@body)))

(defmacro nskk-e2e--with-buffer-body (initial-mode dict-entries &rest body)
  "Set up the E2E buffer and run BODY, without isolating the Prolog DB.
INITIAL-MODE and DICT-ENTRIES are as in `nskk-e2e-with-buffer'.  Callers must
already hold an isolated database; `nskk-e2e-with-buffer' wraps this in one.
Split out so that a caller which has already entered
`nskk-prolog-test-with-isolated-db' does not deep-copy the object graph a
second time -- the outer copy is what isolates the caller, so a nested one
costs a full graph traversal and guarantees nothing extra."
  (declare (indent 2) (debug t))
  `(progn
     (nskk-prolog-assert '((dict-initialized)))
     (nskk-prolog-retract-all 'user-dict-entry 2)
     (nskk-prolog-set-index 'user-dict-entry 2 :trie)
     (let ((nskk-e2e--entries (or ,dict-entries nskk-e2e--default-dict)))
       (dolist (entry nskk-e2e--entries)
         (nskk-prolog-assert
          `((user-dict-entry ,(car entry) ,(cdr entry))))))
     (let ((initial-mode-value ,initial-mode))
       (with-temp-buffer
       (setq unread-command-events nil)
       (electric-indent-local-mode -1)
       (cl-letf (((symbol-function 'read-from-minibuffer)
                  (lambda (&rest _) ""))
                 ((symbol-function 'nskk-candidate-show-list)
                  #'ignore)
                 ((symbol-function 'nskk-candidate-hide-list)
                  #'ignore))
         (nskk-mode 1)
         (when initial-mode-value
           (nskk-set-mode initial-mode-value))
         (nskk-state-set-romaji-buffer "")
         (unwind-protect
             (progn ,@body)
           (ignore-errors (nskk-mode -1))
           (setq unread-command-events nil)
           (nskk-set-henkan-candidate-list-active nil)
           (remove-hook 'nskk-henkan-show-candidates-functions
                        #'nskk-candidate-show-list)
           (remove-hook 'nskk-henkan-hide-candidates-functions
                        #'nskk-candidate-hide-list)))))))

(progn
  (defun nskk-e2e--snapshot-hash-table-variable (symbol)
  "Snapshot SYMBOL's exact binding and hash-table contents."
  (unless (symbolp symbol)
    (error "Invalid style hash-table variable: %S" symbol))
  (let ((bound-p (boundp symbol)))
    (when (and bound-p (not (hash-table-p (symbol-value symbol))))
      (error "Style variable is not a hash table: %S" symbol))
    (list symbol
          bound-p
          (and bound-p (symbol-value symbol))
          (and bound-p
               (nskk-prolog-copy-term (symbol-value symbol))))))

  (defun nskk-e2e--restore-hash-table-variable (snapshot)
    "Restore a hash-table variable from SNAPSHOT."
    (pcase-let ((`(,symbol ,bound-p ,value ,contents) snapshot))
      (when bound-p
        (clrhash value)
        (maphash (lambda (key item)
                   (puthash key item value))
                 contents))
      (if bound-p
          (set symbol value)
        (makunbound symbol))))

  (defun nskk-e2e--snapshot-style-variable (symbol)
    "Snapshot replacement-only style variable SYMBOL."
    (unless (symbolp symbol)
      (error "Invalid style transaction variable: %S" symbol))
    (let ((bound-p (boundp symbol)))
      (list symbol bound-p (and bound-p (symbol-value symbol)))))

  (defun nskk-e2e--restore-style-variable (snapshot)
    "Restore a replacement-only style variable from SNAPSHOT."
    (pcase-let ((`(,symbol ,bound-p ,value) snapshot))
      (if bound-p
          (set symbol value)
        (makunbound symbol))))

  (defun nskk-e2e--snapshot-mode-map ()
    "Snapshot `nskk-mode-map' binding, identity, and contents."
    (let ((bound-p (boundp 'nskk-mode-map)))
      (when (and bound-p
                 (symbol-value 'nskk-mode-map)
                 (not (and (consp (symbol-value 'nskk-mode-map))
                           (keymapp (symbol-value 'nskk-mode-map)))))
        (error "Invalid nskk-mode-map: %S" (symbol-value 'nskk-mode-map)))
      (let ((value (and bound-p (symbol-value 'nskk-mode-map))))
        (list bound-p
              value
              (and (consp value) (car value))
              (and (consp value) (cdr value))))))

  (defun nskk-e2e--restore-mode-map (snapshot)
    "Restore `nskk-mode-map' from SNAPSHOT."
    (pcase-let ((`(,bound-p ,value ,head ,tail) snapshot))
      (when (consp value)
        (setcar value head)
        (setcdr value tail))
      (if bound-p
          (set 'nskk-mode-map value)
        (makunbound 'nskk-mode-map))))

  (defmacro nskk-e2e-with-azik-buffer (initial-mode dict-entries &rest body)
    "Run BODY in an isolated AZIK-enabled E2E buffer.
INITIAL-MODE and DICT-ENTRIES are passed to `nskk-e2e-with-buffer'.
The caller's Prolog database and registered non-Prolog transaction state are
restored exactly after normal return, error, or quit."
    (declare (indent 2) (debug t))
    `(nskk-prolog-test-with-isolated-db
       (let* ((nskk-converter-romaji-style 'azik)
              (nskk-e2e--romaji-table-before (nskk-romaji-table))
              (hash-table-symbols
               (delete-dups
                (copy-sequence
                 (nskk-converter-style-transaction-hash-tables))))
              (hash-table-snapshots
               (mapcar #'nskk-e2e--snapshot-hash-table-variable
                       hash-table-symbols))
              (style-variable-snapshots
               (mapcar #'nskk-e2e--snapshot-style-variable
                       (delete-dups
                        (copy-sequence
                         (nskk-converter-style-transaction-variables)))))
              (mode-map-snapshot (nskk-e2e--snapshot-mode-map)))
         (unwind-protect
             (progn
               (nskk-converter-load-style 'azik)
               (nskk-e2e--with-buffer-body ,initial-mode ,dict-entries
                 ,@body))
           (nskk-set-romaji-table nskk-e2e--romaji-table-before)
           (mapc #'nskk-e2e--restore-hash-table-variable
                 hash-table-snapshots)
           (mapc #'nskk-e2e--restore-style-variable
                 style-variable-snapshots)
           (nskk-e2e--restore-mode-map mode-map-snapshot))))))

;;;;
;;;; Input Helper
;;;;

(defun nskk-e2e--dispatch-event (event)
  "Dispatch a single keyboard EVENT in the current buffer.
EVENT is an integer or character code.
Uses `call-interactively' instead of `execute-kbd-macro' to avoid
batch-mode event queue contamination: `execute-kbd-macro' in Emacs
batch mode leaves residual events in the event queue that persist
across `with-temp-buffer' boundaries and corrupt subsequent tests."
  (let* ((cmd (key-binding (vector event)))
         (last-command-event event))
    (setq this-command cmd)
    (when cmd
      (call-interactively cmd))))

(defmacro nskk-e2e-type (keys)
  (declare (indent 1))
  "Type KEYS by dispatching each key via `call-interactively'.
KEYS is a key sequence string understood by `kbd', e.g. \"ka\", \"C-j\", \"SPC\".

Unlike `execute-kbd-macro', this dispatches each event individually
via `key-binding' + `call-interactively', which avoids the
batch-mode event queue contamination that `execute-kbd-macro' causes.

When `kbd' returns an empty sequence for a non-empty string (e.g.,
\";;\" is parsed as a comment delimiter by `kbd'), falls back to
dispatching raw character codes directly from the string."
  `(let* ((keys-val ,keys)
          (key-vec  (kbd keys-val)))
     (if (and (stringp keys-val)
              (not (string-empty-p keys-val))
              (zerop (length key-vec)))
         (cl-loop for ch across keys-val
                  do (nskk-e2e--dispatch-event ch))
       (cl-loop for i from 0 below (length key-vec)
                do (nskk-e2e--dispatch-event (aref key-vec i))))))

;;;;
;;;; Test Definition Macro
;;;;

(defmacro nskk-deftest-e2e (name docstring &rest body)
  "Define an E2E ERT test.
Test is named nskk-e2e-NAME.  DOCSTRING describes the test.
BODY is the test body, typically containing `nskk-e2e-with-buffer'."
  (declare (indent 2) (doc-string 2))
  `(ert-deftest ,(intern (format "nskk-e2e-%s" name)) ()
     ,docstring
     ,@body))

;;;;
;;;; Assertion Helpers
;;;;

(defun nskk-e2e-assert-buffer (expected &optional message)
  "Assert that (buffer-string) equals EXPECTED.
Use for post-commit state, not during ▼ conversion (use overlay assertion)."
  (let ((actual (buffer-string)))
    (unless (equal actual expected)
      (ert-fail (format "%sBuffer content mismatch:\n  Expected: %S\n  Actual:   %S"
                        (if message (concat message "\n") "")
                        expected actual)))))

(defun nskk-e2e-assert-buffer-matches (regexp &optional message)
  "Assert that (buffer-string) matches REGEXP."
  (let ((actual (buffer-string)))
    (unless (string-match-p regexp actual)
      (ert-fail (format "%sBuffer does not match regexp:\n  Regexp: %S\n  Actual: %S"
                        (if message (concat message "\n") "")
                        regexp actual)))))

(defun nskk-e2e-assert-mode (expected-mode &optional message)
  "Assert that the current NSKK mode is EXPECTED-MODE."
  (let ((actual (nskk-current-mode)))
    (unless (eq actual expected-mode)
      (ert-fail (format "%sMode mismatch:\n  Expected: %S\n  Actual:   %S"
                        (if message (concat message "\n") "")
                        expected-mode actual)))))

(defun nskk-e2e-assert-modeline-contains (expected-str &optional message)
  "Assert that the modeline indicator contains EXPECTED-STR."
  (let ((actual (nskk-modeline-indicator)))
    (unless (and (stringp actual)
                 (string-match-p (regexp-quote expected-str) actual))
      (ert-fail (format "%sModeline indicator mismatch:\n  Expected to contain: %S\n  Actual: %S"
                        (if message (concat message "\n") "")
                        expected-str actual)))))

(defun nskk-e2e-assert-overlay-shows (expected &optional message)
  "Assert that the conversion overlay displays EXPECTED text.
Use this during ▼ (henkan-active) phase to check the current candidate."
  (let ((actual (when (overlayp (nskk-state-conversion-overlay))
                  (overlay-get (nskk-state-conversion-overlay) 'display))))
    (unless (equal actual expected)
      (ert-fail (format "%sOverlay display mismatch:\n  Expected: %S\n  Actual:   %S"
                        (if message (concat message "\n") "")
                        expected actual)))))

(defun nskk-e2e-assert-henkan-phase (expected-phase &optional message)
  "Assert that the current henkan phase is EXPECTED-PHASE.
EXPECTED-PHASE is nil, \\='on (▽), \\='active (▼), \\='list, or \\='registration."
  (let ((actual (when (boundp 'nskk-current-state)
                  (nskk-state-henkan-phase nskk-current-state))))
    (unless (eq actual expected-phase)
      (ert-fail (format "%sHenkan phase mismatch:\n  Expected: %S\n  Actual:   %S"
                        (if message (concat message "\n") "")
                        expected-phase actual)))))

(defun nskk-e2e-assert-converting ()
  "Assert that NSKK is currently in conversion (▼) state."
  (unless (nskk-converting-p)
    (ert-fail "Expected NSKK to be in converting (▼) state")))

(defun nskk-e2e-assert-not-converting ()
  "Assert that NSKK is NOT in conversion state."
  (when (nskk-converting-p)
    (ert-fail "Expected NSKK to NOT be in converting (▼) state")))

;;;;
;;;; PBT Helper
;;;;

(defun nskk-e2e--type-romaji-chars (romaji-str)
  "Type each character of ROMAJI-STR by dispatching keys interactively.
Only sends printable ASCII characters to avoid batch-mode issues."
  (dolist (char (string-to-list romaji-str))
    (when (and (>= char ?\ ) (<= char ?~))
      (nskk-e2e--dispatch-event char))))

(defun nskk-e2e--random-romaji-basic ()
  "Generate a random basic romaji sequence (lowercase a-z only, length 1-8)."
  (let* ((chars "aiueoaiueoaiueokakenokisukusakasonatanoteninanohihuhehomimumanomoyayuyo")
         (len (+ 1 (random 8))))
    (apply #'string
           (cl-loop repeat len
                    collect (aref chars (random (length chars)))))))

(provide 'nskk-e2e-helpers)

;;; nskk-e2e-helpers.el ends here
