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

;; E2E test infrastructure for NSKK.  These tests exercise nskk-mode
;; end-to-end by enabling the full minor mode in a temp buffer and
;; dispatching real key events via `execute-kbd-macro'.  This exercises
;; the complete Emacs keymap dispatch path, unlike integration tests
;; that call functions directly.
;;
;; Architecture:
;;   nskk-e2e-with-buffer     -- macro that sets up the isolated environment
;;   nskk-e2e-type            -- type a key sequence via execute-kbd-macro
;;   nskk-deftest-e2e         -- define an ert-deftest with e2e- prefix
;;   nskk-e2e-assert-*        -- assertion helpers
;;
;; Initialization order (critical for mock dict to work):
;;   1. nskk-prolog-test-with-isolated-db  (copy DB, restore after)
;;   2. (dict-initialized) asserted FIRST  (prevents real dict loading)
;;   3. user-dict-entry facts asserted     (visible via dict-entry/2 bridge)
;;   4. (nskk-mode 1) in with-temp-buffer  (enables mode, skips dict-init)
;;   5. BODY executes
;;   6. Teardown: nskk-mode -1, reset non-buffer-local globals
;;
;; Key design decisions:
;; - Must use user-dict-entry (NOT mock-dict-entry) for SPC conversion to work
;;   because dict-entry/2 bridge only queries user-dict-entry and system-dict-entry
;; - read-from-minibuffer must be mocked to prevent batch-mode blocking
;; - nskk--henkan-candidate-list-active is NOT buffer-local; always reset in teardown
;; - overlay-get for 'display during ▼ phase (buffer-string won't show overlay content)

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
    ;; Okurigana (okuri-ari) entries: key = hiragana-stem + lowercase-consonant.
    ;; Candidates contain only the kanji stem; the okurigana kana is appended by
    ;; the input pipeline from the okurigana buffer.
    ;; Multi-candidate entries (2-3 candidates) enable SPC-cycling regression
    ;; tests in Section 14 of nskk-azik-e2e-test.el and nskk-e2e-okurigana.el.
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
     ;; Step 1: Assert (dict-initialized) BEFORE enabling nskk-mode.
     ;; This prevents nskk-dict-initialize from running and wiping our mock.
     (nskk-prolog-assert '((dict-initialized)))
     ;; Step 2: Set up mock user-dict-entry facts.
     ;; Must use user-dict-entry (NOT mock-dict-entry) so dict-entry/2 bridge works.
     (nskk-prolog-retract-all 'user-dict-entry 2)
     (nskk-prolog-set-index 'user-dict-entry 2 :trie)
     (let ((nskk-e2e--entries (or ,dict-entries nskk-e2e--default-dict)))
       (dolist (entry nskk-e2e--entries)
         (nskk-prolog-assert
          `((user-dict-entry ,(car entry) ,(cdr entry))))))
     ;; Step 3: Enable nskk-mode in a temp buffer.
     (with-temp-buffer
       ;; Clear any pending keyboard events from previous execute-kbd-macro calls.
       ;; In Emacs batch mode, unread-command-events can persist between calls
       ;; and contaminate subsequent tests with leftover input.
       (setq unread-command-events nil)
       ;; Prevent electric-indent from inserting unexpected newlines.
       (electric-indent-local-mode -1)
       ;; Mock read-from-minibuffer to prevent blocking in batch mode.
       ;; This is needed for dictionary registration (nskk-start-registration).
       (cl-letf (((symbol-function 'read-from-minibuffer)
                  (lambda (&rest _) ""))
                 ((symbol-function 'nskk-candidate-show-list)
                  #'ignore)
                 ((symbol-function 'nskk-candidate-hide-list)
                  #'ignore))
         (nskk-mode 1)
         ;; Step 4: Set initial mode after nskk-mode activation.
         (when ,initial-mode
           (nskk--set-mode ,initial-mode))
         ;; Clear any residual romaji buffer.
         (setq nskk--romaji-buffer "")
         (unwind-protect
             (progn ,@body)
           ;; Step 5: Teardown.
           (ignore-errors (nskk-mode -1))
           ;; Clear any events we produced, so the next test starts clean.
           (setq unread-command-events nil)
           ;; Reset non-buffer-local globals that leak between tests.
           (setq nskk--henkan-candidate-list-active nil)
           (remove-hook 'nskk-henkan-show-candidates-functions
                        #'nskk-candidate-show-list)
           (remove-hook 'nskk-henkan-hide-candidates-functions
                        #'nskk-candidate-hide-list))))))

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
         (last-command-event event)
         (this-command cmd))
    (when cmd
      (call-interactively cmd))))

(defmacro nskk-e2e-type (keys)
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
              (> (length keys-val) 0)
              (zerop (length key-vec)))
         ;; Fallback: kbd parsed the string as empty (e.g., ";;" is a comment).
         ;; Dispatch each character code directly.
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
  (let ((actual (when (and (boundp 'nskk--conversion-overlay)
                           (overlayp nskk--conversion-overlay))
                  (overlay-get nskk--conversion-overlay 'display))))
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
