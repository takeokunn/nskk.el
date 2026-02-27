;;; nskk-integration-test.el --- Integration tests -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Authors

;; Author: NSKK Developers
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Integration tests verifying end-to-end behavior across modules:
;; - Input lifecycle (romaji → kana → conversion → commit)
;; - Mode transition flows
;; - Keymap handler dispatch
;; - Conversion pipeline

;;; Code:

(require 'ert)
(require 'nskk-layer-application)
(require 'nskk-input-commands)
(require 'nskk-keymap)
(require 'nskk-state)
(require 'nskk-converter)
(require 'nskk-test-framework)

;;;
;;; Helper Macros
;;;

(defmacro nskk-integration-with-session (mode &rest body)
  "Execute BODY in a full NSKK session initialized to MODE."
  (declare (indent 1))
  `(with-temp-buffer
     (let ((nskk-current-state (nskk-state-create ,mode))
           (nskk-converting-active nil)
           (nskk--conversion-overlay nil)
           (nskk--romaji-buffer "")
           (nskk-converter-auto-start-henkan t))
       (nskk--initialize-romaji-table)
       ,@body)))

(defun nskk-integration--type-char (char)
  "Simulate typing CHAR via nskk-self-insert."
  (let ((last-command-event char))
    (nskk-self-insert 1)))

;;;
;;; End-to-End Input Lifecycle Tests
;;;

(nskk-deftest-integration hiragana-vowel-input
  "Test typing a single vowel in hiragana mode."
  (nskk-integration-with-session 'hiragana
    (nskk-integration--type-char ?a)
    (should (equal (buffer-string) "あ"))))

(nskk-deftest-integration hiragana-consonant-vowel
  "Test typing consonant + vowel produces kana."
  (nskk-integration-with-session 'hiragana
    (nskk-integration--type-char ?k)
    (nskk-integration--type-char ?a)
    (should (equal (buffer-string) "か"))))

(nskk-deftest-integration hiragana-word-sakura
  "Test typing a multi-syllable word: sakura → さくら."
  (nskk-integration-with-session 'hiragana
    (dolist (c '(?s ?a ?k ?u ?r ?a))
      (nskk-integration--type-char c))
    (should (equal (buffer-string) "さくら"))))

(nskk-deftest-integration hiragana-word-kanjini
  "Test typing kanji → かんじ (n+consonant rule)."
  (nskk-integration-with-session 'hiragana
    (dolist (c '(?k ?a ?n ?j ?i))
      (nskk-integration--type-char c))
    (should (equal (buffer-string) "かんじ"))))

(nskk-deftest-integration hiragana-word-kitte
  "Test typing kitte → きって (sokuon rule)."
  (nskk-integration-with-session 'hiragana
    (dolist (c '(?k ?i ?t ?t ?e))
      (nskk-integration--type-char c))
    (should (equal (buffer-string) "きって"))))

(nskk-deftest-integration katakana-vowel-input
  "Test typing a vowel in katakana mode produces katakana."
  (nskk-integration-with-session 'katakana
    (nskk-integration--type-char ?a)
    (should (equal (buffer-string) "ア"))))

(nskk-deftest-integration katakana-word
  "Test typing a word in katakana mode."
  (nskk-integration-with-session 'katakana
    (dolist (c '(?k ?a ?t ?a ?k ?a ?n ?a))
      (nskk-integration--type-char c))
    (should (equal (buffer-string) "カタカナ"))))

(nskk-deftest-integration latin-mode-passthrough
  "Test that latin mode passes characters through."
  (nskk-integration-with-session 'latin
    (nskk-integration--type-char ?h)
    (nskk-integration--type-char ?i)
    (should (equal (buffer-string) "hi"))))

(nskk-deftest-integration ascii-mode-passthrough
  "Test that ascii mode passes characters through."
  (nskk-integration-with-session 'ascii
    (nskk-integration--type-char ?A)
    (nskk-integration--type-char ?B)
    (should (equal (buffer-string) "AB"))))

;;;
;;; Mode Transition Flow Tests
;;;

(nskk-deftest-integration mode-hiragana-to-katakana-via-q
  "Test q key toggles hiragana to katakana."
  (nskk-integration-with-session 'hiragana
    (nskk-handle-q)
    (should (eq (nskk-state-mode nskk-current-state) 'katakana))
    ;; Type in katakana mode
    (nskk-integration--type-char ?a)
    (should (equal (buffer-string) "ア"))))

(nskk-deftest-integration mode-katakana-to-hiragana-via-q
  "Test q key toggles katakana to hiragana."
  (nskk-integration-with-session 'katakana
    (nskk-handle-q)
    (should (eq (nskk-state-mode nskk-current-state) 'hiragana))
    ;; Type in hiragana mode
    (nskk-integration--type-char ?a)
    (should (equal (buffer-string) "あ"))))

(nskk-deftest-integration mode-hiragana-to-latin-via-l
  "Test l key enters latin mode from hiragana."
  (nskk-integration-with-session 'hiragana
    (nskk-handle-l)
    (should (eq (nskk-state-mode nskk-current-state) 'latin))
    ;; Type in latin mode
    (nskk-integration--type-char ?a)
    (should (equal (buffer-string) "a"))))

(nskk-deftest-integration mode-q-in-ascii-inserts-q
  "Test q key inserts literal q in ascii mode."
  (nskk-integration-with-session 'ascii
    (let ((last-command-event ?q))
      (nskk-handle-q))
    (should (equal (buffer-string) "q"))))

(nskk-deftest-integration mode-l-in-ascii-inserts-l
  "Test l key inserts literal l in ascii mode."
  (nskk-integration-with-session 'ascii
    (let ((last-command-event ?l))
      (nskk-handle-l))
    (should (equal (buffer-string) "l"))))

(nskk-deftest-integration mode-roundtrip-with-input
  "Test full mode roundtrip with input at each stage."
  (nskk-integration-with-session 'hiragana
    ;; Type in hiragana
    (nskk-integration--type-char ?a)
    (should (equal (buffer-string) "あ"))
    ;; Toggle to katakana
    (nskk-handle-q)
    (nskk-integration--type-char ?a)
    (should (equal (buffer-string) "あア"))
    ;; Enter latin mode
    (nskk-handle-l)
    (nskk-integration--type-char ?a)
    (should (equal (buffer-string) "あアa"))))

;;;
;;; Uppercase Henkan Start Flow Tests
;;;

(nskk-deftest-integration uppercase-sets-marker-and-inserts
  "Test uppercase letter sets marker and inserts kana with ▽ marker."
  (nskk-integration-with-session 'hiragana
    ;; Uppercase 'A' should set marker, insert ▽ marker, then あ
    (nskk-integration--type-char ?A)
    (should (nskk--conversion-start-active-p))
    (should (equal (buffer-string) "▽あ"))))

(nskk-deftest-integration uppercase-consonant-vowel-sequence
  "Test uppercase consonant then vowel sets marker with ▽."
  (nskk-integration-with-session 'hiragana
    ;; Uppercase 'K' sets marker and inserts ▽, then 'a' produces か
    (nskk-integration--type-char ?K)
    (should (nskk--conversion-start-active-p))
    (nskk-integration--type-char ?a)
    (should (equal (buffer-string) "▽か"))))

;;;
;;; Conversion Pipeline Tests
;;;

(nskk-deftest-integration conversion-with-mock-dictionary
  "Test full conversion pipeline with mocked dictionary."
  (nskk-integration-with-session 'hiragana
    ;; Type uppercase to start henkan
    (nskk-integration--type-char ?A)
    (should (nskk--conversion-start-active-p))
    ;; Mock dictionary search
    (cl-letf (((symbol-function 'nskk-core-search)
               (lambda (_k &optional _t _l) '("亜" "阿"))))
      ;; Space starts conversion
      (let ((last-command-event ? ))
        (nskk-handle-space))
      ;; Should be in conversion mode
      (should nskk-converting-active)
      ;; Commit with return (ddskk: commit + newline)
      (nskk-handle-return)
      ;; Should have committed the first candidate
      (should-not nskk-converting-active)
      ;; ddskk handle-return commits AND inserts newline
      (should (equal (buffer-string) "亜\n")))))

(nskk-deftest-integration conversion-cancel-restores-preedit
  "Test that canceling conversion restores preedit text."
  (nskk-integration-with-session 'hiragana
    ;; Type text with henkan start
    (nskk-integration--type-char ?A)
    (let ((preedit (buffer-string)))
      ;; Mock dictionary and start conversion
      (cl-letf (((symbol-function 'nskk-core-search)
                 (lambda (_k &optional _t _l) '("result"))))
        (let ((last-command-event ? ))
          (nskk-handle-space)))
      (should nskk-converting-active)
      ;; Cancel
      (nskk-cancel-conversion)
      (should-not nskk-converting-active))))

(nskk-deftest-integration space-without-preedit-inserts-space
  "Test SPC inserts literal space when no preedit."
  (nskk-integration-with-session 'hiragana
    ;; Type something without uppercase (no marker)
    (nskk-integration--type-char ?a)
    (let ((last-command-event ? ))
      (nskk-handle-space))
    ;; Should have inserted space after the kana
    (should (equal (buffer-string) "あ "))))

(nskk-deftest-integration return-without-conversion-inserts-newline
  "Test RET inserts newline when not converting."
  (nskk-integration-with-session 'hiragana
    (nskk-integration--type-char ?a)
    (nskk-handle-return)
    (should (equal (buffer-string) "あ\n"))))

;;;
;;; State Cleanup Tests
;;;

(nskk-deftest-integration commit-clears-all-state
  "Test commit clears marker, overlay, candidates, romaji."
  (nskk-integration-with-session 'hiragana
    (nskk-integration--type-char ?A)
    (cl-letf (((symbol-function 'nskk-core-search)
               (lambda (_k &optional _t _l) '("result"))))
      (let ((last-command-event ? ))
        (nskk-handle-space)))
    (should nskk-converting-active)
    (nskk-handle-return)
    ;; All state should be clear
    (should-not nskk-converting-active)
    (should-not (nskk--conversion-start-active-p))
    (should (equal nskk--romaji-buffer ""))
    (should (null (nskk-state-candidates nskk-current-state)))))

(nskk-deftest-integration mode-switch-clears-romaji
  "Test mode switch clears romaji buffer."
  (nskk-integration-with-session 'hiragana
    ;; Type incomplete romaji
    (nskk-integration--type-char ?k)
    (should (equal nskk--romaji-buffer "k"))
    ;; Switch mode clears romaji
    (nskk-handle-q)
    (should (equal nskk--romaji-buffer ""))))

(provide 'nskk-integration-test)

;;; nskk-integration-test.el ends here
