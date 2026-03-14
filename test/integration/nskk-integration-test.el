;;; nskk-integration-test.el --- Integration tests -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
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
(require 'nskk-input)
(require 'nskk-keymap)
(require 'nskk-state)
(require 'nskk-converter)
(require 'nskk-test-framework)
(require 'nskk-test-macros)

;;;
;;; Hiragana Input Lifecycle Tests
;;;

(nskk-describe "hiragana input lifecycle"

  (nskk-deftest-table integration-hiragana-vowels
    :description "Single vowels produce correct hiragana"
    :columns (char expected)
    :rows ((?a "あ") (?i "い") (?u "う") (?e "え") (?o "お"))
    :body (nskk-integration-with-session 'hiragana
            (nskk--integration-type-char char)
            (nskk-should-equal expected (buffer-string))))

  (nskk-it "typing consonant + vowel produces か"
    (nskk-integration-with-session 'hiragana
      (nskk-when (progn
                   (nskk--integration-type-char ?k)
                   (nskk--integration-type-char ?a)))
      (nskk-then (nskk-should-equal "か" (buffer-string)))))

  (nskk-it "typing sakura produces さくら"
    (nskk-integration-with-session 'hiragana
      (nskk-when (dolist (c '(?s ?a ?k ?u ?r ?a))
                   (nskk--integration-type-char c)))
      (nskk-then (nskk-should-equal "さくら" (buffer-string)))))

  (nskk-it "typing kanji produces かんじ via n+consonant rule"
    (nskk-integration-with-session 'hiragana
      (nskk-when (dolist (c '(?k ?a ?n ?j ?i))
                   (nskk--integration-type-char c)))
      (nskk-then (nskk-should-equal "かんじ" (buffer-string)))))

  (nskk-it "typing kitte produces きって via sokuon rule"
    (nskk-integration-with-session 'hiragana
      (nskk-when (dolist (c '(?k ?i ?t ?t ?e))
                   (nskk--integration-type-char c)))
      (nskk-then (nskk-should-equal "きって" (buffer-string))))))

;;;
;;; Katakana Input Lifecycle Tests
;;;

(nskk-describe "katakana input lifecycle"

  (nskk-it "typing 'a' in katakana mode produces ア"
    (nskk-integration-with-session 'katakana
      (nskk-given (nskk-should-mode 'katakana))
      (nskk-when  (nskk--integration-type-char ?a))
      (nskk-then  (nskk-should-equal "ア" (buffer-string)))))

  (nskk-it "typing katakana in katakana mode produces カタカナ"
    (nskk-integration-with-session 'katakana
      (nskk-given (nskk-should-mode 'katakana))
      (nskk-when  (dolist (c '(?k ?a ?t ?a ?k ?a ?n ?a))
                    (nskk--integration-type-char c)))
      (nskk-then  (nskk-should-equal "カタカナ" (buffer-string))))))

;;;
;;; Latin and ASCII Passthrough Tests
;;;

(nskk-describe "latin and ascii passthrough"

  (nskk-it "latin mode passes characters through as-is"
    (nskk-integration-with-session 'latin
      (nskk-when (progn
                   (nskk--integration-type-char ?h)
                   (nskk--integration-type-char ?i)))
      (nskk-then (nskk-should-equal "hi" (buffer-string)))))

  (nskk-it "ascii mode passes characters through as-is"
    (nskk-integration-with-session 'ascii
      (nskk-when (progn
                   (nskk--integration-type-char ?A)
                   (nskk--integration-type-char ?B)))
      (nskk-then (nskk-should-equal "AB" (buffer-string))))))

;;;
;;; Mode Transition Flow Tests
;;;

(nskk-describe "mode transition flow"

  (nskk-it "q key toggles hiragana to katakana and types ア"
    (nskk-integration-with-session 'hiragana
      (nskk-when  (nskk-handle-q))
      (nskk-then  (should (eq (nskk-state-mode nskk-current-state) 'katakana)))
      (nskk-when  (nskk--integration-type-char ?a))
      (nskk-then  (nskk-should-equal "ア" (buffer-string)))))

  (nskk-it "q key toggles katakana to hiragana and types あ"
    (nskk-integration-with-session 'katakana
      (nskk-when  (nskk-handle-q))
      (nskk-then  (should (eq (nskk-state-mode nskk-current-state) 'hiragana)))
      (nskk-when  (nskk--integration-type-char ?a))
      (nskk-then  (nskk-should-equal "あ" (buffer-string)))))

  (nskk-it "l key enters latin mode from hiragana and types a"
    (nskk-integration-with-session 'hiragana
      (nskk-when  (nskk-handle-l))
      (nskk-then  (should (eq (nskk-state-mode nskk-current-state) 'latin)))
      (nskk-when  (nskk--integration-type-char ?a))
      (nskk-then  (nskk-should-equal "a" (buffer-string)))))

  (nskk-it "q key inserts literal q in ascii mode"
    (nskk-integration-with-session 'ascii
      (nskk-when (let ((last-command-event ?q))
                   (nskk-handle-q)))
      (nskk-then (nskk-should-equal "q" (buffer-string)))))

  (nskk-it "l key inserts literal l in ascii mode"
    (nskk-integration-with-session 'ascii
      (nskk-when (let ((last-command-event ?l))
                   (nskk-handle-l)))
      (nskk-then (nskk-should-equal "l" (buffer-string)))))

  (nskk-it "full mode roundtrip produces あアa"
    (nskk-integration-with-session 'hiragana
      ;; Type in hiragana
      (nskk--integration-type-char ?a)
      (nskk-should-equal "あ" (buffer-string))
      ;; Toggle to katakana
      (nskk-handle-q)
      (nskk--integration-type-char ?a)
      (nskk-should-equal "あア" (buffer-string))
      ;; Enter latin mode
      (nskk-handle-l)
      (nskk--integration-type-char ?a)
      (nskk-should-equal "あアa" (buffer-string)))))

;;;
;;; Uppercase Henkan Start Tests
;;;

(nskk-describe "uppercase henkan start"

  (nskk-it "typing A starts henkan with ▽あ"
    (nskk-integration-with-session 'hiragana
      (nskk-when  (nskk--integration-type-char ?A))
      (nskk-then  (should (nskk--conversion-start-active-p))
                  (nskk-should-equal "▽あ" (buffer-string)))))

  (nskk-it "typing K then a starts henkan with ▽か"
    (nskk-integration-with-session 'hiragana
      (nskk-when  (nskk--integration-type-char ?K))
      (nskk-then  (should (nskk--conversion-start-active-p)))
      (nskk-when  (nskk--integration-type-char ?a))
      (nskk-then  (nskk-should-equal "▽か" (buffer-string))))))

;;;
;;; Conversion Pipeline Tests
;;;

(nskk-describe "conversion pipeline"

  (nskk-it "SPC after preedit triggers conversion and commits first candidate"
    (nskk-integration-with-session 'hiragana
      (nskk-when  (nskk--integration-type-char ?A))
      (nskk-then  (should (nskk--conversion-start-active-p)))
      (nskk-with-mocks ((nskk-core-search/k
                         (lambda (_k _t _l on-found _on-not-found)
                           (funcall on-found '("亜" "阿")))))
        (nskk-when (let ((last-command-event ? ))
                     (nskk-handle-space)))
        (nskk-then (should (nskk-converting-p)))
        (nskk-when (nskk-handle-return))
        (nskk-then (should-not (nskk-converting-p))
                   (nskk-should-equal "亜" (buffer-string))))))

  (nskk-it "canceling conversion exits converting state"
    (nskk-integration-with-session 'hiragana
      (nskk--integration-type-char ?A)
      (nskk-with-mocks ((nskk-core-search/k
                         (lambda (_k _t _l on-found _on-not-found)
                           (funcall on-found '("result")))))
        (let ((last-command-event ? ))
          (nskk-handle-space)))
      (nskk-then (should (nskk-converting-p)))
      (nskk-when (nskk-cancel-conversion))
      (nskk-then (should-not (nskk-converting-p)))))

  (nskk-it "SPC without preedit inserts literal space"
    (nskk-integration-with-session 'hiragana
      (nskk--integration-type-char ?a)
      (nskk-when (let ((last-command-event ? ))
                   (nskk-handle-space)))
      (nskk-then (nskk-should-equal "あ " (buffer-string)))))

  (nskk-it "RET without conversion inserts newline"
    (nskk-integration-with-session 'hiragana
      (nskk--integration-type-char ?a)
      (nskk-when  (nskk-handle-return))
      (nskk-then  (nskk-should-equal "あ\n" (buffer-string))))))

;;;
;;; State Cleanup Tests
;;;

(nskk-describe "state cleanup after commit"

  (nskk-it "commit clears marker, overlay, candidates, and romaji buffer"
    (nskk-integration-with-session 'hiragana
      (nskk--integration-type-char ?A)
      (nskk-with-mocks ((nskk-core-search/k
                         (lambda (_k _t _l on-found _on-not-found)
                           (funcall on-found '("result")))))
        (let ((last-command-event ? ))
          (nskk-handle-space)))
      (nskk-then (should (nskk-converting-p)))
      (nskk-when (nskk-handle-return))
      (nskk-then (should-not (nskk-converting-p))
                 (should-not (nskk--conversion-start-active-p))
                 (nskk-should-equal "" nskk--romaji-buffer)
                 (should (null (nskk-state-candidates nskk-current-state))))))

  (nskk-it "mode switch clears romaji buffer"
    (nskk-integration-with-session 'hiragana
      (nskk--integration-type-char ?k)
      (nskk-then (nskk-should-equal "k" nskk--romaji-buffer))
      (nskk-when (nskk-handle-q))
      (nskk-then (nskk-should-equal "" nskk--romaji-buffer)))))

;;;
;;; Backspace Handling Tests
;;;

(nskk-describe "backspace handling"

  (nskk-it "DEL in preedit deletes last kana character leaving ▽"
    (nskk-integration-with-session 'hiragana
      (nskk--integration-type-char ?K)
      (nskk--integration-type-char ?a)
      (nskk-then  (nskk-should-equal "▽か" (buffer-string)))
      (nskk-when  (nskk-handle-backspace))
      (nskk-then  (should (nskk--conversion-start-active-p))
                  (should-not (string-suffix-p "か" (buffer-string)))
                  (nskk-should-equal "▽" (buffer-string)))))

  (nskk-it "DEL on empty preedit cancels preedit entirely"
    (nskk-integration-with-session 'hiragana
      (nskk--integration-type-char ?A)
      (nskk-then  (nskk-should-equal "▽あ" (buffer-string)))
      ;; First DEL: deletes "あ", leaving empty preedit
      (nskk-when  (nskk-handle-backspace))
      (nskk-then  (nskk-should-equal "▽" (buffer-string)))
      ;; Second DEL: empty preedit → cancel preedit
      (nskk-when  (nskk-handle-backspace))
      (nskk-then  (should-not (nskk--conversion-start-active-p))
                  (nskk-should-equal "" (buffer-string)))))

  (nskk-it "DEL in normal mode deletes the preceding character"
    (nskk-integration-with-session 'hiragana
      (nskk--integration-type-char ?a)
      (nskk-then  (nskk-should-equal "あ" (buffer-string)))
      (nskk-when  (nskk-handle-backspace))
      (nskk-then  (nskk-should-equal "" (buffer-string))))))

;;;
;;; Cancel Handler Tests
;;;

(nskk-describe "cancel handler"

  (nskk-it "C-g in preedit clears preedit text and marker"
    (nskk-integration-with-session 'hiragana
      (nskk--integration-type-char ?A)
      (nskk-then  (nskk-should-equal "▽あ" (buffer-string))
                  (should (nskk--conversion-start-active-p)))
      (nskk-when  (nskk-handle-cancel))
      (nskk-then  (should-not (nskk--conversion-start-active-p))
                  (nskk-should-equal "" (buffer-string))))))

;;;
;;; Ctrl-N/Ctrl-P Fallthrough Tests
;;;

(nskk-describe "ctrl-n/ctrl-p fallthrough"

  (nskk-it "C-n in normal mode falls through to next-line without error"
    (nskk-integration-with-session 'hiragana
      (insert "line1\nline2\n")
      (goto-char (point-min))
      (condition-case nil
          (nskk-handle-ctrl-n)
        (beginning-of-buffer nil)
        (end-of-buffer nil))
      (should (> (point) (point-min)))))

  (nskk-it "C-p in normal mode falls through to previous-line without error"
    (nskk-integration-with-session 'hiragana
      (insert "line1\nline2\n")
      (goto-char (point-max))
      (condition-case nil
          (nskk-handle-ctrl-p)
        (beginning-of-buffer nil)
        (end-of-buffer nil))
      (should (< (point) (point-max))))))

(provide 'nskk-integration-test)

;;; nskk-integration-test.el ends here
