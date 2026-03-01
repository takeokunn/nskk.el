;;; nskk-dict-registration-integration-test.el --- Dictionary registration integration tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Integration tests for dictionary registration, user dictionary management,
;; and multi-buffer state isolation.

;;; Code:

(require 'ert)
(require 'nskk-state)
(require 'nskk-dictionary)
(require 'nskk-henkan)
(require 'nskk-input)
(require 'nskk-test-framework)


;;;
;;; Group 1: Dictionary Registration (nskk-dict-register-word)
;;;

(nskk-deftest-integration dict-register-new-word
  "Test registering a new word makes it findable via nskk-dict-lookup."
  (nskk-prolog-test-with-isolated-db
    (let ((nskk--user-dict-index 'user)
          (nskk-dict-modified nil))
      ;; Set up trie index for user-dict-entry predicate
      (nskk-prolog-set-index 'user-dict-entry 2 :trie)
      ;; Register a new word
      (nskk-dict-register-word "てすと" "テスト")
      ;; Should be findable by lookup
      (should (member "テスト" (nskk-dict-lookup "てすと"))))))

(nskk-deftest-integration dict-register-existing-word-no-duplicate
  "Test registering a word already in the dictionary does not create duplicates."
  (nskk-prolog-test-with-isolated-db
    (let ((nskk--user-dict-index 'user)
          (nskk-dict-modified nil))
      ;; Set up initial entry
      (nskk-prolog-set-index 'user-dict-entry 2 :trie)
      (nskk-prolog-assert '((user-dict-entry "さくら" ("桜"))))
      ;; Register the same word again
      (nskk-dict-register-word "さくら" "桜")
      ;; Lookup should return exactly one occurrence
      (let ((candidates (nskk-dict-lookup "さくら")))
        (should (member "桜" candidates))
        ;; Count occurrences: should appear exactly once
        (should (= (length (cl-remove-if-not (lambda (c) (equal c "桜")) candidates)) 1))))))

(nskk-deftest-integration dict-register-prepends-to-candidates
  "Test that a newly registered word appears first in the candidate list."
  (nskk-prolog-test-with-isolated-db
    (let ((nskk--user-dict-index 'user)
          (nskk-dict-modified nil))
      ;; Set up existing entry with multiple candidates
      (nskk-prolog-set-index 'user-dict-entry 2 :trie)
      (nskk-prolog-assert '((user-dict-entry "かんじ" ("漢字" "感じ"))))
      ;; Register a new word for the same reading
      (nskk-dict-register-word "かんじ" "幹事")
      ;; The newly registered word should appear first
      (let ((candidates (nskk-dict-lookup "かんじ")))
        (should (member "幹事" candidates))
        (nskk-should-equal "幹事" (car candidates))))))


(nskk-deftest-integration dict-register-multiple-words
  "Test that multiple registrations for the same reading accumulate correctly."
  (nskk-prolog-test-with-isolated-db
    (let ((nskk--user-dict-index 'user)
          (nskk-dict-modified nil))
      ;; Start with a clean trie
      (nskk-prolog-set-index 'user-dict-entry 2 :trie)
      ;; Register three words for the same reading
      (nskk-dict-register-word "やま" "山")
      (nskk-dict-register-word "やま" "夜魔")
      (nskk-dict-register-word "やま" "ヤマ")
      ;; All three should be findable
      (let ((candidates (nskk-dict-lookup "やま")))
        (should (member "山" candidates))
        (should (member "夜魔" candidates))
        (should (member "ヤマ" candidates))))))

(nskk-deftest-integration dict-register-sets-modified-flag
  "Test that nskk-dict-modified is set to t after a successful registration."
  (nskk-prolog-test-with-isolated-db
    (let ((nskk--user-dict-index 'user)
          (nskk-dict-modified nil))
      ;; Ensure trie index exists
      (nskk-prolog-set-index 'user-dict-entry 2 :trie)
      ;; modified flag starts as nil
      (should (null nskk-dict-modified))
      ;; Register a word
      (nskk-dict-register-word "にほん" "日本")
      ;; modified flag should now be t
      (should (eq nskk-dict-modified t)))))

(nskk-deftest-integration dict-register-empty-key-noop
  "Test that registering with an empty reading string is a no-op and does not error."
  (nskk-prolog-test-with-isolated-db
    (let ((nskk--user-dict-index 'user)
          (nskk-dict-modified nil))
      (nskk-prolog-set-index 'user-dict-entry 2 :trie)
      ;; Should not signal any error
      (should-not (nskk-dict-register-word "" "テスト"))
      ;; Modified flag must remain nil (no registration occurred)
      (should (null nskk-dict-modified)))))

(nskk-deftest-integration dict-register-empty-word-noop
  "Test that registering an empty word string is a no-op and does not error."
  (nskk-prolog-test-with-isolated-db
    (let ((nskk--user-dict-index 'user)
          (nskk-dict-modified nil))
      (nskk-prolog-set-index 'user-dict-entry 2 :trie)
      ;; Should not signal any error
      (should-not (nskk-dict-register-word "てすと" ""))
      ;; Modified flag must remain nil (no registration occurred)
      (should (null nskk-dict-modified)))))

(nskk-deftest-integration dict-register-triggers-hook
  "Test that nskk-jisyo-update-hook is called after a successful registration."
  (nskk-prolog-test-with-isolated-db
    (let ((nskk--user-dict-index 'user)
          (nskk-dict-modified nil)
          (hook-called nil))
      (nskk-prolog-set-index 'user-dict-entry 2 :trie)
      ;; Use cl-letf to shadow the global hook variable during this test
      (cl-letf (((symbol-value 'nskk-jisyo-update-hook)
                 (list (lambda () (setq hook-called t)))))
        ;; hook should not have been called yet
        (should (null hook-called))
        ;; Register a word
        (nskk-dict-register-word "ほん" "本")
        ;; hook must have been called
        (should (eq hook-called t))))))


;;;
;;; Group 2: Multi-Buffer State Isolation
;;;

(nskk-deftest-integration multi-buffer-separate-modes
  "Test that NSKK mode is independent per buffer."
  (let ((buf-a (generate-new-buffer " *nskk-test-a*"))
        (buf-b (generate-new-buffer " *nskk-test-b*")))
    (unwind-protect
        (progn
          ;; Set up buffer A with hiragana mode
          (with-current-buffer buf-a
            (setq-local nskk-current-state (nskk-state-create 'hiragana)))
          ;; Set up buffer B with katakana mode
          (with-current-buffer buf-b
            (setq-local nskk-current-state (nskk-state-create 'katakana)))
          ;; Verify buffer A has hiragana mode
          (with-current-buffer buf-a
            (should (eq (nskk-state-mode nskk-current-state) 'hiragana)))
          ;; Verify buffer B has katakana mode
          (with-current-buffer buf-b
            (should (eq (nskk-state-mode nskk-current-state) 'katakana)))
          ;; Modify buffer B mode and verify buffer A is unaffected
          (with-current-buffer buf-b
            (nskk-state-set nskk-current-state 'mode 'latin))
          (with-current-buffer buf-a
            (should (eq (nskk-state-mode nskk-current-state) 'hiragana)))
          (with-current-buffer buf-b
            (should (eq (nskk-state-mode nskk-current-state) 'latin))))
      (kill-buffer buf-a)
      (kill-buffer buf-b))))

(nskk-deftest-integration multi-buffer-separate-preedit
  "Test that preedit state in buffer A does not affect buffer B."
  (let ((buf-a (generate-new-buffer " *nskk-test-preedit-a*"))
        (buf-b (generate-new-buffer " *nskk-test-preedit-b*")))
    (unwind-protect
        (progn
          ;; Initialize both buffers with hiragana state
          (with-current-buffer buf-a
            (setq-local nskk-current-state (nskk-state-create 'hiragana))
            (setq-local nskk--romaji-buffer ""))
          (with-current-buffer buf-b
            (setq-local nskk-current-state (nskk-state-create 'hiragana))
            (setq-local nskk--romaji-buffer ""))
          ;; Append input to buffer A's state
          (with-current-buffer buf-a
            (nskk-state-append-input nskk-current-state ?か)
            (setq-local nskk--romaji-buffer "k"))
          ;; Buffer B's state should remain unmodified
          (with-current-buffer buf-b
            (nskk-should-equal "" (nskk-state-input-buffer nskk-current-state))
            (nskk-should-equal "" nskk--romaji-buffer))
          ;; Buffer A should reflect the modification
          (with-current-buffer buf-a
            (nskk-should-equal "か" (nskk-state-input-buffer nskk-current-state))
            (nskk-should-equal "k" nskk--romaji-buffer)))
      (kill-buffer buf-a)
      (kill-buffer buf-b))))

(nskk-deftest-integration multi-buffer-romaji-isolation
  "Test that nskk--romaji-buffer is independent per buffer."
  (let ((buf-a (generate-new-buffer " *nskk-test-romaji-a*"))
        (buf-b (generate-new-buffer " *nskk-test-romaji-b*")))
    (unwind-protect
        (progn
          ;; Initialize romaji buffers independently
          (with-current-buffer buf-a
            (setq-local nskk--romaji-buffer ""))
          (with-current-buffer buf-b
            (setq-local nskk--romaji-buffer ""))
          ;; Set romaji in buffer A
          (with-current-buffer buf-a
            (setq nskk--romaji-buffer "ka"))
          ;; Buffer B must not be affected
          (with-current-buffer buf-b
            (nskk-should-equal "" nskk--romaji-buffer))
          ;; Set different romaji in buffer B
          (with-current-buffer buf-b
            (setq nskk--romaji-buffer "shi"))
          ;; Buffer A should still have its own value
          (with-current-buffer buf-a
            (nskk-should-equal "ka" nskk--romaji-buffer)))
      (kill-buffer buf-a)
      (kill-buffer buf-b))))

(nskk-deftest-integration multi-buffer-conversion-state
  "Test that conversion (henkan) state in buffer A does not affect buffer B."
  (let ((buf-a (generate-new-buffer " *nskk-test-conv-a*"))
        (buf-b (generate-new-buffer " *nskk-test-conv-b*")))
    (unwind-protect
        (progn
          ;; Initialize both buffers with fresh hiragana state
          (with-current-buffer buf-a
            (setq-local nskk-current-state (nskk-state-create 'hiragana)))
          (with-current-buffer buf-b
            (setq-local nskk-current-state (nskk-state-create 'hiragana)))
          ;; Put buffer A into henkan-on phase and set candidates
          (with-current-buffer buf-a
            (nskk-state-force-henkan-phase nskk-current-state 'on)
            (nskk-state-set-candidates nskk-current-state '("漢字" "感じ")))
          ;; Buffer B should still have nil henkan-phase and no candidates
          (with-current-buffer buf-b
            (should (null (nskk-state-henkan-phase nskk-current-state)))
            (should (null (nskk-state-candidates nskk-current-state))))
          ;; Buffer A should retain its modified state
          (with-current-buffer buf-a
            (should (eq (nskk-state-henkan-phase nskk-current-state) 'on))
            (nskk-should-equal '("漢字" "感じ") (nskk-state-candidates nskk-current-state))))
      (kill-buffer buf-a)
      (kill-buffer buf-b))))


;;;
;;; Group 3: Dictionary Lookup Order
;;;

(nskk-deftest-integration dict-user-takes-priority
  "Test that user-dict candidates come before system-dict candidates for the same reading."
  (nskk-prolog-test-with-isolated-db
    ;; Set up both user and system dict entries for the same reading
    (nskk-prolog-set-index 'user-dict-entry 2 :trie)
    (nskk-prolog-set-index 'system-dict-entry 2 :trie)
    (nskk-prolog-assert '((user-dict-entry "はな" ("華"))))
    (nskk-prolog-assert '((system-dict-entry "はな" ("花" "鼻"))))
    ;; Lookup via the bridge rule
    (let ((candidates (nskk-dict-lookup "はな")))
      (should (listp candidates))
      ;; User dict candidate must be present
      (should (member "華" candidates))
      ;; System dict candidates must also be present
      (should (member "花" candidates))
      (should (member "鼻" candidates))
      ;; User dict entry must appear before system dict entries
      ;; The bridge rule: dict-entry queries user-dict-entry first, so "華"
      ;; arrives in the first solution set returned before "花"/"鼻".
      (should (< (cl-position "華" candidates :test #'equal)
                 (cl-position "花" candidates :test #'equal))))))

(nskk-deftest-integration dict-lookup-returns-nil-for-unknown
  "Test that looking up a reading not in any dictionary returns nil."
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-set-index 'user-dict-entry 2 :trie)
    (nskk-prolog-set-index 'system-dict-entry 2 :trie)
    ;; Do NOT assert any entry for "ほげほげ"
    (let ((result (nskk-dict-lookup "ほげほげ")))
      (should (null result)))))


(provide 'nskk-dict-registration-integration-test)

;;; nskk-dict-registration-integration-test.el ends here
