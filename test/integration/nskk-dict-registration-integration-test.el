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
(require 'nskk-test-macros)


;;;
;;; Group 1: Dictionary Registration (nskk-dict-register-word)
;;;

(nskk-describe "dictionary registration"

  (nskk-it "registering a new word makes it findable via nskk-dict-lookup"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk--user-dict-index 'user)
            (nskk-dict-modified nil))
        (nskk-prolog-set-index 'user-dict-entry 2 :trie)
        (nskk-when  (nskk-dict-register-word "てすと" "テスト"))
        (nskk-then  (should (member "テスト" (nskk-dict-lookup "てすと")))))))

  (nskk-it "registering a word already in the dictionary does not create duplicates"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk--user-dict-index 'user)
            (nskk-dict-modified nil))
        (nskk-prolog-set-index 'user-dict-entry 2 :trie)
        (nskk-given (nskk-prolog-assert '((user-dict-entry "さくら" ("桜")))))
        (nskk-when  (nskk-dict-register-word "さくら" "桜"))
        (nskk-then
          (let ((candidates (nskk-dict-lookup "さくら")))
            (should (member "桜" candidates))
            (should (= (length (cl-remove-if-not (lambda (c) (equal c "桜")) candidates)) 1)))))))

  (nskk-it "a newly registered word appears first in the candidate list"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk--user-dict-index 'user)
            (nskk-dict-modified nil))
        (nskk-prolog-set-index 'user-dict-entry 2 :trie)
        (nskk-given (nskk-prolog-assert '((user-dict-entry "かんじ" ("漢字" "感じ")))))
        (nskk-when  (nskk-dict-register-word "かんじ" "幹事"))
        (nskk-then
          (let ((candidates (nskk-dict-lookup "かんじ")))
            (should (member "幹事" candidates))
            (nskk-should-equal "幹事" (car candidates)))))))

  (nskk-it "multiple registrations for the same reading accumulate correctly"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk--user-dict-index 'user)
            (nskk-dict-modified nil))
        (nskk-prolog-set-index 'user-dict-entry 2 :trie)
        (nskk-when
          (nskk-dict-register-word "やま" "山")
          (nskk-dict-register-word "やま" "夜魔")
          (nskk-dict-register-word "やま" "ヤマ"))
        (nskk-then
          (let ((candidates (nskk-dict-lookup "やま")))
            (should (member "山" candidates))
            (should (member "夜魔" candidates))
            (should (member "ヤマ" candidates)))))))

  (nskk-it "nskk-dict-modified is set to t after a successful registration"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk--user-dict-index 'user)
            (nskk-dict-modified nil))
        (nskk-prolog-set-index 'user-dict-entry 2 :trie)
        (nskk-given (should (null nskk-dict-modified)))
        (nskk-when  (nskk-dict-register-word "にほん" "日本"))
        (nskk-then  (should (eq nskk-dict-modified t))))))

  (nskk-it "registering with an empty reading string is a no-op and does not error"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk--user-dict-index 'user)
            (nskk-dict-modified nil))
        (nskk-prolog-set-index 'user-dict-entry 2 :trie)
        (nskk-when  (should-not (nskk-dict-register-word "" "テスト")))
        (nskk-then  (should (null nskk-dict-modified))))))

  (nskk-it "registering an empty word string is a no-op and does not error"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk--user-dict-index 'user)
            (nskk-dict-modified nil))
        (nskk-prolog-set-index 'user-dict-entry 2 :trie)
        (nskk-when  (should-not (nskk-dict-register-word "てすと" "")))
        (nskk-then  (should (null nskk-dict-modified))))))

  (nskk-it "nskk-jisyo-update-hook is called after a successful registration"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk--user-dict-index 'user)
            (nskk-dict-modified nil)
            (hook-called nil))
        (nskk-prolog-set-index 'user-dict-entry 2 :trie)
        (cl-letf (((symbol-value 'nskk-jisyo-update-hook)
                   (list (lambda () (setq hook-called t)))))
          (nskk-given (should (null hook-called)))
          (nskk-when  (nskk-dict-register-word "ほん" "本"))
          (nskk-then  (should (eq hook-called t))))))))


;;;
;;; Group 2: Multi-Buffer State Isolation
;;;

(nskk-describe "multi-buffer state isolation"

  (nskk-it "NSKK mode is independent per buffer"
    (let ((buf-a (generate-new-buffer " *nskk-test-a*"))
          (buf-b (generate-new-buffer " *nskk-test-b*")))
      (unwind-protect
          (progn
            (nskk-given
              (with-current-buffer buf-a
                (setq-local nskk-current-state (nskk-state-create 'hiragana)))
              (with-current-buffer buf-b
                (setq-local nskk-current-state (nskk-state-create 'katakana))))
            (nskk-then
              (with-current-buffer buf-a
                (should (eq (nskk-state-mode nskk-current-state) 'hiragana)))
              (with-current-buffer buf-b
                (should (eq (nskk-state-mode nskk-current-state) 'katakana))))
            (nskk-when
              (with-current-buffer buf-b
                (nskk-state-set nskk-current-state 'mode 'latin)))
            (nskk-then
              (with-current-buffer buf-a
                (should (eq (nskk-state-mode nskk-current-state) 'hiragana)))
              (with-current-buffer buf-b
                (should (eq (nskk-state-mode nskk-current-state) 'latin)))))
        (kill-buffer buf-a)
        (kill-buffer buf-b))))

  (nskk-it "preedit state in buffer A does not affect buffer B"
    (let ((buf-a (generate-new-buffer " *nskk-test-preedit-a*"))
          (buf-b (generate-new-buffer " *nskk-test-preedit-b*")))
      (unwind-protect
          (progn
            (nskk-given
              (with-current-buffer buf-a
                (setq-local nskk-current-state (nskk-state-create 'hiragana))
                (setq-local nskk--romaji-buffer ""))
              (with-current-buffer buf-b
                (setq-local nskk-current-state (nskk-state-create 'hiragana))
                (setq-local nskk--romaji-buffer "")))
            (nskk-when
              (with-current-buffer buf-a
                (nskk-state-append-input nskk-current-state ?か)
                (setq-local nskk--romaji-buffer "k")))
            (nskk-then
              (with-current-buffer buf-b
                (nskk-should-equal "" (nskk-state-input-buffer nskk-current-state))
                (nskk-should-equal "" nskk--romaji-buffer))
              (with-current-buffer buf-a
                (nskk-should-equal "か" (nskk-state-input-buffer nskk-current-state))
                (nskk-should-equal "k" nskk--romaji-buffer))))
        (kill-buffer buf-a)
        (kill-buffer buf-b))))

  (nskk-it "nskk--romaji-buffer is independent per buffer"
    (let ((buf-a (generate-new-buffer " *nskk-test-romaji-a*"))
          (buf-b (generate-new-buffer " *nskk-test-romaji-b*")))
      (unwind-protect
          (progn
            (nskk-given
              (with-current-buffer buf-a
                (setq-local nskk--romaji-buffer ""))
              (with-current-buffer buf-b
                (setq-local nskk--romaji-buffer "")))
            (nskk-when
              (with-current-buffer buf-a
                (setq nskk--romaji-buffer "ka")))
            (nskk-then
              (with-current-buffer buf-b
                (nskk-should-equal "" nskk--romaji-buffer)))
            (nskk-when
              (with-current-buffer buf-b
                (setq nskk--romaji-buffer "shi")))
            (nskk-then
              (with-current-buffer buf-a
                (nskk-should-equal "ka" nskk--romaji-buffer))))
        (kill-buffer buf-a)
        (kill-buffer buf-b))))

  (nskk-it "conversion (henkan) state in buffer A does not affect buffer B"
    (let ((buf-a (generate-new-buffer " *nskk-test-conv-a*"))
          (buf-b (generate-new-buffer " *nskk-test-conv-b*")))
      (unwind-protect
          (progn
            (nskk-given
              (with-current-buffer buf-a
                (setq-local nskk-current-state (nskk-state-create 'hiragana)))
              (with-current-buffer buf-b
                (setq-local nskk-current-state (nskk-state-create 'hiragana))))
            (nskk-when
              (with-current-buffer buf-a
                (nskk-state-force-henkan-phase nskk-current-state 'on)
                (nskk-state-set-candidates nskk-current-state '("漢字" "感じ"))))
            (nskk-then
              (with-current-buffer buf-b
                (should (null (nskk-state-henkan-phase nskk-current-state)))
                (should (null (nskk-state-candidates nskk-current-state))))
              (with-current-buffer buf-a
                (should (eq (nskk-state-henkan-phase nskk-current-state) 'on))
                (nskk-should-equal '("漢字" "感じ") (nskk-state-candidates nskk-current-state)))))
        (kill-buffer buf-a)
        (kill-buffer buf-b)))))


;;;
;;; Group 3: Dictionary Lookup Order
;;;

(nskk-describe "dictionary lookup order"

  (nskk-it "user-dict candidates come before system-dict candidates for the same reading"
    (nskk-prolog-test-with-isolated-db
      (nskk-given
        (nskk-prolog-set-index 'user-dict-entry 2 :trie)
        (nskk-prolog-set-index 'system-dict-entry 2 :trie)
        (nskk-prolog-assert '((user-dict-entry "はな" ("華"))))
        (nskk-prolog-assert '((system-dict-entry "はな" ("花" "鼻")))))
      (nskk-then
        (let ((candidates (nskk-dict-lookup "はな")))
          (should (listp candidates))
          (should (member "華" candidates))
          (should (member "花" candidates))
          (should (member "鼻" candidates))
          (should (< (cl-position "華" candidates :test #'equal)
                     (cl-position "花" candidates :test #'equal)))))))

  (nskk-it "looking up a reading not in any dictionary returns nil"
    (nskk-prolog-test-with-isolated-db
      (nskk-given
        (nskk-prolog-set-index 'user-dict-entry 2 :trie)
        (nskk-prolog-set-index 'system-dict-entry 2 :trie))
      (nskk-then
        (let ((result (nskk-dict-lookup "ほげほげ")))
          (should (null result)))))))


;;;
;;; Property-Based Tests
;;;

(require 'nskk-pbt-generators)

(nskk-deftest-cases dict-registration-readings
  (("てすと" . "テスト")
   ("さくら" . "桜")
   ("やま"   . "山"))
  :body
  (nskk-prolog-test-with-isolated-db
    (let ((nskk--user-dict-index 'user)
          (nskk-dict-modified nil))
      (nskk-prolog-set-index 'user-dict-entry 2 :trie)
      (nskk-dict-register-word input expected)
      (should (member expected (nskk-dict-lookup input))))))

(nskk-property-test dict-registration-lookup-roundtrip
  ((q search-query))
  (nskk-prolog-test-with-isolated-db
    (let ((nskk--user-dict-index 'user)
          (nskk-dict-modified nil))
      (nskk-prolog-set-index 'user-dict-entry 2 :trie)
      (let ((word "テスト"))
        (nskk-dict-register-word q word)
        (should (member word (nskk-dict-lookup q))))))
  30)

(nskk-property-test dict-lookup-unknown-reading-returns-nil
  ((q search-query))
  (nskk-prolog-test-with-isolated-db
    ;; Fresh empty DB; any random query should return nil (not in dict)
    (should (null (nskk-dict-lookup q))))
  30)

(nskk-describe "Dict registration property: modified flag"
  (nskk-it "nskk-dict-modified is set after any registration"
    (dotimes (_ 20)
      (nskk-for-all ((q search-query))
        (nskk-prolog-test-with-isolated-db
          (let ((nskk--user-dict-index 'user)
                (nskk-dict-modified nil))
            (nskk-prolog-set-index 'user-dict-entry 2 :trie)
            (nskk-dict-register-word q "テスト")
            (should nskk-dict-modified)))))))


(provide 'nskk-dict-registration-integration-test)

;;; nskk-dict-registration-integration-test.el ends here
