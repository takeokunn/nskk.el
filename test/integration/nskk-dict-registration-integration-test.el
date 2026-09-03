;;; nskk-dict-registration-integration-test.el --- Dictionary registration integration tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Dictionary registration integration tests.

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
        (nskk-prolog-retract-all 'user-dict-entry 2)
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

  (nskk-it "rejects every prohibited registration field before observable effects"
    (nskk-prolog-test-with-isolated-db
      (let* ((dictionary-file
              (make-temp-file "nskk-register-invalid-" nil ".skk"))
             (nskk-dict-user-dictionary-file dictionary-file)
             (nskk--user-dict-index 'user)
             (nskk-dict-modified 'preserved)
             (hook-calls nil)
             (nskk-jisyo-update-hook
              (list (lambda () (push 'called hook-calls))))
             (shared-invalid
              (append
               (mapcar #'string
                       (append (number-sequence 0 31) '(127)))
               '("/" ";" "▽" "▼")))
             (cases
              (append
               (mapcar (lambda (value)
                         (list (concat "よ" value "み") "候補"))
                       shared-invalid)
               (mapcar (lambda (value)
                         (list "よみ" (concat "候" value "補")))
                       shared-invalid)
               '(("" "候補")
                 (42 "候補")
                 ("よ み" "候補")
                 ("よみ" "")
                 ("よみ" 42)))))
        (unwind-protect
            (progn
              (nskk-prolog-set-index 'user-dict-entry 2 :trie)
              (nskk-prolog-assert
               '((user-dict-entry "既存" ("既存候補"))))
              (with-temp-file dictionary-file
                (set-buffer-multibyte nil)
                (insert "literal-before"))
              (let ((before
                     (nskk-dict-transaction-predicate-snapshot
                      (nskk-prolog-clause-key 'user-dict-entry 2))))
                (dolist (case cases)
                  (let ((condition
                         (condition-case err
                             (progn
                               (nskk-dict-register-word
                                (car case) (cadr case))
                               nil)
                           (nskk-dict-error err))))
                    (should
                     (equal condition
                            '(nskk-dict-error
                              "Invalid user dictionary entry")))
                    (should
                     (equal
                      (nskk-dict-transaction-predicate-snapshot
                       (nskk-prolog-clause-key 'user-dict-entry 2))
                      before))
                    (should (eq (nskk-dict-user-index) 'user))
                    (should (eq nskk-dict-modified 'preserved))
                    (should-not hook-calls)
                    (should
                     (equal
                      (with-temp-buffer
                        (set-buffer-multibyte nil)
                        (insert-file-contents-literally
                         dictionary-file)
                        (buffer-string))
                      "literal-before")))))
              (should
               (equal (nskk-dict-lookup "既存")
                      '("既存候補"))))
          (when (file-exists-p dictionary-file)
            (delete-file dictionary-file))))))

  (progn
    (nskk-it "round-trips Unicode and ordinary word spaces through persistence"
      (nskk-prolog-test-with-isolated-db
        (let* ((reading "ゆにこーど")
               (word "候 補😀")
               (dictionary-file
                (make-temp-file "nskk-register-roundtrip-" nil ".skk"))
               (nskk-dict-user-dictionary-file dictionary-file)
               (nskk--user-dict-index 'user)
               (nskk-dict-modified nil))
          (unwind-protect
              (progn
                (nskk-prolog-set-index 'user-dict-entry 2 :trie)
                (should (nskk-dict-register-word reading word))
                (should nskk-dict-modified)
                (nskk-dict-save-user-dictionary)
                (should-not nskk-dict-modified)
                (nskk-prolog-retract-all 'user-dict-entry 2)
                (nskk-dict-set-user-index nil)
                (should
                 (eq (nskk-dict-load-user-dictionary) 'user))
                (should (member word (nskk-dict-lookup reading))))
            (when (file-exists-p dictionary-file)
              (delete-file dictionary-file))))))

    (nskk-it "refuses malformed internal facts without replacing the dictionary file"
      (nskk-prolog-test-with-isolated-db
        (let* ((dictionary-file
                (make-temp-file "nskk-save-invalid-" nil ".skk"))
               (nskk-dict-user-dictionary-file dictionary-file)
               (nskk--user-dict-index 'user)
               (nskk-dict-modified 'preserved))
          (unwind-protect
              (progn
                (nskk-prolog-set-index 'user-dict-entry 2 :trie)
                (nskk-prolog-assert
                 '((user-dict-entry "正常" ("正常候補"))))
                (nskk-prolog-assert
                 '((user-dict-entry "不正" ("候/補"))))
                (with-temp-file dictionary-file
                  (set-buffer-multibyte nil)
                  (insert "literal-before"))
                (let* ((before
                        (nskk-dict-transaction-predicate-snapshot
                         (nskk-prolog-clause-key 'user-dict-entry 2)))
                       (condition
                        (condition-case err
                            (progn
                              (nskk-dict-save-user-dictionary)
                              nil)
                          (nskk-dict-error err))))
                  (should
                   (equal condition
                          '(nskk-dict-error
                            "Invalid user dictionary entry")))
                  (should
                   (equal
                    (nskk-dict-transaction-predicate-snapshot
                     (nskk-prolog-clause-key 'user-dict-entry 2))
                    before))
                  (should (eq (nskk-dict-user-index) 'user))
                  (should (eq nskk-dict-modified 'preserved))
                  (should
                   (equal
                    (with-temp-buffer
                      (set-buffer-multibyte nil)
                      (insert-file-contents-literally
                       dictionary-file)
                      (buffer-string))
                    "literal-before"))))
            (when (file-exists-p dictionary-file)
              (delete-file dictionary-file)))))))

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
                (nskk-state-set-romaji-buffer ""))
              (with-current-buffer buf-b
                (setq-local nskk-current-state (nskk-state-create 'hiragana))
                (nskk-state-set-romaji-buffer "")))
            (nskk-when
              (with-current-buffer buf-a
                (nskk-state-append-input nskk-current-state ?か)
                (nskk-state-set-romaji-buffer "k")))
            (nskk-then
              (with-current-buffer buf-b
                (nskk-should-equal "" (nskk-state-input-buffer nskk-current-state))
                (nskk-should-equal "" (nskk-state-romaji-buffer)))
              (with-current-buffer buf-a
                (nskk-should-equal "か" (nskk-state-input-buffer nskk-current-state))
                (nskk-should-equal "k" (nskk-state-romaji-buffer)))))
        (kill-buffer buf-a)
        (kill-buffer buf-b))))

  (nskk-it "nskk-state-romaji-buffer is independent per buffer"
    (let ((buf-a (generate-new-buffer " *nskk-test-romaji-a*"))
          (buf-b (generate-new-buffer " *nskk-test-romaji-b*")))
      (unwind-protect
          (progn
            (nskk-given
              (with-current-buffer buf-a
                (nskk-state-set-romaji-buffer ""))
              (with-current-buffer buf-b
                (nskk-state-set-romaji-buffer "")))
            (nskk-when
              (with-current-buffer buf-a
                (nskk-state-set-romaji-buffer "ka")))
            (nskk-then
              (with-current-buffer buf-b
                (nskk-should-equal "" (nskk-state-romaji-buffer))))
            (nskk-when
              (with-current-buffer buf-b
                (nskk-state-set-romaji-buffer "shi")))
            (nskk-then
              (with-current-buffer buf-a
                (nskk-should-equal "ka" (nskk-state-romaji-buffer)))))
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
;;;

(require 'nskk-pbt-generators)

(nskk-deftest-table dict-registration-readings
  :columns (input expected)
  :rows (("てすと" "テスト")
         ("さくら" "桜")
         ("やま"   "山"))
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


;;;
;;; Group 4: Dictionary Unregistration (nskk-dict-unregister-word)
;;;

(nskk-describe "dictionary unregistration"

  (nskk-it "unregistering a sole candidate removes the entire entry"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk--user-dict-index 'user)
            (nskk-dict-modified nil))
        (nskk-prolog-set-index 'user-dict-entry 2 :trie)
        (nskk-given (nskk-dict-register-word "てすと" "テスト"))
        (nskk-when  (nskk-dict-unregister-word "てすと" "テスト"))
        (nskk-then  (should (null (nskk-dict-lookup "てすと")))))))

  (nskk-it "unregistering one of multiple candidates keeps others"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk--user-dict-index 'user)
            (nskk-dict-modified nil))
        (nskk-prolog-set-index 'user-dict-entry 2 :trie)
        (nskk-given
          (nskk-prolog-assert
           '((user-dict-entry "ふくすう" ("AAA" "BBB" "CCC")))))
        (nskk-when  (nskk-dict-unregister-word "ふくすう" "BBB"))
        (nskk-then
          (let ((candidates (nskk-prolog-query-value
                             '(user-dict-entry "ふくすう" \?c) '\?c)))
            (should (member "AAA" candidates))
            (should (member "CCC" candidates))
            (should-not (member "BBB" candidates)))))))

  (nskk-it "unregistering a word not in the entry is a no-op"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk--user-dict-index 'user)
            (nskk-dict-modified nil))
        (nskk-prolog-set-index 'user-dict-entry 2 :trie)
        (nskk-given
          (nskk-prolog-assert
           '((user-dict-entry "さくら" ("桜")))))
        (nskk-when
          (should-not (nskk-dict-unregister-word "さくら" "花")))
        (nskk-then
          (should (member "桜" (nskk-dict-lookup "さくら")))))))

  (nskk-it "unregistering from a nonexistent reading is a no-op"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk--user-dict-index 'user)
            (nskk-dict-modified nil))
        (nskk-prolog-set-index 'user-dict-entry 2 :trie)
        (nskk-when
          (should-not
           (nskk-dict-unregister-word "ほげ" "ホゲ")))
        (nskk-then
          (should (null nskk-dict-modified))))))

  (nskk-it "nskk-dict-modified is set to t after unregistration"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk--user-dict-index 'user)
            (nskk-dict-modified nil))
        (nskk-prolog-set-index 'user-dict-entry 2 :trie)
        (nskk-given
          (nskk-dict-register-word "にほん" "日本")
          (setq nskk-dict-modified nil))
        (nskk-when  (nskk-dict-unregister-word "にほん" "日本"))
        (nskk-then  (should (eq nskk-dict-modified t))))))

  (nskk-it "register then unregister is a roundtrip"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk--user-dict-index 'user)
            (nskk-dict-modified nil))
        (nskk-prolog-set-index 'user-dict-entry 2 :trie)
        (nskk-given (nskk-dict-register-word "やま" "山"))
        (nskk-when  (nskk-dict-unregister-word "やま" "山"))
        (nskk-then  (should (null (nskk-dict-lookup "やま"))))))))


(provide 'nskk-dict-registration-integration-test)

;;; nskk-dict-registration-integration-test.el ends here
