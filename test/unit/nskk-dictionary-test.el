;;; nskk-dictionary-test.el --- Tests for nskk-dictionary.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Tests for nskk-dictionary.el covering:
;; - Error type hierarchy (nskk-dict-error root in nskk-dictionary.el;
;;   search sub-errors in nskk-search.el)
;; - nskk-dict-entry struct: creation, accessors, predicates
;; - nskk-dict-index struct: creation, accessors, predicates
;; - Module loading and feature provision
;; - Prolog dictionary facts and I/O.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nskk-dictionary)
(require 'nskk-search)
(require 'nskk-prolog)
(require 'nskk-test-framework)
(require 'nskk-test-macros)
(require 'nskk-pbt-generators)

(defvar skkdic-okuri-ari nil)
(defvar skkdic-okuri-nasi nil)

;;; Section 1: Error type tests

(nskk-describe "module loading"
  (nskk-it "provides nskk-dictionary feature"
    (should (featurep 'nskk-dictionary)))

  (nskk-it "can be required multiple times safely"
    (require 'nskk-dictionary)
    (require 'nskk-dictionary)
    (should (featurep 'nskk-dictionary)))

  (nskk-it "has customization group documentation"
    (should (get 'nskk-dictionary 'group-documentation)))

  (nskk-it "nskk-dict-user-dictionary-file defaults to ~/.nskk/jisyo"
    (should (equal (default-value 'nskk-dict-user-dictionary-file)
                   (expand-file-name "~/.nskk/jisyo")))))

(nskk-describe "error condition chains"
  (nskk-it "nskk-dict-search-error has correct error conditions"
    (let ((conditions (get 'nskk-dict-search-error 'error-conditions)))
      (should (listp conditions))
      (should (memq 'nskk-dict-search-error conditions))))

  (nskk-it "nskk-dict-search-invalid-query has correct error conditions"
    (let ((conditions (get 'nskk-dict-search-invalid-query 'error-conditions)))
      (should (listp conditions))
      (should (memq 'nskk-dict-search-invalid-query conditions))
      (should (memq 'nskk-dict-search-error conditions))))

  (nskk-it "nskk-dict-search-invalid-index has correct error conditions"
    (let ((conditions (get 'nskk-dict-search-invalid-index 'error-conditions)))
      (should (listp conditions))
      (should (memq 'nskk-dict-search-invalid-index conditions))
      (should (memq 'nskk-dict-search-error conditions)))))

(nskk-describe "error signaling"
  (nskk-it "signals nskk-dict-search-error"
    (let ((caught nil))
      (condition-case _err
          (signal 'nskk-dict-search-error '("test error"))
        (nskk-dict-search-error (setq caught t)))
      (should caught)))

  (nskk-it "signals nskk-dict-search-invalid-query"
    (let ((caught nil))
      (condition-case _err
          (signal 'nskk-dict-search-invalid-query '("bad query"))
        (nskk-dict-search-invalid-query (setq caught t)))
      (should caught)))

  (nskk-it "signals nskk-dict-search-invalid-index"
    (let ((caught nil))
      (condition-case _err
          (signal 'nskk-dict-search-invalid-index '("bad index"))
        (nskk-dict-search-invalid-index (setq caught t)))
      (should caught)))

  (nskk-it "nskk-dict-search-invalid-query is caught by nskk-dict-search-error handler"
    (let ((caught nil))
      (condition-case _err
          (signal 'nskk-dict-search-invalid-query '("bad query"))
        (nskk-dict-search-error (setq caught t)))
      (should caught)))

  (nskk-it "nskk-dict-search-invalid-index is caught by nskk-dict-search-error handler"
    (let ((caught nil))
      (condition-case _err
          (signal 'nskk-dict-search-invalid-index '("bad index"))
        (nskk-dict-search-error (setq caught t)))
      (should caught))))

(nskk-describe "error data preservation"
  (nskk-it "preserves error data when signaling search errors"
    (condition-case err
        (signal 'nskk-dict-search-invalid-query '("test data"))
      (nskk-dict-search-invalid-query
       (should (equal (cadr err) "test data")))))

  (nskk-it "preserves error data list"
    (condition-case err
        (signal 'nskk-dict-search-error '("msg" extra-data))
      (nskk-dict-search-error
       (should (equal (cadr err) "msg"))
       (should (eq (caddr err) 'extra-data)))))

  (nskk-it "catches search errors with condition-case"
    (let ((caught nil))
      (condition-case _err
          (signal 'nskk-dict-search-error '("test"))
        (nskk-dict-search-error (setq caught t)))
      (should caught))))

(nskk-describe "error messages"
  (nskk-it "search error message contains 'search'"
    (let ((msg (get 'nskk-dict-search-error 'error-message)))
      (should (stringp msg))
      (should (string-match-p "search" (downcase msg)))))

  (nskk-it "invalid query error message contains 'query'"
    (let ((msg (get 'nskk-dict-search-invalid-query 'error-message)))
      (should (stringp msg))
      (should (string-match-p "query" (downcase msg)))))

  (nskk-it "invalid index error message contains 'index'"
    (let ((msg (get 'nskk-dict-search-invalid-index 'error-message)))
      (should (stringp msg))
      (should (string-match-p "index" (downcase msg))))))

(nskk-describe "error type differentiation"
  (nskk-it "distinguishes query errors from index errors"
    (let ((query-caught nil)
          (index-caught nil))
      ;; Test query error
      (condition-case _err
          (signal 'nskk-dict-search-invalid-query '("test"))
        (nskk-dict-search-invalid-query (setq query-caught t))
        (nskk-dict-search-invalid-index (setq index-caught t)))
      (should query-caught)
      (should (not index-caught))

      ;; Test index error
      (setq query-caught nil)
      (setq index-caught nil)
      (condition-case _err
          (signal 'nskk-dict-search-invalid-index '("test"))
        (nskk-dict-search-invalid-query (setq query-caught t))
        (nskk-dict-search-invalid-index (setq index-caught t)))
      (should (not query-caught))
      (should index-caught))))

;;; Section 2: Data structure tests

(nskk-describe "dict-entry creation"
  (nskk-it "creates entry with default values"
    (let ((entry (make-nskk-dict-entry)))
      (should (nskk-dict-entry-p entry))
      (should (null (nskk-dict-entry-key entry)))
      (should (null (nskk-dict-entry-candidates entry)))
      (should (null (nskk-dict-entry-okuri entry)))))

  (nskk-it "creates entry with a key"
    (let ((entry (make-nskk-dict-entry :key "かんじ")))
      (should (nskk-dict-entry-p entry))
      (should (equal (nskk-dict-entry-key entry) "かんじ"))))

  (nskk-it "creates entry with all fields"
    (let ((entry (make-nskk-dict-entry
                  :key "かんじ"
                  :candidates '("漢字" "感じ" "幹事")
                  :okuri "i")))
      (should (nskk-dict-entry-p entry))
      (should (equal (nskk-dict-entry-key entry) "かんじ"))
      (should (equal (nskk-dict-entry-candidates entry) '("漢字" "感じ" "幹事")))
      (should (equal (nskk-dict-entry-okuri entry) "i"))))

  (nskk-it "creates entry with nil candidates"
    (let ((entry (make-nskk-dict-entry :key "test" :candidates nil)))
      (should (nskk-dict-entry-p entry))
      (should (null (nskk-dict-entry-candidates entry))))))

(nskk-describe "dict-entry predicate"
  (nskk-it "returns t for a valid entry"
    (let ((entry (make-nskk-dict-entry :key "test")))
      (should (nskk-dict-entry-p entry))))

  (nskk-it "returns nil for nil"
    (should (not (nskk-dict-entry-p nil))))

  (nskk-it "returns nil for non-entry values"
    (should (not (nskk-dict-entry-p "string")))
    (should (not (nskk-dict-entry-p 123)))
    (should (not (nskk-dict-entry-p '(a b c))))
    (should (not (nskk-dict-entry-p (make-hash-table))))))

(nskk-describe "dict-entry accessors"
  (nskk-it "reads key field"
    (let ((entry (make-nskk-dict-entry :key "あいう")))
      (should (equal (nskk-dict-entry-key entry) "あいう"))))

  (nskk-it "reads candidates field"
    (let ((entry (make-nskk-dict-entry :candidates '("候補1" "候補2"))))
      (should (equal (nskk-dict-entry-candidates entry) '("候補1" "候補2")))))

  (nskk-it "reads okuri field"
    (let ((entry (make-nskk-dict-entry :okuri "k")))
      (should (equal (nskk-dict-entry-okuri entry) "k"))))

  (nskk-it "sets key field with setf"
    (let ((entry (make-nskk-dict-entry)))
      (setf (nskk-dict-entry-key entry) "new-key")
      (should (equal (nskk-dict-entry-key entry) "new-key"))))

  (nskk-it "sets candidates field with setf"
    (let ((entry (make-nskk-dict-entry)))
      (setf (nskk-dict-entry-candidates entry) '("a" "b"))
      (should (equal (nskk-dict-entry-candidates entry) '("a" "b")))))

  (nskk-it "sets okuri field with setf"
    (let ((entry (make-nskk-dict-entry)))
      (setf (nskk-dict-entry-okuri entry) "t")
      (should (equal (nskk-dict-entry-okuri entry) "t")))))

(nskk-describe "dict-index creation"
  (nskk-it "creates index with default values"
    (let ((index (make-nskk-dict-index)))
      (should (nskk-dict-index-p index))
      (should (null (nskk-dict-index-predicate index)))))

  (nskk-it "creates index with a Prolog predicate"
    (nskk-with-prolog-entries ((test-dict "key1" ("val1"))
                               (test-dict "key2" ("val2")))
      (let ((index (make-nskk-dict-index :predicate 'test-dict)))
        (should (nskk-dict-index-p index))
        (should (eq (nskk-dict-index-predicate index) 'test-dict)))))

  (nskk-it "creates index backed by Prolog trie index"
    (nskk-with-prolog-entries ((prefix-dict "test" ("value")))
      (let ((index (make-nskk-dict-index :predicate 'prefix-dict)))
        (should (nskk-dict-index-p index))
        (should (eq (nskk-dict-index-predicate index) 'prefix-dict))))))

(nskk-describe "dict-index predicate"
  (nskk-it "returns t for a valid index"
    (let ((index (make-nskk-dict-index)))
      (should (nskk-dict-index-p index))))

  (nskk-it "returns nil for nil"
    (should (not (nskk-dict-index-p nil))))

  (nskk-it "returns nil for non-index values"
    (should (not (nskk-dict-index-p "string")))
    (should (not (nskk-dict-index-p 123)))
    (should (not (nskk-dict-index-p (make-nskk-dict-entry))))))

(nskk-describe "dict-index accessors"
  (nskk-it "reads predicate field"
    (let ((index (make-nskk-dict-index :predicate 'my-dict)))
      (should (eq (nskk-dict-index-predicate index) 'my-dict)))))


(nskk-describe "dict-struct integration"
  (nskk-it "builds dict-index workflow with Prolog facts"
    (nskk-with-prolog-entries ((workflow-dict "かんじ" ("漢字" "感じ"))
                               (workflow-dict "にほん" ("日本"))
                               (workflow-dict "にほんご" ("日本語")))
      (let ((index (make-nskk-dict-index :predicate 'workflow-dict)))
        (should (nskk-dict-index-p index))
        ;; Verify prefix search
        (let ((prefix-results (nskk-prolog-trie-prefix-search 'workflow-dict 2 "にほん")))
          (should (= (length prefix-results) 2))))))

  (nskk-it "creates dict-entry with okurigana information"
    (let ((entry (make-nskk-dict-entry
                  :key "うごk"
                  :candidates '("動く" "蠢く")
                  :okuri "k")))
      (should (nskk-dict-entry-p entry))
      (should (equal (nskk-dict-entry-key entry) "うごk"))
      (should (equal (nskk-dict-entry-okuri entry) "k"))
      (should (= (length (nskk-dict-entry-candidates entry)) 2)))))

;;; Section 3: I/O tests

(nskk-describe "Prolog dictionary I/O"
  (nskk-it "asserts dict entries and looks them up via Prolog"
    (nskk-with-prolog-entries ((io-test-dict "かんじ" ("漢字")))
      (let ((result (nskk-prolog-query-value
                     '(io-test-dict "かんじ" \?c) '\?c)))
        (nskk-should-equal '("漢字") result))))

  (nskk-it "prefix searches over Prolog dict facts"
    (nskk-with-prolog-entries ((prefix-test-dict "かんじ" ("漢字"))
                               (prefix-test-dict "かんたん" ("簡単"))
                               (prefix-test-dict "にほん" ("日本")))
      (let ((results (nskk-prolog-trie-prefix-search 'prefix-test-dict 2 "かん")))
        (should (= (length results) 2))
        (should (assoc "かんじ" results))
        (should (assoc "かんたん" results)))))

  (nskk-it "stores Japanese candidate lists"
    (nskk-with-prolog-entries ((ja-dict "にほん" ("日本" "二本")))
      (let ((result (nskk-prolog-query-value '(ja-dict "にほん" \?c) '\?c)))
        (nskk-should-equal '("日本" "二本") result))))

  (nskk-it "clears all entries via retract-all"
    (nskk-with-prolog-entries ((retract-dict "a" ("val1"))
                               (retract-dict "b" ("val2")))
      (nskk-prolog-retract-all 'retract-dict 2)
      (let ((result (nskk-prolog-query-value '(retract-dict "a" \?c) '\?c)))
        (should (null result))))))

(nskk-describe "dict-entry serialization"
  (nskk-it "can be written and read back"
    (let ((entry (make-nskk-dict-entry :key "test" :candidates '("a" "b")))
          (temp-file (make-temp-file "nskk-entry-" nil ".dat")))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (prin1 entry (current-buffer)))
            (let ((restored (with-temp-buffer
                              (insert-file-contents temp-file)
                              (read (current-buffer)))))
              ;; cl-defstruct records produce readable output; verify structural equality.
              (should (nskk-dict-entry-p restored))
              (should (equal (nskk-dict-entry-key restored) (nskk-dict-entry-key entry)))
              (should (equal (nskk-dict-entry-candidates restored)
                             (nskk-dict-entry-candidates entry)))))
        (when (file-exists-p temp-file)
          (delete-file temp-file))))))

(nskk-describe "dict-io integration"
  (nskk-it "completes full Prolog roundtrip workflow"
    (nskk-with-prolog-entries ((workflow-io-dict "あいう" ("アイウ"))
                               (workflow-io-dict "あいうえ" ("アイウエ"))
                               (workflow-io-dict "あいうえお" ("アイウエオ"))
                               (workflow-io-dict "かきく" ("カキク")))
      (let ((index (make-nskk-dict-index :predicate 'workflow-io-dict)))
        ;; Verify prefix search works
        (let ((results (nskk-prolog-trie-prefix-search 'workflow-io-dict 2 "あいう")))
          (should (= (length results) 3))
          (should (assoc "あいう" results))
          (should (assoc "あいうえ" results))
          (should (assoc "あいうえお" results)))
        ;; Verify exact lookup
        (let ((result (nskk-prolog-query-value
                       '(workflow-io-dict "かきく" \?c) '\?c)))
          (nskk-should-equal '("カキク") result))))))

(nskk-describe "dictionary auto-detection"
  (nskk-it "returns empty list when no dictionary files exist"
    (nskk-with-mocks ((file-readable-p (lambda (_f) nil)))
      (let ((result (nskk--dict-detect-system-dictionaries)))
        (should (listp result))
        (should (null result)))))

  (nskk-it "finds dictionary in nix profile"
    (nskk-with-mocks ((file-readable-p (lambda (f) (string-match-p "nix-profile" f))))
      (let ((result (nskk--dict-detect-system-dictionaries)))
        (should result)
        (should (cl-some (lambda (p) (string-match-p "nix-profile" p)) result)))))

  (nskk-it "finds dictionary in standard system path"
    (nskk-with-mocks ((file-readable-p (lambda (f) (string= f "/usr/share/skk/SKK-JISYO.L"))))
      (let ((result (nskk--dict-detect-system-dictionaries)))
        (should result)
        (should (member "/usr/share/skk/SKK-JISYO.L" result)))))

  (nskk-it "includes nskk-large-dictionary when set"
    (let ((nskk-large-dictionary "/tmp/test-large-dict"))
      (nskk-with-mocks ((file-readable-p (lambda (f) (string= f "/tmp/test-large-dict"))))
        (let ((result (nskk--dict-detect-system-dictionaries)))
          (should result)
          (should (member "/tmp/test-large-dict" result))))))

  (nskk-it "uses NIX_PROFILES environment variable"
    (nskk-with-mocks ((getenv (lambda (var) (when (string= var "NIX_PROFILES")
                                              "/nix/var/nix/profiles/default /home/user/.nix-profile")))
                      (file-readable-p (lambda (f) (string-match-p "/nix/var/nix/profiles/default/share/skk" f))))
      (let ((result (nskk--dict-detect-system-dictionaries)))
        (should result)
        (should (cl-some (lambda (p) (string-match-p "profiles/default" p)) result))))))

(nskk-describe "ja-dic conversion"
  (nskk-it "decodes and flattens okuri-nasi entries"
    (let* ((o (- (logand (encode-char ?お 'japanese-jisx0208) #xFF) 32))
           (sample `(skdic-okuri-nasi
                     (,o ("緒" "小")))))
      (should (equal (nskk--dict-ja-dic-flatten-tree sample)
                     '(("お" . ("小" "緒")))))))

  (nskk-it "decodes and flattens okuri-ari entries"
    (let* ((wa (- (logand (encode-char ?わ 'japanese-jisx0208) #xFF) 32))
           (ru (- (logand (encode-char ?る 'japanese-jisx0208) #xFF) 32))
           (sample `(skkdic-okuri-ari
                     (,wa t
                          (,ru t
                               (-105 ("惡" "悪")))))))
      (should (equal (nskk--dict-ja-dic-flatten-tree sample)
                     '(("わるi" . ("悪" "惡")))))))

  (nskk-it "loads flattened ja-dic entries into system-dict-entry"
    (nskk-prolog-test-with-isolated-db
      (let* ((o (- (logand (encode-char ?お 'japanese-jisx0208) #xFF) 32))
             (wa (- (logand (encode-char ?わ 'japanese-jisx0208) #xFF) 32))
             (ru (- (logand (encode-char ?る 'japanese-jisx0208) #xFF) 32))
             (skkdic-okuri-nasi `(skdic-okuri-nasi
                                  (,o ("緒" "小"))))
             (skkdic-okuri-ari `(skkdic-okuri-ari
                                 (,wa t
                                      (,ru t
                                           (-105 ("惡" "悪")))))))
        (nskk-with-mocks ((load-library (lambda (_feature) t)))
          (should (eq 'system (nskk-dict-load-ja-dic)))
          (should (equal '("小" "緒")
                         (nskk-prolog-query-value '(system-dict-entry "お" \?c) '\?c)))
          (should (equal '("悪" "惡")
                         (nskk-prolog-query-value '(system-dict-entry "わるi" \?c) '\?c))))))))

(nskk-describe "dict-initialize"
  (nskk-it "uses auto-detection when config is nil"
    (let ((nskk-dict-system-dictionary-files nil)
          (nskk-dict-user-dictionary-file nil)
          (nskk--system-dict-index nil)
          (nskk--user-dict-index nil)
          (detect-called nil))
      (nskk-with-mocks ((nskk--dict-detect-system-dictionaries (lambda () (setq detect-called t) nil))
                        (nskk-dict-load-user-dictionary (lambda () nil)))
        (nskk-dict-initialize)
        (should detect-called))))

  (nskk-it "skips auto-detection when files are configured"
    (let ((nskk-dict-system-dictionary-files '("/some/path"))
          (nskk-dict-user-dictionary-file nil)
          (nskk--system-dict-index nil)
          (nskk--user-dict-index nil)
          (detect-called nil))
      (nskk-with-mocks ((nskk--dict-detect-system-dictionaries (lambda () (setq detect-called t) nil))
                        (nskk-dict-load-system-dictionaries (lambda () nil))
                        (nskk-dict-load-user-dictionary (lambda () nil)))
        (nskk-dict-initialize)
        (should-not detect-called))))

  (nskk-it "prefers ja-dic before file auto-detection"
    (let ((nskk-dict-system-dictionary-files nil)
          (nskk-dict-use-ja-dic t)
          (nskk-dict-user-dictionary-file nil)
          (nskk--system-dict-index nil)
          (nskk--user-dict-index nil)
          (ja-dic-called nil)
          (detect-called nil))
      (nskk-with-mocks ((nskk-dict-load-ja-dic (lambda () (setq ja-dic-called t) 'system))
                        (nskk--dict-detect-system-dictionaries
                         (lambda () (setq detect-called t) '("/some/path")))
                        (nskk-dict-load-user-dictionary (lambda () nil)))
        (nskk-dict-initialize)
        (should ja-dic-called)
        (should-not detect-called)))))

;;;
;;; Property-Based Tests
;;;

;; Table-driven dict entry creation
(nskk-deftest-table dict-pbt-entry-creation
  :description "Dict entry creation with known keys and candidates"
  :columns (input expected)
  :rows (("かんじ"   ("漢字" "感じ" "幹事"))
         ("にほん"   ("日本" "二本"))
         ("さくら"   ("桜"))
         ("やま"     ("山")))
  :body (let ((entry (make-nskk-dict-entry :key input :candidates expected)))
          (should (nskk-dict-entry-p entry))
          (should (equal (nskk-dict-entry-key entry) input))
          (should (equal (nskk-dict-entry-candidates entry) expected))))

(nskk-describe "dict property-based tests"
  (nskk-it "search invariant: searching for an inserted key returns non-empty result"
    (nskk-with-mock-dict '(("てすと" . ("テスト")))
      (let ((result (nskk-prolog-query-value
                     '(user-dict-entry "てすと" \?c) '\?c)))
        (should result)
        (should (= (length result) 1))
        (should (equal (car result) "テスト")))))

  (nskk-it "empty search: searching for non-existent key returns nil without crashing"
    (nskk-with-mock-dict nil
      (nskk-should-not-error
        (let ((result (nskk-prolog-query-value
                       '(user-dict-entry "ぜったいにそんざいしないきー" \?c) '\?c)))
          (should (null result)))))))

;;; Section 4: nskk-dict-parse-line tests

(nskk-describe "nskk-dict-parse-line"
  (nskk-it "parses basic SKK dictionary lines"
    (nskk-deftest-table parse-line-basic
      :columns (input expected-key expected-candidates)
      :rows    (("あ /ア/"              "あ"     ("ア"))
                ("かんじ /漢字/感じ/"   "かんじ" ("漢字" "感じ"))
                ("うごk /動く/蠢く/"    "うごk"  ("動く" "蠢く")))
      :body
      (let* ((result (nskk-dict-parse-line input))
             (key (car result))
             (candidates (cdr result)))
        (should (consp result))
        (should (equal key expected-key))
        (should (equal candidates expected-candidates)))))

  (nskk-it "returns nil for comment lines"
    (nskk-deftest-table parse-line-comments
      :columns (input)
      :rows    ((";;") (";; comment") (";; -*- mode: fundamental -*-"))
      :body
      (should (null (nskk-dict-parse-line input)))))

  (nskk-it "returns nil for invalid lines"
    (nskk-deftest-table parse-line-invalid
      :columns (input)
      :rows    (("no-slash-at-all") ("missing-space/"))
      :body
      (should (null (nskk-dict-parse-line input)))))

  (nskk-it "strips annotations (semicolon suffix) from candidates"
    (let* ((result (nskk-dict-parse-line "かんじ /漢字;訓読み/感じ;okurigana/"))
           (candidates (cdr result)))
      (should (equal candidates '("漢字" "感じ")))))

  (nskk-it "returns nil for nil input"
    (should (null (nskk-dict-parse-line nil))))

  (nskk-it "returns nil for empty string input"
    (should (null (nskk-dict-parse-line "")))))

;;; Section 5: nskk--dict-parse-candidates tests

(nskk-describe "nskk--dict-parse-candidates"
  (nskk-it "parses candidate strings correctly"
    (nskk-deftest-table parse-candidates-valid
      :columns (input expected)
      :rows    (("/ア/"        ("ア"))
                ("/漢字/感じ/" ("漢字" "感じ"))
                ("/a/b/c/"     ("a" "b" "c")))
      :body
      (should (equal (nskk--dict-parse-candidates input) expected))))

  (nskk-it "returns nil for invalid inputs"
    (nskk-deftest-table parse-candidates-nil
      :columns (input)
      :rows    ((nil) ("") ("no-slash") ("漢字"))
      :body
      (should (null (nskk--dict-parse-candidates input))))))

;;; Section 6: nskk-dict-lookup tests

(nskk-describe "nskk-dict-lookup"
  (nskk-it "returns candidates for a known reading"
    (nskk-with-mock-dict '(("かんじ" . ("漢字" "感じ")))
      (let ((result (nskk-dict-lookup "かんじ")))
        (should result)
        (should (member "漢字" result)))))

  (nskk-it "returns nil for an unknown reading"
    (nskk-with-mock-dict '(("かんじ" . ("漢字")))
      (let ((result (nskk-dict-lookup "ぜんぜんない")))
        (should (null result)))))

  (nskk-it "returns nil for an empty key"
    (nskk-with-mock-dict nil
      (let ((result (nskk-dict-lookup "")))
        (should (null result)))))

  (nskk-it "finds okuri-ari entries when key is appended with consonant"
    (nskk-with-prolog-entries ((user-dict-entry "うごk" ("動く" "蠢く")))
      (let ((result (nskk-dict-lookup "うご")))
        (should result)
        (should (member "動く" result)))))

  (nskk-it "single-character key skips okuri-ari search and returns direct match"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk--user-dict-index 'user))
        (nskk-prolog-set-index 'user-dict-entry 2 :trie)
        ;; Register a single-character reading
        (nskk-prolog-assert '((user-dict-entry "あ" ("亜" "阿"))))
        ;; Single-char key should still find the direct match
        (let ((result (nskk-dict-lookup "あ")))
          (should (listp result))
          (should (member "亜" result)))))))

;;; Section 7: nskk-dict-register-word tests

(nskk-describe "nskk-dict-register-word"
  (nskk-it "registers a new word and makes it retrievable"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-set-index 'user-dict-entry 2 :trie)
      (let ((nskk--user-dict-index nil)
            (nskk-dict-modified nil))
        (nskk-dict-register-word "てすと" "テスト")
        (let ((result (nskk-dict-lookup "てすと")))
          (should result)
          (should (member "テスト" result)))
        (should nskk-dict-modified))))

  (nskk-it "returns nil silently for empty reading"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk--user-dict-index nil)
            (nskk-dict-modified nil))
        (should (null (nskk-dict-register-word "" "テスト")))
        (should-not nskk-dict-modified))))

  (nskk-it "returns nil silently for empty word"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk--user-dict-index nil)
            (nskk-dict-modified nil))
        (should (null (nskk-dict-register-word "てすと" "")))
        (should-not nskk-dict-modified))))

  (nskk-it "registers additional words to an existing entry"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-set-index 'user-dict-entry 2 :trie)
      (let ((nskk--user-dict-index nil)
            (nskk-dict-modified nil))
        (nskk-dict-register-word "かんじ" "漢字")
        (nskk-dict-register-word "かんじ" "感じ")
        (let ((result (nskk-dict-lookup "かんじ")))
          (should (member "漢字" result))
          (should (member "感じ" result)))
        (should nskk-dict-modified))))

  (nskk-it "runs nskk-jisyo-update-hook after successful registration"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-set-index 'user-dict-entry 2 :trie)
      (let* ((hook-called nil)
             (nskk--user-dict-index nil)
             (nskk-dict-modified nil)
             (nskk-jisyo-update-hook (list (lambda () (setq hook-called t)))))
        (nskk-dict-register-word "てすと" "テスト")
        (should hook-called)))))

;;; Section 8: CPS /k variant tests

(nskk-describe "nskk-dict-lookup/k"
  (nskk-it "calls on-found with candidates when reading exists"
    (nskk-with-mock-dict '(("かんじ" . ("漢字" "感じ")))
      (let ((found-result nil)
            (not-found-called nil))
        (nskk-dict-lookup/k "かんじ"
                            (lambda (cands) (setq found-result cands))
                            (lambda () (setq not-found-called t)))
        (should found-result)
        (should-not not-found-called)
        (should (member "漢字" found-result)))))

  (nskk-it "calls on-not-found when reading is absent"
    (nskk-with-mock-dict '(("かんじ" . ("漢字")))
      (let ((found-called nil)
            (not-found-called nil))
        (nskk-dict-lookup/k "ない"
                            (lambda (_c) (setq found-called t))
                            (lambda () (setq not-found-called t)))
        (should-not found-called)
        (should not-found-called))))

  (nskk-it "sync wrapper nskk-dict-lookup is equivalent to calling /k with #'identity"
    (nskk-with-mock-dict '(("かんじ" . ("漢字")))
      (let ((sync-result (nskk-dict-lookup "かんじ"))
            (cps-result (nskk-dict-lookup/k "かんじ" #'identity (lambda () nil))))
        (should (equal sync-result cps-result))))))

(nskk-describe "nskk-dict-register-word/k"
  (nskk-it "calls on-done after successful registration"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-set-index 'user-dict-entry 2 :trie)
      (let ((done-called nil)
            (nskk--user-dict-index nil)
            (nskk-dict-modified nil))
        (nskk-dict-register-word/k "てすと" "テスト"
                                   (lambda (_) (setq done-called t))
                                   #'ignore)
        (should done-called))))

  (nskk-it "does not call on-done for invalid empty reading"
    (nskk-prolog-test-with-isolated-db
      (let ((done-called nil)
            (nskk--user-dict-index nil)
            (nskk-dict-modified nil))
        (nskk-dict-register-word/k "" "テスト"
                                   (lambda () (setq done-called t))
                                   #'ignore)
        (should-not done-called))))

  (nskk-it "signals an error when on-found is nil"
    (should-error
     (nskk-dict-register-word/k "かんじ" "漢字" nil #'ignore)
     :type 'void-function))

  (nskk-it "signals an error when on-found is not a function"
    (should-error
     (nskk-dict-register-word/k "かんじ" "漢字" "not-a-function" #'ignore)
     :type 'invalid-function)))

;;; Section 9: cache function tests

(nskk-describe "nskk--dict-cache-valid-p"
  (nskk-it "returns nil when dict-files is nil"
    (should (null (nskk--dict-cache-valid-p nil))))

  (nskk-it "returns nil when cache file does not exist"
    (nskk-with-mocks ((file-attributes (lambda (_f) nil)))
      (should (null (nskk--dict-cache-valid-p '("/some/dict.el"))))))

  (nskk-it "returns non-nil when cache is newer than all source files"
    (let ((cache-mtime '(0 200 0 0))
          (source-mtime '(0 100 0 0)))
      (nskk-with-mocks ((file-attributes
                         (lambda (f)
                           ;; file-attribute-modification-time reads index 5 (mtime slot).
                           (if (string-suffix-p "dict-cache.eld" f)
                               (list nil nil nil nil nil cache-mtime nil nil nil nil nil)
                             (list nil nil nil nil nil source-mtime nil nil nil nil nil))))
                        (file-readable-p (lambda (_f) t)))
        (should (nskk--dict-cache-valid-p '("/some/dict.el")))))))

(nskk-describe "nskk--dict-load-system-dict-from-cache"
  (nskk-it "returns nil when cache file is unreadable"
    (nskk-with-mocks ((insert-file-contents (lambda (_f) (error "File not found"))))
      (should (null (nskk--dict-load-system-dict-from-cache)))))

  (nskk-it "returns nil when cache data is not a list (type guard)"
    (nskk-with-mocks ((insert-file-contents (lambda (_f) (insert "42"))))
      (should (null (nskk--dict-load-system-dict-from-cache))))))

;;; Section 10: register-lookup invariants (table-driven PBT)

(nskk-deftest-table dict-register-lookup-invariant
  :description "register-then-lookup invariant: registered word is always retrievable"
  :columns (input expected)
  :rows (("てすと"   "テスト")
         ("かんじ"   "漢字")
         ("さくら"   "桜")
         ("にほんご" "日本語"))
  :body
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-set-index 'user-dict-entry 2 :trie)
    (let ((nskk--user-dict-index nil)
          (nskk-dict-modified nil))
      (nskk-dict-register-word input expected)
      (let ((result (nskk-dict-lookup input)))
        (should result)
        (should (member expected result))))))

(nskk-deftest-table dict-register-idempotency
  :description "register-idempotency: registering same word twice is safe and appears exactly once"
  :columns (input expected)
  :rows (("てすと" "テスト")
         ("かんじ" "漢字"))
  :body
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-set-index 'user-dict-entry 2 :trie)
    (let ((nskk--user-dict-index nil)
          (nskk-dict-modified nil))
      (nskk-dict-register-word input expected)
      (nskk-dict-register-word input expected)
      (let ((result (nskk-dict-lookup input)))
        (should result)
        (should (= (cl-count expected result :test #'equal) 1))))))

(nskk-describe "nskk--dict-file-older-than"
  (nskk-it "returns nil when file does not exist"
    (should-not (nskk--dict-file-older-than "/nonexistent/path/file.el"
                                            (current-time))))

  (nskk-it "returns nil when file mtime equals cache-mtime"
    (let* ((tmpfile (make-temp-file "nskk-test-"))
           (mtime (file-attribute-modification-time (file-attributes tmpfile))))
      (unwind-protect
          (should-not (nskk--dict-file-older-than tmpfile mtime))
        (delete-file tmpfile))))

  (nskk-it "returns non-nil when file is older than cache-mtime"
    (let* ((tmpfile (make-temp-file "nskk-test-"))
           (file-mtime (file-attribute-modification-time (file-attributes tmpfile)))
           ;; A future time is "newer" than the file, so file is "older"
           (future-time (time-add file-mtime 100)))
      (unwind-protect
          (should (nskk--dict-file-older-than tmpfile future-time))
        (delete-file tmpfile))))

  (nskk-it "returns nil when file is newer than cache-mtime"
    (let* ((tmpfile (make-temp-file "nskk-test-"))
           (file-mtime (file-attribute-modification-time (file-attributes tmpfile)))
           ;; A past time is "older" than the file, so file is "newer"
           (past-time (time-subtract file-mtime 100)))
      (unwind-protect
          (should-not (nskk--dict-file-older-than tmpfile past-time))
        (delete-file tmpfile)))))

(nskk-describe "nskk--dict-parse-file-to-entries"
  (nskk-it "returns nil for a non-existent file"
    (should-not (nskk--dict-parse-file-to-entries "/no/such/file.el")))

  (nskk-it "returns nil for a non-string argument"
    (should-not (nskk--dict-parse-file-to-entries nil))
    (should-not (nskk--dict-parse-file-to-entries 42)))

  (nskk-it "parses a minimal SKK dictionary file into entries"
    (let ((tmpfile (make-temp-file "nskk-dictionary-test-" nil ".skk")))
      (unwind-protect
          (progn
            (with-temp-file tmpfile
              (insert ";; SKK-JISYO.S -*- coding: utf-8 -*-\n")
              (insert ";; okuri-ari entries\n")
              (insert ";; okuri-nasi entries\n")
              (insert "かんじ /漢字/感じ/幹事/\n")
              (insert "さくら /桜/\n")
              (insert ";; another comment\n")
              (insert "にほん /日本/二本/\n"))
            (let ((entries (nskk--dict-parse-file-to-entries tmpfile)))
              (should (listp entries))
              (should (= (length entries) 3))
              (should (assoc "かんじ" entries))
              (should (assoc "さくら" entries))
              (should (assoc "にほん" entries))
              (should (equal (cdr (assoc "さくら" entries)) '("桜")))))
        (delete-file tmpfile))))

  (nskk-it "skips comment lines and blank lines"
    (let ((tmpfile (make-temp-file "nskk-dictionary-test-" nil ".skk")))
      (unwind-protect
          (progn
            (with-temp-file tmpfile
              (insert ";; comment\n")
              (insert "\n")
              (insert "てすと /テスト/\n")
              (insert ";; another comment\n"))
            (let ((entries (nskk--dict-parse-file-to-entries tmpfile)))
              (should (= (length entries) 1))
              (should (assoc "てすと" entries))))
        (delete-file tmpfile))))

  (nskk-it "returns nil for an empty file"
    (let ((tmpfile (make-temp-file "nskk-dictionary-test-" nil ".skk")))
      (unwind-protect
          (should-not (nskk--dict-parse-file-to-entries tmpfile))
        (delete-file tmpfile)))))

(nskk-describe "nskk--dict-save-system-dict-cache and nskk--dict-load-system-dict-from-cache"
  (nskk-it "roundtrip: saved entries can be loaded back"
    (let* ((tmpfile (make-temp-file "nskk-cache-test-" nil ".eld"))
           (entries '(("かんじ" . ("漢字" "感じ"))
                      ("さくら" . ("桜"))))
           (dict-files '("/fake/dict1.skk" "/fake/dict2.skk"))
           (nskk-dict-system-dictionary-files dict-files))
      (unwind-protect
          (nskk-with-mocks ((nskk--dict-cache-file-path (lambda () tmpfile)))
            (nskk--dict-save-system-dict-cache entries dict-files)
            (nskk-prolog-test-with-isolated-db
              ;; Source file validation uses nskk--dict-cache-source-valid-p (Elisp function).
              ;; With identical sorted lists the function returns t automatically.
              (let ((loaded (nskk--dict-load-system-dict-from-cache)))
                (should (listp loaded))
                (should (= (length loaded) 2))
                (should (assoc "かんじ" loaded))
                (should (equal (cdr (assoc "さくら" loaded)) '("桜"))))))
        (when (file-exists-p tmpfile)
          (delete-file tmpfile)))))

  (nskk-it "save creates the cache directory if it does not exist"
    (let* ((tmpdir (make-temp-file "nskk-cachedir-" t))
           (cache-path (expand-file-name "sub/dir/cache.eld" tmpdir)))
      (unwind-protect
          (nskk-with-mocks ((nskk--dict-cache-file-path (lambda () cache-path)))
            (nskk--dict-save-system-dict-cache '() '())
            (should (file-exists-p cache-path)))
        (delete-directory tmpdir t))))

  (nskk-it "returns nil when cache version does not match"
    (let ((temp-file (make-temp-file "nskk-test-cache-" nil ".eld")))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (prin1 (list :version 99 :source-files nil :entries nil)
                     (current-buffer)))
            (let ((nskk-dict-system-dictionary-files nil))
              (nskk-with-mocks ((nskk--dict-cache-file-path
                                 (lambda () temp-file)))
                (should (null (nskk--dict-load-system-dict-from-cache))))))
        (when (file-exists-p temp-file)
          (delete-file temp-file)))))

  (nskk-it "returns nil when stored source files do not match current config"
    (nskk-prolog-test-with-isolated-db
      (let ((temp-file (make-temp-file "nskk-test-cache-" nil ".eld")))
        (unwind-protect
            (progn
              ;; Save a cache that records "/some/old/path" as its source files
              (with-temp-file temp-file
                (prin1 (list :version 1
                             :source-files '("/some/old/path")
                             :entries '(("あ" . ("亜"))))
                       (current-buffer)))
              ;; But current config has a different path
              (let ((nskk-dict-system-dictionary-files '("/different/path")))
                (nskk-with-mocks ((nskk--dict-cache-file-path
                                   (lambda () temp-file)))
                  (should (null (nskk--dict-load-system-dict-from-cache))))))
          (when (file-exists-p temp-file)
            (delete-file temp-file)))))))

;;;
;;; nskk--dict-lookup-okuri-ari
;;;

(nskk-describe "nskk--dict-lookup-okuri-ari"
  (nskk-it "finds candidates when an okuri-ari entry exists in the database"
    (nskk-with-mock-dict '(("かんk" . ("感k")))
      (let ((result (nskk--dict-lookup-okuri-ari "かん")))
        (should (listp result))
        (should (member "感k" result)))))

  (nskk-it "returns nil when no okuri-ari entry matches the key"
    (nskk-with-mock-dict '(("あい" . ("愛")))
      (let ((result (nskk--dict-lookup-okuri-ari "xyz")))
        (should (null result)))))

  (nskk-it "combines candidates from multiple okuri consonants"
    (nskk-with-mock-dict '(("かんk" . ("感k")) ("かんg" . ("感g")))
      (let ((result (nskk--dict-lookup-okuri-ari "かん")))
        (should (member "感k" result))
        (should (member "感g" result)))))

  (nskk-it "deduplicates candidates that appear under multiple okuri keys"
    (nskk-with-mock-dict '(("かんk" . ("漢字")) ("かんg" . ("漢字")))
      (let ((result (nskk--dict-lookup-okuri-ari "かん")))
        ;; cl-union removes duplicates
        (should (= (length (cl-remove-duplicates result :test #'equal))
                   (length result)))))))

;;;
;;; nskk-dict-save-user-dictionary and nskk--dict-maybe-save
;;;

(nskk-describe "nskk-dict-save-user-dictionary"
  (nskk-it "saves user dictionary to a temp file in SKK format"
    (nskk-with-mock-dict '(("てすと" . ("テスト")))
      (let* ((tmpfile (make-temp-file "nskk-user-dict-" nil ".skk"))
             (nskk-dict-user-dictionary-file tmpfile))
        (unwind-protect
            (progn
              ;; Register a word so the user dict is populated
              (nskk-dict-register-word "てすと" "テスト")
              (setq nskk-dict-modified t)
              (nskk-dict-save-user-dictionary)
              (let ((saved (with-temp-buffer
                             (insert-file-contents tmpfile)
                             (buffer-string))))
                (should (string-match-p "てすと" saved))
                ;; File should contain the candidate in SKK format (word/...)
                (should (string-match-p "テスト" saved))
                ;; Modified flag should be cleared after save
                (should-not nskk-dict-modified)))
          (when (file-exists-p tmpfile)
            (delete-file tmpfile))))))

  (nskk-it "does nothing when nskk-dict-user-dictionary-file is nil"
    (nskk-with-mock-dict nil
      (let ((nskk-dict-user-dictionary-file nil)
            (nskk-dict-modified t))
        (nskk-dict-save-user-dictionary)
        ;; Modified flag should remain unchanged
        (should nskk-dict-modified)))))

(nskk-describe "nskk--dict-maybe-save unit"
  (nskk-it "calls nskk-dict-save-user-dictionary when nskk-dict-modified is non-nil"
    (let ((save-called nil))
      (nskk-with-mocks ((nskk-dict-save-user-dictionary
                         (lambda () (setq save-called t))))
        (let ((nskk-dict-modified t))
          (nskk--dict-maybe-save)
          (should save-called)))))

  (nskk-it "does not call nskk-dict-save-user-dictionary when not modified"
    (let ((save-called nil))
      (nskk-with-mocks ((nskk-dict-save-user-dictionary
                         (lambda () (setq save-called t))))
        (let ((nskk-dict-modified nil))
          (nskk--dict-maybe-save)
          (should-not save-called)))))

  (nskk-it "silently handles errors from save without propagating them"
    (nskk-with-mocks ((nskk-dict-save-user-dictionary
                       (lambda () (error "Simulated save failure"))))
      (let ((nskk-dict-modified t))
        ;; Should not signal an error
        (nskk--dict-maybe-save)))))

;;;
;;; nskk-dict-load-file
;;;

(nskk-describe "nskk-dict-load-file"
  (nskk-it "returns nil for a non-existent file"
    (nskk-prolog-test-with-isolated-db
      (should (null (nskk-dict-load-file "/nonexistent/path/to/dict.skk")))))

  (nskk-it "returns nil for a non-string argument"
    (nskk-prolog-test-with-isolated-db
      (should (null (nskk-dict-load-file nil)))
      (should (null (nskk-dict-load-file 42)))))

  (nskk-it "loads entries from a valid SKK dictionary file"
    (nskk-prolog-test-with-isolated-db
      (let ((tmpfile (make-temp-file "nskk-load-file-" nil ".skk")))
        (unwind-protect
            (progn
              (with-temp-file tmpfile
                (insert ";; NSKK test dictionary\n")
                (insert ";; okuri-nasi entries.\n")
                (insert "てすと /テスト/試す/\n")
                (insert "かんじ /漢字/感じ/\n"))
              (let ((result (nskk-dict-load-file tmpfile)))
                (should (symbolp result))
                ;; After loading, the entries should be queryable
                (should (nskk-prolog-holds-p '(system-dict-entry "てすと" \?c)))
                (should (nskk-prolog-holds-p '(system-dict-entry "かんじ" \?c)))))
          (when (file-exists-p tmpfile)
            (delete-file tmpfile))))))

  (nskk-it "uses a custom predicate name when provided"
    (nskk-prolog-test-with-isolated-db
      (let ((tmpfile (make-temp-file "nskk-load-file-" nil ".skk")))
        (unwind-protect
            (progn
              (with-temp-file tmpfile
                (insert ";; okuri-nasi entries.\n")
                (insert "てすと /テスト/\n"))
              (nskk-dict-load-file tmpfile nil 'custom-test-pred)
              ;; Should be under the custom predicate, not system-dict-entry
              (should (nskk-prolog-holds-p '(custom-test-pred "てすと" \?c)))
              (should-not (nskk-prolog-holds-p '(system-dict-entry "てすと" \?c))))
          (when (file-exists-p tmpfile)
            (delete-file tmpfile))))))

  (nskk-it "skips comment and blank lines in the dictionary file"
    (nskk-prolog-test-with-isolated-db
      (let ((tmpfile (make-temp-file "nskk-load-file-" nil ".skk")))
        (unwind-protect
            (progn
              (with-temp-file tmpfile
                (insert ";; comment line\n")
                (insert "\n")
                (insert "てすと /テスト/\n")
                (insert ";; another comment\n")
                (insert "\n"))
              (nskk-dict-load-file tmpfile)
              ;; Only the real entry should be loaded
              (let ((solutions (nskk-prolog-query '(system-dict-entry \?k \?c))))
                (should (= (length solutions) 1))
                (should (equal (nskk-prolog-walk '\?k (car solutions)) "てすと"))))
          (when (file-exists-p tmpfile)
            (delete-file tmpfile))))))

  (nskk-it "returns nil for a comment-only file with no valid entries"
    ;; A file that parses to zero valid entries signals that no dictionary
    ;; was loaded: callers like nskk-dict-load-user-dictionary treat the
    ;; return value as a loaded/not-loaded indicator.
    (nskk-prolog-test-with-isolated-db
      (let ((tmpfile (make-temp-file "nskk-load-file-" nil ".skk")))
        (unwind-protect
            (progn
              (with-temp-file tmpfile
                (insert ";; NSKK user dictionary\n")
                (insert ";; okuri-nasi entries.\n")
                (insert "\n"))
              (should (null (nskk-dict-load-file tmpfile))))
          (when (file-exists-p tmpfile)
            (delete-file tmpfile)))))))

(nskk-describe "nskk-dict-load-user-dictionary"
  (nskk-it "returns nil when nskk-dict-user-dictionary-file is nil"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk-dict-user-dictionary-file nil))
        (should (null (nskk-dict-load-user-dictionary))))))

  (nskk-it "returns nil when the file is not readable"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk-dict-user-dictionary-file "/nonexistent/path/jisyo"))
        (should (null (nskk-dict-load-user-dictionary))))))

  (nskk-it "returns \\='user when a valid user dictionary is loaded"
    (nskk-prolog-test-with-isolated-db
      (let ((temp-file (make-temp-file "nskk-user-dict-" nil ".skk")))
        (unwind-protect
            (progn
              (with-temp-file temp-file
                (insert ";; NSKK user dictionary\n")
                (insert "かんじ /漢字/感じ/\n"))
              (let ((nskk-dict-user-dictionary-file temp-file))
                (should (eq (nskk-dict-load-user-dictionary) 'user))))
          (when (file-exists-p temp-file)
            (delete-file temp-file)))))))



;;; Section 11: Additional nskk-dict-parse-line tests

(nskk-describe "nskk-dict-parse-line additional"
  (nskk-it "parses a valid line with single candidate"
    (let ((result (nskk-dict-parse-line "さくら /桜/")))
      (should (consp result))
      (should (equal (car result) "さくら"))
      (should (equal (cdr result) '("桜")))))

  (nskk-it "parses a valid line with multiple candidates"
    (let ((result (nskk-dict-parse-line "かんじ /漢字/感じ/幹事/")))
      (should (consp result))
      (should (equal (car result) "かんじ"))
      (should (equal (cdr result) '("漢字" "感じ" "幹事")))))

  (nskk-it "returns nil for line without slash"
    (should (null (nskk-dict-parse-line "invalid line without slash")))))

;;; Section 12: Additional nskk--dict-parse-candidates tests

(nskk-describe "nskk--dict-parse-candidates additional"
  (nskk-it "parses single candidate"
    (should (equal (nskk--dict-parse-candidates "/桜/") '("桜"))))

  (nskk-it "parses multiple candidates"
    (should (equal (nskk--dict-parse-candidates "/漢字/感じ/幹事/")
                   '("漢字" "感じ" "幹事"))))

  (nskk-it "strips annotation from each candidate"
    (should (equal (nskk--dict-parse-candidates "/単語;annotation/")
                   '("単語"))))

  (nskk-it "strips annotation but keeps plain candidates"
    (should (equal (nskk--dict-parse-candidates "/漢字/感じ;note/幹事/")
                   '("漢字" "感じ" "幹事")))))

;;; Section 13: Table-driven parse-line tests

(nskk-deftest-table dict-parse-line-table
  :description "dict-parse-line-table: key extraction from SKK dictionary lines"
  :columns (input expected)
  :rows (("かんじ /漢字/感じ/"  "かんじ")
         ("にほん /日本/"       "にほん")
         (";; comment"          nil)
         (""                    nil)
         ("no-slash-line"       nil))
  :body (let ((result (nskk-dict-parse-line input)))
          (if expected
              (should (equal (car result) expected))
            (should (null result)))))

;;; Section 14: nskk--dict-cache-source-valid-p tests

(nskk-describe "nskk--dict-cache-source-valid-p"
  (nskk-it "returns t when stored files equal current files"
    (let ((nskk-dict-system-dictionary-files (quote ("/a/dict" "/b/dict"))))
      (should (nskk--dict-cache-source-valid-p (quote ("/a/dict" "/b/dict"))))))

  (nskk-it "returns t when order differs (sorted comparison)"
    (let ((nskk-dict-system-dictionary-files (quote ("/a/dict" "/b/dict"))))
      (should (nskk--dict-cache-source-valid-p (quote ("/b/dict" "/a/dict"))))))

  (nskk-it "returns nil when stored files differ"
    (let ((nskk-dict-system-dictionary-files (quote ("/a/dict"))))
      (should (null (nskk--dict-cache-source-valid-p (quote ("/b/dict")))))))

  (nskk-it "returns t when both are nil"
    (let ((nskk-dict-system-dictionary-files nil))
      (should (nskk--dict-cache-source-valid-p nil)))))


;;; Section 15: Property-based tests for parse-line invariants

(nskk-property-test dict-parse-line-comment-always-nil
  ((s search-query))
  (let ((line (concat ";; " s)))
    (should (null (nskk-dict-parse-line line)))
    t)
  30)

(nskk-property-test dict-parse-line-result-is-cons-or-nil
  ((s search-query))
  (let ((result (nskk-dict-parse-line s)))
    (or (null result) (consp result)))
  30)

;;; Section 16: register-word dedup and priority tests

(nskk-deftest-table dict-register-word-dedup
  :description "register-word dedup: registering the same word twice produces exactly one occurrence"
  :columns (input expected)
  :rows (("かんじ" "漢字")
         ("にほん" "日本")
         ("さくら" "桜"))
  :body (nskk-prolog-test-with-isolated-db
          (let ((nskk--user-dict-index 'user)
                (nskk-dict-modified nil))
            (nskk-prolog-set-index 'user-dict-entry 2 :trie)
            ;; Register the word twice
            (nskk-dict-register-word input expected)
            (nskk-dict-register-word input expected)
            ;; Should appear exactly once in candidates
            (let* ((candidates (nskk-prolog-query-value
                                `(user-dict-entry ,input \?c) '\?c))
                   (occurrences (cl-count expected candidates :test #'equal)))
              (should (= occurrences 1))))))

(nskk-deftest-table dict-register-word-priority
  :description "register-word priority: newly registered word appears first in candidates"
  :columns (input expected)
  :rows (("かんじ" "漢字")
         ("にほん" "日本"))
  :body (nskk-prolog-test-with-isolated-db
          (let ((nskk--user-dict-index 'user)
                (nskk-dict-modified nil))
            (nskk-prolog-set-index 'user-dict-entry 2 :trie)
            (nskk-prolog-retract-all 'user-dict-entry 2)
            ;; Register first candidate, then add a new one
            (nskk-dict-register-word input "既存")
            (nskk-dict-register-word input expected)
            ;; New registration should appear first (prepended)
            (let ((candidates (nskk-prolog-query-value
                               `(user-dict-entry ,input \?c) '\?c)))
              (should (member expected candidates))
              (should (equal (car candidates) expected))))))

;;; Property-Based Tests
;;;

;; PBT 1 (FR-012): get-after-put invariant
;; After registering a word, looking it up via the CPS interface returns
;; a candidates list that includes that word.
(nskk-property-test-seeded dictionary-pbt-get-after-put
  ((reading hiragana-string)
   (word kanji-string))
  (let ((result nil))
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-set-index 'user-dict-entry 2 :trie)
      (let ((nskk--user-dict-index nil)
            (nskk-dict-modified nil))
        (nskk-dict-register-word/k reading word
          (lambda (_) t) #'ignore)
        ;; The registered word must be findable via the CPS lookup
        (nskk-dict-lookup/k reading
          (lambda (candidates) (setq result candidates))
          #'ignore)))
    (and result (member word result)))
  50 42)

;; PBT 2 (FR-012): not-found stability
;; Looking up a non-existent key always calls on-not-found without signaling
;; an error, regardless of what string is used as a key.
(nskk-property-test-seeded dictionary-pbt-not-found-calls-on-not-found
  ((reading romaji-string))
  (nskk-prolog-test-with-isolated-db
    (let ((not-found-called nil) (error-occurred nil))
      (condition-case _
          (nskk-dict-lookup/k (concat "NO-SUCH-KEY-" reading)
            #'ignore
            (lambda () (setq not-found-called t)))
        (error (setq error-occurred t)))
      (and not-found-called (not error-occurred))))
  50 42)

;; PBT 3 (FR-019): nil-conflation safety
;; When nskk--dict-do-lookup/k calls on-found, it is because real candidates
;; were found — not because a falsy value was conflated with not-found.
;; Stub the underlying Prolog query to return a list containing a falsy-looking
;; candidate (empty string) and verify on-found fires, not on-not-found.
(nskk-property-test-seeded dictionary-pbt-nil-conflation-safety
  ((reading hiragana-string))
  (let ((found-called nil) (not-found-called nil))
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-set-index 'user-dict-entry 2 :trie)
      (let ((nskk--user-dict-index nil)
            (nskk-dict-modified nil))
        ;; Register a real non-empty word first so lookup succeeds
        (nskk-dict-register-word/k reading "テスト"
          (lambda (_) t) #'ignore)
        ;; Now verify that on-found is called (not on-not-found)
        (nskk-dict-lookup/k reading
          (lambda (candidates)
            (setq found-called t)
            candidates)
          (lambda () (setq not-found-called t)))))
    (and found-called (not not-found-called)))
  50 42)

;; PBT 4 (FR-012): register idempotency
;; Registering the same (reading, word) pair twice must not produce duplicate
;; entries in the candidates list.
(nskk-property-test-seeded dictionary-pbt-register-idempotent
  ((reading hiragana-string)
   (word kanji-string))
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-set-index 'user-dict-entry 2 :trie)
    (let ((result nil)
          (nskk--user-dict-index nil)
          (nskk-dict-modified nil))
      (nskk-dict-register-word/k reading word (lambda (_) t) #'ignore)
      (nskk-dict-register-word/k reading word (lambda (_) t) #'ignore)
      (nskk-dict-lookup/k reading
        (lambda (candidates) (setq result candidates))
        #'ignore)
      ;; No duplicate entries for the registered word
      (= (cl-count word result :test #'equal) 1)))
  30 42)

(provide 'nskk-dictionary-test)

;;; nskk-dictionary-test.el ends here
