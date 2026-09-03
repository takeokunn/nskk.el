;;; nskk-dictionary-test.el --- Tests for nskk-dictionary.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Tests for nskk-dictionary.el.

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
    ;; Check the declared defcustom standard value, not the current value:
    ;; the test framework redirects the live variable to a temp path so
    ;; batch runs never touch the real personal dictionary.
    (should (equal (eval (car (get 'nskk-dict-user-dictionary-file
                                   'standard-value)))
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
      (let ((_index (make-nskk-dict-index :predicate 'workflow-io-dict)))
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
  ;; Mock tree nodes store candidates in the order produced by
  ;; `skkdic-extract-conversion-data' (cons-reversal of ja-dic.el text order).
  ;; nskk passes them through as-is, matching DDSKK's candidate presentation order.
  (nskk-it "decodes and flattens okuri-nasi entries"
    (let* ((o (- (logand (encode-char ?お 'japanese-jisx0208) #xFF) 32))
           (sample `(skdic-okuri-nasi
                     (,o ("緒" "小")))))
      (should (equal (nskk--dict-ja-dic-flatten-tree sample)
                     '(("お" . ("緒" "小")))))))

  (nskk-it "decodes and flattens okuri-ari entries"
    (let* ((wa (- (logand (encode-char ?わ 'japanese-jisx0208) #xFF) 32))
           (ru (- (logand (encode-char ?る 'japanese-jisx0208) #xFF) 32))
           (sample `(skkdic-okuri-ari
                     (,wa t
                          (,ru t
                               (-105 ("惡" "悪")))))))
      ;; Without reverse-candidates flag, stored order is preserved
      (should (equal (nskk--dict-ja-dic-flatten-tree sample)
                     '(("わるi" . ("惡" "悪")))))
      ;; With reverse-candidates flag (for okuri-ari), order is reversed
      (should (equal (nskk--dict-ja-dic-flatten-tree sample t)
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
          ;; okuri-nasi: order preserved as-is
          (should (equal '("緒" "小")
                         (nskk-prolog-query-value '(system-dict-entry "お" \?c) '\?c)))
          ;; okuri-ari: reversed to match SKK-JISYO.L / skkserv order
          (should (equal '("悪" "惡")
                         (nskk-prolog-query-value '(system-dict-entry "わるi" \?c) '\?c))))))))

(nskk-describe "dict-initialize"
  (nskk-it "uses auto-detection when config is nil"
    (let ((nskk-dict-system-dictionary-files nil)
          (nskk-dict-use-ja-dic nil)
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
                        (nskk-dict-load-system-dictionaries (lambda () 'system))
                        (nskk-dict-load-user-dictionary (lambda () nil)))
        (nskk-dict-initialize)
        (should-not detect-called))))

  (nskk-it "force ja-dic when nskk-dict-use-ja-dic is t (skips auto-detect)"
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
        (should-not detect-called))))

  (nskk-it "auto mode prefers auto-detect over ja-dic"
    (let ((nskk-dict-system-dictionary-files nil)
          (nskk-dict-use-ja-dic 'auto)
          (nskk-dict-user-dictionary-file nil)
          (nskk--system-dict-index nil)
          (nskk--user-dict-index nil)
          (ja-dic-called nil)
          (detect-called nil)
          (load-called nil))
      (nskk-with-mocks ((nskk--dict-detect-system-dictionaries
                         (lambda () (setq detect-called t) '("/found/SKK-JISYO.L")))
                        (nskk-dict-load-system-dictionaries
                         (lambda () (setq load-called t) 'system))
                        (nskk-dict-load-ja-dic (lambda () (setq ja-dic-called t) 'system))
                        (nskk-dict-load-user-dictionary (lambda () nil)))
        (nskk-dict-initialize)
        (should detect-called)
        (should load-called)
        (should-not ja-dic-called))))

  (nskk-it "auto mode falls back to ja-dic when no files detected"
    (let ((nskk-dict-system-dictionary-files nil)
          (nskk-dict-use-ja-dic 'auto)
          (nskk-dict-user-dictionary-file nil)
          (nskk--system-dict-index nil)
          (nskk--user-dict-index nil)
          (ja-dic-called nil))
      (nskk-with-mocks ((nskk--dict-detect-system-dictionaries (lambda () nil))
                        (nskk-dict-load-ja-dic (lambda () (setq ja-dic-called t) 'system))
                        (nskk-dict-load-user-dictionary (lambda () nil)))
        (nskk-dict-initialize)
        (should ja-dic-called))))

  (nskk-it "auto mode falls back to ja-dic when auto-detect load fails"
    (let ((nskk-dict-system-dictionary-files nil)
          (nskk-dict-use-ja-dic 'auto)
          (nskk-dict-user-dictionary-file nil)
          (nskk--system-dict-index nil)
          (nskk--user-dict-index nil)
          (ja-dic-called nil))
      (nskk-with-mocks ((nskk--dict-detect-system-dictionaries
                         (lambda () '("/corrupt/file")))
                        (nskk-dict-load-system-dictionaries (lambda () nil))
                        (nskk-dict-load-ja-dic (lambda () (setq ja-dic-called t) 'system))
                        (nskk-dict-load-user-dictionary (lambda () nil)))
        (nskk-dict-initialize)
        (should ja-dic-called))))

  (nskk-it "nil suppresses ja-dic entirely"
    (let ((nskk-dict-system-dictionary-files nil)
          (nskk-dict-use-ja-dic nil)
          (nskk-dict-user-dictionary-file nil)
          (nskk--system-dict-index nil)
          (nskk--user-dict-index nil)
          (ja-dic-called nil))
      (nskk-with-mocks ((nskk--dict-detect-system-dictionaries (lambda () nil))
                        (nskk-dict-load-ja-dic (lambda () (setq ja-dic-called t) 'system))
                        (nskk-dict-load-user-dictionary (lambda () nil)))
        (nskk-dict-initialize)
        (should-not ja-dic-called)))))

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

  (nskk-it "rejects an empty reading with the fixed dictionary error"
  (nskk-prolog-test-with-isolated-db
    (let ((condition
           (condition-case error
               (progn
                 (nskk-dict-register-word "" "候補")
                 nil)
             (nskk-dict-error error))))
      (should
       (equal condition
              '(nskk-dict-error "Invalid user dictionary entry")))
      (should
       (equal (error-message-string condition)
              "Dictionary error: \"Invalid user dictionary entry\"")))))

  (progn
  (nskk-it "rejects an empty word with the fixed dictionary error"
  (nskk-prolog-test-with-isolated-db
    (let ((condition
           (condition-case error
               (progn
                 (nskk-dict-register-word "よみ" "")
                 nil)
             (nskk-dict-error error))))
      (should
       (equal condition
              '(nskk-dict-error "Invalid user dictionary entry")))
      (should
       (equal (error-message-string condition)
              "Dictionary error: \"Invalid user dictionary entry\"")))))

  (nskk-it "rejects unrepresentable words before observable mutation"
  (nskk-prolog-test-with-isolated-db
    (let* ((nskk--search-registered-caches
            (make-hash-table :test 'eq :weakness 'key))
           (cache (nskk-cache-create :type 'lru :capacity 4))
           (hook-calls 0)
           (nskk-jisyo-update-hook
            (list (lambda () (cl-incf hook-calls))
                  #'nskk--search-flush-caches))
           (nskk--user-dict-index 'user)
           (nskk-dict-modified 'preserved)
           (dictionary-file
            (make-temp-file "nskk-register-boundary-" nil ".skk"))
           (nskk-dict-user-dictionary-file dictionary-file)
           (clause-key
            (nskk-prolog-clause-key 'user-dict-entry 2))
           before
           before-file)
      (unwind-protect
          (progn
            (nskk-prolog-set-index 'user-dict-entry 2 :trie)
            (nskk-prolog-assert
             '((user-dict-entry "てすと" ("既存"))))
            (nskk-cache-put cache "cached" "value")
            (nskk--search-register-cache cache)
            (with-temp-file dictionary-file
              (insert "unchanged" (string 0 127) "\n"))
            (setq before
                  (nskk-dict-transaction-predicate-snapshot clause-key)
                  before-file
                  (with-temp-buffer
                    (insert-file-contents-literally dictionary-file)
                    (buffer-string)))
            (dolist
                (case
                 (list
                  (cons "slash" "不/正")
                  (cons "semicolon" "不;正")
                  (cons "newline" "不\n正")
                  (cons "tab" "不\t正")
                  (cons "carriage-return" "不\r正")
                  (cons "NUL" (concat "不" (string 0) "正"))
                  (cons "DEL" (concat "不" (string 127) "正"))
                  (cons "preedit-open" "不▽正")
                  (cons "preedit-select" "不▼正")
                  (cons "empty" "")
                  (cons "non-string" 42)))
              (ert-info ((format "%s candidate" (car case)))
                (let ((condition
                       (condition-case error
                           (progn
                             (nskk-dict-register-word "てすと" (cdr case))
                             nil)
                         (nskk-dict-error error))))
                  (should
                   (equal condition
                          '(nskk-dict-error
                            "Invalid user dictionary entry")))
                  (should
                   (equal
                    (error-message-string condition)
                    "Dictionary error: \"Invalid user dictionary entry\"")))))
            (let ((after
                   (nskk-dict-transaction-predicate-snapshot clause-key)))
              (dotimes (slot (1- (length before)))
                (should
                 (eq (aref before (1+ slot))
                     (aref after (1+ slot))))))
            (should
             (nskk-prolog-holds-p
              '(user-dict-entry "てすと" ("既存"))))
            (should (eq nskk-dict-modified 'preserved))
            (should (= hook-calls 0))
            (should (= (nskk-cache-size cache) 1))
            (should (equal (nskk-cache-get cache "cached") "value"))
            (should
             (equal before-file
                    (with-temp-buffer
                      (insert-file-contents-literally dictionary-file)
                      (buffer-string)))))
        (when (file-exists-p dictionary-file)
          (delete-file dictionary-file))))))

  (nskk-it "round-trips valid Unicode and spaces through save and reload"
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
              (setq nskk--user-dict-index nil)
              (should (eq (nskk-dict-load-user-dictionary) 'user))
              (should (member word (nskk-dict-lookup reading))))
          (when (file-exists-p dictionary-file)
            (delete-file dictionary-file)))))))

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

  (progn
  (nskk-it "runs nskk-jisyo-update-hook after successful registration"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-set-index 'user-dict-entry 2 :trie)
      (let* ((hook-called nil)
             (nskk--user-dict-index 'user)
             (nskk-dict-modified nil)
             (nskk-jisyo-update-hook
              (list (lambda () (setq hook-called t)))))
        (should (nskk-dict-register-word "てすと" "テスト"))
        (should hook-called)
        (should nskk-dict-modified))))

  (nskk-it "rolls back hook and message faults while leaving prior external effects explicit"
    (dolist (stage '(hook message))
      (dolist (kind '(error quit))
        (ert-info ((format "%s at %s boundary" kind stage))
          (nskk-prolog-test-with-isolated-db
            (let* ((nskk--search-registered-caches
                    (make-hash-table :test 'eq :weakness 'key))
                   (cache (nskk-cache-create :type 'lru :capacity 4))
                   (events nil)
                   (later-hooks 0)
                   (message-calls 0)
                   (fault-data (list "registration publication fault" stage kind))
                   (nskk-jisyo-update-hook
                    (list
                     (lambda ()
                       ;; External effects are intentionally not claimed to be
                       ;; reversible; only NSKK's internal state is restored.
                       (push 'external-effect events)
                       (nskk-cache-clear cache))
                     (lambda ()
                       (push 'fault-boundary events)
                       (when (eq stage 'hook)
                         (signal kind fault-data)))
                     (lambda ()
                       (push 'later-observer events)
                       (cl-incf later-hooks))))
                   (nskk--user-dict-index 'user)
                   (nskk-dict-modified 'preserved)
                   (dictionary-file
                    (make-temp-file "nskk-register-publication-" nil ".skk"))
                   (nskk-dict-user-dictionary-file dictionary-file)
                   (clause-key
                    (nskk-prolog-clause-key 'user-dict-entry 2))
                   before
                   before-file
                   cache-hash
                   cache-head
                   cache-tail)
              (unwind-protect
                  (progn
                    (nskk-prolog-set-index 'user-dict-entry 2 :trie)
                    (nskk-prolog-assert
                     '((user-dict-entry "既存" ("候補"))))
                    (nskk-cache-put cache "cached" "value")
                    (nskk--search-register-cache cache)
                    (with-temp-file dictionary-file
                      (insert "unchanged" (string 0 127) "\n"))
                    (setq before
                          (nskk-dict-transaction-predicate-snapshot clause-key)
                          before-file
                          (with-temp-buffer
                            (insert-file-contents-literally dictionary-file)
                            (buffer-string))
                          cache-hash (nskk-cache-lru-hash cache)
                          cache-head (nskk-cache-lru-head cache)
                          cache-tail (nskk-cache-lru-tail cache))
                    (let ((condition
                           (cl-letf
                               (((symbol-function 'message)
                                 (lambda (&rest _arguments)
                                   (cl-incf message-calls)
                                   (when (eq stage 'message)
                                     (signal kind fault-data)))))
                             (condition-case error
                                 (progn
                                   (nskk-dict-register-word "再試験" "成功")
                                   nil)
                               ((error quit) error)))))
                      (should (equal condition (cons kind fault-data))))
                    (let ((after
                           (nskk-dict-transaction-predicate-snapshot clause-key)))
                      (dotimes (slot (1- (length before)))
                        (should
                         (eq (aref before (1+ slot))
                             (aref after (1+ slot))))))
                    (should
                     (nskk-prolog-holds-p
                      '(user-dict-entry "既存" ("候補"))))
                    (should-not
                     (nskk-prolog-holds-p
                      '(user-dict-entry "再試験" ("成功"))))
                    (should (eq nskk--user-dict-index 'user))
                    (should (eq nskk-dict-modified 'preserved))
                    (should (eq (nskk-cache-lru-hash cache) cache-hash))
                    (should (eq (nskk-cache-lru-head cache) cache-head))
                    (should (eq (nskk-cache-lru-tail cache) cache-tail))
                    (should (= (nskk-cache-size cache) 1))
                    (should (equal (nskk-cache-get cache "cached") "value"))
                    (should
                     (equal before-file
                            (with-temp-buffer
                              (insert-file-contents-literally dictionary-file)
                              (buffer-string))))
                    (if (eq stage 'hook)
                        (progn
                          (should
                           (equal (reverse events)
                                  '(external-effect fault-boundary)))
                          (should (= later-hooks 0))
                          (should (= message-calls 0)))
                      (should
                       (equal (reverse events)
                              '(external-effect fault-boundary later-observer)))
                      (should (= later-hooks 1))
                      (should (= message-calls 1)))
                    (let ((nskk-jisyo-update-hook nil))
                      (should
                       (nskk-dict-register-word "再試験" "成功")))
                    (should (member "成功" (nskk-dict-lookup "再試験")))
                    (should nskk-dict-modified))
                (when (file-exists-p dictionary-file)
                  (delete-file dictionary-file))))))))))

  (nskk-it "rejects unrepresentable readings before observable mutation"
  (nskk-prolog-test-with-isolated-db
    (let* ((nskk--search-registered-caches
            (make-hash-table :test 'eq :weakness 'key))
           (cache (nskk-cache-create :type 'lru :capacity 4))
           (hook-calls 0)
           (nskk-jisyo-update-hook
            (list (lambda () (cl-incf hook-calls))
                  #'nskk--search-flush-caches))
           (nskk--user-dict-index 'user)
           (nskk-dict-modified 'preserved)
           (dictionary-file
            (make-temp-file "nskk-register-reading-boundary-" nil ".skk"))
           (nskk-dict-user-dictionary-file dictionary-file)
           (clause-key
            (nskk-prolog-clause-key 'user-dict-entry 2))
           before
           before-file)
      (unwind-protect
          (progn
            (nskk-prolog-set-index 'user-dict-entry 2 :trie)
            (nskk-prolog-assert
             '((user-dict-entry "てすと" ("既存"))))
            (nskk-cache-put cache "cached" "value")
            (nskk--search-register-cache cache)
            (with-temp-file dictionary-file
              (insert "unchanged" (string 0 127) "\n"))
            (setq before
                  (nskk-dict-transaction-predicate-snapshot clause-key)
                  before-file
                  (with-temp-buffer
                    (insert-file-contents-literally dictionary-file)
                    (buffer-string)))
            (dolist
                (case
                 (list
                  (cons "space" "不 正")
                  (cons "slash" "不/正")
                  (cons "semicolon" "不;正")
                  (cons "newline" "不\n正")
                  (cons "tab" "不\t正")
                  (cons "carriage-return" "不\r正")
                  (cons "NUL" (concat "不" (string 0) "正"))
                  (cons "DEL" (concat "不" (string 127) "正"))
                  (cons "preedit-open" "不▽正")
                  (cons "preedit-select" "不▼正")
                  (cons "empty" "")
                  (cons "non-string" 42)))
              (ert-info ((format "%s reading" (car case)))
                (let ((condition
                       (condition-case error
                           (progn
                             (nskk-dict-register-word (cdr case) "候補")
                             nil)
                         (nskk-dict-error error))))
                  (should
                   (equal condition
                          '(nskk-dict-error
                            "Invalid user dictionary entry")))
                  (should
                   (equal
                    (error-message-string condition)
                    "Dictionary error: \"Invalid user dictionary entry\"")))))
            (let ((after
                   (nskk-dict-transaction-predicate-snapshot clause-key)))
              (dotimes (slot (1- (length before)))
                (should
                 (eq (aref before (1+ slot))
                     (aref after (1+ slot))))))
            (should
             (nskk-prolog-holds-p
              '(user-dict-entry "てすと" ("既存"))))
            (should (eq nskk-dict-modified 'preserved))
            (should (= hook-calls 0))
            (should (= (nskk-cache-size cache) 1))
            (should (equal (nskk-cache-get cache "cached") "value"))
            (should
             (equal before-file
                    (with-temp-buffer
                      (insert-file-contents-literally dictionary-file)
                      (buffer-string)))))
        (when (file-exists-p dictionary-file)
          (delete-file dictionary-file))))))

  (progn
  (nskk-it "rolls back repeated retract, assert, and query faults after mutation"
    (dolist (stage '(retract assert query))
      (dolist (kind '(error quit))
        (ert-info ((format "%s fault after %s" kind stage))
          (nskk-prolog-test-with-isolated-db
            (let* ((nskk--search-registered-caches
                    (make-hash-table :test 'eq :weakness 'key))
                   (cache (nskk-cache-create :type 'lru :capacity 4))
                   (hook-calls 0)
                   (fault-calls 0)
                   (nskk-jisyo-update-hook
                    (list (lambda () (cl-incf hook-calls))))
                   (nskk--user-dict-index 'user)
                   (nskk-dict-modified 'preserved)
                   (dictionary-file
                    (make-temp-file "nskk-register-storage-" nil ".skk"))
                   (nskk-dict-user-dictionary-file dictionary-file)
                   (clause-key
                    (nskk-prolog-clause-key 'user-dict-entry 2))
                   (fault-data (list "registration storage fault" stage kind))
                   (real-retract (symbol-function 'nskk-prolog-retract))
                   (real-assert (symbol-function 'nskk-prolog-assert))
                   (real-query (symbol-function 'nskk-prolog-holds-p))
                   before
                   before-file
                   cache-hash
                   cache-head
                   cache-tail)
              (unwind-protect
                  (progn
                    (nskk-prolog-set-index 'user-dict-entry 2 :trie)
                    (nskk-prolog-assert
                     '((user-dict-entry "再試験" ("既存候補"))))
                    (nskk-cache-put cache "cached" "value")
                    (nskk--search-register-cache cache)
                    (with-temp-file dictionary-file
                      (insert "unchanged" (string 0 127) "\n"))
                    (setq before
                          (nskk-dict-transaction-predicate-snapshot clause-key)
                          before-file
                          (with-temp-buffer
                            (insert-file-contents-literally dictionary-file)
                            (buffer-string))
                          cache-hash (nskk-cache-lru-hash cache)
                          cache-head (nskk-cache-lru-head cache)
                          cache-tail (nskk-cache-lru-tail cache))
                    (cl-labels
                        ((fault ()
                           (cl-incf fault-calls)
                           (signal kind fault-data))
                         (register ()
                           (condition-case condition
                               (progn
                                 (nskk-dict-register-word "再試験" "成功")
                                 nil)
                             ((error quit) condition)))
                         (attempt ()
                           (pcase stage
                             ('retract
                              (cl-letf
                                  (((symbol-function 'nskk-prolog-retract)
                                    (lambda (&rest arguments)
                                      (prog1
                                          (apply real-retract arguments)
                                        (fault)))))
                                (register)))
                             ('assert
                              (cl-letf
                                  (((symbol-function 'nskk-prolog-assert)
                                    (lambda (&rest arguments)
                                      (prog1
                                          (apply real-assert arguments)
                                        (fault)))))
                                (register)))
                             ('query
                              (cl-letf
                                  (((symbol-function 'nskk-prolog-holds-p)
                                    (lambda (&rest arguments)
                                      (prog1
                                          (apply real-query arguments)
                                        (fault)))))
                                (register))))))
                      (dotimes (_ 2)
                        (let ((condition (attempt)))
                          (should
                           (equal condition (cons kind fault-data))))
                        (let ((after
                               (nskk-dict-transaction-predicate-snapshot clause-key)))
                          (dotimes (slot (1- (length before)))
                            (should
                             (eq (aref before (1+ slot))
                                 (aref after (1+ slot))))))
                        (should
                         (funcall real-query
                                  '(user-dict-entry "再試験" ("既存候補"))))
                        (should-not
                         (funcall real-query
                                  '(user-dict-entry "再試験" ("成功"))))
                        (should (eq nskk--user-dict-index 'user))
                        (should (eq nskk-dict-modified 'preserved))
                        (should (= hook-calls 0))
                        (should (eq (nskk-cache-lru-hash cache) cache-hash))
                        (should (eq (nskk-cache-lru-head cache) cache-head))
                        (should (eq (nskk-cache-lru-tail cache) cache-tail))
                        (should (= (nskk-cache-size cache) 1))
                        (should
                         (equal before-file
                                (with-temp-buffer
                                  (insert-file-contents-literally dictionary-file)
                                  (buffer-string)))))
                      (should (= fault-calls 2)))
                    (should
                     (nskk-dict-register-word "再試験" "成功"))
                    (should (member "成功" (nskk-dict-lookup "再試験")))
                    (should nskk-dict-modified)
                    (should (= hook-calls 1)))
                (when (file-exists-p dictionary-file)
                  (delete-file dictionary-file)))))))))

  (nskk-it "rolls back lazy-load and index-construction faults before retry"
    (dolist (stage '(load index))
      (dolist (kind '(error quit))
        (ert-info ((format "%s fault after %s" kind stage))
          (nskk-prolog-test-with-isolated-db
            (let* ((nskk--search-registered-caches
                    (make-hash-table :test 'eq :weakness 'key))
                   (cache (nskk-cache-create :type 'lru :capacity 4))
                   (hook-calls 0)
                   (fault-calls 0)
                   (nskk-jisyo-update-hook
                    (list (lambda () (cl-incf hook-calls))))
                   (nskk--user-dict-index nil)
                   (nskk-dict-modified 'preserved)
                   (dictionary-file
                    (make-temp-file "nskk-register-lazy-" nil ".skk"))
                   (nskk-dict-user-dictionary-file
                    (and (eq stage 'load) dictionary-file))
                   (clause-key
                    (nskk-prolog-clause-key 'user-dict-entry 2))
                   (fault-data (list "registration lazy fault" stage kind))
                   (real-load
                    (symbol-function 'nskk-dict-load-user-dictionary))
                   (real-index (symbol-function 'nskk-prolog-set-index))
                   (real-query (symbol-function 'nskk-prolog-holds-p))
                   before
                   before-file
                   cache-hash
                   cache-head
                   cache-tail)
              (unwind-protect
                  (progn
                    (with-temp-file dictionary-file
                      (insert ";; okuri-nasi entries.\nよみ /既存/\n"))
                    (nskk-cache-put cache "cached" "value")
                    (nskk--search-register-cache cache)
                    (setq before
                          (nskk-dict-transaction-predicate-snapshot clause-key)
                          before-file
                          (with-temp-buffer
                            (insert-file-contents-literally dictionary-file)
                            (buffer-string))
                          cache-hash (nskk-cache-lru-hash cache)
                          cache-head (nskk-cache-lru-head cache)
                          cache-tail (nskk-cache-lru-tail cache))
                    (let ((condition
                           (condition-case error
                               (progn
                                 (pcase stage
                                   ('load
                                    (cl-letf
                                        (((symbol-function
                                           'nskk-dict-load-user-dictionary)
                                          (lambda ()
                                            (prog1 (funcall real-load)
                                              (cl-incf fault-calls)
                                              (signal kind fault-data)))))
                                      (nskk-dict-register-word
                                       "再試験" "成功")))
                                   ('index
                                    (cl-letf
                                        (((symbol-function
                                           'nskk-prolog-set-index)
                                          (lambda (&rest arguments)
                                            (prog1
                                                (apply real-index arguments)
                                              (cl-incf fault-calls)
                                              (signal kind fault-data)))))
                                      (nskk-dict-register-word
                                       "再試験" "成功"))))
                                 nil)
                             ((error quit) error))))
                      (should (equal condition (cons kind fault-data))))
                    (should (= fault-calls 1))
                    (let ((after
                           (nskk-dict-transaction-predicate-snapshot clause-key)))
                      (dotimes (slot (1- (length before)))
                        (should
                         (eq (aref before (1+ slot))
                             (aref after (1+ slot))))))
                    (should-not
                     (funcall real-query
                              '(user-dict-entry "再試験" ("成功"))))
                    (should (eq nskk--user-dict-index nil))
                    (should (eq nskk-dict-modified 'preserved))
                    (should (= hook-calls 0))
                    (should (eq (nskk-cache-lru-hash cache) cache-hash))
                    (should (eq (nskk-cache-lru-head cache) cache-head))
                    (should (eq (nskk-cache-lru-tail cache) cache-tail))
                    (should (= (nskk-cache-size cache) 1))
                    (should
                     (equal before-file
                            (with-temp-buffer
                              (insert-file-contents-literally dictionary-file)
                              (buffer-string))))
                    (should
                     (nskk-dict-register-word "再試験" "成功"))
                    (should (member "成功" (nskk-dict-lookup "再試験")))
                    (when (eq stage 'load)
                      (should (member "既存" (nskk-dict-lookup "よみ"))))
                    (should nskk-dict-modified)
                    (should (= hook-calls 1)))
                (when (file-exists-p dictionary-file)
                  (delete-file dictionary-file))))))))))

  (nskk-it "save is a no-op while nskk--dict-save-inhibited is non-nil"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-set-index 'user-dict-entry 2 :trie)
      (let* ((tmp (make-temp-file "nskk-save-inhibit-"))
             (nskk-dict-user-dictionary-file tmp)
             (nskk--user-dict-index 'user)
             (nskk-dict-modified t)
             (nskk--dict-save-inhibited t))
        (unwind-protect
            (progn
              (nskk-prolog-assert '((user-dict-entry "みに" ("ミニ"))))
              (nskk-dict-save-user-dictionary)
              ;; Nothing written, modified flag untouched.
              (should (= 0 (file-attribute-size (file-attributes tmp))))
              (should nskk-dict-modified)
              ;; Lifting the inhibit writes normally.
              (setq nskk--dict-save-inhibited nil)
              (nskk-dict-save-user-dictionary)
              (should (> (file-attribute-size (file-attributes tmp)) 0))
              (should-not nskk-dict-modified))
          (delete-file tmp)))))

  (nskk-it "registered okuri-ari style key is found by stem lookup (round-trip)"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-set-index 'user-dict-entry 2 :trie)
      (let ((nskk--user-dict-index nil)
            (nskk-dict-modified nil))
        ;; Okurigana registration stores the dictionary key ("はしr"),
        ;; which the okuri-ari lookup finds by appending consonants.
        ;; (Single-char stems skip okuri-ari search by design.)
        (nskk-dict-register-word "はしr" "走")
        (let ((result (nskk-dict-lookup "はし")))
          (should (member "走" result)))))))

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

  (nskk-it "rejects invalid fields without invoking either continuation"
    (nskk-prolog-test-with-isolated-db
      (let* ((found-calls 0)
            (not-found-calls 0)
            (hook-calls 0)
            (nskk-jisyo-update-hook
             (list (lambda () (cl-incf hook-calls))))
            (nskk--user-dict-index 'user)
            (nskk-dict-modified 'preserved))
        (dolist (arguments '(("" "テスト") ("てすと" "")))
          (let ((condition
                 (condition-case error
                     (progn
                       (nskk-dict-register-word/k
                        (car arguments) (cadr arguments)
                        (lambda (_) (cl-incf found-calls))
                        (lambda () (cl-incf not-found-calls)))
                       nil)
                   (nskk-dict-error error))))
            (should
             (equal condition
                    '(nskk-dict-error "Invalid user dictionary entry")))))
        (should (= found-calls 0))
        (should (= not-found-calls 0))
        (should (= hook-calls 0))
        (should (eq nskk-dict-modified 'preserved)))))

  (nskk-it "uses not-found only for a valid query without a solution"
    (let ((found-calls 0)
          (not-found-calls 0))
      (cl-letf (((symbol-function 'nskk--dict-register-impl)
                 (lambda (reading word)
                   (should (equal reading "ゆうこう"))
                   (should (equal word "候補"))
                   nil)))
        (nskk-dict-register-word/k
         "ゆうこう" "候補"
         (lambda (_) (cl-incf found-calls))
         (lambda () (cl-incf not-found-calls))))
      (should (= found-calls 0))
      (should (= not-found-calls 1))))

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

(nskk-describe "stable dictionary candidate merging"
  (nskk-it "keeps solution order, first identity, nil, and input lists"
    (let* ((first (propertize "same" 'source 'first))
           (duplicate (propertize "same" 'source 'later))
           (first-list (list first nil "first-only"))
           (second-list (list duplicate nil "second-only"))
           (first-tail (cdr first-list))
           (second-tail (cdr second-list))
           (first-before (copy-sequence first-list))
           (second-before (copy-sequence second-list))
           (solutions `((one . ,first-list) (empty) (two . ,second-list)))
           result)
      (cl-letf (((symbol-function 'nskk-prolog-walk)
                 (lambda (_variable solution)
                   (cdr (assq solution solutions)))))
        (setq result (nskk--dict-collect-candidates '(one empty two))))
      (should
       (equal
        (mapcar (lambda (candidate)
                  (and candidate (substring-no-properties candidate)))
                result)
        '("same" nil "first-only" "second-only")))
      (should (eq (car result) first))
      (should (eq (get-text-property 0 'source (car result)) 'first))
      (should (equal first-list first-before))
      (should (equal second-list second-before))
      (should (eq (cdr first-list) first-tail))
      (should (eq (cdr second-list) second-tail))
      (should-error
       (nskk--dict-merge-candidate-lists '(invalid-candidate-list)))))

  (nskk-it "uses one hash probe per input candidate at 5000 candidates"
    (let* ((first (cl-loop for index below 2500
                           collect (format "candidate-%d" index)))
           (second (mapcar #'copy-sequence first))
           (real-gethash (symbol-function 'gethash))
           (real-puthash (symbol-function 'puthash))
           (probes 0)
           (inserts 0)
           result)
      (cl-letf (((symbol-function 'gethash)
                 (lambda (key table &optional default)
                   (setq probes (1+ probes))
                   (funcall real-gethash key table default)))
                ((symbol-function 'puthash)
                 (lambda (key value table)
                   (setq inserts (1+ inserts))
                   (funcall real-puthash key value table))))
        (setq result
              (nskk--dict-merge-candidate-lists (list first second))))
      (should (= probes 5000))
      (should (= inserts 2500))
      (should (= (length result) 2500))
      (should (equal result first))))

  (nskk-it "preserves consonant and solution order for okuri-ari"
    (let* ((nskk--dict-okuri-consonants '(?k ?g))
           (first (propertize "same" 'source 'okuri-k))
           (duplicate (propertize "same" 'source 'okuri-g))
           (candidate-map
            `((k-one . (,first "k-one"))
              (k-two . ("k-two"))
              (g-one . (,duplicate "g-one"))))
           result)
      (cl-letf (((symbol-function 'nskk-prolog-query)
                 (lambda (query)
                   (pcase (nth 1 query)
                     ("rootk" '(k-one k-two))
                     ("rootg" '(g-one))
                     (_ nil))))
                ((symbol-function 'nskk-prolog-walk)
                 (lambda (_variable solution)
                   (cdr (assq solution candidate-map)))))
        (setq result (nskk--dict-lookup-okuri-ari "root")))
      (should
       (equal (mapcar #'substring-no-properties result)
              '("same" "k-one" "k-two" "g-one")))
      (should (eq (car result) first))
      (should (eq (get-text-property 0 'source (car result)) 'okuri-k))))

  (nskk-it "keeps okuri-nasi candidates ahead of okuri-ari candidates"
    (let* ((first (propertize "same" 'source 'okuri-nasi))
           (duplicate (propertize "same" 'source 'okuri-ari))
           result)
      (cl-letf (((symbol-function 'nskk-prolog-query)
                 (lambda (&rest _arguments) nil))
                ((symbol-function 'nskk--dict-collect-candidates)
                 (lambda (_solutions) (list first "plain")))
                ((symbol-function 'nskk--dict-lookup-okuri-ari)
                 (lambda (_key) (list duplicate "okuri"))))
        (setq result (nskk--dict-do-lookup "よみ")))
      (should
       (equal (mapcar #'substring-no-properties result)
              '("same" "plain" "okuri")))
      (should (eq (car result) first))
      (should
       (eq (get-text-property 0 'source (car result)) 'okuri-nasi)))))

(nskk-describe "linear ja-dic flattening"
  (nskk-it "visits 5000 wide child leaves once in source order"
    (let* ((children
            (cl-loop for index below 5000
                     collect
                     (list index (list (format "candidate-%d" index)))))
           (root (append (list -1 nil) children))
           (tree (list 'test root))
           (decodes 0)
           result)
      (cl-letf (((symbol-function 'nskk--dict-ja-dic-decode-key)
                 (lambda (path)
                   (setq decodes (1+ decodes))
                   (number-to-string (car (last path))))))
        (setq result (nskk--dict-ja-dic-flatten-tree tree)))
      (should (= decodes 5000))
      (should (= (length result) 5000))
      (should (equal (caar result) "0"))
      (should (equal (car (car (last result))) "4999"))
      (should (eq (cdar result) (cadr (car children)))))))
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

;;;; nskk-dict-load-kakutei-dictionary tests

(defmacro nskk-test-with-kakutei-file (entries &rest body)
    "Execute BODY with a temp SKK file containing ENTRIES loaded as kakutei dict.
ENTRIES is a list of (reading . (candidate1 candidate2 ...)) cons cells.
The file is written in SKK-JISYO format, loaded, and cleaned up after BODY."
    (declare (indent 1))
    `(nskk-prolog-test-with-isolated-db
       (let* ((tmpfile (make-temp-file "nskk-test-kakutei-" nil ".jisyo"))
              (nskk-kakutei-jisyo tmpfile)
              (nskk--kakutei-dict-loaded nil))
         (nskk-prolog-with-database-fields
             ((index-bucket-tail-cache (make-hash-table :test #'equal)))
           (unwind-protect
               (progn
                 (with-temp-file tmpfile
                   (insert ";; -*- coding: utf-8 -*-\n")
                   (insert ";; okuri-nasi entries.\n")
                   (dolist (e ,entries)
                     (insert (car e) " /"
                             (string-join (cdr e) "/")
                             "/\n")))
                 ,@body)
             (when (file-exists-p tmpfile)
               (delete-file tmpfile))
             (nskk-prolog-retract-all 'kakutei-dict-entry 2)
             (setq nskk--kakutei-dict-loaded nil))))))

  (defun nskk-test--seed-kakutei-state ()
    "Seed and return the exact live state guarded by kakutei loading."
    (let* ((key (nskk-prolog-clause-key 'kakutei-dict-entry 2))
           (cache-value (list 'old-kakutei-tail-cache)))
      (nskk-prolog-set-index 'kakutei-dict-entry 2 :trie)
      (nskk-prolog-assert
       '((kakutei-dict-entry "ふるい" ("古い"))))
      (nskk-prolog-assert
       '((kakutei-dict-entry "むかし" ("昔"))))
      (puthash key cache-value (nskk-prolog-index-bucket-tail-cache))
      (vector key
              (nskk-dict-transaction-predicate-snapshot key)
              cache-value
              nskk--kakutei-dict-loaded)))

  (defun nskk-test--should-preserve-kakutei-state (state)
    "Assert that the live kakutei state remains reference-identical to STATE."
    (let* ((key (aref state 0))
           (before (aref state 1))
           (after (nskk-dict-transaction-predicate-snapshot key)))
      (dotimes (slot (1- (length before)))
        (should (eq (aref before (1+ slot))
                    (aref after (1+ slot)))))
      (should (eq (aref state 2)
                  (gethash key (nskk-prolog-index-bucket-tail-cache))))
      (should (eq (aref state 3) nskk--kakutei-dict-loaded))
      (should (nskk-prolog-holds-p
               '(kakutei-dict-entry "ふるい" ("古い"))))
      (should (nskk-prolog-holds-p
               '(kakutei-dict-entry "むかし" ("昔"))))
      (should-not (nskk-prolog-holds-p
                   '(kakutei-dict-entry "あたらしい" ("新しい"))))))

(nskk-describe "nskk-dict-load-kakutei-dictionary"
  (nskk-it "preserves exact state when nskk-kakutei-jisyo is nil"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk-kakutei-jisyo nil)
            (nskk--kakutei-dict-loaded 'previous))
        (nskk-prolog-with-database-fields
            ((index-bucket-tail-cache (make-hash-table :test #'equal)))
          (let ((state (nskk-test--seed-kakutei-state)))
            (should (null (nskk-dict-load-kakutei-dictionary)))
            (nskk-test--should-preserve-kakutei-state state))))))

  (nskk-it "preserves exact state when file does not exist"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk-kakutei-jisyo "/nonexistent/path/kakutei.jisyo")
            (nskk--kakutei-dict-loaded 'previous))
        (nskk-prolog-with-database-fields
            ((index-bucket-tail-cache (make-hash-table :test #'equal)))
          (let ((state (nskk-test--seed-kakutei-state)))
            (should (null (nskk-dict-load-kakutei-dictionary)))
            (nskk-test--should-preserve-kakutei-state state))))))

  (nskk-it "returns 'kakutei when file exists and has entries"
    (nskk-test-with-kakutei-file '(("てすと" "テスト"))
      (should (eq (nskk-dict-load-kakutei-dictionary) 'kakutei))))

  (nskk-it "sets nskk--kakutei-dict-loaded to t after loading"
    (nskk-test-with-kakutei-file '(("てすと" "テスト"))
      (nskk-dict-load-kakutei-dictionary)
      (should nskk--kakutei-dict-loaded)))

  (nskk-it "makes entries queryable after load"
    (nskk-test-with-kakutei-file '(("てすと" "テスト"))
      (nskk-dict-load-kakutei-dictionary)
      (let ((result (nskk-prolog-query-value
                     '(kakutei-dict-entry "てすと" \?c) '\?c)))
        (should result)
        (should (member "テスト" result)))))

  (nskk-it "preserves exact state for an empty dictionary"
    (nskk-test-with-kakutei-file nil
      (setq nskk--kakutei-dict-loaded 'previous)
      (let ((state (nskk-test--seed-kakutei-state)))
        (should (null (nskk-dict-load-kakutei-dictionary)))
        (nskk-test--should-preserve-kakutei-state state))))

  (nskk-it "preserves exact state after parse errors and quits"
    (dolist (kind '(error quit))
      (nskk-test-with-kakutei-file '(("あたらしい" "新しい"))
        (setq nskk--kakutei-dict-loaded 'previous)
        (let ((state (nskk-test--seed-kakutei-state))
              (data (list "kakutei parse failure" kind)))
          (cl-letf
              (((symbol-function 'nskk--dict-parse-file-to-entries-strict)
                (lambda (&rest _)
                  (signal kind data))))
            (let ((result
                   (condition-case condition
                       (nskk-dict-load-kakutei-dictionary)
                     (quit condition))))
              (if (eq kind 'quit)
                  (should (equal result (cons kind data)))
                (should (null result)))))
          (nskk-test--should-preserve-kakutei-state state)))))

  (nskk-it "preserves exact state after index setup errors and quits"
    (dolist (kind '(error quit))
      (nskk-test-with-kakutei-file '(("あたらしい" "新しい"))
        (setq nskk--kakutei-dict-loaded 'previous)
        (let ((state (nskk-test--seed-kakutei-state))
              (real-set-index
               (symbol-function 'nskk-prolog-set-index))
              (data (list "kakutei index failure" kind)))
          (cl-letf
              (((symbol-function 'nskk-prolog-set-index)
                (lambda (&rest args)
                  (apply real-set-index args)
                  (signal kind data))))
            (let ((result
                   (condition-case condition
                       (nskk-dict-load-kakutei-dictionary)
                     (quit condition))))
              (if (eq kind 'quit)
                  (should (equal result (cons kind data)))
                (should (null result)))))
          (nskk-test--should-preserve-kakutei-state state)))))

  (nskk-it "preserves exact state after assertion errors and quits"
    (dolist (kind '(error quit))
      (nskk-test-with-kakutei-file '(("あたらしい" "新しい"))
        (setq nskk--kakutei-dict-loaded 'previous)
        (let ((state (nskk-test--seed-kakutei-state))
              (real-assert
               (symbol-function 'nskk-prolog-assert))
              (calls 0)
              (data (list "kakutei assertion failure" kind)))
          (cl-letf
              (((symbol-function 'nskk-prolog-assert)
                (lambda (&rest args)
                  (setq calls (1+ calls))
                  (apply real-assert args)
                  (when (= calls 1)
                    (signal kind data)))))
            (let ((result
                   (condition-case condition
                       (nskk-dict-load-kakutei-dictionary)
                     (quit condition))))
              (if (eq kind 'quit)
                  (should (equal result (cons kind data)))
                (should (null result)))))
          (should (= calls 1))
          (nskk-test--should-preserve-kakutei-state state)))))

  (nskk-it "preserves exact state after publication errors and quits"
    (dolist (kind '(error quit))
      (nskk-test-with-kakutei-file '(("あたらしい" "新しい"))
        (setq nskk--kakutei-dict-loaded 'previous)
        (let ((state (nskk-test--seed-kakutei-state))
              (real-publish
               (symbol-function 'nskk--dict-publish-staged-predicate))
              (data (list "kakutei publication failure" kind)))
          (cl-letf
              (((symbol-function 'nskk--dict-publish-staged-predicate)
                (lambda (staged)
                  (funcall real-publish staged)
                  (signal kind data))))
            (let ((result
                   (condition-case condition
                       (nskk-dict-load-kakutei-dictionary)
                     (quit condition))))
              (if (eq kind 'quit)
                  (should (equal result (cons kind data)))
                (should (null result)))))
          (nskk-test--should-preserve-kakutei-state state)))))

  (nskk-it "replaces the complete live state only after successful staging"
    (nskk-test-with-kakutei-file
        '(("あたらしい" "新しい")
          ("つぎ" "次"))
      (setq nskk--kakutei-dict-loaded 'previous)
      (let* ((state (nskk-test--seed-kakutei-state))
             (key (aref state 0))
             (before (aref state 1)))
        (should (eq (nskk-dict-load-kakutei-dictionary) 'kakutei))
        (should nskk--kakutei-dict-loaded)
        (should-not
         (nskk-prolog-holds-p
          '(kakutei-dict-entry "ふるい" ("古い"))))
        (should-not
         (nskk-prolog-holds-p
          '(kakutei-dict-entry "むかし" ("昔"))))
        (should
         (nskk-prolog-holds-p
          '(kakutei-dict-entry "あたらしい" ("新しい"))))
        (should
         (nskk-prolog-holds-p
          '(kakutei-dict-entry "つぎ" ("次"))))
        (let* ((after (nskk-dict-transaction-predicate-snapshot key))
               (new-index (aref after 6))
               (new-cache (aref after 7)))
          (should (vectorp new-cache))
          (should (= (length new-cache) 3))
          (should (eq (aref new-cache 0) :trie))
          (should (eq (aref new-cache 1) new-index))
          (dolist (slot '(2 3 6 7))
            (should-not
             (eq (aref before slot)
                 (aref after slot))))
          (cl-letf (((symbol-function 'last)
                     (lambda (&rest _)
                       (error "warm staged append scanned a bucket"))))
            (nskk--dict-append-predicate-entries
             'kakutei-dict-entry
             '(("あたらしい" "追加"))))
          (should
           (nskk-prolog-holds-p
            '(kakutei-dict-entry "あたらしい" ("追加")))))))))

;;;; nskk-dict-lookup-kakutei/k tests

(nskk-describe "nskk-dict-lookup-kakutei/k"
  (nskk-it "calls on-not-found when nskk--kakutei-dict-loaded is nil"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk--kakutei-dict-loaded nil)
            (not-found nil))
        (nskk-dict-lookup-kakutei/k "てすと"
                                  (lambda (_) nil)
                                  (lambda () (setq not-found t)))
        (should not-found))))

  (nskk-it "calls on-not-found for non-string reading"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk--kakutei-dict-loaded t)
            (not-found nil))
        (nskk-dict-lookup-kakutei/k nil
                                  (lambda (_) nil)
                                  (lambda () (setq not-found t)))
        (should not-found))))

  (nskk-it "calls on-found with single candidate when unique match"
    (nskk-test-with-kakutei-file '(("てすと" "テスト"))
      (nskk-dict-load-kakutei-dictionary)
      (let ((found nil))
        (nskk-dict-lookup-kakutei/k "てすと"
                                  (lambda (c) (setq found c))
                                  #'ignore)
        (should (equal found "テスト")))))

  (nskk-it "calls on-not-found when entry has multiple candidates"
    (nskk-test-with-kakutei-file '(("かんじ" "漢字" "感じ"))
      (nskk-dict-load-kakutei-dictionary)
      (let ((not-found nil))
        (nskk-dict-lookup-kakutei/k "かんじ"
                                  (lambda (_) nil)
                                  (lambda () (setq not-found t)))
        (should not-found))))

  (nskk-it "calls on-not-found for unknown reading"
    (nskk-test-with-kakutei-file '(("てすと" "テスト"))
      (nskk-dict-load-kakutei-dictionary)
      (let ((not-found nil))
        (nskk-dict-lookup-kakutei/k "ぜんぜんない"
                                  (lambda (_) nil)
                                  (lambda () (setq not-found t)))
        (should not-found)))))

;;;
;;; Dict save sanitization tests
;;;

(nskk-describe "nskk-dict-save-user-dictionary sanitization"
  (nskk-it "rejects every prohibited internal key and candidate value atomically"
    (let ((control-cases
           (mapcar
            (lambda (code)
              (cons (format "U+%04X" code)
                    (concat "不" (string code) "正")))
            (number-sequence 0 31))))
      (dolist (role '(key candidate))
        (dolist
            (case
             (append
              control-cases
              (list
               (cons "U+007F" (concat "不" (string 127) "正"))
               (cons "slash" "不/正")
               (cons "semicolon" "不;正")
               (cons "hiragana preedit marker" "不▽正")
               (cons "katakana preedit marker" "不▼正")
               (cons "empty" "")
               (cons "non-string" 42))
              (when (eq role 'key)
                (list (cons "ordinary space" "不 正")))))
          (ert-info ((format "%s in %s" (car case) role))
            (nskk-prolog-test-with-isolated-db
              (let* ((invalid (cdr case))
                     (key (if (eq role 'key) invalid "よみ"))
                     (candidates
                      (if (eq role 'candidate)
                          (list "正常" invalid "正常2")
                        '("候補")))
                     (bindings (list (list key candidates)))
                     (dictionary-file
                      (make-temp-file "nskk-save-field-boundary-" nil ".skk"))
                     (nskk-dict-user-dictionary-file dictionary-file)
                     (nskk--user-dict-index 'user)
                     (nskk-dict-modified 'preserved)
                     (clause-key
                      (nskk-prolog-clause-key 'user-dict-entry 2))
                     before
                     before-file)
                (unwind-protect
                    (progn
                      (nskk-prolog-set-index 'user-dict-entry 2 :trie)
                      (nskk-prolog-assert
                       '((user-dict-entry "既存" ("既存候補"))))
                      (with-temp-file dictionary-file
                        (insert "unchanged" (string 0 127) "\n"))
                      (setq before
                            (nskk-dict-transaction-predicate-snapshot clause-key)
                            before-file
                            (with-temp-buffer
                              (insert-file-contents-literally dictionary-file)
                              (buffer-string)))
                      (let ((condition
                             (cl-letf
                                 (((symbol-function
                                    'nskk-prolog-query-bindings)
                                   (lambda (&rest _arguments) bindings)))
                               (condition-case error
                                   (progn
                                     (nskk-dict-save-user-dictionary)
                                     nil)
                                 (nskk-dict-error error)))))
                        (should
                         (equal condition
                                '(nskk-dict-error
                                  "Invalid user dictionary entry"))))
                      (let ((after
                             (nskk-dict-transaction-predicate-snapshot clause-key)))
                        (dotimes (slot (1- (length before)))
                          (should
                           (eq (aref before (1+ slot))
                               (aref after (1+ slot))))))
                      (should
                       (nskk-prolog-holds-p
                        '(user-dict-entry "既存" ("既存候補"))))
                      (should (equal bindings (list (list key candidates))))
                      (should (eq nskk--user-dict-index 'user))
                      (should (eq nskk-dict-modified 'preserved))
                      (should
                       (equal before-file
                              (with-temp-buffer
                                (insert-file-contents-literally dictionary-file)
                                (buffer-string)))))
                  (when (file-exists-p dictionary-file)
                    (delete-file dictionary-file))))))))))

  (nskk-it "rejects malformed candidate collections before replacing output"
    (dolist (candidates (list nil 42 '("正常" . "tail")))
      (ert-info ((format "candidate collection %S" candidates))
        (nskk-prolog-test-with-isolated-db
          (let* ((dictionary-file
                  (make-temp-file "nskk-save-collection-boundary-" nil ".skk"))
                 (nskk-dict-user-dictionary-file dictionary-file)
                 (nskk--user-dict-index 'user)
                 (nskk-dict-modified 'preserved)
                 (clause-key
                  (nskk-prolog-clause-key 'user-dict-entry 2))
                 before
                 before-file)
            (unwind-protect
                (progn
                  (nskk-prolog-set-index 'user-dict-entry 2 :trie)
                  (nskk-prolog-assert
                   (list
                    (list 'user-dict-entry "よみ" candidates)))
                  (with-temp-file dictionary-file
                    (insert "unchanged" (string 0 127) "\n"))
                  (setq before
                        (nskk-dict-transaction-predicate-snapshot clause-key)
                        before-file
                        (with-temp-buffer
                          (insert-file-contents-literally dictionary-file)
                          (buffer-string)))
                  (let ((condition
                         (condition-case error
                             (progn
                               (nskk-dict-save-user-dictionary)
                               nil)
                           (nskk-dict-error error))))
                    (should
                     (equal condition
                            '(nskk-dict-error
                              "Invalid user dictionary entry"))))
                  (let ((after
                         (nskk-dict-transaction-predicate-snapshot clause-key)))
                    (dotimes (slot (1- (length before)))
                      (should
                       (eq (aref before (1+ slot))
                           (aref after (1+ slot))))))
                  (should (eq nskk--user-dict-index 'user))
                  (should (eq nskk-dict-modified 'preserved))
                  (should
                   (equal before-file
                          (with-temp-buffer
                            (insert-file-contents-literally dictionary-file)
                            (buffer-string)))))
              (when (file-exists-p dictionary-file)
                (delete-file dictionary-file))))))))

  (nskk-it "does not create a parent directory, temp file, or output on invalid input"
    (nskk-prolog-test-with-isolated-db
      (let* ((root (make-temp-file "nskk-save-preflight-" t))
             (parent (expand-file-name "nested/deeper" root))
             (dictionary-file (expand-file-name "user.skk" parent))
             (nskk-dict-user-dictionary-file dictionary-file)
             (nskk--user-dict-index 'user)
             (nskk-dict-modified 'preserved)
             (clause-key
              (nskk-prolog-clause-key 'user-dict-entry 2))
             before)
        (unwind-protect
            (progn
              (nskk-prolog-set-index 'user-dict-entry 2 :trie)
              (nskk-prolog-assert
               '((user-dict-entry "既存" ("既存候補"))))
              (nskk-prolog-assert
               '((user-dict-entry "bad/reading" ("候補"))))
              (setq before
                    (nskk-dict-transaction-predicate-snapshot clause-key))
              (let ((condition
                     (condition-case error
                         (progn
                           (nskk-dict-save-user-dictionary)
                           nil)
                       (nskk-dict-error error))))
                (should
                 (equal condition
                        '(nskk-dict-error
                          "Invalid user dictionary entry"))))
              (let ((after
                     (nskk-dict-transaction-predicate-snapshot clause-key)))
                (dotimes (slot (1- (length before)))
                  (should
                   (eq (aref before (1+ slot))
                       (aref after (1+ slot))))))
              (should-not (file-directory-p parent))
              (should-not (file-exists-p dictionary-file))
              (should-not
               (directory-files root nil directory-files-no-dot-files-regexp))
              (should (eq nskk--user-dict-index 'user))
              (should (eq nskk-dict-modified 'preserved)))
          (when (file-directory-p root)
            (delete-directory root t)))))))

(nskk-describe "dictionary transactional loading and cache hardening"
  (nskk-it "replaces user facts only after each complete valid load"
    (nskk-prolog-test-with-isolated-db
      (let ((temp-file (make-temp-file "nskk-user-reload-" nil ".skk")))
	(unwind-protect
	    (let ((nskk-dict-user-dictionary-file temp-file))
	      (with-temp-file temp-file
		(insert "ふるい /古い/" (string 10)))
	      (should (eq (nskk-dict-load-user-dictionary) (quote user)))
	      (should (nskk-prolog-holds-p
		       (quote (user-dict-entry "ふるい" ("古い")))))
	      (with-temp-file temp-file
		(insert "あたらしい /新しい/" (string 10)))
	      (should (eq (nskk-dict-load-user-dictionary) (quote user)))
	      (should-not (nskk-prolog-holds-p
			   (quote (user-dict-entry "ふるい" ("古い")))))
	      (should (nskk-prolog-holds-p
		       (quote (user-dict-entry "あたらしい" ("新しい"))))))
	  (delete-file temp-file)))))

  (nskk-it "preserves user facts for empty or malformed input"
    (nskk-prolog-test-with-isolated-db
      (let ((temp-file (make-temp-file "nskk-user-invalid-" nil ".skk")))
	(unwind-protect
	    (progn
	      (nskk-prolog-set-index (quote user-dict-entry) 2 :trie)
	      (nskk-prolog-assert
	       (quote ((user-dict-entry "きぞん" ("既存")))))
	      (let ((nskk-dict-user-dictionary-file temp-file))
		(dolist (content
			 (list "" "ただしくない行"
			       (concat "せいじょう /正常/" (string 10) "不正")))
		  (with-temp-file temp-file (insert content))
		  (should-not (nskk-dict-load-user-dictionary))
		  (should (nskk-prolog-holds-p
			   (quote (user-dict-entry "きぞん" ("既存"))))))))
	  (delete-file temp-file)))))

  (nskk-it "rejects malformed dotted circular and multiple cache forms"
    (let* ((temp-file (make-temp-file "nskk-cache-invalid-" nil ".eld"))
	   (nskk-dict-system-dictionary-files (quote ("/dict"))))
      (unwind-protect
	  (nskk-with-mocks ((nskk--dict-cache-file-path
			     (lambda () temp-file)))
	    (dolist (content
		     (quote ("("
			     "(a . b)"
			     "#1=(#1#)"
			     "nil nil"
			     "(:version 1 :source-files (/dict) :entries ((a 1)))")))
	      (with-temp-file temp-file (insert content))
	      (should-not (nskk--dict-load-system-dict-from-cache))))
	(delete-file temp-file))))

  (nskk-it "rejects oversized cache without changing system facts"
    (nskk-prolog-test-with-isolated-db
      (let ((temp-file (make-temp-file "nskk-cache-large-" nil ".eld")))
	(unwind-protect
	    (progn
	      (with-temp-file temp-file (insert "0123456789"))
	      (nskk-prolog-set-index (quote system-dict-entry) 2 :trie)
	      (nskk-prolog-assert
	       (quote ((system-dict-entry "きぞん" ("既存")))))
	      (let ((nskk--dict-cache-max-bytes 8)
		    (nskk-dict-system-dictionary-files (quote ("/dict"))))
		(nskk-with-mocks ((nskk--dict-cache-file-path
				   (lambda () temp-file)))
		  (should (= (nskk--dict-load-from-cache) 0))
		  (should (nskk-prolog-holds-p
			   (quote (system-dict-entry "きぞん" ("既存"))))))))
	  (delete-file temp-file)))))

  (nskk-it "refuses to replace a symbolic-link output path"
    (let* ((temp-dir (make-temp-file "nskk-atomic-link-" t))
           (target (expand-file-name "target" temp-dir))
           (output (expand-file-name "output" temp-dir)))
      (unwind-protect
          (progn
            (with-temp-file target (insert "old"))
            (make-symbolic-link target output)
            (should-error
             (nskk-dict-with-atomic-file output (insert "new"))
             :type (quote file-error))
            (with-temp-buffer
              (insert-file-contents target)
              (should (equal (buffer-string) "old"))))
        (delete-directory temp-dir t))))

  (nskk-it "refuses to replace a directory output path before writing"
    (let ((output (make-temp-file "nskk-atomic-directory-" t))
          (body-called nil))
      (unwind-protect
          (progn
            (should-error
             (nskk-dict-with-atomic-file output
               (setq body-called t)
               (insert "new"))
             :type (quote file-error))
            (should-not body-called)
            (should (file-directory-p output)))
        (delete-directory output t))))

  (nskk-it "refuses to replace a FIFO output path before writing"
    (let ((output
           (make-temp-name
            (expand-file-name "nskk-atomic-fifo-"
                              temporary-file-directory)))
          (body-called nil))
      (skip-unless (executable-find "mkfifo"))
      (unwind-protect
          (progn
            (unless
                (= 0 (call-process "mkfifo" nil nil nil output))
              (ert-skip "mkfifo cannot create a FIFO"))
            (should-error
             (nskk-dict-with-atomic-file output
               (setq body-called t)
               (insert "new"))
             :type (quote file-error))
            (should-not body-called)
            (should (file-exists-p output))
            (should-not (file-regular-p output)))
        (when (file-exists-p output)
          (delete-file output))))))

  (nskk-describe "bounded transactional dictionary reads"
    (nskk-it "rejects post-read byte overflow without changing ordinary facts"
      (nskk-prolog-test-with-isolated-db
        (let ((file (make-temp-file "nskk-bounded-" nil ".skk")))
          (unwind-protect
              (progn
                (with-temp-file file (insert "x"))
                (nskk-prolog-set-index (quote bounded-dict-entry) 2 :trie)
                (nskk-prolog-assert
                 (quote ((bounded-dict-entry "existing" ("value")))))
                (let ((nskk--dict-cache-max-bytes 8))
                  (nskk-with-mocks
                      ((insert-file-contents
                        (lambda (&rest _args) (insert "123456789"))))
                    (should-not
                     (nskk-dict-load-file file nil
                                          (quote bounded-dict-entry)))))
                (should
                 (nskk-prolog-holds-p
                  (quote (bounded-dict-entry "existing" ("value"))))))
            (delete-file file)))))

    (nskk-it "preserves user facts when reading fails after partial insertion"
      (nskk-prolog-test-with-isolated-db
        (let ((file (make-temp-file "nskk-partial-" nil ".skk")))
          (unwind-protect
              (progn
                (nskk-prolog-set-index (quote user-dict-entry) 2 :trie)
                (nskk-prolog-assert
                 (quote ((user-dict-entry "existing" ("value")))))
                (let ((nskk-dict-user-dictionary-file file))
                  (nskk-with-mocks
                      ((insert-file-contents
                        (lambda (&rest _args)
                          (insert "partial")
                          (error "simulated read failure"))))
                    (should-not (nskk-dict-load-user-dictionary))))
                (should
                 (nskk-prolog-holds-p
                  (quote (user-dict-entry "existing" ("value"))))))
            (delete-file file)))))

    (nskk-it "preserves system facts and cache when a source exceeds the limit"
      (nskk-prolog-test-with-isolated-db
        (let ((source (make-temp-file "nskk-system-large-" nil ".skk"))
              (cache (make-temp-file "nskk-system-cache-" nil ".eld")))
          (unwind-protect
              (progn
                (with-temp-file source (insert "123456789"))
                (with-temp-file cache (insert "unchanged"))
                (nskk-prolog-set-index (quote system-dict-entry) 2 :trie)
                (nskk-prolog-assert
                 (quote ((system-dict-entry "existing" ("value")))))
                (let ((nskk--dict-cache-max-bytes 8)
                      (nskk-dict-cache-enabled t))
                  (nskk-with-mocks
                      ((nskk--dict-cache-file-path (lambda () cache)))
                    (should (= 0 (nskk--dict-load-from-files
                                  (list source))))))
                (should
                 (nskk-prolog-holds-p
                  (quote (system-dict-entry "existing" ("value")))))
                (with-temp-buffer
                  (insert-file-contents cache)
                  (should (equal (buffer-string) "unchanged"))))
            (delete-file source)
            (delete-file cache))))))

  (nskk-describe "bounded binary cache read boundaries"
    (nskk-it "decodes multibyte data whose encoded bytes exactly equal the limit"
      (let ((file (make-temp-file "nskk-bounded-multibyte-" nil ".skk")))
        (unwind-protect
            (progn
              (let ((coding-system-for-write (quote utf-8-unix)))
                (with-temp-file file
                  (insert "あ")))
              (let ((nskk--dict-cache-max-bytes 3))
                (with-temp-buffer
                  (nskk--dict-insert-file-contents-bounded
                   file (quote utf-8-unix))
                  (should (equal (buffer-string) "あ")))))
          (delete-file file))))

    (nskk-it "reads limit plus one byte and rejects growth after stat"
      (let ((file (make-temp-file "nskk-bounded-growth-" nil ".skk"))
            (seen-end nil))
        (unwind-protect
            (progn
              (with-temp-file file
                (insert "1234"))
              (let ((nskk--dict-cache-max-bytes 4))
                (nskk-with-mocks
                    ((insert-file-contents
                      (lambda (_file &optional _visit _begin end &rest _args)
                        (setq seen-end end)
                        (insert "12345"))))
                  (with-temp-buffer
                    (should-error
                     (nskk--dict-insert-file-contents-bounded file nil)
                     :type (quote error)))))
              (should (= seen-end 5)))
          (delete-file file))))

    (nskk-it "rejects trailing non-whitespace after an otherwise valid cache form"
      (let ((file (make-temp-file "nskk-cache-trailing-" nil ".eld"))
            (nskk-dict-system-dictionary-files (quote ("/dict"))))
        (unwind-protect
            (progn
              (with-temp-file file
                (prin1
                 (list :version 1
                       :source-files (quote ("/dict"))
                       :entries nil)
                 (current-buffer))
                (insert " trailing-garbage"))
              (nskk-with-mocks
                  ((nskk--dict-cache-file-path (lambda () file)))
                (should-not (nskk--dict-load-system-dict-from-cache))))
          (delete-file file)))))
  (nskk-describe "dictionary initialization hooks"
  (nskk-it "runs the initialization hook after loading completes"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk-dict-system-dictionary-files nil)
            (nskk-dict-use-ja-dic nil)
            (nskk-dict-user-dictionary-file nil)
            (nskk--system-dict-index nil)
            (nskk--user-dict-index nil)
            (nskk-dict-initialize-hook nil)
            (hook-calls 0))
        (add-hook 'nskk-dict-initialize-hook
                  (lambda () (cl-incf hook-calls)))
        (nskk-with-mocks
            ((nskk--dict-detect-system-dictionaries (lambda () nil))
             (nskk-dict-load-user-dictionary (lambda () nil))
             (nskk-dict-load-kakutei-dictionary (lambda () nil)))
          (nskk-dict-initialize)
          (should (= 1 hook-calls))))))

  (nskk-it "continues initialization observers after an ordinary hook error"
    (nskk-prolog-test-with-isolated-db
      (let* ((events nil)
             (nskk-dict-system-dictionary-files nil)
             (nskk-dict-use-ja-dic nil)
             (nskk-dict-user-dictionary-file nil)
             (nskk--system-dict-index nil)
             (nskk--user-dict-index nil)
             (nskk-dict-initialize-hook (list #'nskk--search-flush-caches))
             (messages nil)
             (flushes 0))
        (with-temp-buffer
          (setq-local nskk-dict-initialize-hook
                      (list
                       (lambda ()
                         (push 'local-failure events)
                         (error "initialize hook failure"))
                       (lambda () (push 'local-observer events))
                       t))
          (nskk-with-mocks
              ((nskk--dict-detect-system-dictionaries (lambda () nil))
               (nskk-dict-load-user-dictionary (lambda () nil))
               (nskk-dict-load-kakutei-dictionary (lambda () nil))
               (nskk--search-flush-caches
                (lambda ()
                  (push 'global-cache-invalidation events)
                  (cl-incf flushes)))
               (message
                (lambda (fmt &rest args)
                  (push (apply #'format fmt args) messages))))
            (nskk-dict-initialize)))
        (should (nskk-prolog-holds-p '(dict-initialized)))
        (should
         (equal (nreverse events)
                '(local-failure local-observer global-cache-invalidation)))
        (should (= 1 flushes))
        (should
         (cl-some
          (lambda (text)
            (string-match-p "dict-initialize-hook error" text))
          messages)))))

  (nskk-it "propagates initialization hook quit before later observers"
    (nskk-prolog-test-with-isolated-db
      (let* ((events nil)
             (nskk-dict-system-dictionary-files nil)
             (nskk-dict-use-ja-dic nil)
             (nskk-dict-user-dictionary-file nil)
             (nskk--system-dict-index nil)
             (nskk--user-dict-index nil)
             (nskk-dict-initialize-hook
              (list (lambda () (push 'global-observer events))))
             caught)
        (with-temp-buffer
          (setq-local nskk-dict-initialize-hook
                      (list
                       (lambda ()
                         (push 'quit events)
                         (signal 'quit '("initialize hook quit" payload)))
                       (lambda () (push 'later-local-observer events))
                       t))
          (nskk-with-mocks
              ((nskk--dict-detect-system-dictionaries (lambda () nil))
               (nskk-dict-load-user-dictionary (lambda () nil))
               (nskk-dict-load-kakutei-dictionary (lambda () nil)))
            (setq caught
                  (condition-case signal-condition
                      (nskk-dict-initialize)
                    (quit signal-condition)))))
        (should (equal caught '(quit "initialize hook quit" payload)))
        (should (equal (nreverse events) '(quit)))
        (should (nskk-prolog-holds-p '(dict-initialized)))))))

  (nskk-describe "explicit okuri marker lookup"
    (nskk-it "does not append another consonant to an explicit marker"
      (nskk-with-prolog-entries
          ((user-dict-entry "うごk" ("direct")))
        (nskk-with-mocks
            ((nskk--dict-lookup-okuri-ari
              (lambda (&rest _)
                (error "Explicit marker triggered okuri lookup"))))
          (should (member "direct" (nskk-dict-lookup "うごk")))))))

(nskk-describe "bounded dictionary regular-file boundary"
	       (nskk-it "rejects a FIFO before attempting any read"
			(let ((fifo (make-temp-name
				     (expand-file-name
				      "nskk-dictionary-fifo-"
				      temporary-file-directory)))
			      (read-called nil))
			  (skip-unless (executable-find "mkfifo"))
			  (unwind-protect
			      (progn
				(should
				 (= 0 (call-process "mkfifo" nil nil nil fifo)))
				(cl-letf
				    (((symbol-function 'insert-file-contents)
				      (lambda (&rest _)
					(setq read-called t)
					(error "FIFO read attempted"))))
				  (with-temp-buffer
				    (should-error
				     (nskk--dict-insert-file-contents-bounded fifo nil)
				     :type 'error)))
				(should-not read-called))
			    (when (file-exists-p fifo)
			      (delete-file fifo)))))

	       (nskk-it "continues to read a symlink to a regular file"
			(let* ((directory
				(make-temp-file "nskk-dictionary-link-" t))
			       (target (expand-file-name "target.skk" directory))
			       (link (expand-file-name "link.skk" directory)))
			  (unwind-protect
			      (progn
				(with-temp-file target
				  (insert "regular"))
				(make-symbolic-link target link)
				(with-temp-buffer
				  (nskk--dict-insert-file-contents-bounded link 'utf-8-unix)
				  (should (equal (buffer-string) "regular"))))
			    (delete-directory directory t))))

	       (nskk-it "reads an owned regular file from a read-only parent directory"
			(let* ((directory
				(make-temp-file "nskk-dictionary-read-only-parent-" t))
			       (file (expand-file-name "dictionary.skk" directory)))
			  (unwind-protect
			      (progn
				(with-temp-file file
				  (insert "regular"))
				(set-file-modes directory #o555)
				(with-temp-buffer
				  (nskk--dict-insert-file-contents-bounded file nil)
				  (should (equal (buffer-string) "regular"))))
			    (set-file-modes directory #o700)
			    (delete-directory directory t))))

	       (nskk-it "fails closed without a source read when hard-link policy denies pinning"
			  (let ((file (make-temp-file "nskk-dict-pin-policy-"))
				(buffer (generate-new-buffer " *nskk-dict-pin-policy*"))
				(source-read-called nil))
			    (unwind-protect
				(progn
				  (with-temp-file file
				    (insert "original\n"))
				  (with-current-buffer buffer
				    (insert "sentinel")
				    (cl-letf (((symbol-function 'add-name-to-file)
					       (lambda (&rest _args)
						 (signal 'file-error
							 '("Operation not permitted"
							   "protected_hardlinks policy"))))
					      ((symbol-function 'insert-file-contents)
					       (lambda (&rest _args)
						 (setq source-read-called t)
						 (error "source path must not be read"))))
				      (let ((condition
					     (should-error
					      (nskk--dict-insert-file-contents-bounded file nil)
					      :type 'error)))
					(should
					 (string-match-p
					  "Cannot safely read unpinned file"
					  (error-message-string condition)))))
				    (should-not source-read-called)
				    (should (equal (buffer-string) "sentinel"))))
			      (when (buffer-live-p buffer)
				(kill-buffer buffer))
			      (ignore-errors (delete-file file)))))

		 (nskk-it "rejects a symbolic-link snapshot before any read"
			  (let ((file (make-temp-file "nskk-dict-symlink-snapshot-"))
				(buffer (generate-new-buffer " *nskk-dict-symlink-snapshot*"))
				(read-called nil))
			    (unwind-protect
				(progn
				  (with-temp-file file
				    (insert "original\n"))
				  (with-current-buffer buffer
				    (insert "sentinel")
				    (cl-letf (((symbol-function 'add-name-to-file)
					       (lambda (_old new &optional _ok-if-already-exists)
						 (make-symbolic-link file new)))
					      ((symbol-function 'insert-file-contents)
					       (lambda (&rest _args)
						 (setq read-called t)
						 (error "symbolic snapshot read attempted"))))
				      (let ((condition
					     (should-error
					      (nskk--dict-insert-file-contents-bounded file nil)
					      :type 'error)))
					(should
					 (string-match-p
					  "Pinned snapshot is a symbolic link"
					  (error-message-string condition)))))
				    (should-not read-called)
				    (should (equal (buffer-string) "sentinel"))))
			      (when (buffer-live-p buffer)
				(kill-buffer buffer))
			      (ignore-errors (delete-file file)))))
(nskk-it "reads through a normal hard-link pin in a root-owned sticky base"
  (let* ((directory (make-temp-file "nskk-dict-normal-pin-" t))
         (snapshot-base (make-temp-file "nskk-dict-root-sticky-" t))
         (snapshot-base-normalized
          (directory-file-name (expand-file-name snapshot-base)))
         (snapshot-base-true
          (directory-file-name (file-truename snapshot-base)))
         (add-destinations nil)
         (real-file-attributes (symbol-function 'file-attributes))
         (real-file-modes (symbol-function 'file-modes))
         (real-add-name-to-file (symbol-function 'add-name-to-file)))
    (unwind-protect
        (let ((file (expand-file-name "dictionary" directory))
              (temporary-file-directory snapshot-base))
          (with-temp-file file
            (insert "pinned\n"))
          (with-temp-buffer
            (cl-letf (((symbol-function 'file-attributes)
                       (lambda (path &optional id-format)
                         (let ((attributes
                                (funcall real-file-attributes path id-format))
                               (normalized
                                (directory-file-name
                                 (expand-file-name path))))
                           (if (member normalized
                                       (list snapshot-base-normalized
                                             snapshot-base-true))
                               (let ((copy (copy-sequence attributes)))
                                 (setcar (nthcdr 2 copy) 0)
                                 copy)
                             attributes))))
                      ((symbol-function 'file-modes)
                       (lambda (path &optional flag)
                         (let ((normalized
                                (directory-file-name
                                 (expand-file-name path))))
                           (if (member normalized
                                       (list snapshot-base-normalized
                                             snapshot-base-true))
                               #o1777
                             (funcall real-file-modes path flag)))))
                      ((symbol-function 'add-name-to-file)
                       (lambda (old new &optional ok-if-already-exists)
                         (push new add-destinations)
                         (funcall real-add-name-to-file
                                  old new ok-if-already-exists))))
              (nskk--dict-insert-file-contents-bounded file nil)
              (should (equal (buffer-string) "pinned\n"))))
          (should (= (length add-destinations) 1))
          (should
           (string-prefix-p
            (file-name-as-directory snapshot-base-normalized)
            (car add-destinations))))
      (ignore-errors (delete-directory snapshot-base t))
      (ignore-errors (delete-directory directory t)))))
(nskk-it "falls back from a cross-device temp pin to a source-side pin"
  (let ((directory (make-temp-file "nskk-dict-source-pin-" t))
        (snapshot-base (make-temp-file "nskk-dict-temp-pin-" t))
        (add-calls 0)
        (real-add-name-to-file (symbol-function (quote add-name-to-file))))
    (unwind-protect
        (let ((file (expand-file-name "dictionary" directory))
              (temporary-file-directory snapshot-base))
          (with-temp-file file
            (insert "source-side\n"))
          (with-temp-buffer
            (cl-letf (((symbol-function (quote add-name-to-file))
                       (lambda (old new &optional ok-if-already-exists)
                         (cl-incf add-calls)
                         (if (= add-calls 1)
                             (signal (quote file-error)
                                     (quote ("Invalid cross-device link")))
                           (funcall real-add-name-to-file
                                    old new ok-if-already-exists)))))
              (nskk--dict-insert-file-contents-bounded file nil)
              (should (equal (buffer-string) "source-side\n"))))
          (should (= add-calls 2)))
      (ignore-errors (delete-directory snapshot-base t))
      (ignore-errors (delete-directory directory t)))))
(nskk-it "uses the immutable direct fallback after all pins are denied"
  (let ((directory (make-temp-file "nskk-dict-direct-fallback-" t))
        (add-calls 0)
        (pinning t)
        (real-file-writable-p (symbol-function 'file-writable-p)))
    (unwind-protect
        (let ((file (expand-file-name "dictionary" directory)))
          (with-temp-file file
            (insert "direct\n"))
          (with-temp-buffer
            (cl-letf (((symbol-function 'file-writable-p)
                       (lambda (path)
                         (and pinning
                              (funcall real-file-writable-p path))))
                      ((symbol-function 'add-name-to-file)
                       (lambda (&rest _args)
                         (cl-incf add-calls)
                         (setq pinning nil)
                         (signal 'file-error
                                 '("Operation not permitted")))))
              (nskk--dict-insert-file-contents-bounded file nil)
              (should (equal (buffer-string) "direct\n"))))
          (should (> add-calls 0)))
      (ignore-errors (delete-directory directory t)))))
(nskk-it "rejects a mutable direct path without reading the source"
  (let ((directory (make-temp-file "nskk-dict-mutable-direct-" t))
        (buffer (generate-new-buffer " *nskk-dict-mutable-direct*"))
        (add-calls 0)
        (source-read-called nil))
    (unwind-protect
        (let ((file (expand-file-name "dictionary" directory)))
          (with-temp-file file
            (insert "mutable\n"))
          (with-current-buffer buffer
            (insert "sentinel")
            (cl-letf (((symbol-function 'add-name-to-file)
                       (lambda (&rest _args)
                         (cl-incf add-calls)
                         (signal 'file-error
                                 '("Operation not permitted"))))
                      ((symbol-function 'insert-file-contents)
                       (lambda (&rest _args)
                         (setq source-read-called t)
                         (error "source path must not be read"))))
              (let ((condition
                     (should-error
                      (nskk--dict-insert-file-contents-bounded file nil)
                      :type 'error)))
                (should
                 (string-match-p
                  "Cannot safely read unpinned file"
                  (error-message-string condition)))))
            (should (equal (buffer-string) "sentinel")))
          (should (> add-calls 0))
          (should-not source-read-called))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (ignore-errors (delete-directory directory t)))))
(nskk-it "rejects unsafe snapshot bases before temporary-file, link, or read I/O"
  (let* ((source-directory (make-temp-file "nskk-dict-unsafe-base-source-" t))
         (ancestor (make-temp-file "nskk-dict-unsafe-base-parent-" t))
         (snapshot-base (expand-file-name "private" ancestor))
         (file (expand-file-name "dictionary" source-directory))
         (real-file-attributes (symbol-function 'file-attributes))
         (real-file-modes (symbol-function 'file-modes))
         (real-file-acl (symbol-function 'file-acl)))
    (unwind-protect
        (progn
          (make-directory snapshot-base)
          (set-file-modes ancestor #o700)
          (set-file-modes snapshot-base #o700)
          (with-temp-file file
            (insert "unsafe-base\n"))
          (dolist (case '(untrusted-sticky untrusted-ancestor base-acl ancestor-acl))
            (let ((temporary-file-directory snapshot-base)
                  (make-temp-calls 0)
                  (link-calls 0)
                  (read-calls 0))
              (cl-letf
                  (((symbol-function 'file-attributes)
                    (lambda (path &optional id-format)
                      (let ((attrs (funcall real-file-attributes path id-format))
                            (normalized (directory-file-name
                                         (expand-file-name path))))
                        (if (and attrs
                                 (or (and (eq case 'untrusted-sticky)
                                          (equal normalized snapshot-base))
                                     (and (eq case 'untrusted-ancestor)
                                          (equal normalized ancestor))))
                            (let ((copy (copy-tree attrs)))
                              (setcar (nthcdr 2 copy) 4242)
                              copy)
                          attrs))))
                   ((symbol-function 'file-modes)
                    (lambda (path)
                      (if (and (eq case 'untrusted-sticky)
                               (equal (directory-file-name
                                       (expand-file-name path))
                                      snapshot-base))
                          #o1777
                        (funcall real-file-modes path))))
                   ((symbol-function 'file-acl)
                    (lambda (path)
                      (let ((normalized (directory-file-name
                                         (expand-file-name path))))
                        (if (or (and (eq case 'base-acl)
                                     (equal normalized snapshot-base))
                                (and (eq case 'ancestor-acl)
                                     (equal normalized ancestor)))
                            '((mock . acl))
                          (funcall real-file-acl path)))))
                   ((symbol-function 'file-writable-p)
                    (lambda (path)
                      (let ((normalized (directory-file-name
                                         (expand-file-name path))))
                        (or (equal normalized snapshot-base)
                            (equal normalized file)
                            (equal normalized
                                   (directory-file-name
                                    (file-truename file)))))))
                   ((symbol-function 'make-temp-file)
                    (lambda (&rest _args)
                      (cl-incf make-temp-calls)
                      (error "unsafe base reached make-temp-file")))
                   ((symbol-function 'add-name-to-file)
                    (lambda (&rest _args)
                      (cl-incf link-calls)
                      (error "unsafe base reached add-name-to-file")))
                   ((symbol-function 'insert-file-contents)
                    (lambda (&rest _args)
                      (cl-incf read-calls)
                      (error "unsafe base reached insert-file-contents"))))
                (with-temp-buffer
                  (should-error
                   (nskk--dict-insert-file-contents-bounded file nil)
                   :type 'error)))
              (should (= make-temp-calls 0))
              (should (= link-calls 0))
              (should (equal (list case read-calls) (list case 0))))))
      (ignore-errors (delete-directory ancestor t))
      (ignore-errors (delete-directory source-directory t)))))
(nskk-it "rejects immutable direct fallback below an untrusted 0700 parent without reading"
  (let* ((directory (make-temp-file "nskk-dict-untrusted-direct-parent-" t))
         (snapshot-base (make-temp-file "nskk-dict-untrusted-direct-temp-" t))
         (file (expand-file-name "dictionary" directory))
         (buffer (generate-new-buffer " *nskk-dict-untrusted-direct*"))
         (real-file-attributes (symbol-function (quote file-attributes)))
         (real-file-writable-p (symbol-function (quote file-writable-p)))
         (pinning t)
         (add-calls 0)
         (read-calls 0))
    (unwind-protect
        (progn
          (set-file-modes directory #o700)
          (with-temp-file file
            (insert "untrusted-parent\n"))
          (with-current-buffer buffer
            (insert "sentinel")
            (let ((temporary-file-directory snapshot-base))
              (cl-letf
                  (((symbol-function (quote file-attributes))
                    (lambda (path &optional id-format)
                      (let ((attrs (funcall real-file-attributes path id-format)))
                        (if (and attrs
                                 (equal (directory-file-name
                                         (expand-file-name path))
                                        directory))
                            (let ((copy (copy-tree attrs)))
                              (setcar (nthcdr 2 copy) 4242)
                              copy)
                          attrs))))
                   ((symbol-function (quote file-writable-p))
                    (lambda (path)
                      (and pinning
                           (funcall real-file-writable-p path))))
                   ((symbol-function (quote add-name-to-file))
                    (lambda (&rest _args)
                      (cl-incf add-calls)
                      (setq pinning nil)
                      (signal (quote file-error)
                              (quote ("Operation not permitted")))))
                   ((symbol-function (quote insert-file-contents))
                    (lambda (&rest _args)
                      (cl-incf read-calls)
                      (error "untrusted direct source was read"))))
                (should-error
                 (nskk--dict-insert-file-contents-bounded file nil)
                 :type (quote error))))
            (should (equal (buffer-string) "sentinel")))
          (should (> add-calls 0))
          (should (= read-calls 0)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (ignore-errors (delete-directory snapshot-base t))
      (ignore-errors (delete-directory directory t)))))

(nskk-it "leaves the caller buffer unchanged when post-read validation fails"
  (let* ((directory (make-temp-file "nskk-dict-post-read-source-" t))
         (snapshot-base (make-temp-file "nskk-dict-post-read-temp-" t))
         (file (expand-file-name "dictionary" directory))
         (buffer (generate-new-buffer " *nskk-dict-post-read-atomic*"))
         (real-file-attributes (symbol-function (quote file-attributes)))
         (real-insert-file-contents
          (symbol-function (quote insert-file-contents)))
         (read-complete nil)
         (read-calls 0))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "validated\n"))
          (with-current-buffer buffer
            (insert "sentinel")
            (let ((temporary-file-directory snapshot-base))
              (cl-letf
                  (((symbol-function (quote insert-file-contents))
                    (lambda (path &rest args)
                      (prog1
                          (apply real-insert-file-contents path args)
                        (cl-incf read-calls)
                        (setq read-complete t))))
                   ((symbol-function (quote file-attributes))
                    (lambda (path &optional id-format)
                      (let ((attrs (funcall real-file-attributes path id-format)))
                        (if (and attrs
                                 read-complete
                                 (equal (file-name-nondirectory path) "contents"))
                            (let ((copy (copy-tree attrs)))
                              (setcar (nthcdr 7 copy)
                                      (1+ (file-attribute-size attrs)))
                              copy)
                          attrs)))))
                (should-error
                 (nskk--dict-insert-file-contents-bounded file nil)
                 :type (quote error))))
            (should (equal (buffer-string) "sentinel")))
          (should (= read-calls 1)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (ignore-errors (delete-directory snapshot-base t))
      (ignore-errors (delete-directory directory t)))))



	       (nskk-it "does not hang when a validated file is replaced by a FIFO"
			(let* ((mkfifo (executable-find "mkfifo"))
			       (file (make-temp-file "nskk-dictionary-fifo-race-"))
			       (root (or (locate-dominating-file default-directory "src")
					 default-directory))
			       (source-directory (expand-file-name "src" root))
			       (emacs (expand-file-name invocation-name invocation-directory))
			       (child-form
				(prin1-to-string
				 `(progn
				    (setq load-prefer-newer t)
				    (require 'cl-lib)
				    (require 'nskk-dictionary)
				    (let ((real-add-name-to-file
					   (symbol-function 'add-name-to-file)))
				      (condition-case condition
					  (progn
					    (cl-letf
						(((symbol-function 'add-name-to-file)
						  (lambda (old new &optional ok-if-already-exists)
						    (delete-file old)
						    (unless
							(= 0 (call-process
							      ,mkfifo nil nil nil old))
						      (error "mkfifo failed"))
						    (funcall
						     real-add-name-to-file
						     old new ok-if-already-exists))))
					      (with-temp-buffer
						(nskk--dict-insert-file-contents-bounded
						 ,file nil)))
					    (kill-emacs 12))
					(error
					 (princ (error-message-string condition))
					 (kill-emacs 0)))))))
			       (buffer (generate-new-buffer " *nskk-fifo-race*"))
			       process)
			  (skip-unless mkfifo)
			  (unwind-protect
			      (progn
				(with-temp-file file
				  (insert "regular"))
				(setq process
				      (make-process
				       :name "nskk-fifo-race"
				       :buffer buffer
				       :command
				       (list emacs "-Q" "--batch"
					     "-L" source-directory
					     "--eval" child-form)
				       :connection-type 'pipe
				       :noquery t))
				(let ((deadline (+ (float-time) 5.0)))
				  (while (and (process-live-p process)
					      (< (float-time) deadline))
				    (accept-process-output process 0.05))
				  (when (process-live-p process)
				    (delete-process process)
				    (ert-fail "Pinned read hung after FIFO replacement")))
				(let ((output (with-current-buffer buffer (buffer-string))))
				  (should (= (process-exit-status process) 0))
				  (should
				   (string-match-p
				    (concat
				     "NSKK: \\(?:Cannot safely read unpinned file"
				     "\\|File changed before pinned read\\)")
				    output))))
			    (when (and process (process-live-p process))
			      (delete-process process))
			    (when (buffer-live-p buffer)
			      (kill-buffer buffer))
			    (when (file-exists-p file)
			      (delete-file file))))))

    (nskk-describe "incremental dictionary append identity"
  (nskk-it "preserves database and index cons identity and order"
    (dolist (type '(:hash :trie nil))
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-with-database-fields
            ((index-bucket-tail-cache (make-hash-table :test #'equal)))
          (let* ((predicate
                (intern (format "append-identity-%s" (or type "none"))))
               (key (nskk-prolog-clause-key predicate 2)))
          (when type
            (nskk-prolog-set-index predicate 2 type))
          (nskk-prolog-assert
           (list (list predicate "same" '("old-1"))))
          (nskk-prolog-assert
           (list (list predicate "other" '("old-2"))))
          (let* ((database (gethash key (nskk-prolog-database)))
                 (database-second (cdr database))
                 (database-tail
                  (gethash key (nskk-prolog-database-tails)))
                 (index
                  (and type
                       (nskk-prolog-transaction-index key type)))
                 (bucket
                  (and type
                       (nskk-prolog-transaction-index-bucket
                        type index "same")))
                 (bucket-tail (and bucket (last bucket))))
            (nskk--dict-append-predicate-entries
             predicate
             '(("same" "new-1")
               ("new" "new-2")))
            (let ((appended (gethash key (nskk-prolog-database))))
              (should (eq appended database))
              (should (eq (cdr appended) database-second))
              (should
               (eq (cdr database-tail) (nthcdr 2 appended)))
              (should
               (equal
                (mapcar (lambda (clause) (cadr (car clause)))
                        appended)
                '("same" "other" "same" "new")))
              (should
               (eq (gethash key (nskk-prolog-database-tails))
                   (last appended))))
            (if type
                (let ((appended-bucket
                       (nskk-prolog-transaction-index-bucket
                        type index "same")))
                  (should
                   (eq index
                       (nskk-prolog-transaction-index key type)))
                  (should (eq appended-bucket bucket))
                  (should
                   (eq (cdr bucket-tail)
                       (cdr appended-bucket)))
                  (should
                   (equal
                    (mapcar
                     (lambda (clause) (cadr (car clause)))
                     appended-bucket)
                    '("same" "same")))
                  (should
                   (gethash key
                            (nskk-prolog-index-bucket-tail-cache))))
              (should-not
               (gethash key (nskk-prolog-index-config)))
              (should-not
               (gethash key
                        (nskk-prolog-index-bucket-tail-cache)))))))))))
(nskk-describe "incremental dictionary append strategies and cache"
  (nskk-it "retains empty strategies and defaults fresh predicates to trie"
    (dolist (type '(:hash :trie :list))
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-with-database-fields
            ((index-bucket-tail-cache (make-hash-table :test #'equal)))
          (let* ((predicate
                (intern (format "append-strategy-%s" type)))
               (key (nskk-prolog-clause-key predicate 2)))
          (nskk-prolog-set-index predicate 2 type)
          (nskk--dict-append-predicate-entries
           predicate '(("key" "value")))
          (should (eq (gethash key (nskk-prolog-index-config)) type))
          (should
           (equal
            (gethash key (nskk-prolog-database))
            (list (list (list predicate "key" '("value"))))))))))
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-with-database-fields
          ((index-bucket-tail-cache (make-hash-table :test #'equal)))
        (let* ((predicate 'append-fresh-default)
             (key (nskk-prolog-clause-key predicate 2)))
        (nskk--dict-append-predicate-entries
         predicate '(("key" "value")))
        (should (eq (gethash key (nskk-prolog-index-config)) :trie))
        (should (gethash key (nskk-prolog-trie-indices)))))))

  (nskk-it "uses cached bucket tails for repeated duplicate-key appends"
    (dolist (type '(:hash :trie))
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-with-database-fields
            ((index-bucket-tail-cache (make-hash-table :test #'equal)))
          (let* ((predicate
                (intern (format "append-warm-tail-%s" type)))
               (key (nskk-prolog-clause-key predicate 2))
               (entries
                (cl-loop repeat 256 collect '("same" "new"))))
          (nskk-prolog-set-index predicate 2 type)
          (nskk--dict-append-predicate-entries
           predicate '(("same" "old")))
          (cl-letf (((symbol-function 'last)
                     (lambda (&rest _)
                       (error "warm append scanned a bucket"))))
            (nskk--dict-append-predicate-entries predicate entries))
          (should
           (= (length (gethash key (nskk-prolog-database))) 257)))))))

  (nskk-it "invalidates stale tails after an external append"
    (dolist (type '(:hash :trie))
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-with-database-fields
            ((index-bucket-tail-cache (make-hash-table :test #'equal)))
          (let* ((predicate
                (intern (format "append-cache-invalidate-%s" type)))
               (key (nskk-prolog-clause-key predicate 2)))
          (nskk-prolog-set-index predicate 2 type)
          (nskk--dict-append-predicate-entries
           predicate '(("same" "initial")))
          (let* ((index (nskk-prolog-transaction-index key type))
                 (initial-cell
                  (nskk-prolog-transaction-index-bucket
                   type index "same"))
                 (cache-entry
                  (gethash
                   key (nskk-prolog-index-bucket-tail-cache)))
                 (cache-buckets (aref cache-entry 2))
                 (tail-info (gethash "same" cache-buckets))
                 (cached-tail (aref tail-info 1))
                 (external-clause
                  (list (list predicate "same" '("external"))))
                 (external-cell (list external-clause))
                 (real-last (symbol-function 'last))
                 (last-calls 0))
            (should (= (length cache-entry) 3))
            (should (eq (aref cache-entry 0) type))
            (should (eq (aref cache-entry 1) index))
            (should (eq (aref tail-info 0) initial-cell))
            (should (eq cached-tail initial-cell))
            (should-not (cdr cached-tail))
            (setcdr cached-tail external-cell)
            (should (eq (cdr cached-tail) external-cell))
            (cl-letf (((symbol-function 'last)
                       (lambda (&rest args)
                         (setq last-calls (1+ last-calls))
                         (apply real-last args))))
              (nskk--dict-append-predicate-entries
               predicate '(("same" "final"))))
            (should (= last-calls 1))
            (let* ((bucket
                    (nskk-prolog-transaction-index-bucket
                     type index "same"))
                   (final-cell (cdr external-cell))
                   (updated-entry
                    (gethash
                     key (nskk-prolog-index-bucket-tail-cache)))
                   (updated-info
                    (gethash "same" (aref updated-entry 2)))
                   (updated-tail (aref updated-info 1)))
              (should (eq bucket initial-cell))
              (should (eq (cdr initial-cell) external-cell))
              (should (eq (cdr external-cell) final-cell))
              (should-not (cdr final-cell))
              (should
               (equal
                (mapcar (lambda (clause) (caddr (car clause))) bucket)
                '(("initial") ("external") ("final"))))
              (should (eq (aref updated-info 0) bucket))
              (should (eq updated-tail final-cell))
              (should-not (cdr updated-tail))
              (setq last-calls 0)
              (cl-letf (((symbol-function 'last)
                         (lambda (&rest args)
                           (setq last-calls (1+ last-calls))
                           (apply real-last args))))
                (nskk--dict-append-predicate-entries
                 predicate '(("same" "warm"))))
              (should (= last-calls 0))
              (let ((warm-cell (cdr final-cell)))
                (should (eq (cdr final-cell) warm-cell))
                (should-not (cdr warm-cell))
                (should
                 (equal
                  (mapcar (lambda (clause) (caddr (car clause))) bucket)
                  '(("initial") ("external") ("final") ("warm"))))
                (remhash key (nskk-prolog-index-bucket-tail-cache))
                (setq last-calls 0)
                (cl-letf (((symbol-function 'last)
                           (lambda (&rest args)
                             (setq last-calls (1+ last-calls))
                             (apply real-last args))))
                  (nskk--dict-append-predicate-entries
                   predicate '(("same" "cold"))))
                (should (= last-calls 1))
                (let* ((cold-cell (cdr warm-cell))
                       (cold-entry
                        (gethash
                         key (nskk-prolog-index-bucket-tail-cache)))
                       (cold-info
                        (gethash "same" (aref cold-entry 2)))
                       (cold-tail (aref cold-info 1)))
                  (should (eq (cdr warm-cell) cold-cell))
                  (should-not (cdr cold-cell))
                  (should
                   (equal
                    (mapcar
                     (lambda (clause) (caddr (car clause)))
                     bucket)
                    '(("initial")
                      ("external")
                      ("final")
                      ("warm")
                      ("cold"))))
                  (should (eq (aref cold-info 0) bucket))
                  (should (eq cold-tail cold-cell))
                  (should-not (cdr cold-tail))))))))))))
(nskk-describe "incremental public dictionary loads"
  (nskk-it "reuses the existing database spine across repeated file loads"
    (nskk-prolog-test-with-isolated-db
      (let ((first-file (make-temp-file "nskk-append-first-" nil ".skk"))
            (second-file (make-temp-file "nskk-append-second-" nil ".skk"))
            (predicate 'append-public-load))
        (unwind-protect
            (nskk-prolog-with-database-fields
                ((index-bucket-tail-cache (make-hash-table :test #'equal)))
              (let* ((key (nskk-prolog-clause-key predicate 2)))
              (with-temp-file first-file
                (insert "first /one/\n"))
              (with-temp-file second-file
                (insert "second /two/\n"))
              (should
               (eq (nskk-dict-load-file first-file nil predicate)
                   predicate))
              (let ((database (gethash key (nskk-prolog-database)))
                    (database-tail
                     (gethash key (nskk-prolog-database-tails))))
                (should
                 (eq (nskk-dict-load-file second-file nil predicate)
                     predicate))
                (let ((appended (gethash key (nskk-prolog-database))))
                  (should (eq appended database))
                  (should (eq (cdr database-tail) (cdr appended)))
                  (should
                   (equal
                    (mapcar (lambda (clause) (cadr (car clause)))
                            appended)
                    '("first" "second")))))))
          (when (file-exists-p first-file)
            (delete-file first-file))
          (when (file-exists-p second-file)
            (delete-file second-file)))))))
(nskk-describe "incremental append setup rollback"
  (nskk-it "restores a fresh predicate after index setup errors and quits"
    (dolist (kind '(error quit))
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-with-database-fields
            ((index-bucket-tail-cache (make-hash-table :test #'equal)))
          (let* ((predicate
                (intern (format "append-setup-rollback-%s" kind)))
               (key (nskk-prolog-clause-key predicate 2))
               (before (nskk-dict-transaction-predicate-snapshot key))
               (real-set-index
                (symbol-function 'nskk-prolog-set-index))
               (data (list "append setup rollback" kind))
               (caught nil))
          (cl-letf
              (((symbol-function 'nskk-prolog-set-index)
                (lambda (&rest args)
                  (apply real-set-index args)
                  (signal kind data))))
            (condition-case condition
                (nskk--dict-append-predicate-entries
                 predicate '(("key" "candidate")))
              ((error quit) (setq caught condition))))
          (should (eq (car caught) kind))
          (should (equal (cdr caught) data))
          (let ((after (nskk-dict-transaction-predicate-snapshot key)))
            (dotimes (slot (1- (length before)))
              (should
               (eq (aref before (1+ slot))
                   (aref after (1+ slot))))))
          (should-not
           (gethash key (nskk-prolog-index-bucket-tail-cache)))))))))
(nskk-describe "incremental append partial publication rollback"
  (nskk-it "restores exact spines after indexed publication errors and quits"
    (dolist (type '(:hash :trie))
      (dolist (kind '(error quit))
        (nskk-prolog-test-with-isolated-db
          (nskk-prolog-with-database-fields
              ((index-bucket-tail-cache (make-hash-table :test #'equal)))
            (let* ((predicate
                  (intern
                   (format "append-partial-%s-%s" type kind)))
                 (key (nskk-prolog-clause-key predicate 2))
                 (data (list "append partial rollback" type kind))
                 (caught nil)
                 (calls 0))
            (nskk-prolog-set-index predicate 2 type)
            (nskk--dict-append-predicate-entries
             predicate '(("same" "old")))
            (let* ((before (nskk-dict-transaction-predicate-snapshot key))
                   (previous-cache
                    (gethash key
                             (nskk-prolog-index-bucket-tail-cache)))
                   (database
                    (gethash key (nskk-prolog-database)))
                   (database-tail
                    (gethash key (nskk-prolog-database-tails)))
                   (index
                    (nskk-prolog-transaction-index key type))
                   (bucket
                    (nskk-prolog-transaction-index-bucket
                     type index "same"))
                   (bucket-tail (last bucket))
                   (real-set-bucket
                    (symbol-function
                     'nskk-prolog-transaction-set-index-bucket)))
              (cl-letf
                  (((symbol-function
                     'nskk-prolog-transaction-set-index-bucket)
                    (lambda (&rest args)
                      (apply real-set-bucket args)
                      (setq calls (1+ calls))
                      (when (= calls 2)
                        (signal kind data)))))
                (condition-case condition
                    (nskk--dict-append-predicate-entries
                     predicate
                     '(("same" "candidate")
                       ("new" "candidate")))
                  ((error quit) (setq caught condition))))
              (should (eq (car caught) kind))
              (should (equal (cdr caught) data))
              (let ((after (nskk-dict-transaction-predicate-snapshot key)))
                (dotimes (slot (1- (length before)))
                  (should
                   (eq (aref before (1+ slot))
                       (aref after (1+ slot))))))
              (should (eq database
                          (gethash key (nskk-prolog-database))))
              (should (eq database-tail
                          (gethash key
                                   (nskk-prolog-database-tails))))
              (should-not (cdr database-tail))
              (should
               (eq index
                   (nskk-prolog-transaction-index key type)))
              (should
               (eq bucket
                   (nskk-prolog-transaction-index-bucket
                    type index "same")))
              (should-not (cdr bucket-tail))
              (should-not
               (nskk-prolog-transaction-index-bucket
                type index "new"))
              (should
               (eq previous-cache
                   (gethash key
                            (nskk-prolog-index-bucket-tail-cache))))))))))))
(nskk-describe "incremental append deferred quit rollback"
  (nskk-it "undoes all mutations when a pending quit is observed"
    (dolist (type '(:hash :trie nil))
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-with-database-fields
            ((index-bucket-tail-cache (make-hash-table :test #'equal)))
          (let* ((predicate
                (intern
                 (format "append-deferred-quit-%s"
                         (or type "none"))))
               (key (nskk-prolog-clause-key predicate 2))
               (caught nil))
          (if type
              (progn
                (nskk-prolog-set-index predicate 2 type)
                (nskk--dict-append-predicate-entries
                 predicate '(("same" "old"))))
            (nskk-prolog-assert
             (list (list predicate "same" '("old")))))
          (let* ((before (nskk-dict-transaction-predicate-snapshot key))
                 (previous-cache
                  (gethash key (nskk-prolog-index-bucket-tail-cache)))
                 (database
                  (gethash key (nskk-prolog-database)))
                 (database-tail
                  (gethash key (nskk-prolog-database-tails)))
                 (index
                  (and type
                       (nskk-prolog-transaction-index key type)))
                 (bucket
                  (and type
                       (nskk-prolog-transaction-index-bucket
                        type index "same")))
                 (bucket-tail (and bucket (last bucket))))
            (let ((quit-flag t)
                  (inhibit-quit t))
              (condition-case condition
                  (nskk--dict-append-predicate-entries
                   predicate
                   '(("same" "candidate")
                     ("new" "candidate")))
                (quit (setq caught condition quit-flag nil))))
            (should (eq (car caught) 'quit))
            (let ((after (nskk-dict-transaction-predicate-snapshot key)))
              (dotimes (slot (1- (length before)))
                (should
                 (eq (aref before (1+ slot))
                     (aref after (1+ slot))))))
            (should
             (eq database (gethash key (nskk-prolog-database))))
            (should
             (eq database-tail
                 (gethash key (nskk-prolog-database-tails))))
            (should-not (cdr database-tail))
            (should
             (eq previous-cache
                 (gethash key
                          (nskk-prolog-index-bucket-tail-cache))))
            (if type
                (progn
                  (should
                   (eq index
                       (nskk-prolog-transaction-index key type)))
                  (should
                   (eq bucket
                       (nskk-prolog-transaction-index-bucket
                        type index "same")))
                  (should-not (cdr bucket-tail))
                  (should-not
                   (nskk-prolog-transaction-index-bucket
                    type index "new")))
              (should-not
               (gethash key (nskk-prolog-index-config)))))))))))
(nskk-describe "dictionary predicate publication rollback boundaries"
    (nskk-it "restores exact facts after bulk cache and publish errors and quits"
      (dolist (kind '(error quit))
        (dolist (stage '(bulk cache publish))
          (nskk-prolog-test-with-isolated-db
            (let* ((predicate 'system-dict-entry)
                   (key (nskk-prolog-clause-key predicate 2))
                   (data (list "dictionary rollback" stage kind))
                   (caught nil)
                   (before nil))
              (nskk-prolog-set-index predicate 2 :trie)
              (nskk-prolog-assert
               '((system-dict-entry "existing" ("value"))))
              (setq before (nskk-dict-transaction-predicate-snapshot key))
              (pcase stage
                ('bulk
                 (let ((real-bulk
                        (symbol-function 'nskk-prolog-trie-bulk-assert)))
                   (cl-letf
                       (((symbol-function 'nskk-prolog-trie-bulk-assert)
                         (lambda (&rest args)
                           (apply real-bulk args)
                           (signal kind data))))
                     (condition-case condition
                         (nskk--dict-replace-predicate-entries
                          predicate
                          '(("replacement" ("candidate"))))
                       ((error quit) (setq caught condition))))))
                ('cache
                 (let ((staged
                        (nskk--dict-stage-predicate-entries
                         predicate
                         '(("replacement" ("candidate"))))))
                   (cl-letf
                       (((symbol-function 'nskk--dict-save-system-dict-cache)
                         (lambda (&rest _)
                           (signal kind data))))
                     (condition-case condition
                         (nskk--dict-commit-staged-predicate
                          staged
                          (lambda ()
                            (nskk--dict-save-system-dict-cache nil nil)))
                       ((error quit) (setq caught condition))))))
                ('publish
                 (let ((staged
                        (nskk--dict-stage-predicate-entries
                         predicate
                         '(("replacement" ("candidate")))))
                       (real-publish
                        (symbol-function
                         'nskk--dict-publish-staged-predicate)))
                   (cl-letf
                       (((symbol-function 'nskk--dict-publish-staged-predicate)
                         (lambda (candidate)
                           (funcall real-publish candidate)
                           (signal kind data))))
                     (condition-case condition
                         (nskk--dict-commit-staged-predicate staged)
                       ((error quit) (setq caught condition)))))))
              (should (eq (car caught) kind))
              (should (equal (cdr caught) data))
              (should
               (equal (cl-subseq before 1)
                      (cl-subseq
                       (nskk-dict-transaction-predicate-snapshot key) 1)))
              (should
               (nskk-prolog-holds-p
                '(system-dict-entry "existing" ("value"))))
              (should-not
               (nskk-prolog-holds-p
                '(system-dict-entry "replacement" ("candidate"))))))))))

  (defconst nskk-dictionary-test--display-attack-properties
    '(display keymap local-map mouse-face help-echo face)
    "Text properties that untrusted dictionary display copies must remove.")

  (defun nskk-dictionary-test--attack-string (text)
    "Return TEXT carrying hostile display properties and no-learn metadata."
    (propertize text
                'display "spoofed"
                'keymap 'spoof-keymap
                'local-map 'spoof-local-map
                'mouse-face 'highlight
                'help-echo "spoof help"
                'face 'error
                'nskk-no-learn t))

  (defun nskk-dictionary-test--should-be-display-safe (text)
    "Assert that TEXT has no hostile display properties."
    (dolist (property nskk-dictionary-test--display-attack-properties)
      (should-not
       (text-property-not-all 0 (length text) property nil text))))

  (defun nskk-dictionary-test--should-retain-attack-properties (text)
    "Assert that the hostile properties and metadata remain on source TEXT."
    (should (equal (get-text-property 0 'display text) "spoofed"))
    (should (eq (get-text-property 0 'keymap text) 'spoof-keymap))
    (should (eq (get-text-property 0 'local-map text) 'spoof-local-map))
    (should (eq (get-text-property 0 'mouse-face text) 'highlight))
    (should (equal (get-text-property 0 'help-echo text) "spoof help"))
    (should (eq (get-text-property 0 'face text) 'error))
    (should (eq (get-text-property 0 'nskk-no-learn text) t)))

  (nskk-describe "untrusted dictionary display properties"
    (nskk-it "sanitizes register messages without changing Prolog arguments"
      (let* ((reading
              (nskk-dictionary-test--attack-string "よみ"))
             (word
              (nskk-dictionary-test--attack-string "候補"))
             (nskk--user-dict-index t)
             (nskk-dict-modified nil)
             (query nil)
             (rendered nil)
             (hook-called nil)
             (nskk-jisyo-update-hook
              (list (lambda () (setq hook-called t)))))
        (cl-letf
            (((symbol-function 'nskk-prolog-holds-p)
              (lambda (candidate)
                (setq query candidate)
                t))
             ((symbol-function 'message)
              (lambda (format-string &rest args)
                (setq rendered
                      (apply #'format format-string args)))))
          (should (nskk--dict-register-impl reading word)))
        (should (equal rendered "NSKK: Registered よみ -> 候補"))
        (nskk-dictionary-test--should-be-display-safe rendered)
        (should (eq (car query) 'dict-register))
        (should (eq (cadr query) reading))
        (should (eq (caddr query) word))
        (should hook-called)
        (should nskk-dict-modified)
        (nskk-dictionary-test--should-retain-attack-properties reading)
        (nskk-dictionary-test--should-retain-attack-properties word)))

    (nskk-it "sanitizes unregister messages without changing Prolog arguments"
      (let* ((reading
              (nskk-dictionary-test--attack-string "よみ"))
             (word
              (nskk-dictionary-test--attack-string "候補"))
             (nskk--user-dict-index t)
             (nskk-dict-modified nil)
             (query nil)
             (rendered nil)
             (hook-called nil))
        (cl-letf
            (((symbol-function 'nskk-prolog-holds-p)
              (lambda (candidate)
                (setq query candidate)
                t))
             ((symbol-function 'nskk--dict-run-update-hook)
              (lambda ()
                (setq hook-called t)))
             ((symbol-function 'message)
              (lambda (format-string &rest args)
                (setq rendered
                      (apply #'format format-string args)))))
          (should (nskk--dict-unregister-impl reading word)))
        (should (equal rendered "NSKK: Unregistered よみ -> 候補"))
        (nskk-dictionary-test--should-be-display-safe rendered)
        (should (eq (car query) 'dict-unregister))
        (should (eq (cadr query) reading))
        (should (eq (caddr query) word))
        (should hook-called)
        (should nskk-dict-modified)
        (nskk-dictionary-test--should-retain-attack-properties reading)
        (nskk-dictionary-test--should-retain-attack-properties word)))

    (nskk-it "sanitizes save preflight errors without changing bindings"
      (let* ((key
              (nskk-dictionary-test--attack-string "よみ"))
             (invalid
              (nskk-dictionary-test--attack-string "不/\n正"))
             (bindings (list (list key (list invalid))))
             (nskk-dict-user-dictionary-file
              (make-temp-file "nskk-display-safe-save-" nil ".skk"))
             (nskk--user-dict-index t)
             (nskk-dict-modified t)
             (query nil)
             (variables nil)
             (condition nil)
             before-file)
        (unwind-protect
            (progn
              (with-temp-file nskk-dict-user-dictionary-file
                (insert "unchanged" (string 0 127) "\n"))
              (setq before-file
                    (with-temp-buffer
                      (insert-file-contents-literally
                       nskk-dict-user-dictionary-file)
                      (buffer-string)))
              (cl-letf
                  (((symbol-function 'nskk-prolog-query-bindings)
                    (lambda (candidate requested-variables)
                      (setq query candidate
                            variables requested-variables)
                      bindings)))
                (condition-case error
                    (nskk--dict-save-user-dictionary-1)
                  (nskk-dict-error
                   (setq condition error))))
              (should
               (equal condition
                      '(nskk-dict-error
                        "Invalid user dictionary entry")))
              (let ((rendered (error-message-string condition)))
                (should
                 (equal rendered
                        "Dictionary error: \"Invalid user dictionary entry\""))
                (nskk-dictionary-test--should-be-display-safe rendered))
              (should
               (equal query
                      '(user-dict-entry \?k \?c)))
              (should (equal variables '(\?k \?c)))
              (should (eq (caar bindings) key))
              (should (eq (caadar bindings) invalid))
              (should nskk-dict-modified)
              (should
               (equal before-file
                      (with-temp-buffer
                        (insert-file-contents-literally
                         nskk-dict-user-dictionary-file)
                        (buffer-string))))
              (nskk-dictionary-test--should-retain-attack-properties key)
              (nskk-dictionary-test--should-retain-attack-properties invalid))
          (when (file-exists-p nskk-dict-user-dictionary-file)
            (delete-file nskk-dict-user-dictionary-file))))))

  (nskk-describe "rollback secondary failure retention"
    (nskk-it "retains only failed regions and blocks publication until retry succeeds"
      (dolist (kind '(error quit))
        (let* ((owner (list 'rollback-helper kind))
               (primary (list kind "primary publication failure" 'payload))
               (predicate-fails t)
               (publication-count 0)
               events
               caught
               next-condition)
          (unwind-protect
              (progn
                (nskk-dict-transaction-clear-pending-rollback owner)
                (cl-letf (((symbol-function 'display-warning)
                           (lambda (&rest _)
                             (signal kind '("warning failure")))))
                  (condition-case condition
                      (nskk-dict-transaction-rollback-and-resignal
                       owner
                       primary
                       (list
                        (cons
                         'predicate
                         (lambda ()
                           (push 'predicate events)
                           (when predicate-fails
                             (error "predicate rollback failure"))))
                        (cons
                         '(cache 0)
                         (lambda ()
                           (push 'cache events)))
                        (cons
                         'loaded-binding
                         (lambda ()
                           (push 'loaded-binding events)))))
                    ((error quit)
                     (setq caught condition)))
                  (should (equal caught primary))
                  (should
                   (equal events '(loaded-binding cache predicate)))
                  (let ((pending (nskk-dict-transaction-pending-rollback owner)))
                    (should pending)
                    (should
                     (equal
                      (mapcar #'car (plist-get pending :restorers))
                      '(predicate)))
                    (should
                     (equal
                      (mapcar #'car (plist-get pending :failures))
                      '(predicate))))
                  (condition-case condition
                      (progn
                        (nskk-dict-transaction-ensure-rollback-complete owner)
                        (cl-incf publication-count))
                    (nskk-dict-rollback-incomplete
                     (setq next-condition condition)))
                  (should (= publication-count 0))
                  (let ((payload (caddr next-condition)))
                    (should (equal (plist-get payload :owner) owner))
                    (should (equal (plist-get payload :primary) primary))
                    (should
                     (equal
                      (mapcar #'car (plist-get payload :failures))
                      '(predicate))))
                  (setq predicate-fails nil)
                  (should-not (nskk-dict-transaction-retry-pending-rollback owner))
                  (should-not (nskk-dict-transaction-pending-rollback owner))
                  (should
                   (equal
                    events
                    '(predicate predicate loaded-binding cache predicate)))))
            (nskk-dict-transaction-clear-pending-rollback owner))))))
(defun nskk-dictionary-test--file-bytes (path)
  "Return the literal contents of PATH."
  (with-temp-buffer (insert-file-contents-literally path) (buffer-string)))
(defun nskk-dictionary-test--capture-condition (thunk)
  "Call THUNK and return its error or quit condition."
  (condition-case
      condition
      (progn
        (funcall thunk)
        nil)
    ((error quit) condition)))
(nskk-describe
 "dictionary atomic save adversarial fixture"
 (nskk-it
  "passes complete TEXT to exclusive creation and preserves modes"
  (let* ((temp-dir (make-temp-file "nskk-atomic-text-" t))
         (output (expand-file-name "user.skk" temp-dir))
         (original-make-temp-file (symbol-function 'make-temp-file))
         calls
         created-modes
         created-identifiers)
    (unwind-protect (cl-letf
		     (((symbol-function 'make-temp-file)
                       (lambda (prefix &optional dir-flag suffix text)
			 (let ((temp (funcall original-make-temp-file prefix dir-flag suffix text)))
			   (push (list prefix dir-flag suffix text) calls)
			   (push (logand (file-modes temp) #o777) created-modes)
			   (push (nskk--dict-file-identifier temp) created-identifiers)
			   temp))))
		     (nskk--dict-call-with-atomic-file
		      output
		      (lambda ()
			(insert "first")))
		     (should
		      (equal
                       (car calls)
                       (list (concat (expand-file-name output) ".") nil nil "first")))
		     (should (= (car created-modes) #o600))
		     (should (= (logand (file-modes output) #o777) #o600))
		     (should (equal (nskk-dictionary-test--file-bytes output) "first"))
		     (should (equal (nskk--dict-file-identifier output) (car created-identifiers)))
		     (set-file-modes output #o640)
		     (nskk--dict-call-with-atomic-file
		      output
		      (lambda ()
			(insert "second")))
		     (should (equal (cadddr (car calls)) "second"))
		     (should (= (car created-modes) #o600))
		     (should (= (logand (file-modes output) #o777) #o640))
		     (should (equal (nskk-dictionary-test--file-bytes output) "second")))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))
 (nskk-it
  "runs the commit callback with quits inhibited"
  (let* ((temp-dir (make-temp-file "nskk-atomic-callback-" t))
         (output (expand-file-name "user.skk" temp-dir))
         (dirty 'preserved)
         callback-inhibit)
    (unwind-protect (progn
		      (nskk--dict-call-with-atomic-file
		       output
		       (lambda ()
			 (insert "committed"))
		       (lambda ()
			 (setq callback-inhibit inhibit-quit
			       dirty nil)))
		      (should callback-inhibit)
		      (should-not dirty)
		      (should (equal (nskk-dictionary-test--file-bytes output) "committed")))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))
 (nskk-it
  "does not delete attacker files on opaque creation failures"
  (dolist (kind '(error quit))
    (ert-info
     ((format "%S" kind))
     (let* ((temp-dir (make-temp-file "nskk-atomic-create-fault-" t))
            (output (expand-file-name "user.skk" temp-dir))
            (attacker (concat output ".attacker"))
            (dirty 'preserved)
            (fault-data (list 'make-temp-file 'text-write kind 73))
            old-identifier
            caught)
       (unwind-protect (progn
			 (with-temp-file output (insert "old"))
			 (with-temp-file attacker (insert "attacker"))
			 (setq old-identifier (nskk--dict-file-identifier output))
			 (setq caught (cl-letf
				       (((symbol-function 'make-temp-file)
					 (lambda (&rest _arguments)
					   (signal kind fault-data))))
				       (nskk-dictionary-test--capture-condition
					(lambda ()
					  (nskk--dict-call-with-atomic-file
					   output
					   (lambda ()
					     (insert "must-not-publish"))
					   (lambda ()
					     (setq dirty nil)))))))
			 (should (eq (car caught) kind))
			 (should (equal (cdr caught) fault-data))
			 (should (eq dirty 'preserved))
			 (should (equal (nskk--dict-file-identifier output) old-identifier))
			 (should (equal (nskk-dictionary-test--file-bytes output) "old"))
			 (should (equal (nskk-dictionary-test--file-bytes attacker) "attacker")))
         (when (file-directory-p temp-dir)
           (delete-directory temp-dir t)))))))
 (nskk-it
  "leaves a post-return replacement unowned and unpublished"
  (let* ((temp-dir (make-temp-file "nskk-atomic-replace-" t))
         (output (expand-file-name "user.skk" temp-dir))
         (dirty 'preserved)
         (original-make-temp-file (symbol-function 'make-temp-file))
         (original-set-file-modes (symbol-function 'set-file-modes))
         temp
         old-identifier
         caught)
    (unwind-protect (progn
		      (with-temp-file output (insert "old"))
		      (set-file-modes output #o640)
		      (setq old-identifier (nskk--dict-file-identifier output))
		      (setq caught (cl-letf
				    (((symbol-function 'make-temp-file)
				      (lambda (&rest arguments)
					(setq temp (apply original-make-temp-file arguments))))
				     ((symbol-function 'set-file-modes)
				      (lambda (path mode &optional flag)
					(if (equal path temp) (progn
								(delete-file path)
								(with-temp-file path (insert "attacker"))
								(funcall original-set-file-modes path #o600 flag))
					  (funcall original-set-file-modes path mode flag)))))
				    (nskk-dictionary-test--capture-condition
				     (lambda ()
				       (nskk--dict-call-with-atomic-file
					output
					(lambda ()
					  (insert "must-not-publish"))
					(lambda ()
					  (setq dirty nil)))))))
		      (should
		       (equal caught (list 'file-error "Atomic temporary file identity changed" temp)))
		      (should (eq dirty 'preserved))
		      (should (equal (nskk--dict-file-identifier output) old-identifier))
		      (should (equal (nskk-dictionary-test--file-bytes output) "old"))
		      (should (file-exists-p temp))
		      (should (equal (nskk-dictionary-test--file-bytes temp) "attacker")))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))
 (nskk-it
  "preserves the exact condition across precommit mode and id faults"
  (dolist (fault
           '(file-modes set-file-modes identifier-capture identifier-verify rename-file))
    (dolist (kind '(error quit))
      (ert-info
       ((format "%S %S" fault kind))
       (let* ((temp-dir (make-temp-file "nskk-atomic-precommit-" t))
              (output (expand-file-name "user.skk" temp-dir))
              (dirty 'preserved)
              (original-make-temp-file (symbol-function 'make-temp-file))
              (original-file-modes (symbol-function 'file-modes))
              (original-set-file-modes (symbol-function 'set-file-modes))
              (original-identifier (symbol-function 'nskk--dict-file-identifier))
              (original-rename-file (symbol-function 'rename-file))
              (fault-data (list fault kind 73))
              temp
              temp-identifier-calls
              old-identifier
              caught)
         (unwind-protect (progn
			   (with-temp-file output (insert "old"))
			   (set-file-modes output #o640)
			   (setq old-identifier (nskk--dict-file-identifier output))
			   (setq caught (cl-letf
					 (((symbol-function 'make-temp-file)
					   (lambda (&rest arguments)
					     (setq temp (apply original-make-temp-file arguments))))
					  ((symbol-function 'file-modes)
					   (lambda (path)
					     (if (and (eq fault 'file-modes) (equal path output)) (signal kind fault-data)
					       (funcall original-file-modes path))))
					  ((symbol-function 'set-file-modes)
					   (lambda (&rest arguments)
					     (if (and (eq fault 'set-file-modes) (equal (car arguments) temp)) (signal kind fault-data)
					       (apply original-set-file-modes arguments))))
					  ((symbol-function 'nskk--dict-file-identifier)
					   (lambda (path)
					     (if (equal path temp) (progn
								     (setq temp-identifier-calls (1+ (or temp-identifier-calls 0)))
								     (if (or
									  (and (eq fault 'identifier-capture) (= temp-identifier-calls 1))
									  (and (eq fault 'identifier-verify) (= temp-identifier-calls 2))) (signal kind fault-data)
								       (funcall original-identifier path)))
					       (funcall original-identifier path))))
					  ((symbol-function 'rename-file)
					   (lambda (&rest arguments)
					     (if (eq fault 'rename-file) (signal kind fault-data)
					       (apply original-rename-file arguments)))))
					 (nskk-dictionary-test--capture-condition
					  (lambda ()
					    (nskk--dict-call-with-atomic-file
					     output
					     (lambda ()
					       (insert "must-not-publish"))
					     (lambda ()
					       (setq dirty nil)))))))
			   (should (eq (car caught) kind))
			   (should (equal (cdr caught) fault-data))
			   (should (eq dirty 'preserved))
			   (should (equal (nskk--dict-file-identifier output) old-identifier))
			   (should (equal (nskk-dictionary-test--file-bytes output) "old"))
			   (should (= (logand (file-modes output) #o777) #o640))
			   (if (eq fault 'identifier-capture) (progn
								(should (file-exists-p temp))
								(should (equal (nskk-dictionary-test--file-bytes temp) "must-not-publish")))
			     (should-not (and temp (file-exists-p temp))))
			   (nskk--dict-call-with-atomic-file
			    output
			    (lambda ()
			      (insert "retry"))
			    (lambda ()
			      (setq dirty nil)))
			   (should-not dirty)
			   (should (equal (nskk-dictionary-test--file-bytes output) "retry"))
			   (should (= (logand (file-modes output) #o777) #o640)))
           (when (file-directory-p temp-dir)
             (delete-directory temp-dir t))))))))
 (nskk-it
  "publishes and clears dirty state on rename after-faults"
  (dolist (kind '(error quit))
    (ert-info
     ((format "%S" kind))
     (let* ((temp-dir (make-temp-file "nskk-atomic-rename-after-" t))
            (output (expand-file-name "user.skk" temp-dir))
            (dirty 'preserved)
            (original-make-temp-file (symbol-function 'make-temp-file))
            (original-rename-file (symbol-function 'rename-file))
            (fault-data (list 'rename-file 'after kind 73))
            temp-identifier
            callback-inhibit
            caught)
       (unwind-protect (progn
			 (with-temp-file output (insert "old"))
			 (set-file-modes output #o640)
			 (setq caught (cl-letf
				       (((symbol-function 'make-temp-file)
					 (lambda (&rest arguments)
					   (let ((temp (apply original-make-temp-file arguments)))
					     (setq temp-identifier (nskk--dict-file-identifier temp))
					     temp)))
					((symbol-function 'rename-file)
					 (lambda (&rest arguments)
					   (prog1
					       (apply original-rename-file arguments)
					     (signal kind fault-data)))))
				       (nskk-dictionary-test--capture-condition
					(lambda ()
					  (nskk--dict-call-with-atomic-file
					   output
					   (lambda ()
					     (insert "committed"))
					   (lambda ()
					     (setq callback-inhibit inhibit-quit
						   dirty nil)))))))
			 (should (eq (car caught) kind))
			 (should (equal (cdr caught) fault-data))
			 (should callback-inhibit)
			 (should-not dirty)
			 (should (equal (nskk-dictionary-test--file-bytes output) "committed"))
			 (should (equal (nskk--dict-file-identifier output) temp-identifier))
			 (should (= (logand (file-modes output) #o777) #o640))
			 (should
			  (equal
			   (directory-files temp-dir nil directory-files-no-dot-files-regexp)
			   '("user.skk"))))
         (when (file-directory-p temp-dir)
           (delete-directory temp-dir t)))))))
 (nskk-it
  "keeps committed state across save-message before and after faults"
  (dolist (position '(before after))
    (dolist (kind '(error quit))
      (ert-info
       ((format "%S %S" position kind))
       (let* ((temp-dir (make-temp-file "nskk-atomic-message-" t))
              (output (expand-file-name "user.skk" temp-dir))
              (nskk-dict-user-dictionary-file output)
              (nskk--user-dict-index 'user)
              (nskk-dict-modified t)
              (original-message (symbol-function 'message))
              (fault-data (list 'message position kind 73))
              caught)
         (unwind-protect (progn
			   (with-temp-file output (insert "old"))
			   (set-file-modes output #o640)
			   (setq caught (cl-letf
					 (((symbol-function 'nskk-prolog-query-bindings)
					   (lambda (&rest _arguments)
					     '(("reading" ("candidate")))))
					  ((symbol-function 'message)
					   (lambda (format-string &rest arguments)
					     (if (equal format-string "NSKK: User dictionary saved to %s") (progn
													     (when (eq position 'after)
													       (apply original-message format-string arguments))
													     (signal kind fault-data))
					       (apply original-message format-string arguments)))))
					 (nskk-dictionary-test--capture-condition #'nskk--dict-save-user-dictionary-1)))
			   (should (eq (car caught) kind))
			   (should (equal (cdr caught) fault-data))
			   (should-not nskk-dict-modified)
			   (should
			    (string-match-p "reading /candidate/" (nskk-dictionary-test--file-bytes output)))
			   (should (= (logand (file-modes output) #o777) #o640))
			   (should
			    (equal
			     (directory-files temp-dir nil directory-files-no-dot-files-regexp)
			     '("user.skk"))))
           (when (file-directory-p temp-dir)
             (delete-directory temp-dir t))))))))
 (nskk-it
  "cleans with quits inhibited without masking the primary fault"
  (dolist (primary-kind '(error quit))
    (dolist (cleanup-kind '(error quit))
      (ert-info
       ((format "%S then %S" primary-kind cleanup-kind))
       (let* ((temp-dir (make-temp-file "nskk-atomic-cleanup-" t))
              (output (expand-file-name "user.skk" temp-dir))
              (dirty 'preserved)
              (original-delete-file (symbol-function 'delete-file))
              (primary-data (list 'rename-file primary-kind 73))
              (cleanup-data (list 'delete-file cleanup-kind 91))
              cleanup-called
              cleanup-inhibit
              caught)
         (unwind-protect (progn
			   (with-temp-file output (insert "old"))
			   (setq caught (cl-letf
					 (((symbol-function 'rename-file)
					   (lambda (&rest _arguments)
					     (signal primary-kind primary-data)))
					  ((symbol-function 'delete-file)
					   (lambda (&rest arguments)
					     (setq cleanup-called t
						   cleanup-inhibit inhibit-quit)
					     (prog1
						 (apply original-delete-file arguments)
					       (signal cleanup-kind cleanup-data)))))
					 (nskk-dictionary-test--capture-condition
					  (lambda ()
					    (nskk--dict-call-with-atomic-file
					     output
					     (lambda ()
					       (insert "must-not-publish"))
					     (lambda ()
					       (setq dirty nil)))))))
			   (should (eq (car caught) primary-kind))
			   (should (equal (cdr caught) primary-data))
			   (should cleanup-called)
			   (should cleanup-inhibit)
			   (should (eq dirty 'preserved))
			   (should (equal (nskk-dictionary-test--file-bytes output) "old"))
			   (should
			    (equal
			     (directory-files temp-dir nil directory-files-no-dot-files-regexp)
			     '("user.skk"))))
           (when (file-directory-p temp-dir)
             (delete-directory temp-dir t)))))))))

(provide 'nskk-dictionary-test)

;;; nskk-dictionary-test.el ends here
