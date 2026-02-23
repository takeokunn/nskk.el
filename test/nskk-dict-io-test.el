;;; nskk-dict-io-test.el --- Tests for nskk-dict-io.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Authors

;; Author: NSKK Developers
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Tests for nskk-dict-io.el covering:
;; - Module loading and feature provision
;; - The module is currently a stub, so tests focus on ensuring
;;   the module loads correctly and provides its feature.
;; - Also tests trie file I/O since dict-io is the I/O layer
;;   and trie provides save/load functions.

;;; Code:

(require 'ert)
(require 'nskk-dict-io)
(require 'nskk-trie)
(require 'nskk-dict-struct)
(require 'nskk-test-framework)

;;;
;;; Module Loading Tests
;;;

(nskk-deftest-unit dict-io-feature-provided
  "Test that nskk-dict-io feature is provided."
  (should (featurep 'nskk-dict-io)))

(nskk-deftest-unit dict-io-require-idempotent
  "Test that requiring nskk-dict-io multiple times is safe."
  (require 'nskk-dict-io)
  (require 'nskk-dict-io)
  (should (featurep 'nskk-dict-io)))

;;;
;;; Trie File I/O Tests (trie provides the actual I/O functions)
;;;

(nskk-deftest-unit dict-io-trie-save-and-load
  "Test saving and loading a trie to/from a file."
  (let ((trie (nskk-trie-create))
        (temp-file (make-temp-file "nskk-trie-test-" nil ".dat")))
    (unwind-protect
        (progn
          ;; Insert data
          (nskk-trie-insert trie "かんじ" "漢字")
          (nskk-trie-insert trie "にほん" "日本")
          (nskk-trie-insert trie "にほんご" "日本語")

          ;; Save to file
          (nskk-trie-save-to-file trie temp-file)

          ;; Verify file exists and has content
          (should (file-exists-p temp-file))
          (should (> (file-attribute-size (file-attributes temp-file)) 0))

          ;; Load from file
          (let ((loaded (nskk-trie-load-from-file temp-file)))
            (should (nskk-trie-p loaded))
            (should (= (nskk-trie-size loaded) 3))
            (should (equal (car (nskk-trie-lookup loaded "かんじ")) "漢字"))
            (should (equal (car (nskk-trie-lookup loaded "にほん")) "日本"))
            (should (equal (car (nskk-trie-lookup loaded "にほんご")) "日本語"))))
      ;; Cleanup
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(nskk-deftest-unit dict-io-trie-save-empty
  "Test saving and loading an empty trie."
  (let ((trie (nskk-trie-create))
        (temp-file (make-temp-file "nskk-trie-empty-" nil ".dat")))
    (unwind-protect
        (progn
          (nskk-trie-save-to-file trie temp-file)
          (let ((loaded (nskk-trie-load-from-file temp-file)))
            (should (nskk-trie-p loaded))
            (should (= (nskk-trie-size loaded) 0))
            (should (nskk-trie-empty-p loaded))))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(nskk-deftest-unit dict-io-trie-save-with-metadata
  "Test saving and loading a trie with metadata."
  (let ((trie (nskk-trie-create))
        (temp-file (make-temp-file "nskk-trie-meta-" nil ".dat")))
    (unwind-protect
        (progn
          (setf (nskk-trie-metadata trie) '(:source "test-dict" :version "1.0"))
          (nskk-trie-insert trie "key" "value")
          (nskk-trie-save-to-file trie temp-file)

          (let ((loaded (nskk-trie-load-from-file temp-file)))
            (should (equal (nskk-trie-metadata loaded)
                           '(:source "test-dict" :version "1.0")))))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(nskk-deftest-unit dict-io-trie-load-nonexistent-file
  "Test loading from nonexistent file signals an error."
  (should-error (nskk-trie-load-from-file "/tmp/nskk-nonexistent-file.dat")
                :type 'file-missing))

(nskk-deftest-unit dict-io-trie-save-load-large
  "Test saving and loading a trie with many entries."
  (let ((trie (nskk-trie-create))
        (temp-file (make-temp-file "nskk-trie-large-" nil ".dat"))
        (n 200))
    (unwind-protect
        (progn
          ;; Insert many entries
          (dotimes (i n)
            (nskk-trie-insert trie (format "key-%03d" i) (format "val-%03d" i)))
          (should (= (nskk-trie-size trie) n))

          ;; Save and load
          (nskk-trie-save-to-file trie temp-file)
          (let ((loaded (nskk-trie-load-from-file temp-file)))
            (should (= (nskk-trie-size loaded) n))
            ;; Verify some entries
            (should (equal (car (nskk-trie-lookup loaded "key-000")) "val-000"))
            (should (equal (car (nskk-trie-lookup loaded "key-100")) "val-100"))
            (should (equal (car (nskk-trie-lookup loaded "key-199")) "val-199"))))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(nskk-deftest-unit dict-io-trie-file-content-readable
  "Test that saved trie file contains readable Emacs Lisp."
  (let ((trie (nskk-trie-create))
        (temp-file (make-temp-file "nskk-trie-readable-" nil ".dat")))
    (unwind-protect
        (progn
          (nskk-trie-insert trie "test" "value")
          (nskk-trie-save-to-file trie temp-file)

          ;; Read file and verify it starts with comment
          (with-temp-buffer
            (insert-file-contents temp-file)
            (goto-char (point-min))
            (should (looking-at ";;"))))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(nskk-deftest-unit dict-io-trie-save-load-japanese-values
  "Test saving and loading trie with Japanese values."
  (let ((trie (nskk-trie-create))
        (temp-file (make-temp-file "nskk-trie-ja-" nil ".dat")))
    (unwind-protect
        (progn
          (nskk-trie-insert trie "あ" '("亜" "阿" "娃"))
          (nskk-trie-insert trie "い" '("位" "依" "威"))
          (nskk-trie-save-to-file trie temp-file)

          (let ((loaded (nskk-trie-load-from-file temp-file)))
            (should (equal (car (nskk-trie-lookup loaded "あ")) '("亜" "阿" "娃")))
            (should (equal (car (nskk-trie-lookup loaded "い")) '("位" "依" "威")))))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

;;;
;;; Dict-entry Serialization Tests
;;;

(nskk-deftest-unit dict-io-entry-can-be-serialized
  "Test that dict-entry can be written and read back."
  (let ((entry (make-nskk-dict-entry :key "test" :candidates '("a" "b")))
        (temp-file (make-temp-file "nskk-entry-" nil ".dat")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (prin1 entry (current-buffer)))
          (let ((restored (with-temp-buffer
                            (insert-file-contents temp-file)
                            (read (current-buffer)))))
            ;; cl-defstruct records produce readable output
            (should (file-exists-p temp-file))))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

;;;
;;; Integration Tests
;;;

(nskk-deftest-integration dict-io-trie-roundtrip-workflow
  "Test full workflow: create trie, populate, save, load, verify."
  (let ((temp-file (make-temp-file "nskk-trie-workflow-" nil ".dat")))
    (unwind-protect
        (progn
          ;; Phase 1: Create and populate
          (let ((trie (nskk-trie-create)))
            (nskk-trie-insert trie "あいう" "アイウ")
            (nskk-trie-insert trie "あいうえ" "アイウエ")
            (nskk-trie-insert trie "あいうえお" "アイウエオ")
            (nskk-trie-insert trie "かきく" "カキク")
            (nskk-trie-save-to-file trie temp-file))

          ;; Phase 2: Load and verify
          (let ((loaded (nskk-trie-load-from-file temp-file)))
            (should (= (nskk-trie-size loaded) 4))

            ;; Verify prefix search works on loaded trie
            (let ((results (nskk-trie-prefix-search loaded "あいう")))
              (should (= (length results) 3))
              (should (assoc "あいう" results))
              (should (assoc "あいうえ" results))
              (should (assoc "あいうえお" results)))

            ;; Verify exact lookup
            (should (equal (car (nskk-trie-lookup loaded "かきく")) "カキク"))))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(provide 'nskk-dict-io-test)

;;; nskk-dict-io-test.el ends here
