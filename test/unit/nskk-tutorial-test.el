;;; nskk-tutorial-test.el --- Unit tests for nskk-tutorial  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: i18n, testing

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Unit tests for the NSKK tutorial module.

;;; Code:

(require 'ert)
(require 'nskk-test-framework)
(require 'nskk-tutorial)


;;;;
;;;; Lesson Data Integrity
;;;;

(ert-deftest nskk-tutorial-test/lessons-not-empty ()
  "Tutorial should have at least one lesson."
  (should (> (length nskk-tutorial--lessons) 0)))

(ert-deftest nskk-tutorial-test/lessons-count ()
  "Tutorial should have exactly 15 lessons."
  (should (= (length nskk-tutorial--lessons) 15)))

(ert-deftest nskk-tutorial-test/lessons-have-required-keys ()
  "Every lesson must have :title, :explanation, and :exercises."
  (dolist (lesson nskk-tutorial--lessons)
    (should (plist-get lesson :title))
    (should (plist-get lesson :explanation))
    (should (plist-get lesson :exercises))))

(ert-deftest nskk-tutorial-test/exercises-have-required-keys ()
  "Every exercise must have :instruction, :hint, and either :expected or :validator."
  (dolist (lesson nskk-tutorial--lessons)
    (dolist (ex (plist-get lesson :exercises))
      (should (plist-get ex :instruction))
      (should (plist-get ex :hint))
      (should (or (plist-get ex :expected)
                  (plist-get ex :validator))))))

(ert-deftest nskk-tutorial-test/lesson-titles-are-strings ()
  "All lesson titles should be strings."
  (dolist (lesson nskk-tutorial--lessons)
    (should (stringp (plist-get lesson :title)))))

(ert-deftest nskk-tutorial-test/lesson-explanations-are-strings ()
  "All lesson explanations should be strings."
  (dolist (lesson nskk-tutorial--lessons)
    (should (stringp (plist-get lesson :explanation)))))

(ert-deftest nskk-tutorial-test/exercises-are-lists ()
  "All lesson exercises should be non-empty lists."
  (dolist (lesson nskk-tutorial--lessons)
    (let ((exercises (plist-get lesson :exercises)))
      (should (listp exercises))
      (should (> (length exercises) 0)))))


;;;;
;;;; Mini Dictionary Integrity
;;;;

(ert-deftest nskk-tutorial-test/mini-dict-not-empty ()
  "Mini dictionary should have entries."
  (should (> (length nskk-tutorial--mini-dict) 0)))

(ert-deftest nskk-tutorial-test/mini-dict-entries-are-valid ()
  "Each mini dictionary entry should be a cons of (string . list-of-strings)."
  (dolist (entry nskk-tutorial--mini-dict)
    (should (consp entry))
    (should (stringp (car entry)))
    (should (listp (cdr entry)))
    (should (> (length (cdr entry)) 0))
    (dolist (candidate (cdr entry))
      (should (stringp candidate)))))

(ert-deftest nskk-tutorial-test/mini-dict-covers-basic-exercises ()
  "Mini dictionary should contain entries for basic conversion exercises."
  (let ((keys (mapcar #'car nskk-tutorial--mini-dict)))
    (should (member "かんじ" keys))
    (should (member "へんかん" keys))
    (should (member "にほんご" keys))
    (should (member "さくら" keys))
    (should (member "うみ" keys))))

(ert-deftest nskk-tutorial-test/mini-dict-covers-okurigana ()
  "Mini dictionary should contain okurigana entries for lesson 6."
  (let ((keys (mapcar #'car nskk-tutorial--mini-dict)))
    (should (member "かk" keys))
    (should (member "よm" keys))
    (should (member "みr" keys))))

(ert-deftest nskk-tutorial-test/mini-dict-covers-dcomp ()
  "Mini dictionary should contain dcomp prefix entries for lesson 10."
  (let ((keys (mapcar #'car nskk-tutorial--mini-dict)))
    (should (member "かんけい" keys))
    (should (member "かんきょう" keys))
    (should (member "かんたん" keys))))

(ert-deftest nskk-tutorial-test/mini-dict-covers-candidate-list ()
  "Mini dictionary should have enough candidates for lesson 11."
  (let ((entry (assoc "こうえん" nskk-tutorial--mini-dict)))
    (should entry)
    (should (>= (length (cdr entry)) 7))))

(ert-deftest nskk-tutorial-test/mini-dict-covers-advanced ()
  "Mini dictionary should contain entries for advanced lessons 12-14."
  (let ((keys (mapcar #'car nskk-tutorial--mini-dict)))
    (should (member "でんわ" keys))
    (should (member "がっこう" keys))
    (should (member "ぶんしょう" keys))
    (should (member "さくせい" keys))
    (should (member "かんせい" keys))))

(ert-deftest nskk-tutorial-test/mini-dict-kanji-has-multiple-candidates ()
  "The かんじ entry should have multiple candidates for lesson 5."
  (let ((entry (assoc "かんじ" nskk-tutorial--mini-dict)))
    (should entry)
    (should (>= (length (cdr entry)) 3))))


;;;;
;;;; Validator Functions
;;;;

(ert-deftest nskk-tutorial-test/validate-hiragana-mode-when-active ()
  "Hiragana mode validator should return t when in hiragana mode."
  (with-temp-buffer
    (nskk-with-mock-dict nil
      (nskk-mode 1)
      (nskk--set-mode 'hiragana)
      (should (nskk-tutorial--validate-hiragana-mode))
      (ignore-errors (nskk-mode -1)))))

(ert-deftest nskk-tutorial-test/validate-hiragana-mode-when-ascii ()
  "Hiragana mode validator should return nil when in ascii mode."
  (with-temp-buffer
    (nskk-with-mock-dict nil
      (nskk-mode 1)
      (nskk--set-mode 'ascii)
      (should-not (nskk-tutorial--validate-hiragana-mode))
      (ignore-errors (nskk-mode -1)))))

(ert-deftest nskk-tutorial-test/validate-hiragana-mode-when-no-nskk ()
  "Hiragana mode validator should return nil when nskk-mode is off."
  (with-temp-buffer
    (should-not (nskk-tutorial--validate-hiragana-mode))))


;;;;
;;;; Face Definitions
;;;;

(ert-deftest nskk-tutorial-test/faces-defined ()
  "All tutorial faces should be defined."
  (should (facep 'nskk-tutorial-header-face))
  (should (facep 'nskk-tutorial-instruction-face))
  (should (facep 'nskk-tutorial-input-area-face))
  (should (facep 'nskk-tutorial-success-face))
  (should (facep 'nskk-tutorial-hint-face)))


;;;;
;;;; Prolog DB Isolation
;;;;

(ert-deftest nskk-tutorial-test/copy-hash-table-basic ()
  "Deep-copying a hash table should produce an independent copy."
  (let ((original (make-hash-table :test 'equal)))
    (puthash "key1" '(a b c) original)
    (puthash "key2" '(d e f) original)
    (let ((copy (nskk-tutorial--copy-hash-table original)))
      (should (equal (gethash "key1" copy) '(a b c)))
      (should (equal (gethash "key2" copy) '(d e f)))
      ;; Mutation should not affect original
      (puthash "key1" '(x y z) copy)
      (should (equal (gethash "key1" original) '(a b c))))))

(ert-deftest nskk-tutorial-test/copy-hash-table-nested ()
  "Deep-copying should handle nested hash tables."
  (let ((outer (make-hash-table :test 'equal))
        (inner (make-hash-table :test 'equal)))
    (puthash "a" 1 inner)
    (puthash "nested" inner outer)
    (let ((copy (nskk-tutorial--copy-hash-table outer)))
      (should (= (gethash "a" (gethash "nested" copy)) 1))
      (puthash "a" 99 (gethash "nested" copy))
      (should (= (gethash "a" inner) 1)))))


;;;;
;;;; Buffer Name
;;;;

(ert-deftest nskk-tutorial-test/buffer-name ()
  "Tutorial buffer should be named *NSKK Tutorial*."
  (should (string= nskk-tutorial--buffer-name "*NSKK Tutorial*")))


;;;;
;;;; Header Line
;;;;

(ert-deftest nskk-tutorial-test/header-line-format ()
  "Header line should include lesson number and title."
  (with-temp-buffer
    (setq-local nskk-tutorial--current-lesson 0)
    (let ((header (nskk-tutorial--header-line)))
      (should (string-match-p "レッスン 1/" header))
      (should (string-match-p "はじめに" header)))))

(ert-deftest nskk-tutorial-test/advanced-lessons-exist ()
  "Lessons 9-15 should exist with expected titles."
  (let ((titles (mapcar (lambda (l) (plist-get l :title))
                        nskk-tutorial--lessons)))
    (should (string-match-p "Abbrev" (nth 8 titles)))
    (should (string-match-p "動的補完" (nth 9 titles)))
    (should (string-match-p "候補リスト" (nth 10 titles)))
    (should (string-match-p "Sticky" (nth 11 titles)))
    (should (string-match-p "数値変換" (nth 12 titles)))
    (should (string-match-p "AZIK" (nth 13 titles)))
    (should (string-match-p "総合練習" (nth 14 titles)))))


(provide 'nskk-tutorial-test)

;;; nskk-tutorial-test.el ends here
