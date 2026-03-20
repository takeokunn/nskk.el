;;; nskk-annotation-test.el --- Tests for nskk-annotation.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Tests for nskk-annotation.el covering:
;; - Function existence (fboundp)
;; - Prolog initialization (nskk-annotation-initialize)
;; - Annotation registration and lookup
;; - Annotation format helper
;; - Display toggle (nskk--annotation-visible state)
;; - nskk-annotation-clear
;; - nskk-annotation-show-for-candidate guard conditions

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nskk-annotation)
(require 'nskk-prolog)
(require 'nskk-test-framework)
(require 'nskk-test-macros)

;;;; Function Existence

(nskk-describe "nskk-annotation function existence"
  (nskk-it "nskk-annotation-initialize is defined"
    (should (fboundp 'nskk-annotation-initialize)))
  (nskk-it "nskk-annotation-register is defined"
    (should (fboundp 'nskk-annotation-register)))
  (nskk-it "nskk-annotation-lookup is defined"
    (should (fboundp 'nskk-annotation-lookup)))
  (nskk-it "nskk-annotation-clear is defined"
    (should (fboundp 'nskk-annotation-clear)))
  (nskk-it "nskk-annotation-toggle-display is defined"
    (should (fboundp 'nskk-annotation-toggle-display)))
  (nskk-it "nskk-annotation-show-for-candidate is defined"
    (should (fboundp 'nskk-annotation-show-for-candidate))))

;;;; Initialization

(nskk-describe "nskk-annotation-initialize"
  (nskk-it "is idempotent"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk--annotation-initialized nil))
        (nskk-annotation-initialize)
        (nskk-annotation-initialize)
        (should nskk--annotation-initialized)))))

;;;; Register and Lookup

(nskk-describe "nskk-annotation-register and lookup"
  (nskk-it "registers and retrieves an annotation"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk--annotation-initialized nil))
        (nskk-annotation-initialize)
        (nskk-annotation-register "かんじ" "漢字" "common kanji")
        (should (equal (nskk-annotation-lookup "かんじ" "漢字") "common kanji")))))

  (nskk-it "returns nil for unregistered reading+candidate"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk--annotation-initialized nil))
        (nskk-annotation-initialize)
        (should (null (nskk-annotation-lookup "unknown" "候補"))))))

  (nskk-it "returns nil when not initialized"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk--annotation-initialized nil))
        (should (null (nskk-annotation-lookup "かんじ" "漢字"))))))

  (nskk-it "distinguishes different readings for same candidate"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk--annotation-initialized nil))
        (nskk-annotation-initialize)
        (nskk-annotation-register "かんじ" "漢字" "Chinese character")
        (nskk-annotation-register "かんじる" "感じる" "to feel")
        (should (equal (nskk-annotation-lookup "かんじ" "漢字") "Chinese character"))
        (should (equal (nskk-annotation-lookup "かんじる" "感じる") "to feel")))))

  (nskk-it "distinguishes different candidates for same reading"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk--annotation-initialized nil))
        (nskk-annotation-initialize)
        (nskk-annotation-register "かんじ" "漢字" "Chinese character")
        (nskk-annotation-register "かんじ" "感じ" "feeling")
        (should (equal (nskk-annotation-lookup "かんじ" "漢字") "Chinese character"))
        (should (equal (nskk-annotation-lookup "かんじ" "感じ") "feeling"))))))

;;;; Format Helper

(nskk-describe "nskk--annotation-format"
  (nskk-it "returns nil for nil annotation"
    (should (null (nskk--annotation-format nil))))
  (nskk-it "returns nil for empty string annotation"
    (should (null (nskk--annotation-format ""))))
  (nskk-it "returns a propertized string for non-empty annotation"
    (let ((result (nskk--annotation-format "test annotation")))
      (should (stringp result))
      (should (string-match-p "test annotation" result))
      (should (string-prefix-p " [" result))
      (should (string-suffix-p "]" result))))
  (nskk-it "applies nskk-annotation-face"
    (let ((result (nskk--annotation-format "hello")))
      (should (eq (get-text-property 0 'face result) 'nskk-annotation-face)))))

;;;; Clear

(nskk-describe "nskk-annotation-clear"
  (nskk-it "clears nskk--annotation-current"
    (with-temp-buffer
      (setq nskk--annotation-current "some annotation")
      (nskk-annotation-clear)
      (should (null nskk--annotation-current)))))

;;;; Show for Candidate Guards

(nskk-describe "nskk-annotation-show-for-candidate"
  (nskk-it "is a no-op when nskk-show-annotation is nil"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk--annotation-initialized nil))
        (nskk-annotation-initialize)
        (nskk-annotation-register "よみ" "候補" "note")
        (with-temp-buffer
          (let ((nskk-show-annotation nil))
            ;; Should not error; annotation-current stays nil
            (nskk-annotation-show-for-candidate "よみ" "候補")
            (should (null nskk--annotation-current)))))))

  (nskk-it "sets annotation-current when nskk-show-annotation is t"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk--annotation-initialized nil))
        (nskk-annotation-initialize)
        (nskk-annotation-register "よみ" "候補" "note text")
        (with-temp-buffer
          (let ((nskk-show-annotation t)
                (message-log-max nil))
            (nskk-annotation-show-for-candidate "よみ" "候補")
            (should (equal nskk--annotation-current "note text")))))))

  (nskk-it "sets annotation-current to nil for candidate without annotation"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk--annotation-initialized nil))
        (nskk-annotation-initialize)
        (with-temp-buffer
          (setq nskk--annotation-current "stale")
          (let ((nskk-show-annotation t)
                (message-log-max nil))
            (nskk-annotation-show-for-candidate "よみ" "無注釈")
            (should (null nskk--annotation-current))))))))

;;;; Toggle Display

(nskk-describe "nskk-annotation-toggle-display"
  (nskk-it "toggles nskk--annotation-visible"
    (with-temp-buffer
      (let ((nskk--annotation-visible t))
        (nskk-annotation-toggle-display)
        (should (null nskk--annotation-visible))
        (nskk-annotation-toggle-display)
        (should nskk--annotation-visible))))

  (nskk-it "does not error when toggling off with no current annotation"
    (with-temp-buffer
      (let ((nskk--annotation-visible t)
            (nskk--annotation-current nil)
            (message-log-max nil))
        (should-not (condition-case err
                        (progn (nskk-annotation-toggle-display) nil)
                      (error err))))))

  (nskk-it "does not error when toggling on with a current annotation"
    (with-temp-buffer
      (let ((nskk--annotation-visible nil)
            (nskk--annotation-current "test note")
            (message-log-max nil))
        (should-not (condition-case err
                        (progn (nskk-annotation-toggle-display) nil)
                      (error err)))))))

;;;; Load from Candidates Helper

(nskk-describe "nskk--annotation-load-from-candidates"
  (nskk-it "registers annotations from pair list"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk--annotation-initialized nil))
        (nskk-annotation-initialize)
        (nskk--annotation-load-from-candidates
         "よみ"
         '(("候補1" . "annotation1") ("候補2" . nil) ("候補3" . "annotation3")))
        (should (equal (nskk-annotation-lookup "よみ" "候補1") "annotation1"))
        (should (null  (nskk-annotation-lookup "よみ" "候補2")))
        (should (equal (nskk-annotation-lookup "よみ" "候補3") "annotation3")))))

  (nskk-it "skips empty annotation strings"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk--annotation-initialized nil))
        (nskk-annotation-initialize)
        (nskk--annotation-load-from-candidates
         "よみ"
         '(("候補" . "")))
        (should (null (nskk-annotation-lookup "よみ" "候補")))))))

(provide 'nskk-annotation-test)

;;; nskk-annotation-test.el ends here
