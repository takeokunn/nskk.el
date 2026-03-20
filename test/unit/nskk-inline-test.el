;;; nskk-inline-test.el --- Tests for nskk-inline.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Tests for nskk-inline.el covering:
;; - Function existence (fboundp)
;; - Customization variables (nskk-show-inline)
;; - Face definitions
;; - nskk--inline-build-horizontal: propertized string, nskk-inline-face
;; - nskk--inline-build-vertical: starts with newline, nskk-inline-face
;; - nskk-inline-show-candidate: no-op when nskk-show-inline is nil
;; - nskk-inline-show-candidate: no-op for empty/nil candidate
;; - nskk-inline-hide: safe when overlay is nil
;; - nskk-inline-show-registration-badge: no-op when nskk-show-inline is nil

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nskk-inline)
(require 'nskk-state)
(require 'nskk-test-framework)
(require 'nskk-test-macros)

;;;; Function Existence

(nskk-describe "nskk-inline function existence"
  (nskk-it "nskk-inline-show-candidate is defined"
    (should (fboundp 'nskk-inline-show-candidate)))
  (nskk-it "nskk-inline-hide is defined"
    (should (fboundp 'nskk-inline-hide)))
  (nskk-it "nskk-inline-show-registration-badge is defined"
    (should (fboundp 'nskk-inline-show-registration-badge)))
  (nskk-it "nskk--inline-anchor is defined"
    (should (fboundp 'nskk--inline-anchor)))
  (nskk-it "nskk--inline-build-horizontal is defined"
    (should (fboundp 'nskk--inline-build-horizontal)))
  (nskk-it "nskk--inline-build-vertical is defined"
    (should (fboundp 'nskk--inline-build-vertical))))

;;;; Customization Variables

(nskk-describe "nskk-inline customization variables"
  (nskk-it "nskk-show-inline is defined"
    (should (boundp 'nskk-show-inline)))
  (nskk-it "nskk-show-inline defaults to nil"
    (should (null nskk-show-inline))))

;;;; Face Definitions

(nskk-describe "nskk-inline faces"
  (nskk-it "nskk-inline-face is defined"
    (should (facep 'nskk-inline-face)))
  (nskk-it "nskk-jisyo-registration-badge-face is defined"
    (should (facep 'nskk-jisyo-registration-badge-face))))

;;;; Build Helpers

(nskk-describe "nskk--inline-build-horizontal"
  (nskk-it "returns a string"
    (should (stringp (nskk--inline-build-horizontal "候補"))))
  (nskk-it "contains the candidate text"
    (should (string-match-p "候補" (nskk--inline-build-horizontal "候補"))))
  (nskk-it "starts with a space separator"
    (should (string-prefix-p " " (nskk--inline-build-horizontal "候補"))))
  (nskk-it "applies nskk-inline-face"
    (let ((result (nskk--inline-build-horizontal "test")))
      (should (eq (get-text-property 0 'face result) 'nskk-inline-face)))))

(nskk-describe "nskk--inline-build-vertical"
  (nskk-it "returns a string"
    (should (stringp (nskk--inline-build-vertical "候補"))))
  (nskk-it "contains the candidate text"
    (should (string-match-p "候補" (nskk--inline-build-vertical "候補"))))
  (nskk-it "starts with a newline"
    (should (string-prefix-p "\n" (nskk--inline-build-vertical "候補"))))
  (nskk-it "applies nskk-inline-face"
    (let ((result (nskk--inline-build-vertical "test")))
      (should (eq (get-text-property 0 'face result) 'nskk-inline-face)))))

;;;; Anchor

(nskk-describe "nskk--inline-anchor"
  (nskk-it "returns point when conversion overlay is nil"
    (with-temp-buffer
      (insert "abc")
      (goto-char 2)
      (let ((nskk--conversion-overlay nil))
        (should (= (nskk--inline-anchor) (point)))))))

;;;; Show Candidate Guards

(nskk-describe "nskk-inline-show-candidate"
  (nskk-it "is a no-op when nskk-show-inline is nil"
    (with-temp-buffer
      (let ((nskk-show-inline nil)
            (nskk--inline-overlay nil))
        (nskk-inline-show-candidate "候補")
        (should (null nskk--inline-overlay)))))

  (nskk-it "is a no-op for nil candidate"
    (with-temp-buffer
      (let ((nskk-show-inline t)
            (nskk--inline-overlay nil))
        (nskk-inline-show-candidate nil)
        (should (null nskk--inline-overlay)))))

  (nskk-it "is a no-op for empty string candidate"
    (with-temp-buffer
      (let ((nskk-show-inline t)
            (nskk--inline-overlay nil))
        (nskk-inline-show-candidate "")
        (should (null nskk--inline-overlay)))))

  (nskk-it "creates overlay when nskk-show-inline is t"
    (with-temp-buffer
      (insert "あ")
      (let ((nskk-show-inline t)
            (nskk--inline-overlay nil)
            (nskk--conversion-overlay nil))
        (nskk-inline-show-candidate "亜")
        (unwind-protect
            (should (overlayp nskk--inline-overlay))
          (nskk-delete-overlay nskk--inline-overlay)))))

  (nskk-it "creates overlay when nskk-show-inline is vertical"
    (with-temp-buffer
      (insert "あ")
      (let ((nskk-show-inline 'vertical)
            (nskk--inline-overlay nil)
            (nskk--conversion-overlay nil))
        (nskk-inline-show-candidate "亜")
        (unwind-protect
            (should (overlayp nskk--inline-overlay))
          (nskk-delete-overlay nskk--inline-overlay))))))

;;;; Hide

(nskk-describe "nskk-inline-hide"
  (nskk-it "is safe to call when overlay is nil"
    (with-temp-buffer
      (let ((nskk--inline-overlay nil))
        (should-not (condition-case err
                        (progn (nskk-inline-hide) nil)
                      (error err))))))

  (nskk-it "deletes existing overlay"
    (with-temp-buffer
      (insert "あ")
      (let ((nskk--inline-overlay (make-overlay 1 1)))
        (nskk-inline-hide)
        (should (null nskk--inline-overlay))))))

;;;; Registration Badge

(nskk-describe "nskk-inline-show-registration-badge"
  (nskk-it "is a no-op when nskk-show-inline is nil"
    (with-temp-buffer
      (let ((nskk-show-inline nil)
            (nskk--inline-overlay nil))
        (nskk-inline-show-registration-badge)
        (should (null nskk--inline-overlay)))))

  (nskk-it "creates overlay when nskk-show-inline is t"
    (with-temp-buffer
      (insert "あ")
      (let ((nskk-show-inline t)
            (nskk--inline-overlay nil)
            (nskk--conversion-overlay nil))
        (nskk-inline-show-registration-badge)
        (unwind-protect
            (should (overlayp nskk--inline-overlay))
          (nskk-delete-overlay nskk--inline-overlay))))))

(provide 'nskk-inline-test)

;;; nskk-inline-test.el ends here
