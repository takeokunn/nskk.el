;;; nskk-inline-test.el --- Tests for nskk-inline.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Tests for nskk-inline.el.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nskk-inline)
(require 'nskk-state)
(require 'nskk-prolog)
(require 'nskk-test-framework)
(require 'nskk-test-macros)

;;;; Customization Variables

(nskk-describe "nskk-inline customization variables"
  (nskk-it "nskk-show-inline defaults to nil"
    (should (null nskk-show-inline))))

;;;; Rendered After-String Content

(nskk-deftest-table
  inline-show-candidate-after-string
  :columns
  (style expected-after)
  :rows
  ((t " 亜")
   (horizontal " 亜")
   (vertical "\n亜"))
  :description
  "nskk-inline-show-candidate installs the exact rendered after-string,
fully faced with nskk-inline-face"
  :body
  (with-temp-buffer
    (insert "あ")
    (let ((nskk-show-inline style)
          (nskk--inline-overlay nil))
      (nskk-state-set-conversion-overlay nil)
      (unwind-protect
          (progn
            (nskk-inline-show-candidate "亜")
            (let ((after (overlay-get nskk--inline-overlay 'after-string)))
              (should (equal after expected-after))
              (dotimes (index (length after))
                (should (eq (get-text-property index 'face after)
                            'nskk-inline-face)))))
        (nskk-delete-overlay nskk--inline-overlay)))))

;;;; Anchor

(nskk-describe "nskk--inline-anchor"
  (nskk-it "returns point when conversion overlay is nil"
    (with-temp-buffer
      (insert "abc")
      (goto-char 2)
      (nskk-state-set-conversion-overlay nil)
      (should (= (nskk--inline-anchor) (point)))))

  (nskk-it "returns the conversion overlay's end when it exists"
    (with-temp-buffer
      (insert "abcdef")
      (goto-char 6)
      (let ((conversion-overlay (make-overlay 2 4)))
        (unwind-protect
            (progn
              (nskk-state-set-conversion-overlay conversion-overlay)
              (should (= (nskk--inline-anchor) 4))
              (should (/= (nskk--inline-anchor) (point))))
          (delete-overlay conversion-overlay)
          (nskk-state-set-conversion-overlay nil))))))

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
            (nskk--inline-overlay nil))
        (nskk-state-set-conversion-overlay nil)
        (nskk-inline-show-candidate "亜")
        (unwind-protect
            (should (overlayp nskk--inline-overlay))
          (nskk-delete-overlay nskk--inline-overlay)))))

  (nskk-it "creates overlay when nskk-show-inline is vertical"
    (with-temp-buffer
      (insert "あ")
      (let ((nskk-show-inline 'vertical)
            (nskk--inline-overlay nil))
        (nskk-state-set-conversion-overlay nil)
        (nskk-inline-show-candidate "亜")
        (unwind-protect
            (should (overlayp nskk--inline-overlay))
          (nskk-delete-overlay nskk--inline-overlay)))))

  (nskk-it "reuses the existing overlay object across calls"
    (with-temp-buffer
      (insert "あ")
      (let ((nskk-show-inline t)
            (nskk--inline-overlay nil))
        (nskk-state-set-conversion-overlay nil)
        (unwind-protect
            (progn
              (nskk-inline-show-candidate "亜")
              (let ((first-overlay nskk--inline-overlay))
                (should (overlayp first-overlay))
                (nskk-inline-show-candidate "唖")
                (should (eq nskk--inline-overlay first-overlay))
                (should (equal (overlay-get nskk--inline-overlay 'after-string)
                               " 唖"))))
          (nskk-delete-overlay nskk--inline-overlay)))))

  (nskk-it "sets the overlay priority to nskk-overlay-priority-inline"
    (with-temp-buffer
      (insert "あ")
      (let ((nskk-show-inline t)
            (nskk--inline-overlay nil))
        (nskk-state-set-conversion-overlay nil)
        (nskk-inline-show-candidate "亜")
        (unwind-protect
            (should (= (overlay-get nskk--inline-overlay 'priority)
                       nskk-overlay-priority-inline))
          (nskk-delete-overlay nskk--inline-overlay))))))

;;;; Hide

(nskk-describe "nskk-inline-hide"
  (nskk-it "is safe to call when overlay is nil"
    (with-temp-buffer
      (let ((nskk--inline-overlay nil))
        (nskk-should-not-error (nskk-inline-hide)))))

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
            (nskk--inline-overlay nil))
        (nskk-state-set-conversion-overlay nil)
        (nskk-inline-show-registration-badge)
        (unwind-protect
            (should (overlayp nskk--inline-overlay))
          (nskk-delete-overlay nskk--inline-overlay)))))

  (nskk-it "sets after-string to an unfaced newline followed by the faced badge"
    (with-temp-buffer
      (insert "あ")
      (let ((nskk-show-inline t)
            (nskk--inline-overlay nil))
        (nskk-state-set-conversion-overlay nil)
        (nskk-inline-show-registration-badge)
        (unwind-protect
            (let ((after (overlay-get nskk--inline-overlay 'after-string)))
              (should (equal after (concat "\n" "↓辞書登録中↓")))
              (should (null (get-text-property 0 'face after)))
              (cl-loop for index from 1 below (length after)
                       do (should (eq (get-text-property index 'face after)
                                      'nskk-jisyo-registration-badge-face))))
          (nskk-delete-overlay nskk--inline-overlay))))))

;;;; Presentation Action Registration

(nskk-describe "nskk-inline presentation action registration"
  (nskk-it "registers this module's callback for cleanup"
    (should (memq 'nskk-inline-hide (nskk-prolog-presentation-actions 'cleanup))))

  (nskk-it "registers this module's callback for finalize"
    (should (memq 'nskk--inline-finalize
                  (nskk-prolog-presentation-actions 'finalize))))

  (nskk-it "registers this module's callback for show-candidate"
    (should (memq 'nskk-inline-show-candidate
                  (nskk-prolog-presentation-actions 'show-candidate))))

  (nskk-it "registers this module's callback for show-registration-badge"
    (should (memq 'nskk-inline-show-registration-badge
                  (nskk-prolog-presentation-actions 'show-registration-badge))))

  ;; `nskk-inline-hide' and `nskk--inline-finalize' have identical bodies, so a
  ;; dedup would naturally register the former for both phases.  That silently
  ;; removes the second sweep: `nskk--run-presentation-actions' swallows a
  ;; signalling cleanup callback, and only a separately-named finalize callback
  ;; still deletes the overlay.
  (nskk-it "keeps finalize on a callback the cleanup phase does not share"
    (let ((finalize (nskk-prolog-presentation-actions 'finalize)))
      (should (memq 'nskk--inline-finalize finalize))
      (should-not (memq 'nskk-inline-hide finalize)))))

;;;; Sanitization

(nskk-describe "untrusted inline display properties"
  (nskk-it "sanitizes candidates before applying inline face"
    (dolist (spec '((t " 候補")
                    (vertical "\n候補")))
      (let* ((source (propertize "候補"
                                  'display "spoofed"
                                  'keymap (make-sparse-keymap)
                                  'local-map (make-sparse-keymap)
                                  'mouse-face 'highlight
                                  'help-echo "untrusted"
                                  'face 'error
                                  'nskk-no-learn t))
             (source-copy (copy-sequence source))
             (rendered (nskk--inline-render source (car spec))))
        (should (equal (substring-no-properties rendered) (cadr spec)))
        (dolist (property '(display keymap local-map mouse-face help-echo))
          (should-not
           (text-property-not-all 0 (length rendered) property nil rendered)))
        (dotimes (index (length rendered))
          (should (eq (get-text-property index 'face rendered)
                      'nskk-inline-face)))
        (should (equal source source-copy))
        (should (eq (get-text-property 0 'face source) 'error))
        (should (eq (get-text-property 0 'nskk-no-learn source) t))))))

(provide 'nskk-inline-test)

;;; nskk-inline-test.el ends here
