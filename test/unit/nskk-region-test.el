;;; nskk-region-test.el --- Tests for nskk-region.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Tests for nskk-region.el.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nskk-region)
(require 'nskk-test-framework)
(require 'nskk-test-macros)

;;;; ASCII ↔ Full-Width Single Character Conversion

(nskk-deftest-table ascii-to-zenkaku-chars
  :columns (char expected)
  :rows ((?A "Ａ")
         (?a "ａ")
         (?z "ｚ")
         (?Z "Ｚ")
         (?0 "０")
         (?9 "９")
         (?! "！")
         (?~ "～")
         (?  "\u3000"))
  :body
  (should (equal (nskk--ascii-char-to-zenkaku char) expected)))

(nskk-deftest-table zenkaku-to-ascii-chars
  :columns (char expected)
  :rows ((?Ａ "A")
         (?ａ "a")
         (?ｚ "z")
         (?Ｚ "Z")
         (?０ "0")
         (?９ "9")
         (?！ "!")
         (?～ "~")
         (?\u3000 " "))
  :body
  (should (equal (nskk--zenkaku-char-to-ascii char) expected)))

(nskk-describe "nskk--ascii-char-to-zenkaku passthrough"
  (nskk-it "converts tab to tab (non-printable passthrough)"
    (should (equal (nskk--ascii-char-to-zenkaku ?\t) "\t")))
  (nskk-it "passes through kanji unchanged"
    (should (equal (nskk--ascii-char-to-zenkaku ?漢) "漢"))))

(nskk-describe "nskk--zenkaku-char-to-ascii passthrough"
  (nskk-it "passes through hiragana unchanged"
    (should (equal (nskk--zenkaku-char-to-ascii ?あ) "あ")))
  (nskk-it "passes through katakana unchanged"
    (should (equal (nskk--zenkaku-char-to-ascii ?ア) "ア"))))

;;;; String-Level Conversion

(nskk-describe "nskk--string-ascii-to-zenkaku"
  (nskk-it "converts ASCII string to full-width"
    (should (equal (nskk--string-ascii-to-zenkaku "abc") "ａｂｃ")))
  (nskk-it "converts uppercase ASCII"
    (should (equal (nskk--string-ascii-to-zenkaku "ABC") "ＡＢＣ")))
  (nskk-it "converts digits"
    (should (equal (nskk--string-ascii-to-zenkaku "123") "１２３")))
  (nskk-it "converts space to ideographic space"
    (should (equal (nskk--string-ascii-to-zenkaku "a b") "ａ\u3000ｂ")))
  (nskk-it "passes through non-ASCII chars"
    (should (equal (nskk--string-ascii-to-zenkaku "あ") "あ")))
  (nskk-it "handles empty string"
    (should (equal (nskk--string-ascii-to-zenkaku "") "")))
  (nskk-it "round-trips with zenkaku-to-ascii"
    (let ((original "Hello World 123!"))
      (should (equal (nskk--string-zenkaku-to-ascii
                      (nskk--string-ascii-to-zenkaku original))
                     original)))))

(nskk-describe "nskk--string-zenkaku-to-ascii"
  (nskk-it "converts full-width string to ASCII"
    (should (equal (nskk--string-zenkaku-to-ascii "ａｂｃ") "abc")))
  (nskk-it "converts uppercase full-width"
    (should (equal (nskk--string-zenkaku-to-ascii "ＡＢＣ") "ABC")))
  (nskk-it "converts ideographic space to ASCII space"
    (should (equal (nskk--string-zenkaku-to-ascii "\u3000") " ")))
  (nskk-it "passes through hiragana unchanged"
    (should (equal (nskk--string-zenkaku-to-ascii "あいう") "あいう")))
  (nskk-it "handles empty string"
    (should (equal (nskk--string-zenkaku-to-ascii "") ""))))

;;;; Region Commands (buffer-based)

(nskk-describe "nskk-katakana-region"
  (nskk-it "converts hiragana region to katakana"
    (with-temp-buffer
      (insert "あいう")
      (nskk-katakana-region (point-min) (point-max))
      (should (equal (buffer-string) "アイウ"))))
  (nskk-it "passes through non-hiragana content"
    (with-temp-buffer
      (insert "ABC")
      (nskk-katakana-region (point-min) (point-max))
      (should (equal (buffer-string) "ABC"))))
  (nskk-it "handles empty region"
    (with-temp-buffer
      (nskk-katakana-region (point-min) (point-max))
      (should (equal (buffer-string) "")))))

(nskk-describe "nskk-hiragana-region"
  (nskk-it "converts katakana region to hiragana"
    (with-temp-buffer
      (insert "アイウ")
      (nskk-hiragana-region (point-min) (point-max))
      (should (equal (buffer-string) "あいう"))))
  (nskk-it "passes through non-katakana content"
    (with-temp-buffer
      (insert "abc")
      (nskk-hiragana-region (point-min) (point-max))
      (should (equal (buffer-string) "abc"))))
  (nskk-it "is inverse of katakana-region"
    (with-temp-buffer
      (insert "さしすせそ")
      (let ((original (buffer-string)))
        (nskk-katakana-region (point-min) (point-max))
        (nskk-hiragana-region (point-min) (point-max))
        (should (equal (buffer-string) original))))))

(nskk-describe "nskk-jisx0208-latin-region"
  (nskk-it "converts ASCII region to full-width"
    (with-temp-buffer
      (insert "abc")
      (nskk-jisx0208-latin-region (point-min) (point-max))
      (should (equal (buffer-string) "ａｂｃ"))))
  (nskk-it "handles mixed ASCII and non-ASCII"
    (with-temp-buffer
      (insert "aあb")
      (nskk-jisx0208-latin-region (point-min) (point-max))
      (should (equal (buffer-string) "ａあｂ")))))

(nskk-describe "nskk-latin-region"
  (nskk-it "converts full-width region to ASCII"
    (with-temp-buffer
      (insert "ａｂｃ")
      (nskk-latin-region (point-min) (point-max))
      (should (equal (buffer-string) "abc"))))
  (nskk-it "is inverse of jisx0208-latin-region for pure ASCII"
    (with-temp-buffer
      (insert "Hello123")
      (let ((original (buffer-string)))
        (nskk-jisx0208-latin-region (point-min) (point-max))
        (nskk-latin-region (point-min) (point-max))
        (should (equal (buffer-string) original))))))

(nskk-describe "nskk-hankaku-katakana-region"
  (nskk-it "converts zenkaku katakana to hankaku"
    (with-temp-buffer
      (insert "アイウ")
      (nskk-hankaku-katakana-region (point-min) (point-max))
      (should (equal (buffer-string) "\uff71\uff72\uff73"))))
  (nskk-it "converts a full row of zenkaku katakana to hankaku"
    (with-temp-buffer
      (insert "アイウエオ")
      (nskk-hankaku-katakana-region (point-min) (point-max))
      (should (equal (buffer-string) "\uff71\uff72\uff73\uff74\uff75"))))
  (nskk-it "passes through non-katakana unchanged"
    (with-temp-buffer
      (insert "abc")
      (nskk-hankaku-katakana-region (point-min) (point-max))
      (should (equal (buffer-string) "abc")))))

(nskk-describe "nskk-zenkaku-katakana-region"
  (nskk-it "converts hankaku katakana to zenkaku"
    (with-temp-buffer
      (insert "\uff71\uff72\uff73")
      (nskk-zenkaku-katakana-region (point-min) (point-max))
      (should (equal (buffer-string) "アイウ"))))
  (nskk-it "is inverse of hankaku-katakana-region"
    (with-temp-buffer
      (insert "アイウエオ")
      (let ((original (buffer-string)))
        (nskk-hankaku-katakana-region (point-min) (point-max))
        (nskk-zenkaku-katakana-region (point-min) (point-max))
        (should (equal (buffer-string) original))))))

(defconst nskk-region-test--interactive-cases
  '((nskk-hiragana-region "アイウ" "あいう")
    (nskk-katakana-region "あいう" "アイウ")
    (nskk-hankaku-katakana-region "アイウ" "ｱｲｳ")
    (nskk-zenkaku-katakana-region "ｱｲｳ" "アイウ")
    (nskk-jisx0208-latin-region "abc" "ａｂｃ")
    (nskk-latin-region "ａｂｃ" "abc"))
  "Interactive region conversion command test cases.")

(ert-deftest nskk-region-test/inactive-region-signals-without-modification ()
  "Signal a user error and preserve the buffer when no region is active."
  (dolist (entry nskk-region-test--interactive-cases)
    (let ((command (nth 0 entry))
          (input (nth 1 entry)))
      (with-temp-buffer
        (insert input)
        (let ((before (buffer-string))
              (transient-mark-mode t)
              (mark-active nil))
          (should-error (call-interactively command) :type 'user-error)
          (should (equal (buffer-string) before)))))))

(ert-deftest nskk-region-test/active-region-normal-direction ()
  "Convert every command with point after mark."
  (dolist (entry nskk-region-test--interactive-cases)
    (let ((command (nth 0 entry))
          (input (nth 1 entry))
          (expected (nth 2 entry)))
      (with-temp-buffer
        (insert input)
        (let ((transient-mark-mode t))
          (goto-char (point-max))
          (push-mark (point-min) t t)
          (should (use-region-p))
          (call-interactively command)
          (should (equal (buffer-string) expected)))))))

(ert-deftest nskk-region-test/active-region-reverse-direction ()
  "Convert every command with point before mark."
  (dolist (entry nskk-region-test--interactive-cases)
    (let ((command (nth 0 entry))
          (input (nth 1 entry))
          (expected (nth 2 entry)))
      (with-temp-buffer
        (insert input)
        (let ((transient-mark-mode t))
          (push-mark (point-max) t t)
          (goto-char (point-min))
          (should (use-region-p))
          (call-interactively command)
          (should (equal (buffer-string) expected)))))))

(ert-deftest nskk-region-test/change-group-restores-region-on-hook-failures ()
  "Restore text and active-region state for every injected hook failure."
  (dolist (entry nskk-region-test--interactive-cases)
    (dolist (phase '(before after))
      (dolist (condition '(error quit))
        (let ((command (nth 0 entry))
              (input (nth 1 entry)))
          (with-temp-buffer
            (insert input)
            (goto-char (point-max))
            (push-mark (point-min) t t)
            (setq mark-active t)
            (let ((before-text (buffer-string))
                  (before-point (point))
                  (before-mark (mark))
                  (before-mark-active mark-active)
                  (before-change-functions
                   (when (eq phase 'before)
                     (list
                      (lambda (&rest _)
                        (signal condition
                                '(injected-region-conversion))))))
                  (after-change-functions
                   (when (eq phase 'after)
                     (list
                      (lambda (&rest _)
                        (signal condition
                                '(injected-region-conversion)))))))
              (let ((caught
                     (condition-case condition-data
                         (progn
                           (funcall command (point-min) (point-max))
                           nil)
                       (quit condition-data)
                       (error condition-data))))
                (should
                 (equal caught
                        (list condition
                              'injected-region-conversion)))
                (should (equal (buffer-string) before-text))
                (should (= (point) before-point))
                (should (= (mark) before-mark))
                (should (eq mark-active before-mark-active))))))))))

(ert-deftest nskk-region-test/read-only-restores-active-region-state ()
  "Leave text and active-region state intact in read-only buffers."
  (dolist (entry nskk-region-test--interactive-cases)
    (let ((command (nth 0 entry))
          (input (nth 1 entry)))
      (with-temp-buffer
        (insert input)
        (goto-char (point-max))
        (push-mark (point-min) t t)
        (setq mark-active t)
        (let ((before-text (buffer-string))
              (before-point (point))
              (before-mark (mark))
              (before-mark-active mark-active)
              (buffer-read-only t))
          (should-error
           (funcall command (point-min) (point-max))
           :type 'buffer-read-only)
          (should (equal (buffer-string) before-text))
          (should (= (point) before-point))
          (should (= (mark) before-mark))
          (should (eq mark-active before-mark-active)))))))

;;;; Partial Region

(nskk-deftest-table partial-region-conversion
  :columns (command input expected)
  :rows ((nskk-hiragana-region "アイウ" "あいう")
         (nskk-katakana-region "あいう" "アイウ")
         (nskk-hankaku-katakana-region "アイウ" "ｱｲｳ")
         (nskk-zenkaku-katakana-region "ｱｲｳ" "アイウ")
         (nskk-jisx0208-latin-region "abc" "ａｂｃ")
         (nskk-latin-region "ａｂｃ" "abc")
         (nskk-zenkaku-katakana-region "ｶﾞ" "ガ"))
  :body
  (with-temp-buffer
    (insert "前" input "後")
    (let ((beg (1+ (point-min))))
      (funcall command beg (+ beg (length input)))
      (should (equal (buffer-string) (concat "前" expected "後")))
      (should (= (point) (+ beg (length expected)))))))

(ert-deftest nskk-region-test/interactive-bounds-locate-an-interior-region ()
  "Convert only the selected span when the region is interior to the buffer."
  (with-temp-buffer
    (insert "前アイウ後")
    (let ((transient-mark-mode t))
      (push-mark 2 t t)
      (goto-char 5)
      (should (use-region-p))
      (call-interactively 'nskk-hiragana-region)
      (should (equal (buffer-string) "前あいう後")))))

(ert-deftest nskk-region-test/stale-mark-without-transient-mark-mode-signals ()
  "Reject a mark left active while `transient-mark-mode' is off."
  (with-temp-buffer
    (insert "アイウ")
    (goto-char (point-max))
    (push-mark (point-min) t t)
    (let ((transient-mark-mode nil)
          (mark-active t))
      (should-error (call-interactively 'nskk-hiragana-region) :type 'user-error)
      (should (equal (buffer-string) "アイウ")))))

(ert-deftest nskk-region-test/converts-with-an-inactive-mark-and-strict-mark-reading ()
  "Convert while the mark is set but inactive and `mark-even-if-inactive' is nil.
Saving the mark has to read it unconditionally; a reading that honours
activation signals `mark-inactive' here and aborts the conversion."
  (with-temp-buffer
    (insert "アイウ")
    (let ((transient-mark-mode t)
          (mark-even-if-inactive nil))
      (push-mark (point-min) t nil)
      (setq mark-active nil)
      (nskk-hiragana-region (point-min) (point-max))
      (should (equal (buffer-string) "あいう")))))

(ert-deftest nskk-region-test/restores-mark-deactivated-by-failing-body ()
  "Restore mark activation when the failing body itself deactivated the mark."
  (with-temp-buffer
    (insert "あいう")
    (goto-char (point-max))
    (push-mark (point-min) t t)
    (setq mark-active t)
    (let ((before-change-functions
           (list (lambda (&rest _)
                   (setq mark-active nil)
                   (signal 'error '(injected-region-conversion))))))
      (let ((caught (condition-case data
                        (nskk-katakana-region (point-min) (point-max))
                      (error data))))
        (should (equal caught '(error injected-region-conversion)))
        (should (eq mark-active t))))))

(provide 'nskk-region-test)

;;; nskk-region-test.el ends here
