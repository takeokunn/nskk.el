;;; nskk-region-test.el --- Tests for nskk-region.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Tests for nskk-region.el covering:
;; - Function existence (fboundp)
;; - ASCII to full-width conversion (nskk--ascii-char-to-zenkaku)
;; - Full-width to ASCII conversion (nskk--zenkaku-char-to-ascii)
;; - String-level bidirectional conversion
;; - Region commands: hiragana-region, katakana-region
;; - Region commands: hankaku/zenkaku-katakana-region
;; - Region commands: jisx0208-latin-region, latin-region
;; - Edge cases: empty string, mixed content, non-convertible chars

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nskk-region)
(require 'nskk-test-framework)
(require 'nskk-test-macros)

;;;; Function Existence

(nskk-describe "nskk-region function existence"
  (nskk-it "nskk-hiragana-region is defined"
    (should (fboundp 'nskk-hiragana-region)))
  (nskk-it "nskk-katakana-region is defined"
    (should (fboundp 'nskk-katakana-region)))
  (nskk-it "nskk-hankaku-katakana-region is defined"
    (should (fboundp 'nskk-hankaku-katakana-region)))
  (nskk-it "nskk-zenkaku-katakana-region is defined"
    (should (fboundp 'nskk-zenkaku-katakana-region)))
  (nskk-it "nskk-jisx0208-latin-region is defined"
    (should (fboundp 'nskk-jisx0208-latin-region)))
  (nskk-it "nskk-latin-region is defined"
    (should (fboundp 'nskk-latin-region))))

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
      ;; ア→ｱ(U+FF71), イ→ｲ(U+FF72), ウ→ｳ(U+FF73)
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
      ;; Half-width katakana: ｱｲｳ
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

(provide 'nskk-region-test)

;;; nskk-region-test.el ends here
