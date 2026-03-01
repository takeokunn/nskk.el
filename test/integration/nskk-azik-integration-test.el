;;; nskk-azik-integration-test.el --- AZIK integration tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Integration tests for the AZIK extended romaji input system.
;; Tests verify that AZIK rules are correctly applied when the
;; azik converter style is active.

;;; Code:

(require 'ert)
(require 'nskk-input)
(require 'nskk-keymap)
(require 'nskk-state)
(require 'nskk-converter)
(require 'nskk-azik)
(require 'nskk-test-framework)
(require 'nskk-integration-test)

;;;
;;; Helper Macros
;;;

(defmacro nskk-azik-with-session (mode &rest body)
  "Like `nskk-integration-with-session' but with AZIK style loaded.
Restores the standard romaji table after BODY completes so that
subsequent non-AZIK tests are not affected."
  (declare (indent 1))
  `(nskk-integration-with-session ,mode
     (nskk-converter-load-style 'azik)
     (unwind-protect
         (progn ,@body)
       (nskk-converter-load-style 'standard))))

;;;
;;; Group 1: Special Keys
;;;

(nskk-deftest-integration azik-special-key-semicolon-sokuon
  "Test that ; produces っ (sokuon) in AZIK mode."
  (nskk-azik-with-session 'hiragana
    (nskk-integration--type-char ?\;)
    (nskk-should-equal "っ" (buffer-string))))

(nskk-deftest-integration azik-special-key-colon-choonpu
  "Test that : produces ー (long vowel mark) in AZIK mode."
  (nskk-azik-with-session 'hiragana
    (nskk-integration--type-char ?:)
    (nskk-should-equal "ー" (buffer-string))))

;;;
;;; Group 2: Hatsuon Extensions
;;;

(nskk-deftest-integration azik-hatsuon-kz-kan
  "Test that kz produces かん (hatsuon extension for k row, a+n)."
  (nskk-azik-with-session 'hiragana
    (nskk-integration--type-char ?k)
    (nskk-integration--type-char ?z)
    (nskk-should-equal "かん" (buffer-string))))

(nskk-deftest-integration azik-hatsuon-kk-kin
  "Test that kk produces きん (hatsuon extension for k row, i+n)."
  (nskk-azik-with-session 'hiragana
    (nskk-integration--type-char ?k)
    (nskk-integration--type-char ?k)
    (nskk-should-equal "きん" (buffer-string))))

(nskk-deftest-integration azik-hatsuon-kj-kun
  "Test that kj produces くん (hatsuon extension for k row, u+n)."
  (nskk-azik-with-session 'hiragana
    (nskk-integration--type-char ?k)
    (nskk-integration--type-char ?j)
    (nskk-should-equal "くん" (buffer-string))))

(nskk-deftest-integration azik-hatsuon-kd-ken
  "Test that kd produces けん (hatsuon extension for k row, e+n)."
  (nskk-azik-with-session 'hiragana
    (nskk-integration--type-char ?k)
    (nskk-integration--type-char ?d)
    (nskk-should-equal "けん" (buffer-string))))

(nskk-deftest-integration azik-hatsuon-kl-kon
  "Test that kl produces こん (hatsuon extension for k row, o+n)."
  (nskk-azik-with-session 'hiragana
    (nskk-integration--type-char ?k)
    (nskk-integration--type-char ?l)
    (nskk-should-equal "こん" (buffer-string))))

;;;
;;; Group 3: Double Vowel Extensions
;;;

(nskk-deftest-integration azik-double-vowel-kq-kai
  "Test that kq produces かい (double vowel: k row, a+i)."
  (nskk-azik-with-session 'hiragana
    (nskk-integration--type-char ?k)
    (nskk-integration--type-char ?q)
    (nskk-should-equal "かい" (buffer-string))))

(nskk-deftest-integration azik-double-vowel-kh-kuu
  "Test that kh produces くう (double vowel: k row, u+u)."
  (nskk-azik-with-session 'hiragana
    (nskk-integration--type-char ?k)
    (nskk-integration--type-char ?h)
    (nskk-should-equal "くう" (buffer-string))))

(nskk-deftest-integration azik-double-vowel-kw-kei
  "Test that kw produces けい (double vowel: k row, e+i)."
  (nskk-azik-with-session 'hiragana
    (nskk-integration--type-char ?k)
    (nskk-integration--type-char ?w)
    (nskk-should-equal "けい" (buffer-string))))

(nskk-deftest-integration azik-double-vowel-kp-kou
  "Test that kp produces こう (double vowel: k row, o+u)."
  (nskk-azik-with-session 'hiragana
    (nskk-integration--type-char ?k)
    (nskk-integration--type-char ?p)
    (nskk-should-equal "こう" (buffer-string))))

;;;
;;; Group 4: Youon (g substitutes for y)
;;;

(nskk-deftest-integration azik-youon-kga-kya
  "Test that kga produces きゃ (youon: g substitutes for y in k row)."
  (nskk-azik-with-session 'hiragana
    (nskk-integration--type-char ?k)
    (nskk-integration--type-char ?g)
    (nskk-integration--type-char ?a)
    (nskk-should-equal "きゃ" (buffer-string))))

(nskk-deftest-integration azik-youon-kgu-kyu
  "Test that kgu produces きゅ (youon: g substitutes for y in k row)."
  (nskk-azik-with-session 'hiragana
    (nskk-integration--type-char ?k)
    (nskk-integration--type-char ?g)
    (nskk-integration--type-char ?u)
    (nskk-should-equal "きゅ" (buffer-string))))

(nskk-deftest-integration azik-youon-kge-kye
  "Test that kge produces きぇ (youon: g substitutes for y in k row)."
  (nskk-azik-with-session 'hiragana
    (nskk-integration--type-char ?k)
    (nskk-integration--type-char ?g)
    (nskk-integration--type-char ?e)
    (nskk-should-equal "きぇ" (buffer-string))))

(nskk-deftest-integration azik-youon-kgo-kyo
  "Test that kgo produces きょ (youon: g substitutes for y in k row)."
  (nskk-azik-with-session 'hiragana
    (nskk-integration--type-char ?k)
    (nskk-integration--type-char ?g)
    (nskk-integration--type-char ?o)
    (nskk-should-equal "きょ" (buffer-string))))

;;;
;;; Group 5: Word Shortcuts
;;;

(nskk-deftest-integration azik-word-sr-suru
  "Test that sr produces する (word shortcut)."
  (nskk-azik-with-session 'hiragana
    (nskk-integration--type-char ?s)
    (nskk-integration--type-char ?r)
    (nskk-should-equal "する" (buffer-string))))

(nskk-deftest-integration azik-word-ms-masu
  "Test that ms produces ます (word shortcut)."
  (nskk-azik-with-session 'hiragana
    (nskk-integration--type-char ?m)
    (nskk-integration--type-char ?s)
    (nskk-should-equal "ます" (buffer-string))))

(nskk-deftest-integration azik-word-mt-mata
  "Test that mt produces また (word shortcut)."
  (nskk-azik-with-session 'hiragana
    (nskk-integration--type-char ?m)
    (nskk-integration--type-char ?t)
    (nskk-should-equal "また" (buffer-string))))

(nskk-deftest-integration azik-word-mn-mono
  "Test that mn produces もの (word shortcut)."
  (nskk-azik-with-session 'hiragana
    (nskk-integration--type-char ?m)
    (nskk-integration--type-char ?n)
    (nskk-should-equal "もの" (buffer-string))))

;;;
;;; Group 6: Same-finger Alternatives
;;;

(nskk-deftest-integration azik-same-finger-kf-ki
  "Test that kf produces き (same-finger alternative for k row)."
  (nskk-azik-with-session 'hiragana
    (nskk-integration--type-char ?k)
    (nskk-integration--type-char ?f)
    (nskk-should-equal "き" (buffer-string))))

(nskk-deftest-integration azik-same-finger-rf-ru
  "Test that rf produces る (same-finger alternative for r row)."
  (nskk-azik-with-session 'hiragana
    (nskk-integration--type-char ?r)
    (nskk-integration--type-char ?f)
    (nskk-should-equal "る" (buffer-string))))

(nskk-deftest-integration azik-same-finger-yf-yu
  "Test that yf produces ゆ (same-finger alternative for y row)."
  (nskk-azik-with-session 'hiragana
    (nskk-integration--type-char ?y)
    (nskk-integration--type-char ?f)
    (nskk-should-equal "ゆ" (buffer-string))))

;;;
;;; Group 7: Standard Rules Still Work in AZIK
;;;

(nskk-deftest-integration azik-standard-ka-still-works
  "Test that standard ka still produces か in AZIK mode."
  (nskk-azik-with-session 'hiragana
    (nskk-integration--type-char ?k)
    (nskk-integration--type-char ?a)
    (nskk-should-equal "か" (buffer-string))))

(nskk-deftest-integration azik-standard-vowel-a-still-works
  "Test that vowel a still produces あ in AZIK mode."
  (nskk-azik-with-session 'hiragana
    (nskk-integration--type-char ?a)
    (nskk-should-equal "あ" (buffer-string))))

;;;
;;; Group 8: AZIK + Preedit Combination
;;;

(nskk-deftest-integration azik-uppercase-sets-preedit-marker
  "Test that uppercase S in AZIK mode sets the preedit (▽) marker."
  (nskk-azik-with-session 'hiragana
    (nskk-integration--type-char ?S)
    (should (nskk--conversion-start-active-p))
    (should (string-prefix-p "▽" (buffer-string)))))

(nskk-deftest-integration azik-uppercase-sr-preedit-suru
  "Test that uppercase S followed by r in AZIK mode produces ▽する in preedit."
  (nskk-azik-with-session 'hiragana
    (nskk-integration--type-char ?S)
    (should (nskk--conversion-start-active-p))
    (nskk-integration--type-char ?r)
    (nskk-should-equal "▽する" (buffer-string))))

(provide 'nskk-azik-integration-test)

;;; nskk-azik-integration-test.el ends here
