;;; nskk-azik-integration-test.el --- AZIK integration tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; AZIK integration tests.

;;; Code:

(require 'ert)
(require 'nskk-input)
(require 'nskk-keymap)
(require 'nskk-state)
(require 'nskk-converter)
(require 'nskk-azik)
(require 'nskk-test-framework)
(require 'nskk-test-macros)
(require 'nskk-pbt-generators)

;;;
;;; Group 1: Special Keys
;;;

(nskk-describe "AZIK special keys integration"

  (nskk-deftest-table azik-special-keys
    :description "Special keys produce expected kana in AZIK mode"
    :columns (char expected)
    :rows ((?\; "っ") (?:  "ー"))
    :body (nskk-azik-with-session 'hiragana
            (nskk--integration-type-char char)
            (nskk-should-equal expected (buffer-string)))))

;;;
;;; Group 2: Hatsuon Extensions
;;;

(nskk-describe "AZIK hatsuon extensions"

  (nskk-deftest-table azik-integration-hatsuon-k-row
    :description "k-row hatsuon extensions produce kana+ん (integration session)"
    :columns (second-char expected)
    :rows ((?k "きん") (?z "かん") (?j "くん") (?d "けん") (?l "こん"))
    :body (nskk-azik-with-session 'hiragana
            (nskk--integration-type-char ?k)
            (nskk--integration-type-char second-char)
            (nskk-should-equal expected (buffer-string)))))

;;;
;;; Group 3: Double Vowel Extensions
;;;

(nskk-describe "AZIK double vowel extensions"

  (nskk-deftest-table azik-double-vowels-k-row
    :description "k-row double vowel extensions produce vowel pairs"
    :columns (second-char expected)
    :rows ((?q "かい") (?h "くう") (?w "けい") (?p "こう"))
    :body (nskk-azik-with-session 'hiragana
            (nskk--integration-type-char ?k)
            (nskk--integration-type-char second-char)
            (nskk-should-equal expected (buffer-string)))))

;;;
;;; Group 4: Youon (g substitutes for y)
;;;

(nskk-describe "AZIK youon extensions"

  (nskk-deftest-table azik-youon-k-row
    :description "kg+vowel produces ki-youon compound"
    :columns (vowel-char expected)
    :rows ((?a "きゃ") (?u "きゅ") (?e "きぇ") (?o "きょ"))
    :body (nskk-azik-with-session 'hiragana
            (nskk--integration-type-char ?k)
            (nskk--integration-type-char ?g)
            (nskk--integration-type-char vowel-char)
            (nskk-should-equal expected (buffer-string)))))

;;;
;;; Group 5: Word Shortcuts
;;;

(nskk-describe "AZIK word shortcuts"

  (nskk-deftest-table azik-word-shortcuts
    :description "Two-key combinations expand to common words"
    :columns (first-char second-char expected)
    :rows ((?s ?r "する") (?m ?s "ます") (?m ?t "また") (?m ?n "もの"))
    :body (nskk-azik-with-session 'hiragana
            (nskk--integration-type-char first-char)
            (nskk--integration-type-char second-char)
            (nskk-should-equal expected (buffer-string)))))

;;;
;;; Group 6: Same-finger Alternatives
;;;

(nskk-describe "AZIK same-finger alternatives"

  (nskk-deftest-table azik-same-finger-alternatives
    :description "Same-finger alternative keys produce single kana"
    :columns (first-char second-char expected)
    :rows ((?k ?f "き") (?r ?f "る") (?y ?f "ゆ"))
    :body (nskk-azik-with-session 'hiragana
            (nskk--integration-type-char first-char)
            (nskk--integration-type-char second-char)
            (nskk-should-equal expected (buffer-string)))))

;;;
;;; Group 7: Standard Rules Still Work in AZIK
;;;

(nskk-describe "AZIK standard rule compatibility"

  (nskk-it "standard ka still produces か in AZIK mode"
    (nskk-azik-with-session 'hiragana
      (nskk-when (progn
                   (nskk--integration-type-char ?k)
                   (nskk--integration-type-char ?a)))
      (nskk-then (nskk-should-equal "か" (buffer-string)))))

  (nskk-it "vowel a still produces あ in AZIK mode"
    (nskk-azik-with-session 'hiragana
      (nskk-when (nskk--integration-type-char ?a))
      (nskk-then (nskk-should-equal "あ" (buffer-string))))))

;;;
;;; Group 8: AZIK + Preedit Combination
;;;

(nskk-describe "AZIK preedit combination"

  (nskk-it "uppercase S in AZIK mode sets the preedit marker"
    (nskk-azik-with-session 'hiragana
      (nskk-when (nskk--integration-type-char ?S))
      (nskk-then (should (nskk-conversion-start-active-p))
                 (should (string-prefix-p "▽" (buffer-string))))))

  (nskk-it "uppercase S followed by r produces ▽する in preedit"
    (nskk-azik-with-session 'hiragana
      (nskk-when (progn
                   (nskk--integration-type-char ?S)
                   (nskk--integration-type-char ?r)))
      (nskk-then (should (nskk-conversion-start-active-p))
                 (nskk-should-equal "▽する" (buffer-string))))))

;;;
;;; Group 9: AZIK Rules (PBT)
;;;

(nskk-describe "AZIK rules (PBT)"

  (nskk-it "every registered AZIK pattern produces non-empty output (all patterns)"
    (nskk-given "an AZIK input session")
    (nskk-when "typing each of the registered AZIK patterns in sequence")
    (nskk-then "every pattern produces non-empty kana output"
      (let ((failures nil)
            (all-patterns (nskk--pbt-get-all-azik-patterns)))
        (nskk-azik-with-session 'hiragana
          (dolist (pattern all-patterns)
            (erase-buffer)
            (nskk-state-set-romaji-buffer "")
            (nskk-state-set nskk-current-state 'input-buffer "")
            (condition-case err
                (progn
                  (cl-loop for ch across pattern
                           do (nskk--integration-type-char ch))
                  (unless (and (stringp (buffer-string))
                               (not (string-empty-p (buffer-string))))
                    (push pattern failures)))
              (error (push (list :error pattern err) failures)))))
        (when failures
          (ert-fail (format "AZIK patterns produced empty output for %d/%d patterns:\n%S"
                            (length failures)
                            (length all-patterns)
                            (seq-take failures 10)))))))

  (nskk-property-test azik-rule-produces-string-output
    ((rule azik-rule-with-expected))
    (let ((input-str (car rule)))
      (nskk-azik-with-session 'hiragana
        (cl-loop for ch across input-str
                 do (nskk--integration-type-char ch))
        (and (stringp (buffer-string))
             (not (string-empty-p (buffer-string))))))
    30))

(provide 'nskk-azik-integration-test)

;;; nskk-azik-integration-test.el ends here
