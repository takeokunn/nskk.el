;;; nskk-modeline-test.el --- Tests for nskk-modeline.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Tests for nskk-modeline.el covering:
;; - Function existence (fboundp / macrop)
;; - Indicator return value when state is nil
;; - Indicator suffix text per mode (hiragana, katakana, abbrev, ascii, latin, jisx0208-latin)
;; - Face text-property per mode
;; - Prolog mode-properties/5 facts for all 6 modes (direct is not a valid mode)
;; - nskk-cursor--mode-color returns non-nil string for each mode
;; - Cursor color faces (nskk-cursor-*) are defined and return valid colors
;; - nskk-modeline-format customization
;; - nskk-modeline-update callable without error
;; - Face definitions (facep)
;; - nskk-define-mode-entry macro existence

;;; Code:

(require 'ert)
(require 'nskk-modeline)
(require 'nskk-state)
(require 'nskk-test-framework)
(require 'nskk-test-macros)

(nskk-describe "nskk-modeline-indicator function existence"
  (nskk-it "is defined as a function"
    (should (fboundp 'nskk-modeline-indicator))))

(nskk-describe "nskk-modeline-indicator nil-state behavior"
  (nskk-it "returns a string when nskk-current-state is nil"
    (let ((nskk-current-state nil))
      (let ((result (nskk-modeline-indicator)))
        (should (stringp result)))))

  (nskk-it "returns empty string when state is nil"
    (let ((nskk-current-state nil))
      (should (string= (nskk-modeline-indicator) "")))))

(nskk-deftest-table modeline-indicator-suffix
  :columns (mode expected-pattern)
  :rows ((hiragana "かな")
         (katakana "カナ")
         (abbrev "aA")
         (ascii "SKK")
         (latin "SKK")
         (jisx0208-latin "全英"))
  :body (nskk-with-state mode
          (let ((indicator (nskk-modeline-indicator)))
            (should (stringp indicator))
            (should (string-prefix-p " " indicator))
            (should (string-match-p expected-pattern indicator)))))

(nskk-describe "nskk-modeline-indicator bracket format"
  (nskk-it "hiragana indicator uses the [%m] format"
    (let* ((nskk-modeline-format "[%m]")
           (nskk-current-state (nskk-state-create 'hiragana))
           (indicator (nskk-modeline-indicator)))
      (should (string-prefix-p "[" indicator))
      (should (string-suffix-p "]" indicator))))

  (nskk-it "ascii indicator uses the [%m] format"
    (let* ((nskk-modeline-format "[%m]")
           (nskk-current-state (nskk-state-create 'ascii))
           (indicator (nskk-modeline-indicator)))
      (should (string-prefix-p "[" indicator))
      (should (string-suffix-p "]" indicator)))))

(nskk-describe "nskk-modeline-indicator face text-property"
  (nskk-it "hiragana indicator carries nskk-modeline-hiragana-face"
    (nskk-with-state 'hiragana
      (let* ((indicator (nskk-modeline-indicator))
             (face (get-text-property 0 'face indicator)))
        (should (eq face 'nskk-modeline-hiragana-face)))))

  (nskk-it "katakana indicator carries nskk-modeline-katakana-face"
    (nskk-with-state 'katakana
      (let* ((indicator (nskk-modeline-indicator))
             (face (get-text-property 0 'face indicator)))
        (should (eq face 'nskk-modeline-katakana-face)))))

  (nskk-it "abbrev indicator carries nskk-modeline-abbrev-face"
    (nskk-with-state 'abbrev
      (let* ((indicator (nskk-modeline-indicator))
             (face (get-text-property 0 'face indicator)))
        (should (eq face 'nskk-modeline-abbrev-face)))))

  (nskk-it "jisx0208-latin indicator carries nskk-modeline-jisx0208-latin-face"
    (nskk-with-state 'jisx0208-latin
      (let* ((indicator (nskk-modeline-indicator))
             (face (get-text-property 0 'face indicator)))
        (should (eq face 'nskk-modeline-jisx0208-latin-face)))))

  (nskk-it "ascii indicator carries nskk-modeline-direct-face"
    (nskk-with-state 'ascii
      (let* ((indicator (nskk-modeline-indicator))
             (face (get-text-property 0 'face indicator)))
        (should (eq face 'nskk-modeline-direct-face)))))

  (nskk-it "latin indicator carries nskk-modeline-direct-face"
    (nskk-with-state 'latin
      (let* ((indicator (nskk-modeline-indicator))
             (face (get-text-property 0 'face indicator)))
        (should (eq face 'nskk-modeline-direct-face))))))

(nskk-deftest-table modeline-prolog-mode-properties-exist
  :columns (mode)
  :rows ((hiragana) (katakana) (abbrev) (ascii) (latin) (jisx0208-latin))
  :body (let ((result (nskk-prolog-query-one
                       `(mode-properties ,mode \?s \?f \?h \?c))))
          (should result)))

(nskk-describe "Prolog mode-properties/5 special cases"
  (nskk-it "direct is an alias of ascii with no separate mode-properties fact"
    ;; `direct' is defined in nskk-define-mode-entry for face lookup only; it is
    ;; not a standalone mode in nskk-state-modes nor in mode-properties/5.
    ;; nskk-cursor--mode-color therefore returns nil for direct, which is expected.
    (let ((result (nskk-prolog-query-one '(mode-properties direct \?s \?f \?h \?c))))
      (should-not result))))

(nskk-deftest-table modeline-prolog-mode-display-string
  :columns (mode expected-string)
  :rows ((hiragana "かな")
         (katakana "カナ")
         (abbrev "aA")
         (jisx0208-latin "全英")
         (ascii "SKK"))
  :body (let ((s (nskk-prolog-query-value
                  `(mode-properties ,mode \?s \?f \?h \?c) '\?s)))
          (should (string= s expected-string))))

(nskk-deftest-table modeline-cursor-color-per-mode
  :columns (mode)
  :rows ((hiragana) (katakana) (ascii) (latin) (jisx0208-latin) (abbrev))
  :body (let ((color (nskk-cursor--mode-color mode)))
          (should (stringp color))
          (should (> (length color) 0))))

(nskk-describe "nskk-cursor--mode-color special cases"
  (nskk-it "returns nil for direct (no standalone mode-properties fact)"
    ;; `direct' is a face alias in nskk-modeline.el but not in mode-properties/5.
    ;; ascii/latin cover this display case; direct returns nil for cursor color.
    (let ((color (nskk-cursor--mode-color 'direct)))
      (should-not color))))

(nskk-describe "nskk-modeline-format customization"
  (nskk-it "default format is \" %m\" (ddskk-compatible)"
    (should (string= nskk-modeline-format " %m")))

  (nskk-it "format \"(%m)\" changes the indicator to use parentheses"
    (let* ((nskk-modeline-format "(%m)")
           (nskk-current-state (nskk-state-create 'hiragana))
           (indicator (nskk-modeline-indicator)))
      (should (stringp indicator))
      (should (string-prefix-p "(" indicator))
      (should (string-suffix-p ")" indicator))
      (should (string-match-p "かな" indicator))))

  (nskk-it "format with custom prefix and suffix is applied"
    (let* ((nskk-modeline-format "NSKK:%m!")
           (nskk-current-state (nskk-state-create 'ascii))
           (indicator (nskk-modeline-indicator)))
      (should (stringp indicator))
      (should (string-prefix-p "NSKK:" indicator))
      (should (string-suffix-p "!" indicator))
      (should (string-match-p "SKK" indicator)))))

(nskk-describe "nskk-modeline-update"
  (nskk-it "is defined as a function"
    (should (fboundp 'nskk-modeline-update)))

  (nskk-it "does not error when nskk-current-state is nil"
    (let ((nskk-current-state nil))
      (should-not (condition-case err
                      (progn (nskk-modeline-update) nil)
                    (error err)))))

  (nskk-it "does not error when nskk-current-state is set"
    (nskk-with-state 'hiragana
      (let ((nskk--last-cursor-color nil))
        (should-not (condition-case err
                        (progn (nskk-modeline-update) nil)
                      (error err)))))))

(nskk-describe "modeline face definitions"
  (nskk-it "nskk-modeline-hiragana-face is defined"
    (should (facep 'nskk-modeline-hiragana-face)))

  (nskk-it "nskk-modeline-katakana-face is defined"
    (should (facep 'nskk-modeline-katakana-face)))

  (nskk-it "nskk-modeline-abbrev-face is defined"
    (should (facep 'nskk-modeline-abbrev-face)))

  (nskk-it "nskk-modeline-jisx0208-latin-face is defined"
    (should (facep 'nskk-modeline-jisx0208-latin-face)))

  (nskk-it "nskk-modeline-direct-face is defined"
    (should (facep 'nskk-modeline-direct-face))))

(nskk-describe "nskk-define-mode-entry macro"
  (nskk-it "is defined as a function"
    (should (fboundp 'nskk-define-mode-entry)))

  (nskk-it "is a macro, not a plain function"
    (should (macrop 'nskk-define-mode-entry))))

(nskk-describe "nskk-cursor-update"
  (nskk-it "nskk-cursor-update is defined"
    (should (fboundp 'nskk-cursor-update)))

  (nskk-it "nskk-cursor--mode-color is defined"
    (should (fboundp 'nskk-cursor--mode-color)))

  (nskk-it "does nothing when nskk-use-color-cursor is nil"
    (nskk-with-state 'hiragana
      (let ((nskk-use-color-cursor nil)
            (nskk--last-cursor-color "initial"))
        ;; Should return without error and without modifying nskk--last-cursor-color
        (nskk-cursor-update)
        (should (string= nskk--last-cursor-color "initial"))))))

(nskk-describe "nskk-modeline-indicator unknown mode fallback"
  (nskk-it "query returns nil for a non-registered mode"
    (nskk-with-state 'hiragana
      ;; Temporarily set state to a non-registered mode struct by mutating mode slot
      ;; We test the fallback via nskk-prolog-query-value returning nil for an unknown mode.
      (let ((result (nskk-prolog-query-value
                     `(mode-properties nonexistent-mode ,'\?s ,'\?f ,'\?h ,'\?c) '\?s)))
        ;; mode-properties/5 has no fact for nonexistent-mode — query returns nil
        (should (null result)))))

  (nskk-it "nskk-cursor--mode-color returns nil for an unregistered mode"
    (let ((color (nskk-cursor--mode-color 'nonexistent-mode)))
      (should (null color)))))

(nskk-describe "nskk-modeline module features"
  (nskk-it "nskk-modeline provides its feature"
    (should (featurep 'nskk-modeline)))

  (nskk-it "nskk-state is loaded"
    (should (featurep 'nskk-state)))

  (nskk-it "nskk-prolog is loaded"
    (should (featurep 'nskk-prolog))))

;;;
;;; PBT-001 — Exhaustive mode indicator invariant
;;;

(nskk-property-test-exhaustive modeline-all-modes-return-nonempty
  '(hiragana katakana ascii latin jisx0208-latin abbrev)
  (let ((nskk-current-state (nskk-state-create item)))
    (and (stringp (nskk-modeline-indicator))
         (> (length (nskk-modeline-indicator)) 0))))

;;;
;;; PBT-002 — Format string invariant (seeded PBT with valid-mode generator)
;;;

(nskk-property-test-seeded modeline-format-bracket-invariant
  ((mode valid-mode))
  (let* ((nskk-modeline-format "[%m]")
         (nskk-current-state (if (memq mode '(hiragana katakana ascii latin jisx0208-latin abbrev))
                                 (nskk-state-create mode)
                               (nskk-state-create 'hiragana)))
         (indicator (nskk-modeline-indicator)))
    ;; When state is set and format is "[%m]", indicator must start with "[" and end with "]"
    (if (null nskk-current-state)
        t
      (and (stringp indicator)
           (string-prefix-p "[" indicator)
           (string-suffix-p "]" indicator))))
  50
  42)

;;;
;;; PBT-003 — Face consistency table-driven test
;;;

(nskk-deftest-table modeline-face-consistency
  :columns (mode expected-face)
  :rows ((hiragana      nskk-modeline-hiragana-face)
         (katakana      nskk-modeline-katakana-face)
         (ascii         nskk-modeline-direct-face)
         (latin         nskk-modeline-direct-face)
         (jisx0208-latin nskk-modeline-jisx0208-latin-face)
         (abbrev        nskk-modeline-abbrev-face))
  :body (let* ((nskk-current-state (nskk-state-create mode))
               (indicator (nskk-modeline-indicator))
               (actual-face (get-text-property 0 'face indicator)))
          (should (eq actual-face expected-face))))

(nskk-describe "cursor color faces"
  (nskk-it "nskk-cursor-hiragana face is defined"
    (should (facep 'nskk-cursor-hiragana)))

  (nskk-it "nskk-cursor-katakana face is defined"
    (should (facep 'nskk-cursor-katakana)))

  (nskk-it "nskk-cursor-latin face is defined"
    (should (facep 'nskk-cursor-latin)))

  (nskk-it "nskk-cursor-jisx0208-latin face is defined"
    (should (facep 'nskk-cursor-jisx0208-latin)))

  (nskk-it "nskk-cursor-abbrev face is defined"
    (should (facep 'nskk-cursor-abbrev)))

  (nskk-it "cursor face :background attributes return a color string"
    (dolist (face '(nskk-cursor-hiragana nskk-cursor-katakana
                    nskk-cursor-latin nskk-cursor-jisx0208-latin
                    nskk-cursor-abbrev))
      (let ((color (face-attribute face :background nil t)))
        (should (stringp color))
        (should (not (memq color '(nil unspecified)))))))

  (nskk-it "nskk-cursor--mode-color returns face :background value for hiragana"
    (let ((color (nskk-cursor--mode-color 'hiragana))
          (face-color (face-attribute 'nskk-cursor-hiragana :background nil t)))
      (should (stringp color))
      (should (string= color face-color)))))

(provide 'nskk-modeline-test)

;;; nskk-modeline-test.el ends here
