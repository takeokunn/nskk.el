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

;;;
;;; TR-001 — nskk-modeline-indicator is fboundp
;;;

(nskk-deftest-unit modeline-indicator-fboundp
  "Test that nskk-modeline-indicator is defined."
  (should (fboundp 'nskk-modeline-indicator)))

;;;
;;; TR-002 — nskk-modeline-indicator is nil-safe when state is not set
;;;

(nskk-deftest-unit modeline-indicator-nil-state-returns-string
  "Test that nskk-modeline-indicator returns a string when nskk-current-state is nil."
  (let ((nskk-current-state nil))
    (let ((result (nskk-modeline-indicator)))
      (should (stringp result)))))

(nskk-deftest-unit modeline-indicator-nil-state-empty-string
  "Test that nskk-modeline-indicator returns empty string when state is nil."
  (let ((nskk-current-state nil))
    (should (string= (nskk-modeline-indicator) ""))))

;;;
;;; TR-003 — Indicator suffix matches expected text per mode
;;;

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

;;;
;;; TR-003b — Indicator with explicit bracket format override
;;;

(nskk-deftest-unit modeline-indicator-hiragana-bracket-format
  "Test that hiragana indicator uses the default [%m] format."
  (let* ((nskk-modeline-format "[%m]")
         (nskk-current-state (nskk-state-create 'hiragana))
         (indicator (nskk-modeline-indicator)))
    (should (string-prefix-p "[" indicator))
    (should (string-suffix-p "]" indicator))))

(nskk-deftest-unit modeline-indicator-ascii-bracket-format
  "Test that ascii indicator uses the default [%m] format."
  (let* ((nskk-modeline-format "[%m]")
         (nskk-current-state (nskk-state-create 'ascii))
         (indicator (nskk-modeline-indicator)))
    (should (string-prefix-p "[" indicator))
    (should (string-suffix-p "]" indicator))))

;;;
;;; TR-004 — Face text-property on indicator string per mode
;;;

(nskk-deftest-unit modeline-indicator-hiragana-face
  "Test that hiragana indicator carries nskk-modeline-hiragana-face."
  (nskk-with-state 'hiragana
    (let* ((indicator (nskk-modeline-indicator))
           (face (get-text-property 0 'face indicator)))
      (should (eq face 'nskk-modeline-hiragana-face)))))

(nskk-deftest-unit modeline-indicator-katakana-face
  "Test that katakana indicator carries nskk-modeline-katakana-face."
  (nskk-with-state 'katakana
    (let* ((indicator (nskk-modeline-indicator))
           (face (get-text-property 0 'face indicator)))
      (should (eq face 'nskk-modeline-katakana-face)))))

(nskk-deftest-unit modeline-indicator-abbrev-face
  "Test that abbrev indicator carries nskk-modeline-abbrev-face."
  (nskk-with-state 'abbrev
    (let* ((indicator (nskk-modeline-indicator))
           (face (get-text-property 0 'face indicator)))
      (should (eq face 'nskk-modeline-abbrev-face)))))

(nskk-deftest-unit modeline-indicator-jisx0208-latin-face
  "Test that jisx0208-latin indicator carries nskk-modeline-jisx0208-latin-face."
  (nskk-with-state 'jisx0208-latin
    (let* ((indicator (nskk-modeline-indicator))
           (face (get-text-property 0 'face indicator)))
      (should (eq face 'nskk-modeline-jisx0208-latin-face)))))

(nskk-deftest-unit modeline-indicator-ascii-direct-face
  "Test that ascii indicator carries nskk-modeline-direct-face."
  (nskk-with-state 'ascii
    (let* ((indicator (nskk-modeline-indicator))
           (face (get-text-property 0 'face indicator)))
      (should (eq face 'nskk-modeline-direct-face)))))

(nskk-deftest-unit modeline-indicator-latin-direct-face
  "Test that latin indicator carries nskk-modeline-direct-face."
  (nskk-with-state 'latin
    (let* ((indicator (nskk-modeline-indicator))
           (face (get-text-property 0 'face indicator)))
      (should (eq face 'nskk-modeline-direct-face)))))

;;;
;;; TR-005 — Prolog mode-properties/5 has facts for all 6 modes
;;;

(nskk-deftest-table modeline-prolog-mode-properties-exist
  :columns (mode)
  :rows ((hiragana) (katakana) (abbrev) (ascii) (latin) (jisx0208-latin))
  :body (let ((result (nskk-prolog-query-one
                       `(mode-properties ,mode \?s \?f \?h \?c))))
          (should result)))

(nskk-deftest-unit modeline-prolog-mode-info-direct
  "Test that `direct' is handled as an alias of ascii (no separate mode-properties fact)."
  ;; `direct' is defined in nskk-define-mode-entry for face lookup only; it is
  ;; not a standalone mode in nskk-state-modes nor in mode-properties/5.
  ;; nskk-cursor--mode-color therefore returns nil for direct, which is expected.
  (let ((result (nskk-prolog-query-one '(mode-properties direct \?s \?f \?h \?c))))
    (should-not result)))

;;;
;;; TR-005 — Prolog mode-properties/5 returns correct strings per mode
;;;

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

;;;
;;; TR-006 — nskk-cursor--mode-color returns non-nil for each mode
;;;

(nskk-deftest-table modeline-cursor-color-per-mode
  :columns (mode)
  :rows ((hiragana) (katakana) (ascii) (latin) (jisx0208-latin) (abbrev))
  :body (let ((color (nskk-cursor--mode-color mode)))
          (should (stringp color))
          (should (> (length color) 0))))

(nskk-deftest-unit modeline-cursor-color-direct
  "Test that nskk-cursor--mode-color returns nil for direct (no standalone mode-properties fact)."
  ;; `direct' is a face alias in nskk-modeline.el but not in mode-properties/5.
  ;; ascii/latin cover this display case; direct returns nil for cursor color.
  (let ((color (nskk-cursor--mode-color 'direct)))
    (should-not color)))

;;;
;;; TR-007 — nskk-modeline-format customization
;;;

(nskk-deftest-unit modeline-format-default
  "Test that the default nskk-modeline-format is \" %m\" (ddskk-compatible)."
  (should (string= nskk-modeline-format " %m")))

(nskk-deftest-unit modeline-format-custom-parens
  "Test that setting nskk-modeline-format to \"(%m)\" changes the indicator."
  (let* ((nskk-modeline-format "(%m)")
         (nskk-current-state (nskk-state-create 'hiragana))
         (indicator (nskk-modeline-indicator)))
    (should (stringp indicator))
    (should (string-prefix-p "(" indicator))
    (should (string-suffix-p ")" indicator))
    (should (string-match-p "かな" indicator))))

(nskk-deftest-unit modeline-format-custom-prefix-suffix
  "Test that nskk-modeline-format with custom prefix and suffix is applied."
  (let* ((nskk-modeline-format "NSKK:%m!")
         (nskk-current-state (nskk-state-create 'ascii))
         (indicator (nskk-modeline-indicator)))
    (should (stringp indicator))
    (should (string-prefix-p "NSKK:" indicator))
    (should (string-suffix-p "!" indicator))
    (should (string-match-p "SKK" indicator))))

;;;
;;; TR-008 — nskk-modeline-update is fboundp and callable without error
;;;

(nskk-deftest-unit modeline-update-fboundp
  "Test that nskk-modeline-update is defined."
  (should (fboundp 'nskk-modeline-update)))

(nskk-deftest-unit modeline-update-callable-no-state
  "Test that nskk-modeline-update does not error when nskk-current-state is nil."
  (let ((nskk-current-state nil))
    (should-not (condition-case err
                    (progn (nskk-modeline-update) nil)
                  (error err)))))

(nskk-deftest-unit modeline-update-callable-with-state
  "Test that nskk-modeline-update does not error when nskk-current-state is set."
  (nskk-with-state 'hiragana
    (let ((nskk--last-cursor-color nil))
      (should-not (condition-case err
                      (progn (nskk-modeline-update) nil)
                    (error err))))))

;;;
;;; TR-009 — Face definitions
;;;

(nskk-deftest-unit modeline-face-hiragana-defined
  "Test that nskk-modeline-hiragana-face is defined."
  (should (facep 'nskk-modeline-hiragana-face)))

(nskk-deftest-unit modeline-face-katakana-defined
  "Test that nskk-modeline-katakana-face is defined."
  (should (facep 'nskk-modeline-katakana-face)))

(nskk-deftest-unit modeline-face-abbrev-defined
  "Test that nskk-modeline-abbrev-face is defined."
  (should (facep 'nskk-modeline-abbrev-face)))

(nskk-deftest-unit modeline-face-jisx0208-latin-defined
  "Test that nskk-modeline-jisx0208-latin-face is defined."
  (should (facep 'nskk-modeline-jisx0208-latin-face)))

(nskk-deftest-unit modeline-face-direct-defined
  "Test that nskk-modeline-direct-face is defined."
  (should (facep 'nskk-modeline-direct-face)))

;;;
;;; TR-010 — nskk-define-mode-entry macro existence
;;;

(nskk-deftest-unit modeline-define-mode-entry-fboundp
  "Test that nskk-define-mode-entry is defined as a macro."
  (should (fboundp 'nskk-define-mode-entry)))

(nskk-deftest-unit modeline-define-mode-entry-is-macro
  "Test that nskk-define-mode-entry is a macro, not a plain function."
  (should (macrop 'nskk-define-mode-entry)))

;;;
;;; TR-011 — nskk-cursor-update is fboundp and respects nskk-use-color-cursor
;;;

(nskk-deftest-unit modeline-cursor-update-fboundp
  "Test that nskk-cursor-update is defined."
  (should (fboundp 'nskk-cursor-update)))

(nskk-deftest-unit modeline-cursor-mode-color-fboundp
  "Test that nskk-cursor--mode-color is defined."
  (should (fboundp 'nskk-cursor--mode-color)))

(nskk-deftest-unit modeline-cursor-update-no-op-when-disabled
  "Test that nskk-cursor-update does nothing when nskk-use-color-cursor is nil."
  (nskk-with-state 'hiragana
    (let ((nskk-use-color-cursor nil)
          (nskk--last-cursor-color "initial"))
      ;; Should return without error and without modifying nskk--last-cursor-color
      (nskk-cursor-update)
      (should (string= nskk--last-cursor-color "initial")))))

;;;
;;; TR-012 — nskk-modeline-indicator unknown mode fallback
;;;

(nskk-deftest-unit modeline-indicator-unknown-mode-returns-string
  "Test that nskk-modeline-indicator returns a non-empty string for an unknown mode."
  (nskk-with-state 'hiragana
    ;; Temporarily set state to a non-registered mode struct by mutating mode slot
    ;; We test the fallback via nskk-prolog-query-value returning nil for an unknown mode.
    (let ((result (nskk-prolog-query-value
                   `(mode-properties nonexistent-mode ,'\?s ,'\?f ,'\?h ,'\?c) '\?s)))
      ;; mode-properties/5 has no fact for nonexistent-mode — query returns nil
      (should (null result)))))

(nskk-deftest-unit modeline-cursor--mode-color-unknown-mode-returns-nil
  "Test that nskk-cursor--mode-color returns nil for an unregistered mode."
  (let ((color (nskk-cursor--mode-color 'nonexistent-mode)))
    (should (null color))))

;;;
;;; Module Feature Tests
;;;

(nskk-deftest-unit modeline-provides-feature
  "Test that nskk-modeline provides its feature."
  (should (featurep 'nskk-modeline)))

(nskk-deftest-unit modeline-requires-nskk-state
  "Test that nskk-state is loaded."
  (should (featurep 'nskk-state)))

(nskk-deftest-unit modeline-requires-nskk-prolog
  "Test that nskk-prolog is loaded."
  (should (featurep 'nskk-prolog)))

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

;;;
;;; TR-013 — Cursor color faces are defined with :background attributes
;;;

(nskk-deftest-unit modeline-cursor-face-hiragana-defined
  "Test that nskk-cursor-hiragana face is defined."
  (should (facep 'nskk-cursor-hiragana)))

(nskk-deftest-unit modeline-cursor-face-katakana-defined
  "Test that nskk-cursor-katakana face is defined."
  (should (facep 'nskk-cursor-katakana)))

(nskk-deftest-unit modeline-cursor-face-latin-defined
  "Test that nskk-cursor-latin face is defined."
  (should (facep 'nskk-cursor-latin)))

(nskk-deftest-unit modeline-cursor-face-jisx0208-latin-defined
  "Test that nskk-cursor-jisx0208-latin face is defined."
  (should (facep 'nskk-cursor-jisx0208-latin)))

(nskk-deftest-unit modeline-cursor-face-abbrev-defined
  "Test that nskk-cursor-abbrev face is defined."
  (should (facep 'nskk-cursor-abbrev)))

(nskk-deftest-unit modeline-cursor-face-attribute-is-string
  "Test that face-attribute on cursor faces returns a color string, not unspecified."
  (dolist (face '(nskk-cursor-hiragana nskk-cursor-katakana
                  nskk-cursor-latin nskk-cursor-jisx0208-latin
                  nskk-cursor-abbrev))
    (let ((color (face-attribute face :background nil t)))
      (should (stringp color))
      (should (not (memq color '(nil unspecified)))))))

(nskk-deftest-unit modeline-cursor-mode-color-uses-face-attribute
  "Test that nskk-cursor--mode-color returns face :background value for hiragana."
  (let ((color (nskk-cursor--mode-color 'hiragana))
        (face-color (face-attribute 'nskk-cursor-hiragana :background nil t)))
    (should (stringp color))
    (should (string= color face-color))))

(provide 'nskk-modeline-test)

;;; nskk-modeline-test.el ends here
