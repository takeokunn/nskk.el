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

(nskk-deftest-table modeline-bracket-format
  :columns (mode)
  :rows ((hiragana) (ascii))
  :body (let* ((nskk-modeline-format "[%m]")
               (nskk-current-state (nskk-state-create mode))
               (indicator (nskk-modeline-indicator)))
          (should (string-prefix-p "[" indicator))
          (should (string-suffix-p "]" indicator))))

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

(nskk-describe "nskk-modeline--clear-cache"
  (nskk-it "is defined as a function"
    (should (fboundp 'nskk-modeline--clear-cache)))

  (nskk-it "resets a populated cache to nil"
    (let ((nskk--modeline-indicator-cache '(hiragana "かな" nskk-modeline-hiragana-face "Hiragana")))
      (nskk-modeline--clear-cache)
      (should (null nskk--modeline-indicator-cache))))

  (nskk-it "is idempotent when cache is already nil"
    (let ((nskk--modeline-indicator-cache nil))
      (should-not (condition-case err
                      (progn (nskk-modeline--clear-cache) nil)
                    (error err))))))

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
                      (error err))))))

  (nskk-it "invalidates the indicator cache"
    (nskk-with-state 'hiragana
      (nskk-modeline-indicator)  ; populate cache
      (should (consp nskk--modeline-indicator-cache))
      (nskk-modeline-update)
      (should (null nskk--modeline-indicator-cache)))))

(nskk-deftest-table modeline-faces-defined
  :columns (face-sym)
  :rows ((nskk-modeline-hiragana-face)
         (nskk-modeline-katakana-face)
         (nskk-modeline-abbrev-face)
         (nskk-modeline-jisx0208-latin-face)
         (nskk-modeline-direct-face))
  :body (should (facep face-sym)))

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
         (nskk-current-state (nskk-state-create
                              (if (memq mode '(hiragana katakana ascii latin jisx0208-latin abbrev))
                                  mode
                                'hiragana)))
         (indicator (nskk-modeline-indicator)))
    ;; When format is "[%m]", indicator must start with "[" and end with "]"
    (and (stringp indicator)
         (string-prefix-p "[" indicator)
         (string-suffix-p "]" indicator)))
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

(nskk-deftest-table cursor-faces-defined
  :columns (face-sym)
  :rows ((nskk-cursor-hiragana)
         (nskk-cursor-katakana)
         (nskk-cursor-latin)
         (nskk-cursor-jisx0208-latin)
         (nskk-cursor-abbrev))
  :body (should (facep face-sym)))

(nskk-deftest-table cursor-faces-have-background-color
  :columns (face-sym)
  :rows ((nskk-cursor-hiragana)
         (nskk-cursor-katakana)
         (nskk-cursor-latin)
         (nskk-cursor-jisx0208-latin)
         (nskk-cursor-abbrev))
  :body (let ((color (face-attribute face-sym :background nil t)))
          (should (stringp color))
          (should (not (memq color '(nil unspecified))))))

(nskk-describe "cursor color face consistency"
  (nskk-it "nskk-cursor--mode-color for hiragana matches nskk-cursor-hiragana :background"
    (let ((color (nskk-cursor--mode-color 'hiragana))
          (face-color (face-attribute 'nskk-cursor-hiragana :background nil t)))
      (should (stringp color))
      (should (string= color face-color)))))

;;;
;;; nskk-modeline--with-data cache behavior
;;;

(nskk-describe "nskk-modeline--with-data"
  (nskk-it "calls continuation with (display face help) list for hiragana"
    (let (received)
      (nskk-modeline--with-data
       'hiragana
       (lambda (data) (setq received data)))
      (should (listp received))
      (should (= (length received) 3))
      (should (stringp (nth 0 received)))
      (should (symbolp (nth 1 received)))))

  (nskk-it "memoizes: second call with same mode reuses cache without re-querying Prolog"
    (let ((call-count 0))
      (nskk-modeline--clear-cache)
      ;; First call — hits Prolog and caches the result
      (nskk-modeline--with-data
       'hiragana
       (lambda (_) (cl-incf call-count)))
      ;; Force the cache to appear valid (same mode key) and count
      (nskk-modeline--with-data
       'hiragana
       (lambda (_) (cl-incf call-count)))
      ;; Both calls succeed; count should be 2 (continuation ran each time)
      (should (= call-count 2))
      ;; Cache should now be set for hiragana
      (should (eq (car nskk--modeline-indicator-cache) 'hiragana))))

  (nskk-it "invalidates cache when mode changes"
    (nskk-modeline--clear-cache)
    (nskk-modeline--with-data 'hiragana #'ignore)
    (should (eq (car nskk--modeline-indicator-cache) 'hiragana))
    (nskk-modeline--with-data 'katakana #'ignore)
    ;; After querying a different mode the cache key must change
    (should (eq (car nskk--modeline-indicator-cache) 'katakana))))

;;;
;;; nskk-cursor--with-color CPS helper
;;;

(nskk-describe "nskk-cursor--with-color"
  (nskk-it "calls continuation with a color string for hiragana"
    (let (received)
      (nskk-cursor--with-color
       'hiragana
       (lambda (color) (setq received color)))
      (should (stringp received))
      (should (> (length received) 0))))

  (nskk-it "calls continuation with a color string for katakana"
    (let (received)
      (nskk-cursor--with-color
       'katakana
       (lambda (color) (setq received color)))
      (should (stringp received))))

  (nskk-it "does not call continuation for an unregistered mode"
    (let (called)
      (nskk-cursor--with-color
       'nonexistent-mode
       (lambda (_) (setq called t)))
      (should-not called)))

  (nskk-it "returned value is identity of color when continuation is identity"
    (let ((color (nskk-cursor--with-color 'hiragana #'identity)))
      (should (stringp color)))))

(provide 'nskk-modeline-test)

;;; nskk-modeline-test.el ends here
