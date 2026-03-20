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
;; - nskk--cursor-with-color returns non-nil string for each mode
;; - Cursor color faces (nskk-cursor-*) are defined and return valid colors
;; - nskk-modeline-format customization
;; - nskk-modeline-update callable without error
;; - Face definitions (facep)
;; - nskk-define-mode-entry macro existence

;;; Code:

(require 'ert)
(require 'cl-lib)
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
  :rows ((hiragana) (katakana) (katakana-半角) (abbrev) (ascii) (latin) (jisx0208-latin))
  :body (let ((result (nskk-prolog-query-one
                       `(mode-properties ,mode \?s \?f \?h \?c))))
          (should result)))

(nskk-describe "Prolog mode-properties/5 special cases"
  (nskk-it "direct is an alias of ascii with no separate mode-properties fact"
    ;; `direct' is defined in nskk-define-mode-entry for face lookup only; it is
    ;; not a standalone mode in nskk-state-modes nor in mode-properties/5.
    ;; nskk--cursor-with-color therefore returns nil for direct, which is expected.
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
  :rows ((hiragana) (katakana) (katakana-半角) (ascii) (latin) (jisx0208-latin) (abbrev))
  :body (let ((color (nskk--cursor-with-color mode)))
          (should (stringp color))
          (should (not (string-empty-p color)))))

(nskk-describe "nskk--cursor-with-color special cases"
  (nskk-it "returns nil for direct (no standalone mode-properties fact)"
    ;; `direct' is a face alias in nskk-modeline.el but not in mode-properties/5.
    ;; ascii/latin cover this display case; direct returns nil for cursor color.
    (let ((color (nskk--cursor-with-color 'direct)))
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

(nskk-describe "nskk--modeline-clear-cache"
  (nskk-it "is defined as a function"
    (should (fboundp 'nskk--modeline-clear-cache)))

  (nskk-it "resets a populated cache to nil"
    (let ((nskk--modeline-indicator-cache '(hiragana "かな" nskk-modeline-hiragana-face "Hiragana")))
      (nskk--modeline-clear-cache)
      (should (null nskk--modeline-indicator-cache))))

  (nskk-it "is idempotent when cache is already nil"
    (let ((nskk--modeline-indicator-cache nil))
      (nskk-should-not-error (nskk--modeline-clear-cache)))))

(nskk-describe "nskk-modeline-update"
  (nskk-it "is defined as a function"
    (should (fboundp 'nskk-modeline-update)))

  (nskk-it "does not error when nskk-current-state is nil"
    (let ((nskk-current-state nil))
      (nskk-should-not-error (nskk-modeline-update))))

  (nskk-it "does not error when nskk-current-state is set"
    (nskk-with-state 'hiragana
      (let ((nskk--last-cursor-color nil))
        (nskk-should-not-error (nskk-modeline-update)))))

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

  (nskk-it "does nothing when nskk-use-color-cursor is nil"
    (nskk-with-state 'hiragana
      (let ((nskk-use-color-cursor nil)
            (nskk--last-cursor-color "initial"))
        ;; Should return without error and without modifying nskk--last-cursor-color
        (nskk-cursor-update)
        (should (string= nskk--last-cursor-color "initial")))))

  (nskk-it "updates nskk--last-cursor-color when cursor color is available"
    (nskk-with-state 'hiragana
      (let ((nskk-use-color-cursor t)
            (nskk--last-cursor-color nil))
        (nskk-cursor-update)
        (should (stringp nskk--last-cursor-color))
        (should (not (string-empty-p nskk--last-cursor-color))))))

  (nskk-it "does not re-apply cursor color when color is unchanged"
    (nskk-with-state 'hiragana
      (let ((nskk-use-color-cursor t)
            (nskk--last-cursor-color nil))
        (nskk-cursor-update)
        (let ((color-after-first nskk--last-cursor-color))
          (nskk-cursor-update)
          (should (equal nskk--last-cursor-color color-after-first)))))))

(nskk-describe "nskk-modeline-indicator unknown mode fallback"
  (nskk-it "query returns nil for a non-registered mode"
    (nskk-with-state 'hiragana
      ;; Temporarily set state to a non-registered mode struct by mutating mode slot
      ;; We test the fallback via nskk-prolog-query-value returning nil for an unknown mode.
      (let ((result (nskk-prolog-query-value
                     `(mode-properties nonexistent-mode ,'\?s ,'\?f ,'\?h ,'\?c) '\?s)))
        ;; mode-properties/5 has no fact for nonexistent-mode — query returns nil
        (should (null result)))))

  (nskk-it "nskk--cursor-with-color returns nil for an unregistered mode"
    (let ((color (nskk--cursor-with-color 'nonexistent-mode)))
      (should (null color)))))

(nskk-describe "nskk-modeline-indicator fallback values"
  (nskk-it "returns string containing NSKK when Prolog returns no data"
    ;; Mock nskk-prolog-query-values to return nil, simulating a mode with no fact
    (nskk--modeline-clear-cache)
    (nskk-with-mocks ((nskk-prolog-query-values (lambda (&rest _) nil)))
      (let* ((nskk-current-state (nskk-state-create 'hiragana))
             (indicator (nskk-modeline-indicator)))
        (should (stringp indicator))
        (should (string-match-p "NSKK" indicator)))))

  (nskk-it "uses default face when Prolog returns no data"
    (nskk--modeline-clear-cache)
    (nskk-with-mocks ((nskk-prolog-query-values (lambda (&rest _) nil)))
      (let* ((nskk-current-state (nskk-state-create 'hiragana))
             (indicator (nskk-modeline-indicator)))
        (should (eq (get-text-property 0 'face indicator) 'default))))))

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
         (not (string-empty-p (nskk-modeline-indicator))))))

;;;
;;; PBT-002 — Format string invariant (exhaustive over all valid modes)
;;;

(nskk-property-test-exhaustive modeline-format-bracket-invariant
  '(hiragana katakana ascii latin jisx0208-latin abbrev)
  (let* ((nskk-modeline-format "[%m]")
         (nskk-current-state (nskk-state-create item))
         (indicator (nskk-modeline-indicator)))
    ;; When format is "[%m]", indicator must start with "[" and end with "]"
    (and (stringp indicator)
         (string-prefix-p "[" indicator)
         (string-suffix-p "]" indicator))))

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
;;; PBT-004 — Exhaustive cursor color invariant
;;;

(nskk-property-test-exhaustive modeline-cursor-color-all-modes-non-empty
  '(hiragana katakana katakana-半角 ascii latin jisx0208-latin abbrev)
  (let ((color (nskk--cursor-with-color item)))
    (and (stringp color) (not (string-empty-p color)))))

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
  (nskk-it "nskk--cursor-with-color for hiragana matches nskk-cursor-hiragana :background"
    (let ((color (nskk--cursor-with-color 'hiragana))
          (face-color (face-attribute 'nskk-cursor-hiragana :background nil t)))
      (should (stringp color))
      (should (string= color face-color)))))

;;;
;;; nskk--modeline-with-data cache behavior
;;;

(nskk-describe "nskk--modeline-with-data"
  (nskk-it "calls on-found with (display face help) list for hiragana"
    (let (received)
      (nskk--modeline-with-data/k
       'hiragana
       (lambda (data) (setq received data))
       #'ignore)
      (should (listp received))
      (should (= (length received) 3))
      (should (stringp (nth 0 received)))
      (should (symbolp (nth 1 received)))))

  (nskk-it "memoizes: second call with same mode does not re-query Prolog"
    (let ((query-count 0))
      (nskk--modeline-clear-cache)
      (nskk-with-mocks ((nskk-prolog-query-values
                         (lambda (&rest _)
                           (cl-incf query-count)
                           (list "かな" 'nskk-modeline-hiragana-face "Hiragana input mode"))))
        (nskk--modeline-with-data/k 'hiragana #'ignore #'ignore)
        (nskk--modeline-with-data/k 'hiragana #'ignore #'ignore)
        ;; Prolog should only be queried once; second call hits the cache
        (should (= query-count 1))
        (should (eq (car nskk--modeline-indicator-cache) 'hiragana)))))

  (nskk-it "invalidates cache when mode changes"
    (nskk--modeline-clear-cache)
    (nskk--modeline-with-data/k 'hiragana #'ignore #'ignore)
    (should (eq (car nskk--modeline-indicator-cache) 'hiragana))
    (nskk--modeline-with-data/k 'katakana #'ignore #'ignore)
    (should (eq (car nskk--modeline-indicator-cache) 'katakana)))

  (nskk-it "calls on-not-found for an unregistered mode"
    (let (found-called not-found-called)
      (nskk--modeline-clear-cache)
      (nskk--modeline-with-data/k
       'nonexistent-mode-xyz
       (lambda (_) (setq found-called t))
       (lambda () (setq not-found-called t)))
      (should-not found-called)
      (should not-found-called)))

  (nskk-it "sync nskk--modeline-with-data returns list for hiragana"
    (nskk--modeline-clear-cache)
    (let ((data (nskk--modeline-with-data 'hiragana)))
      (should (listp data))
      (should (= (length data) 3))
      (should (stringp (nth 0 data)))))

  (nskk-it "sync nskk--modeline-with-data returns nil for unregistered mode"
    (nskk--modeline-clear-cache)
    (let ((data (nskk--modeline-with-data 'nonexistent-mode-xyz)))
      (should (null data)))))

;;;
;;; nskk--cursor-with-color CPS helper
;;;

(nskk-describe "nskk--cursor-with-color"
  (nskk-it "returns a color string for hiragana"
    (let ((color (nskk--cursor-with-color 'hiragana)))
      (should (stringp color))
      (should (not (string-empty-p color)))))

  (nskk-it "returns a color string for katakana"
    (let ((color (nskk--cursor-with-color 'katakana)))
      (should (stringp color))))

  (nskk-it "returns nil for an unregistered mode"
    (let ((color (nskk--cursor-with-color 'nonexistent-mode)))
      (should-not color))))

(provide 'nskk-modeline-test)

;;; nskk-modeline-test.el ends here
