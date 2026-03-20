;;; nskk-modeline-state-integration-test.el --- Modeline↔State integration tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Integration tests for the cross-module call chain between
;; nskk-modeline.el (Layer 5: Presentation) and nskk-state.el (Layer 2: Domain).
;;
;; Tests verify that:
;; - `nskk-modeline-indicator' correctly queries mode-properties/5 via Prolog,
;;   crossing the module boundary from Presentation to Domain.
;; - The per-mode memoization cache in `nskk--modeline-indicator-cache' is
;;   keyed on the mode symbol stored in the state struct.
;; - Cache invalidation via `nskk--modeline-clear-cache' propagates the state
;;   change across the modeline↔state boundary.
;; - `nskk-modeline-update' triggers cache clearing (cross-module call chain).
;;
;; Note: Tests bind `nskk-use-color-cursor' to nil to prevent
;; `set-cursor-color' calls that are inappropriate in batch mode.

;;; Code:

(require 'ert)
(require 'nskk-modeline)
(require 'nskk-state)
(require 'nskk-prolog)
(require 'nskk-test-framework)
(require 'nskk-test-macros)

;;;; Modeline indicator queries state

(nskk-describe "modeline indicator from state"

  (nskk-it "returns empty string when nskk-current-state is nil"
    (nskk-with-state nil
      (should (string= "" (nskk-modeline-indicator)))))

  (nskk-it "returns non-empty string for hiragana mode"
    (nskk-with-state 'hiragana
      (let ((nskk-use-color-cursor nil))
        (should (not (string-empty-p (nskk-modeline-indicator)))))))

  (nskk-it "hiragana indicator contains かな"
    (nskk-with-state 'hiragana
      (let ((nskk-use-color-cursor nil))
        (should (string-match-p "かな" (nskk-modeline-indicator))))))

  (nskk-it "katakana indicator contains カナ"
    (nskk-with-state 'katakana
      (let ((nskk-use-color-cursor nil))
        (should (string-match-p "カナ" (nskk-modeline-indicator))))))

  (nskk-it "ascii indicator contains SKK"
    (nskk-with-state 'ascii
      (let ((nskk-use-color-cursor nil))
        (should (string-match-p "SKK" (nskk-modeline-indicator))))))

  (nskk-it "latin indicator contains SKK"
    (nskk-with-state 'latin
      (let ((nskk-use-color-cursor nil))
        (should (string-match-p "SKK" (nskk-modeline-indicator))))))

  (nskk-it "hiragana and katakana indicators are distinct"
    (let ((nskk-use-color-cursor nil))
      (let ((ind-hira (nskk-with-state 'hiragana
                        (let ((nskk--modeline-indicator-cache nil))
                          (nskk-modeline-indicator))))
            (ind-kata (nskk-with-state 'katakana
                        (let ((nskk--modeline-indicator-cache nil))
                          (nskk-modeline-indicator)))))
        (should-not (string= ind-hira ind-kata))))))

;;;; Memoization cache crosses module boundary

(nskk-describe "modeline indicator cache"

  (nskk-it "cache is nil before first indicator call"
    (nskk-with-state 'hiragana
      (let ((nskk--modeline-indicator-cache nil))
        (should-not nskk--modeline-indicator-cache))))

  (nskk-it "indicator call populates the cache"
    (nskk-with-state 'hiragana
      (let ((nskk-use-color-cursor nil)
            (nskk--modeline-indicator-cache nil))
        (nskk-when (nskk-modeline-indicator))
        (nskk-then (should nskk--modeline-indicator-cache)))))

  (nskk-it "cache is keyed by the current mode symbol"
    (nskk-with-state 'hiragana
      (let ((nskk-use-color-cursor nil)
            (nskk--modeline-indicator-cache nil))
        (nskk-when (nskk-modeline-indicator))
        (nskk-then
          (should (eq (car nskk--modeline-indicator-cache) 'hiragana))))))

  (nskk-it "nskk--modeline-clear-cache removes the cached entry"
    (nskk-with-state 'hiragana
      (let ((nskk-use-color-cursor nil)
            (nskk--modeline-indicator-cache nil))
        (nskk-given (nskk-modeline-indicator))
        (nskk-when (nskk--modeline-clear-cache))
        (nskk-then (should-not nskk--modeline-indicator-cache)))))

  (nskk-it "second indicator call with same mode reuses cached data"
    (nskk-with-state 'hiragana
      (let ((nskk-use-color-cursor nil)
            (nskk--modeline-indicator-cache nil))
        (let ((first  (nskk-modeline-indicator))
              (second (nskk-modeline-indicator)))
          (should (string= first second)))))))

;;;; nskk-modeline-update crosses the presentation↔domain boundary

(nskk-describe "modeline-update cross-module call chain"

  (nskk-it "nskk-modeline-update clears the indicator cache"
    (with-temp-buffer
      (nskk-with-state 'hiragana
        (let ((nskk-use-color-cursor nil)
              (nskk--modeline-indicator-cache
               (cons 'hiragana '("かな" default "Hiragana"))))
          (nskk-when (nskk-modeline-update))
          (nskk-then (should-not nskk--modeline-indicator-cache))))))

  (nskk-it "nskk-cursor-update does not signal when nskk-use-color-cursor is nil"
    (nskk-with-state 'hiragana
      (let ((nskk-use-color-cursor nil))
        (should-not (condition-case nil
                        (progn (nskk-cursor-update) nil)
                      (error t))))))

  (nskk-it "nskk-modeline-update does not signal when state is nil"
    (with-temp-buffer
      (nskk-with-state nil
        (let ((nskk-use-color-cursor nil))
          (should-not (condition-case nil
                          (progn (nskk-modeline-update) nil)
                        (error t))))))))

;;;; Cursor color

(nskk-describe "cursor color"

  (nskk-it "calls set-cursor-color when nskk-use-color-cursor is t"
    (nskk-with-state 'hiragana
      (let (captured-color)
        (nskk-with-mocks ((set-cursor-color (lambda (color)
                                              (setq captured-color color))))
          (nskk-when
            (let ((nskk-use-color-cursor t))
              (nskk-cursor-update)))
          (nskk-then
            (should (stringp captured-color)))))))

  (nskk-it "does not call set-cursor-color when nskk-use-color-cursor is nil"
    (nskk-with-state 'hiragana
      (let ((call-count 0))
        (nskk-with-mocks ((set-cursor-color (lambda (_color)
                                              (cl-incf call-count))))
          (nskk-when
            (let ((nskk-use-color-cursor nil))
              (nskk-cursor-update)))
          (nskk-then
            (should (= 0 call-count)))))))

  (nskk-it "does not call set-cursor-color when state is nil"
    (nskk-with-state nil
      (let ((call-count 0))
        (nskk-with-mocks ((set-cursor-color (lambda (_color)
                                              (cl-incf call-count))))
          (nskk-when
            (let ((nskk-use-color-cursor t))
              (nskk-cursor-update)))
          (nskk-then
            (should (= 0 call-count))))))))

;;;; Mode-line format

(nskk-describe "mode-line format"

  (nskk-it "nskk-modeline-indicator returns a string for hiragana mode"
    (nskk-with-state 'hiragana
      (let ((nskk-use-color-cursor nil))
        (should (stringp (nskk-modeline-indicator))))))

  (nskk-it "hiragana indicator string contains the hiragana mode marker"
    (nskk-with-state 'hiragana
      (let ((nskk-use-color-cursor nil)
            (nskk--modeline-indicator-cache nil))
        (should (string-match-p "かな" (nskk-modeline-indicator))))))

  (nskk-it "indicators differ between hiragana and ascii modes"
    (let ((nskk-use-color-cursor nil))
      (let ((ind-hira (nskk-with-state 'hiragana
                        (let ((nskk--modeline-indicator-cache nil))
                          (nskk-modeline-indicator))))
            (ind-ascii (nskk-with-state 'ascii
                         (let ((nskk--modeline-indicator-cache nil))
                           (nskk-modeline-indicator)))))
        (should-not (string= ind-hira ind-ascii))))))

(provide 'nskk-modeline-state-integration-test)

;;; nskk-modeline-state-integration-test.el ends here
