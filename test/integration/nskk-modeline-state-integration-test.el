;;; nskk-modeline-state-integration-test.el --- Modeline↔State integration tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Modeline↔State integration tests.

;;; Code:

(require 'ert)
(require 'nskk-modeline)
(require 'nskk-state)
(require 'nskk-prolog)
(require 'nskk-test-framework)
(require 'nskk-test-macros)

;;;; Modeline indicator queries state

(nskk-describe "modeline indicator from state"

  (nskk-it "indicators differ between hiragana and ascii modes"
    (let ((nskk-use-color-cursor nil))
      (let ((ind-hira (nskk-with-state 'hiragana
                        (let ((nskk--modeline-indicator-cache nil))
                          (nskk-modeline-indicator))))
            (ind-ascii (nskk-with-state 'ascii
                         (let ((nskk--modeline-indicator-cache nil))
                           (nskk-modeline-indicator)))))
        (should-not (string= ind-hira ind-ascii))))))

;;;; Memoization cache crosses module boundary

(nskk-describe "modeline indicator cache"

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
        (nskk-then (should-not nskk--modeline-indicator-cache))))))

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
        (nskk-should-not-error (nskk-cursor-update)))))

  (nskk-it "nskk-modeline-update does not signal when state is nil"
    (with-temp-buffer
      (nskk-with-state nil
        (let ((nskk-use-color-cursor nil))
          (nskk-should-not-error (nskk-modeline-update)))))))

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

(provide 'nskk-modeline-state-integration-test)

;;; nskk-modeline-state-integration-test.el ends here
