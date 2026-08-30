;;; nskk-presentation-action-test.el --- Tests for the presentation-action cleanup protocol -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Tests for the `presentation-action/2' registration protocol introduced in
;; FR-001 (henkan -> inline dependency inversion).
;;
;; nskk-henkan no longer references nskk-inline directly.  Inline registers
;; its cleanup (`nskk-inline-hide') and terminal-finalize
;; (`nskk--inline-finalize') callbacks via the `presentation-action/2' fact
;; table, and `nskk--clear-conversion-context' enumerates and runs them.
;;
;; This file verifies the terminal-cleanup invariant: when a registered
;; cleanup callback signals, the finalize stage still removes the inline
;; overlay (and henkan re-asserts its own overlays) before re-signaling the
;; original condition.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nskk-henkan)
(require 'nskk-state)
(require 'nskk-prolog)
(require 'nskk-inline)
(require 'nskk-test-framework)
(require 'nskk-test-macros)

(nskk-describe "presentation-action terminal cleanup"
  (nskk-it "removes every overlay when a cleanup callback signals an error"
    (with-temp-buffer
      (insert "x")
      (let* ((nskk-current-state (nskk-state-create 'hiragana))
             (conversion-overlay (make-overlay (point-min) (point-max)))
             (pending-overlay (make-overlay (point-min) (point-max)))
             (dcomp-overlay (make-overlay (point-min) (point-max)))
             (inline-overlay (make-overlay (point-min) (point-max)))
             (nskk--conversion-overlay conversion-overlay)
             (nskk--pending-romaji-overlay pending-overlay)
             (nskk--dcomp-multiple-overlay dcomp-overlay)
             (nskk--inline-overlay inline-overlay)
             (nskk--conversion-start-marker nil)
             (nskk--romaji-buffer "")
             (nskk--dcomp-candidates nil)
             (nskk--dcomp-prefix nil)
             (nskk--dcomp-index 0)
             (nskk--henkan-candidate-list-active nil)
             caught)
        (cl-letf (((symbol-function 'nskk-inline-hide)
                   (lambda () (signal 'error '(cleanup-fault payload)))))
          (setq caught
                (condition-case condition
                    (progn (nskk--clear-conversion-context) nil)
                  (error condition))))
        (should (eq (car caught) 'error))
        (should (equal (cdr caught) '(cleanup-fault payload)))
        (should-not nskk--conversion-overlay)
        (should-not (overlay-buffer conversion-overlay))
        (should-not nskk--pending-romaji-overlay)
        (should-not (overlay-buffer pending-overlay))
        (should-not nskk--dcomp-multiple-overlay)
        (should-not (overlay-buffer dcomp-overlay))
        (should-not nskk--inline-overlay)
        (should-not (overlay-buffer inline-overlay))))))

(provide 'nskk-presentation-action-test)

;;; nskk-presentation-action-test.el ends here
