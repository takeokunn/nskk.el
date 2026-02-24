;;; nskk-okurigana-test.el --- Tests for okurigana functionality -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

(require 'ert)
(require 'nskk-state)
(require 'nskk-input-commands)

;;; Code:

;;;; Okurigana Detection Tests

(ert-deftest nskk-test-detect-okurigana-uppercase ()
  "Test uppercase character detection returns lowercase."
  (should (eq (nskk-detect-okurigana-char ?K) ?k))
  (should (eq (nskk-detect-okurigana-char ?S) ?s))
  (should (eq (nskk-detect-okurigana-char ?T) ?t))
  (should (eq (nskk-detect-okurigana-char ?N) ?n)))

(ert-deftest nskk-test-detect-okurigana-lowercase-nil ()
  "Test that lowercase characters return nil."
  (should (null (nskk-detect-okurigana-char ?k)))
  (should (null (nskk-detect-okurigana-char ?a)))
  (should (null (nskk-detect-okurigana-char ?z))))

(ert-deftest nskk-test-detect-okurigana-non-alpha-nil ()
  "Test that non-alphabetic characters return nil."
  (should (null (nskk-detect-okurigana-char ?1)))
  (should (null (nskk-detect-okurigana-char ? )))
  (should (null (nskk-detect-okurigana-char ?.))))

;;;; State Okurigana Tests

(ert-deftest nskk-test-state-okurigana-set-get ()
  "Test okurigana storage in state metadata."
  (let ((state (nskk-state-create)))
    (nskk-state-set-okurigana state ?k)
    (should (eq (nskk-state-get-okurigana state) ?k))
    (nskk-state-set-okurigana state ?s)
    (should (eq (nskk-state-get-okurigana state) ?s))))

(ert-deftest nskk-test-state-okurigana-initial-nil ()
  "Test that okurigana is initially nil."
  (let ((state (nskk-state-create)))
    (should (null (nskk-state-get-okurigana state)))))

;;;; Test Runner

(defun nskk-okurigana-test-run-all ()
  "Run all okurigana tests."
  (interactive)
  (ert-run-tests-interactively "^nskk-test-.*okurigana"))

(provide 'nskk-okurigana-test)

;;; nskk-okurigana-test.el ends here