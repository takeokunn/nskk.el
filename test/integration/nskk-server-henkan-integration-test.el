;;; nskk-server-henkan-integration-test.el --- Server↔Henkan pipeline integration tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Integration tests for the server fallthrough pipeline in nskk-core-search.
;;
;; In nskk-henkan.el, nskk-core-search with :exact type implements:
;;   (or (nskk-dict-lookup key)
;;       (when (and nskk-server-enable (nskk-server-ensure-open))
;;         (nskk-server-lookup key)))
;;
;; These tests exercise this two-module interaction using a dual-mock strategy:
;; - A mock dictionary (nskk-with-mock-dict) that does NOT contain the target key,
;;   causing nskk-dict-lookup to return nil and triggering the server path.
;; - An in-process Elisp TCP mock skkserv (nskk--server-start-mock-server from
;;   nskk-test-framework.el) that responds with pre-defined candidates.
;;
;; Tests verify:
;; - When server is disabled, nskk-core-search does not attempt network I/O.
;; - When the dictionary misses and the server is enabled, server candidates
;;   are returned from nskk-core-search.
;; - When the dictionary hits, its result takes priority over the server.
;; - An unknown key returns nil even if the server is reachable.

;;; Code:

(require 'ert)
(require 'nskk-server)
(require 'nskk-henkan)
(require 'nskk-state)
(require 'nskk-custom)
(require 'nskk-test-framework)
(require 'nskk-test-macros)

;;;; Helper: dict that does not contain the test key "てすと"

;; The mock dict contains only "zzz" so that nskk-dict-lookup "てすと" → nil,
;; forcing the server fallthrough path in nskk-core-search.
(defconst nskk-server-henkan--dict-without-test-key
  '(("zzz" . ("zzz")))
  "A one-entry mock dictionary guaranteed not to contain \"てすと\".")

;;;; Server disabled — no network call

(nskk-describe "server fallthrough disabled"

  (nskk-it "nskk-core-search returns nil when server disabled and dict misses"
    (nskk-with-mock-dict nskk-server-henkan--dict-without-test-key
      (let ((nskk-server-enable nil)
            (nskk--server-process nil))
        (should (null (nskk-core-search "てすと"))))))

  (nskk-it "nskk-core-search returns nil for a non-string key"
    (nskk-with-mock-dict nskk-server-henkan--dict-without-test-key
      (let ((nskk-server-enable nil))
        (should (null (nskk-core-search nil))))))

  (nskk-it "nskk-server-lookup is not called when server disabled"
    (nskk-with-mock-dict nskk-server-henkan--dict-without-test-key
      (let ((nskk-server-enable nil)
            (nskk--server-process nil)
            (lookup-called nil))
        (nskk-with-mocks ((nskk-server-lookup/k
                           (lambda (_key _on-found _on-not-found)
                             (setq lookup-called t))))
          (nskk-core-search "てすと"))
        (should-not lookup-called)))))

;;;; Server fallthrough with in-process mock skkserv

(nskk-describe "server fallthrough with mock skkserv"

  (nskk-it "nskk-core-search falls through to server when dict misses"
    (nskk-with-mock-dict nskk-server-henkan--dict-without-test-key
      (let* ((mock (nskk--server-start-mock-server
                    '(("てすと" . "1/テスト/\n"))))
             (server-proc (car mock))
             (port         (cdr mock))
             (nskk-server-enable t)
             (nskk-server-host "127.0.0.1")
             (nskk-server-portnum port)
             (nskk--server-process nil)
             (nskk--server-kill-emacs-hook-registered nil))
        (unwind-protect
            (progn
              (should (nskk-server-open))
              (let ((result (nskk-core-search "てすと")))
                ;; Dict missed → server provided candidates
                (should result)
                (should (listp result))
                (should (member "テスト" result))))
          (nskk-server-close)
          (delete-process server-proc)))))

  (nskk-it "nskk-core-search returns nil when key unknown to both dict and server"
    (nskk-with-mock-dict nskk-server-henkan--dict-without-test-key
      (let* ((mock (nskk--server-start-mock-server
                    '(("てすと" . "1/テスト/\n"))))
             (server-proc (car mock))
             (port         (cdr mock))
             (nskk-server-enable t)
             (nskk-server-host "127.0.0.1")
             (nskk-server-portnum port)
             (nskk--server-process nil)
             (nskk--server-kill-emacs-hook-registered nil))
        (unwind-protect
            (progn
              (should (nskk-server-open))
              ;; Neither dict nor mock server know this key
              (should (null (nskk-core-search "zzzzzzzz"))))
          (nskk-server-close)
          (delete-process server-proc)))))

  (nskk-it "dict hit takes priority: server candidates are not used"
    ;; Mock dict contains "てすと" → dict-lookup returns ("辞書の結果")
    ;; Mock server also has "てすと" with a different answer
    ;; nskk-core-search must return the dict result, not the server result.
    (nskk-with-mock-dict '(("てすと" . ("辞書の結果")))
      (let* ((mock (nskk--server-start-mock-server
                    '(("てすと" . "1/サーバの結果/\n"))))
             (server-proc (car mock))
             (port         (cdr mock))
             (nskk-server-enable t)
             (nskk-server-host "127.0.0.1")
             (nskk-server-portnum port)
             (nskk--server-process nil)
             (nskk--server-kill-emacs-hook-registered nil))
        (unwind-protect
            (progn
              (should (nskk-server-open))
              (let ((result (nskk-core-search "てすと")))
                ;; Dict hit → dict result must be returned
                (should result)
                (should (member "辞書の結果" result))
                (should-not (member "サーバの結果" result))))
          (nskk-server-close)
          (delete-process server-proc))))))

(provide 'nskk-server-henkan-integration-test)

;;; nskk-server-henkan-integration-test.el ends here
