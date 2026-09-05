;;; nskk-server-henkan-integration-test.el --- Server↔Henkan pipeline integration tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Server↔Henkan pipeline integration tests.

;;; Code:

(require 'ert)
(require 'nskk-server)
(require 'nskk-annotation)
(require 'nskk-henkan)
(require 'nskk-state)
(require 'nskk)
(require 'nskk-test-framework)
(require 'nskk-test-macros)

;;;; Helper: dict that does not contain the test key "てすと"

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

  (nskk-it "preserves server annotations for candidates returned by core search"
    (nskk-with-mock-dict nskk-server-henkan--dict-without-test-key
      (let* ((mock (nskk--server-start-mock-server
                    '(("てすと" . "1/テスト;注釈/試験/\n"))))
             (server-proc (car mock))
             (nskk-server-enable t)
             (nskk-server-host "127.0.0.1")
             (nskk-server-portnum (cdr mock))
             (nskk--server-process nil)
             (nskk--server-kill-emacs-hook-registered nil)
             (nskk--annotation-initialized nil))
        (unwind-protect
            (progn
              (should (nskk-server-open))
              (should-not (nskk-annotation-lookup "てすと" "テスト"))
              (should (equal (nskk-core-search "てすと") '("テスト" "試験")))
              (should (equal (nskk-annotation-lookup "てすと" "テスト") "注釈"))
              (should-not (nskk-annotation-lookup "てすと" "試験")))
          (nskk-server-close)
          (delete-process server-proc)))))

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
              (should (null (nskk-core-search "zzzzzzzz"))))
          (nskk-server-close)
          (delete-process server-proc)))))

  (nskk-it "server hit takes priority: dict candidates are not used"
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
                (should result)
                (should (member "サーバの結果" result))
                (should-not (member "辞書の結果" result))))
          (nskk-server-close)
          (delete-process server-proc))))))

;;;; User dictionary merged with server (opt-in via
;;;; `nskk-search-merge-user-dict-with-server')

(nskk-describe "user dictionary merged with server"

  (nskk-it "merges user dict candidates ahead of server when enabled"
    (nskk-with-mock-dict '(("ゆき" . ("優響")))
      (let* ((mock (nskk--server-start-mock-server
                    '(("ゆき" . "1/雪/行き/\n"))))
             (server-proc (car mock))
             (port         (cdr mock))
             (nskk-server-enable t)
             (nskk-server-host "127.0.0.1")
             (nskk-server-portnum port)
             (nskk--server-process nil)
             (nskk--server-kill-emacs-hook-registered nil)
             (nskk-search-merge-user-dict-with-server t))
        (unwind-protect
            (progn
              (should (nskk-server-open))
              (let ((result (nskk-core-search "ゆき")))
                (should (equal result '("優響" "雪" "行き")))))
          (nskk-server-close)
          (delete-process server-proc)))))

  (nskk-it "deduplicates candidates shared by user dict and server"
    (nskk-with-mock-dict '(("ゆき" . ("優響" "雪")))
      (let* ((mock (nskk--server-start-mock-server
                    '(("ゆき" . "1/雪/行き/\n"))))
             (server-proc (car mock))
             (port         (cdr mock))
             (nskk-server-enable t)
             (nskk-server-host "127.0.0.1")
             (nskk-server-portnum port)
             (nskk--server-process nil)
             (nskk--server-kill-emacs-hook-registered nil)
             (nskk-search-merge-user-dict-with-server t))
        (unwind-protect
            (progn
              (should (nskk-server-open))
              (let ((result (nskk-core-search "ゆき")))
                (should (equal result '("優響" "雪" "行き")))))
          (nskk-server-close)
          (delete-process server-proc)))))

  (nskk-it "falls back to server when merge enabled but user dict misses"
    (nskk-with-mock-dict nskk-server-henkan--dict-without-test-key
      (let* ((mock (nskk--server-start-mock-server
                    '(("ゆき" . "1/雪/\n"))))
             (server-proc (car mock))
             (port         (cdr mock))
             (nskk-server-enable t)
             (nskk-server-host "127.0.0.1")
             (nskk-server-portnum port)
             (nskk--server-process nil)
             (nskk--server-kill-emacs-hook-registered nil)
             (nskk-search-merge-user-dict-with-server t))
        (unwind-protect
            (progn
              (should (nskk-server-open))
              (let ((result (nskk-core-search "ゆき")))
                (should (member "雪" result))))
          (nskk-server-close)
          (delete-process server-proc))))))

(provide 'nskk-server-henkan-integration-test)

;;; nskk-server-henkan-integration-test.el ends here
