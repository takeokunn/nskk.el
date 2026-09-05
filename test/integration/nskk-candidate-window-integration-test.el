;;; nskk-candidate-window-integration-test.el --- Candidate window integration tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Candidate window integration tests.

;;; Code:

(require 'ert)
(require 'nskk-candidate-window)
(require 'nskk-state)
(require 'nskk-test-framework)
(require 'nskk-test-macros)
(require 'nskk-pbt-generators)

;;;; Overlay lifecycle

(nskk-describe "candidate overlay lifecycle"

  (nskk-it "show-list makes candidate-list-active-p return t"
    (with-temp-buffer
      (let ((nskk-henkan-show-candidates-keys '(?a ?s ?d ?f ?j ?k ?l))
            (nskk-henkan-number-to-display-candidates 7))
        (nskk-candidate-show-list '("漢字" "感じ" "幹事") 0)
        (should (nskk-candidate-list-active-p)))))

  (nskk-it "hide-list after show makes candidate-list-active-p return nil"
    (with-temp-buffer
      (let ((nskk-henkan-show-candidates-keys '(?a ?s ?d ?f ?j ?k ?l))
            (nskk-henkan-number-to-display-candidates 7))
        (nskk-candidate-show-list '("漢字" "感じ") 0)
        (should (nskk-candidate-list-active-p))
        (nskk-candidate-hide-list)
        (should-not (nskk-candidate-list-active-p)))))

  (nskk-it "show→hide→show cycle restores active state"
    (with-temp-buffer
      (let ((nskk-henkan-show-candidates-keys '(?a ?s ?d ?f ?j ?k ?l))
            (nskk-henkan-number-to-display-candidates 7))
        (nskk-candidate-show-list '("漢字") 0)
        (nskk-candidate-hide-list)
        (nskk-candidate-show-list '("感じ") 0)
        (should (nskk-candidate-list-active-p)))))

  (nskk-it "hide-list is safe to call when not active"
    (with-temp-buffer
      (should-not (condition-case nil
                      (progn (nskk-candidate-hide-list) nil)
                    (error t))))))

;;;; Hook wiring integration

(nskk-describe "candidate window hook wiring"

  (nskk-it "show hook triggers candidate display"
    (with-temp-buffer
      (let ((nskk-henkan-show-candidates-keys '(?a ?s ?d ?f ?j ?k ?l))
            (nskk-henkan-number-to-display-candidates 7)
            (nskk-henkan-show-candidates-functions nil))
        (add-hook 'nskk-henkan-show-candidates-functions
                  #'nskk-candidate-show-list)
        (unwind-protect
            (progn
              (run-hook-with-args 'nskk-henkan-show-candidates-functions
                                  '("漢字" "感じ" "幹事") 0)
              (should (nskk-candidate-list-active-p)))
          (remove-hook 'nskk-henkan-show-candidates-functions
                       #'nskk-candidate-show-list)))))

  (nskk-it "hide hook clears candidate display"
    (with-temp-buffer
      (let ((nskk-henkan-show-candidates-keys '(?a ?s ?d ?f ?j ?k ?l))
            (nskk-henkan-number-to-display-candidates 7)
            (nskk-henkan-show-candidates-functions nil)
            (nskk-henkan-hide-candidates-functions nil))
        (add-hook 'nskk-henkan-show-candidates-functions
                  #'nskk-candidate-show-list)
        (add-hook 'nskk-henkan-hide-candidates-functions
                  #'nskk-candidate-hide-list)
        (unwind-protect
            (progn
              (run-hook-with-args 'nskk-henkan-show-candidates-functions
                                  '("漢字" "感じ") 0)
              (should (nskk-candidate-list-active-p))
              (run-hooks 'nskk-henkan-hide-candidates-functions)
              (should-not (nskk-candidate-list-active-p)))
          (remove-hook 'nskk-henkan-show-candidates-functions
                       #'nskk-candidate-show-list)
          (remove-hook 'nskk-henkan-hide-candidates-functions
                       #'nskk-candidate-hide-list)))))

  (nskk-it "select-candidate-by-key-function variable can be set to nskk-candidate-list-select-by-key"
    (let ((nskk-henkan-select-candidate-by-key-function
           #'nskk-candidate-list-select-by-key))
      (let ((candidates '("漢字" "感じ" "幹事")))
        (should (= 0 (funcall nskk-henkan-select-candidate-by-key-function
                              ?a candidates 0)))))))

(nskk-deftest-table candidate-key-selection
  :columns (key-char expected-index)
  :rows ((?a 0) (?l 6))
  :body
  (with-temp-buffer
    (let ((nskk-henkan-show-candidates-keys '(?a ?s ?d ?f ?j ?k ?l))
          (nskk-henkan-number-to-display-candidates 7)
          (candidates '("a" "b" "c" "d" "e" "f" "g")))
      (nskk-candidate-show-list candidates 0)
      (let ((result (nskk-candidate-list-select-by-key key-char candidates 0)))
        (nskk-candidate-hide-list)
        (should (= result expected-index))))))

(nskk-describe "Candidate window property: show/hide cycle"
  (nskk-it "repeated show/hide cycles always end inactive"
    (dotimes (_ 20)
      (nskk-for-all ((cv candidates-with-valid-index))
        (with-temp-buffer
          (let ((nskk-henkan-show-candidates-keys '(?a ?s ?d ?f ?j ?k ?l))
                (nskk-henkan-number-to-display-candidates 7)
                (candidates (plist-get cv :candidates))
                (idx (plist-get cv :index)))
            (dotimes (_ 3)
              (nskk-candidate-show-list candidates idx)
              (should (nskk-candidate-list-active-p))
              (nskk-candidate-hide-list))
            (should-not (nskk-candidate-list-active-p))))))))

(nskk-describe "Candidate window transactional hook recovery"
  (nskk-it "recovers from overlay-put quit and retries through the show hook"
    (with-temp-buffer
      (let ((nskk-henkan-show-candidates-keys '(?a ?s ?d ?f ?j ?k ?l))
            (nskk-henkan-number-to-display-candidates 7)
            (nskk-henkan-show-candidates-functions nil)
            (payload (list 'candidate-overlay-put-quit))
            (original-overlay-put (symbol-function 'overlay-put))
            saved-overlay
            caught)
        (add-hook 'nskk-henkan-show-candidates-functions
                  #'nskk-candidate-show-list)
        (unwind-protect
            (progn
              (cl-letf (((symbol-function 'overlay-put)
                         (lambda (overlay property value)
                           (prog1
                               (funcall original-overlay-put
                                        overlay property value)
                             (setq saved-overlay overlay)
                             (signal 'quit (list payload))))))
                (condition-case condition
                    (run-hook-with-args
                     'nskk-henkan-show-candidates-functions
                     '("candidate-a" "candidate-b") 0)
                  (quit
                   (setq caught condition))))
              (should (eq (car caught) 'quit))
              (should (eq (cadr caught) payload))
              (should-not nskk--candidate-list-active)
              (should-not (nskk-state-candidate-overlay))
              (should (overlayp saved-overlay))
              (should-not (overlay-buffer saved-overlay))
              (run-hook-with-args
               'nskk-henkan-show-candidates-functions
               '("candidate-a" "candidate-b") 0)
              (should (nskk-candidate-list-active-p))
              (should (overlayp (nskk-state-candidate-overlay))))
          (remove-hook 'nskk-henkan-show-candidates-functions
                       #'nskk-candidate-show-list)
          (nskk-candidate-hide-list))))))

(provide 'nskk-candidate-window-integration-test)

;;; nskk-candidate-window-integration-test.el ends here
