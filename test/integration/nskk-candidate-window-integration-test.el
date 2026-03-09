;;; nskk-candidate-window-integration-test.el --- Candidate window integration tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Integration tests for the hook wiring between nskk-candidate-window.el
;; (Layer 5: Presentation) and the henkan pipeline.
;;
;; In nskk.el, nskk--enable wires the candidate window into the pipeline:
;;   (add-hook 'nskk-henkan-show-candidates-functions #'nskk-candidate-show-list)
;;   (add-hook 'nskk-henkan-hide-candidates-functions #'nskk-candidate-hide-list)
;;   (setq nskk-henkan-select-candidate-by-key-function
;;         #'nskk-candidate-list-select-by-key)
;;
;; These tests replicate that wiring manually and verify:
;; - Overlay lifecycle: show → active, hide → inactive.
;; - show/hide/show cycle restores the active state correctly.
;; - nskk-candidate-list-select-by-key returns the correct absolute index
;;   for home-row selection keys, including across page offsets.
;; - run-hook-with-args on the show hook triggers overlay display.
;; - run-hooks on the hide hook clears overlay display.
;;
;; Overlays are fully batch-safe in Emacs; make-overlay works in
;; noninteractive mode, so no display guard is needed here.

;;; Code:

(require 'ert)
(require 'nskk-candidate-window)
(require 'nskk-state)
(require 'nskk-test-framework)
(require 'nskk-test-macros)

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

;;;; Key selection (home-row index mapping)

(nskk-describe "candidate key selection"

  (nskk-it "select-by-key with ?a returns absolute index 0"
    (let ((candidates '("漢字" "感じ" "幹事" "環境" "官邸" "換気" "韓国")))
      (should (= 0 (nskk-candidate-list-select-by-key ?a candidates 0)))))

  (nskk-it "select-by-key with ?s returns absolute index 1"
    (let ((candidates '("漢字" "感じ" "幹事" "環境" "官邸" "換気" "韓国")))
      (should (= 1 (nskk-candidate-list-select-by-key ?s candidates 0)))))

  (nskk-it "select-by-key with ?d returns absolute index 2"
    (let ((candidates '("漢字" "感じ" "幹事" "環境" "官邸" "換気" "韓国")))
      (should (= 2 (nskk-candidate-list-select-by-key ?d candidates 0)))))

  (nskk-it "select-by-key returns nil when position exceeds candidate count"
    ;; Only 1 candidate; ?s would map to absolute index 1 — out of range.
    (let ((candidates '("漢字")))
      (should (null (nskk-candidate-list-select-by-key ?s candidates 0)))))

  (nskk-it "select-by-key with page offset returns correct absolute index"
    ;; Page 2 starts at index 7; ?a → position 0 → absolute 7.
    (let ((candidates (make-list 14 "候補")))
      (should (= 7 (nskk-candidate-list-select-by-key ?a candidates 7)))))

  (nskk-it "select-by-key with ?s on page 2 offset returns absolute index 8"
    ;; Page 2, second key ?s → position 1 → absolute 7+1=8.
    (let ((candidates (make-list 14 "候補")))
      (should (= 8 (nskk-candidate-list-select-by-key ?s candidates 7))))))

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

;;;
;;; Property-Based Tests
;;;

(require 'nskk-pbt-generators)

(nskk-deftest-table candidate-key-selection
  :columns (key-char expected-index)
  :rows ((?a 0) (?s 1) (?d 2) (?f 3) (?j 4) (?k 5) (?l 6))
  :body
  (with-temp-buffer
    (let ((nskk-henkan-show-candidates-keys '(?a ?s ?d ?f ?j ?k ?l))
          (nskk-henkan-number-to-display-candidates 7)
          (candidates '("a" "b" "c" "d" "e" "f" "g")))
      (nskk-candidate-show-list candidates 0)
      (let ((result (nskk-candidate-list-select-by-key key-char candidates 0)))
        (nskk-candidate-hide-list)
        (should (= result expected-index))))))

(nskk-property-test candidate-show-hide-active-p-consistency
  ((cv candidates-with-valid-index))
  (with-temp-buffer
    (let ((nskk-henkan-show-candidates-keys '(?a ?s ?d ?f ?j ?k ?l))
          (nskk-henkan-number-to-display-candidates 7)
          (candidates (plist-get cv :candidates))
          (idx (plist-get cv :index)))
      (nskk-candidate-show-list candidates idx)
      (should (nskk-candidate-list-active-p))
      (nskk-candidate-hide-list)
      (should (null (nskk-candidate-list-active-p)))))
  30)

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
              (nskk-candidate-hide-list))
            (should-not (nskk-candidate-list-active-p))))))))


(provide 'nskk-candidate-window-integration-test)

;;; nskk-candidate-window-integration-test.el ends here
