;;; nskk-conversion-flow-pbt-test.el --- Conversion flow PBT tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test, property-based
;; Homepage: https://github.com/takeokunn/nskk.el

;; This file is part of NSKK.

;; NSKK is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; NSKK is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with NSKK.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:


;;; Code:

(require 'ert)
(require 'nskk-test-framework)
(require 'nskk-test-macros)
(require 'nskk-pbt-generators)
(require 'nskk-pbt-shrink)
(require 'nskk-converter)
(require 'nskk-dictionary)
(require 'nskk-henkan)


;;;;
;;;; CPS Tests: /k suffix function calling convention
;;;;

(nskk-property-test-with-shrinking nskk-property-cps-converter-convert-calls-exactly-one-branch
  ((pattern romaji-pattern))
  (let ((call-count 0)
        (branch nil))
    (condition-case nil
        (nskk-converter-convert/k
         pattern
         (lambda (_kana _remaining) (cl-incf call-count) (setq branch 'match))
         (lambda (_prefix) (cl-incf call-count) (setq branch 'incomplete))
         (lambda () (cl-incf call-count) (setq branch 'fail)))
      (error (setq branch 'error call-count 1)))
    (and (= call-count 1)
         (memq branch '(match incomplete fail))))
  50)

(nskk-describe "CPS: nskk-converter-convert/k dispatch invariants"

  (nskk-it "on-match continuation receives the expected kana and remaining input exactly once"
    (dolist (case '(("ka" "か" "") ("ki" "き" "")
                    ("a" "あ" "") ("i" "い" "") ("u" "う" "")
                    ("sha" "しゃ" "") ("chi" "ち" "") ("tsu" "つ" "")
                    ("kaki" "か" "ki")))
      (ert-info ((format "romaji=%S" (car case)))
        (let ((result-value nil)
              (remaining-value nil)
              (call-count 0)
              (branch-called nil))
          (nskk-converter-convert/k
           (car case)
           (lambda (kana remaining)
             (cl-incf call-count)
             (setq branch-called 'match result-value kana remaining-value remaining))
           (lambda (_prefix) (cl-incf call-count) (setq branch-called 'incomplete))
           (lambda () (cl-incf call-count) (setq branch-called 'fail)))
          (should (= call-count 1))
          (should (eq branch-called 'match))
          (should (equal result-value (cadr case)))
          (should (equal remaining-value (caddr case))))))))

;;;;
;;;; CPS Tests: nskk-dict-lookup/k mutual exclusion
;;;;

(nskk-property-test-with-shrinking nskk-property-cps-dict-lookup-exactly-one-branch
  ((key search-query))
  (nskk-with-mock-dict nil
    (let ((found-count 0)
          (not-found-count 0))
      (nskk-dict-lookup/k
       key
       (lambda (_entry) (cl-incf found-count))
       (lambda () (cl-incf not-found-count)))
      (= (+ found-count not-found-count) 1)))
  50)

(nskk-describe "CPS: nskk-dict-lookup/k mutual exclusion"

  (nskk-it "calls exactly one of on-found or on-not-found"
    (nskk-with-mock-dict nil
      (dolist (case '(("かんじ" "漢字" "感じ" "幹事")
                      ("にほん" "日本" "二本")
                      ("nonexistent-xyz") ("さくら" "桜") ("does-not-exist")))
        (ert-info ((format "key=%S" (car case)))
          (let ((found-count 0)
                (not-found-count 0)
                (result-value nil))
            (nskk-dict-lookup/k
             (car case)
             (lambda (entry) (cl-incf found-count) (setq result-value entry))
             (lambda () (cl-incf not-found-count)))
            (should (= found-count (if (cdr case) 1 0)))
            (should (= not-found-count (if (cdr case) 0 1)))
            (should (equal result-value (cdr case)))))))))


(ert-deftest nskk-henkan-numeric-annotation-optional-module-fresh-process ()
  "Preserve raw user annotations with and without the optional module."
  (let* ((source-directory
          (file-name-directory (symbol-file 'nskk-commit-current 'defun)))
         (directory (make-temp-file "nskk-annotation-module-" t))
         (probe (expand-file-name "probe.el" directory))
         (emacs (expand-file-name invocation-name invocation-directory)))
    (unwind-protect
        (dolist (loaded '(nil t))
          (with-temp-file probe
            (prin1
             `(progn
                (setq user-emacs-directory ,(file-name-as-directory directory)
                      temporary-file-directory user-emacs-directory
                      nskk-dict-user-dictionary-file
                      ,(expand-file-name "user" directory)
                      nskk-study-file ,(expand-file-name "study" directory)
                      nskk-search-learning-file
                      ,(expand-file-name "learning" directory)
                      nskk-dict-system-dictionary-files nil
                      nskk-dict-use-ja-dic nil)
                (add-to-list 'load-path ,source-directory)
                (require 'ert)
                (require 'nskk-henkan)
                ,@(when loaded '((require 'nskk-annotation)))
                (should (eq (featurep 'nskk-annotation) ,loaded))
                (should (eq (and (fboundp 'nskk-annotation-lookup) t) ,loaded))
                (should (file-in-directory-p
                         (symbol-file 'nskk-commit-current 'defun)
                         ,source-directory))
                (nskk-prolog-set-index 'user-dict-entry 2 :trie)
                (nskk-prolog-assert
                 '((user-dict-entry "34" ("三十四" "卅四"))))
                (nskk-prolog-assert
                 '((user-dict-source-entry "#こ" (("#4;expanded") . nil))))
                (should (equal (nskk--dict-user-annotation "#こ" "#4")
                               "expanded"))
                (with-temp-buffer
                  (let* ((state (nskk-state-create 'hiragana))
                         (nskk-current-state state)
                         (candidate (copy-sequence "卅四"))
                         (nskk--user-dict-index 'user)
                         (nskk-jisyo-update-hook nil))
                    (insert nskk-henkan-active-marker candidate)
                    (nskk-state-set-conversion-start-marker
                     (copy-marker (point-min)))
                    (nskk-state-set-conversion-overlay
                     (make-overlay
                      (+ (point-min) (length nskk-henkan-active-marker))
                      (point-max)))
                    (nskk-state-set-candidates state (list candidate))
                    (setf (nskk-state-current-index state) 0)
                    (nskk-state-force-henkan-phase state 'active)
                    (nskk-state-put-metadata state 'henkan-reading "34こ")
                    (nskk-state-put-metadata
                     state 'annotation-candidates (list (cons candidate "#4")))
                    (nskk-state-put-metadata
                     state 'numeric-raw-candidates
                     (list (cons candidate "卅四;expanded")))
                    (nskk-commit-current)
                    (should (equal (buffer-string) "卅四"))
                    (should (equal (nskk--dict-user-annotation "34" "卅四")
                                   "expanded"))))
                (princ "ANNOTATION-CHECKS=6 PASS\n"))
             (current-buffer)))
          (with-temp-buffer
            (let ((exit (call-process emacs nil t nil "-Q" "--batch" "-l" probe)))
              (ert-info ((format "module=%S exit=%S output=%s"
                                 loaded exit (buffer-string)))
                (should (equal exit 0))
                (should (string-match-p "^ANNOTATION-CHECKS=6 PASS$"
                                        (buffer-string)))))))
      (delete-directory directory t))))

(provide 'nskk-conversion-flow-pbt-test)

;;; nskk-conversion-flow-pbt-test.el ends here
