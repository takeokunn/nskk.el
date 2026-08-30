;;; nskk-study-test.el --- Tests for nskk-study.el -*- lexical-binding: t; -*-
;; Copyright (C) 2026 NSKK Authors
;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test
;; This file is part of NSKK.
;;; Commentary:
;; Unit tests for nskk-study.el covering:
;; - Study association recording
;; - Candidate reordering based on associations
;; - Kakutei history ring management
;; - Max-distance filtering
;; - First-candidate skip behavior
;; - nskk-no-learn text property respect
;; - Save/load round-trip persistence
;;; Code:
(require 'ert)

(require 'nskk-study)

(require 'nskk-prolog)

(require 'nskk-test-framework)

(require 'nskk-test-macros)

;;;
;;; Kakutei History Ring
;;;
(nskk-describe "nskk--study-push-kakutei"
  (nskk-it "pushes an entry onto the ring"
    (let ((nskk--study-kakutei-ring nil)
          (nskk-study-search-times 5))
      (nskk--study-push-kakutei "雨" 10 (current-buffer))
      (should (= (length nskk--study-kakutei-ring) 1))
      (should (equal (plist-get (car nskk--study-kakutei-ring) :word) "雨"))))

  (nskk-it "caps ring size to nskk-study-search-times"
    (let ((nskk--study-kakutei-ring nil)
          (nskk-study-search-times 3))
      (nskk--study-push-kakutei "A" 1 (current-buffer))
      (nskk--study-push-kakutei "B" 2 (current-buffer))
      (nskk--study-push-kakutei "C" 3 (current-buffer))
      (nskk--study-push-kakutei "D" 4 (current-buffer))
      (should (= (length nskk--study-kakutei-ring) 3))
      ;; Most recent at head
      (should (equal (plist-get (car nskk--study-kakutei-ring) :word) "D"))))

  (nskk-it "stores point and buffer in entry"
    (let ((nskk--study-kakutei-ring nil)
          (nskk-study-search-times 5))
      (with-temp-buffer
        (nskk--study-push-kakutei "雨" 42 (current-buffer))
        (should (= (plist-get (car nskk--study-kakutei-ring) :point) 42))
        (should (eq (plist-get (car nskk--study-kakutei-ring) :buffer) (current-buffer)))))))

(nskk-describe
  "nskk--study-recent-words"
  (nskk-it
    "returns words in most-recent-first order"
    (let ((nskk--study-kakutei-ring nil)
          (nskk-study-search-times 5))
      (nskk--study-push-kakutei "A" 1 (current-buffer))
      (nskk--study-push-kakutei "B" 2 (current-buffer))
      (should (equal (nskk--study-recent-words) '("B" "A"))))))

;;;
;;; Distance Check
;;;
(nskk-describe
  "nskk--study-distance-ok-p"
  (nskk-it
    "returns t when no max-distance set"
    (let ((nskk-study-max-distance nil)
          (nskk--study-kakutei-ring
          (list (list :word "雨" :point 10 :buffer (current-buffer)))))
      (should (nskk--study-distance-ok-p 100 (current-buffer)))))
  (nskk-it
    "returns t when ring is empty"
    (let ((nskk-study-max-distance 30)
          (nskk--study-kakutei-ring nil))
      (should (nskk--study-distance-ok-p 100 (current-buffer)))))
  (nskk-it
    "returns t when within max-distance in same buffer"
    (let ((nskk-study-max-distance 30)
          (nskk--study-kakutei-ring
          (list (list :word "雨" :point 10 :buffer (current-buffer)))))
      (should (nskk--study-distance-ok-p 35 (current-buffer)))))
  (nskk-it
    "returns nil when beyond max-distance"
    (let ((nskk-study-max-distance 30)
          (nskk--study-kakutei-ring
          (list (list :word "雨" :point 10 :buffer (current-buffer)))))
      (should-not (nskk--study-distance-ok-p 50 (current-buffer)))))
  (nskk-it
    "returns nil when in different buffer"
    (let ((nskk-study-max-distance 30))
      (with-temp-buffer
        (let ((other-buf (current-buffer)))
          (with-temp-buffer
            (let ((nskk--study-kakutei-ring (list (list :word "雨" :point 10 :buffer other-buf))))
              (should-not (nskk--study-distance-ok-p 15 (current-buffer))))))))))

;;;
;;; Study Association Recording
;;;
(nskk-describe "nskk-study-record"
  (nskk-it "records an association between previous word and current reading/candidate"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'study-association 3)
      (let ((nskk-study-max-distance nil)
            (nskk-study-first-candidate t)
            (nskk--study-kakutei-ring
             (list (list :word "雨" :point 10 :buffer (current-buffer)))))
        (nskk-study-record "ふる" "降る")
        (should (equal (nskk-prolog-query-value
                        '(study-association "雨" "ふる" \?c) '\?c)
                       "降る")))))

  (nskk-it "replaces existing association for same (prev, reading) pair"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'study-association 3)
      (let ((nskk-study-max-distance nil)
            (nskk-study-first-candidate t)
            (nskk--study-kakutei-ring
             (list (list :word "雨" :point 10 :buffer (current-buffer)))))
        (nskk-study-record "ふる" "振る")
        (nskk-study-record "ふる" "降る")
        (should (equal (nskk-prolog-query-value
                        '(study-association "雨" "ふる" \?c) '\?c)
                       "降る"))
        ;; Only one association should exist
        (should (= (length (nskk-prolog-query '(study-association "雨" "ふる" \?c))) 1)))))

  (nskk-it "skips recording when ring is empty"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'study-association 3)
      (let ((nskk-study-max-distance nil)
            (nskk-study-first-candidate t)
            (nskk--study-kakutei-ring nil))
        (nskk-study-record "ふる" "降る")
        (should-not (nskk-prolog-query '(study-association \?p \?r \?c))))))

  (nskk-it "skips recording when nskk-study-first-candidate is nil and index is 0"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'study-association 3)
      (let ((nskk-study-max-distance nil)
            (nskk-study-first-candidate nil)
            (nskk--study-kakutei-ring
             (list (list :word "雨" :point 10 :buffer (current-buffer)))))
        (nskk-study-record "ふる" "降る" 0)
        (should-not (nskk-prolog-query '(study-association \?p \?r \?c))))))

  (nskk-it "records when nskk-study-first-candidate is nil but index > 0"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'study-association 3)
      (let ((nskk-study-max-distance nil)
            (nskk-study-first-candidate nil)
            (nskk--study-kakutei-ring
             (list (list :word "雨" :point 10 :buffer (current-buffer)))))
        (nskk-study-record "ふる" "降る" 2)
        (should (equal (nskk-prolog-query-value
                        '(study-association "雨" "ふる" \?c) '\?c)
                       "降る")))))

  (nskk-it "skips nskk-no-learn candidates"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'study-association 3)
      (let ((nskk-study-max-distance nil)
            (nskk-study-first-candidate t)
            (nskk--study-kakutei-ring
             (list (list :word "雨" :point 10 :buffer (current-buffer))))
            (no-learn-cand (propertize "降る" 'nskk-no-learn t)))
        (nskk-study-record "ふる" no-learn-cand)
        (should-not (nskk-prolog-query '(study-association \?p \?r \?c))))))

  (nskk-it "skips recording when distance exceeds max"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'study-association 3)
      (with-temp-buffer
        (insert (make-string 100 ?x))
        (let ((nskk-study-max-distance 10)
              (nskk-study-first-candidate t)
              (nskk--study-kakutei-ring
               (list (list :word "雨" :point 5 :buffer (current-buffer)))))
          ;; Point at 101 is far from 5
          (nskk-study-record "ふる" "降る")
          (should-not (nskk-prolog-query '(study-association \?p \?r \?c))))))))

;;;
;;; Candidate Reordering
;;;
(nskk-describe "nskk-study-reorder"
  (nskk-it "promotes associated candidate to front"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'study-association 3)
      (nskk-prolog-assert '((study-association "雨" "ふる" "降る")))
      (let ((nskk--study-kakutei-ring
             (list (list :word "雨" :point 10 :buffer (current-buffer)))))
        (let ((result (nskk-study-reorder "ふる" '("振る" "降る" "古る"))))
          (should (equal (car result) "降る"))))))

  (nskk-it "returns original order when no association matches"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'study-association 3)
      (let ((nskk--study-kakutei-ring
             (list (list :word "雨" :point 10 :buffer (current-buffer)))))
        (let ((result (nskk-study-reorder "ふる" '("振る" "降る"))))
          (should (equal result '("振る" "降る")))))))

  (nskk-it "returns candidates unchanged when ring is empty"
    (let ((nskk--study-kakutei-ring nil))
      (should (equal (nskk-study-reorder "ふる" '("振る" "降る"))
                     '("振る" "降る")))))

  (nskk-it "returns nil for nil candidates"
    (let ((nskk--study-kakutei-ring
           (list (list :word "雨" :point 10 :buffer (current-buffer)))))
      (should (null (nskk-study-reorder "ふる" nil)))))

  (nskk-it "searches multiple ring entries for associations"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'study-association 3)
      ;; Association from second ring entry
      (nskk-prolog-assert '((study-association "天気" "ふる" "降る")))
      (let ((nskk--study-kakutei-ring
             (list (list :word "雨" :point 10 :buffer (current-buffer))
                   (list :word "天気" :point 5 :buffer (current-buffer)))))
        (let ((result (nskk-study-reorder "ふる" '("振る" "降る" "古る"))))
          (should (equal (car result) "降る")))))))

;;;
;;; After-Kakutei Entry Point
;;;
(nskk-describe "nskk-study-after-kakutei"
  (nskk-it "records association and pushes to ring"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'study-association 3)
      (let ((nskk-study-max-distance nil)
            (nskk-study-first-candidate t)
            (nskk-study-search-times 5)
            (nskk--study-kakutei-ring
             (list (list :word "雨" :point 10 :buffer (current-buffer)))))
        (nskk-study-after-kakutei "ふる" "降る")
        ;; Association recorded
        (should (equal (nskk-prolog-query-value
                        '(study-association "雨" "ふる" \?c) '\?c)
                       "降る"))
        ;; Ring updated
        (should (equal (plist-get (car nskk--study-kakutei-ring) :word) "降る")))))

  (nskk-it "pushes to ring even without prior context"
    (let ((nskk-study-max-distance nil)
          (nskk-study-first-candidate t)
          (nskk-study-search-times 5)
          (nskk--study-kakutei-ring nil))
      (nskk-study-after-kakutei "あめ" "雨")
      (should (= (length nskk--study-kakutei-ring) 1))
      (should (equal (plist-get (car nskk--study-kakutei-ring) :word) "雨")))))

;;;
;;; Persistence
;;;
(nskk-describe "nskk-study-save and nskk-study-load"
  (nskk-it "round-trips study associations through save/load"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'study-association 3)
      (nskk-prolog-assert '((study-association "雨" "ふる" "降る")))
      (nskk-prolog-assert '((study-association "天気" "よほう" "予報")))
      (let ((nskk-study-file (make-temp-file "nskk-study-test-" nil ".dat")))
        (unwind-protect
            (progn
              (nskk-study-save)
              ;; Clear and reload
              (nskk-prolog-retract-all 'study-association 3)
              (should-not (nskk-prolog-query '(study-association \?p \?r \?c)))
              (nskk-study-load)
              (should (equal (nskk-prolog-query-value
                              '(study-association "雨" "ふる" \?c) '\?c)
                             "降る"))
              (should (equal (nskk-prolog-query-value
                              '(study-association "天気" "よほう" \?c) '\?c)
                             "予報")))
          (delete-file nskk-study-file)))))

  (nskk-it "does not announce success when atomic publication fails"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'study-association 3)
      (nskk-prolog-assert
       '((study-association "new-context" "new-reading" "new-candidate")))
      (let ((nskk-study-file
             (make-temp-file "nskk-study-rename-failure-" nil ".dat"))
            (messages nil))
        (unwind-protect
            (progn
              (with-temp-file nskk-study-file
                (prin1 '(("old-context" "old-reading" "old-candidate"))
                       (current-buffer)))
              (nskk-with-mocks
                  ((rename-file
                    (lambda (&rest _arguments)
                      (signal 'file-error '("rename failure"))))
                   (message
                    (lambda (fmt &rest args)
                      (push (apply #'format fmt args) messages))))
                (nskk-study-save))
              (should-not
               (cl-some
                (lambda (message)
                  (string-match-p "Study data saved" message))
                messages))
              (should
               (cl-some
                (lambda (message)
                  (string-match-p "Failed to save study data" message))
                messages))
              (should
               (equal
                (with-temp-buffer
                  (insert-file-contents nskk-study-file)
                  (read (current-buffer)))
                '(("old-context" "old-reading" "old-candidate")))))
          (when (file-exists-p nskk-study-file)
            (delete-file nskk-study-file))))))

  (nskk-it "announces success only after the new file is published"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'study-association 3)
      (nskk-prolog-assert
       '((study-association "published-context"
                            "published-reading"
                            "published-candidate")))
      (let ((nskk-study-file
             (make-temp-file "nskk-study-published-" nil ".dat"))
            observed)
        (unwind-protect
            (progn
              (nskk-with-mocks
                  ((message
                    (lambda (fmt &rest args)
                      (let ((text (apply #'format fmt args)))
                        (when (string-match-p "Study data saved" text)
                          (setq observed
                                (with-temp-buffer
                                  (insert-file-contents nskk-study-file)
                                  (read (current-buffer)))))
                        text))))
                (nskk-study-save))
              (should
               (member
                '("published-context"
                  "published-reading"
                  "published-candidate")
                observed)))
          (when (file-exists-p nskk-study-file)
            (delete-file nskk-study-file)))))))

;;;
;;; nskk-study-load validation tests
;;;
(nskk-describe
  "nskk-study-load validation"
  (nskk-it
    "preserves existing data when an entry is malformed"
    (let ((tmpfile (make-temp-file "nskk-test-study")))
      (unwind-protect (progn
          (with-temp-file tmpfile (prin1 (quote (("new" "reading"))) (current-buffer)))
          (let ((nskk-study-file tmpfile))
            (nskk-prolog-test-with-isolated-db
              (nskk-prolog-retract-all (quote study-association) 3)
              (nskk-prolog-assert (quote ((study-association "old" "reading" "candidate"))))
              (nskk-study-load)
              (should
                (nskk-prolog-holds-p (quote (study-association "old" "reading" "candidate"))))
              (should
                (= 1 (length (nskk-prolog-query (quote (study-association \?p \?r \?c)))))))))
        (delete-file tmpfile))))
  (nskk-it
    "replaces existing data without duplicates on repeated loads"
    (let ((tmpfile (make-temp-file "nskk-test-study")))
      (unwind-protect (progn
          (with-temp-file
            tmpfile
            (prin1 (quote (("prev" "reading" "cand"))) (current-buffer)))
          (let ((nskk-study-file tmpfile))
            (nskk-prolog-test-with-isolated-db
              (nskk-prolog-retract-all (quote study-association) 3)
              (nskk-prolog-assert (quote ((study-association "old" "reading" "candidate"))))
              (nskk-study-load)
              (nskk-study-load)
              (should-not
                (nskk-prolog-holds-p (quote (study-association "old" "reading" "candidate"))))
              (should
                (nskk-prolog-holds-p (quote (study-association "prev" "reading" "cand"))))
              (should
                (= 1 (length (nskk-prolog-query (quote (study-association \?p \?r \?c)))))))))
        (delete-file tmpfile))))
  (nskk-it
    "preserves existing data when the file grows past the size limit"
    (let ((tmpfile (make-temp-file "nskk-test-study-growing"))
          (nskk--study-max-file-size 8))
      (unwind-protect (progn
          (with-temp-file
            tmpfile
            (prin1 (quote (("new" "reading" "candidate"))) (current-buffer)))
          (let ((nskk-study-file tmpfile))
            (nskk-prolog-test-with-isolated-db
              (nskk-prolog-retract-all (quote study-association) 3)
              (nskk-prolog-assert (quote ((study-association "old" "reading" "candidate"))))
              (cl-letf
                (((symbol-function (quote file-attribute-size))
                    (lambda (_attributes)
                      1)))
                (nskk-study-load))
              (should
                (nskk-prolog-holds-p (quote (study-association "old" "reading" "candidate"))))
              (should
                (= 1 (length (nskk-prolog-query (quote (study-association \?p \?r \?c)))))))))
        (delete-file tmpfile)))))

(ert-deftest
  nskk-study-test/load-rejects-symbolic-link
  ()
  "A study file reached through a symbolic link must not be loaded."
  (let* ((directory (make-temp-file "nskk-study-link-" t))
         (target (expand-file-name "target.el" directory))
         (link (expand-file-name "study.el" directory)))
    (unwind-protect (progn
        (with-temp-file
          target
          (prin1 '(("new" "reading" "candidate")) (current-buffer)))
        (make-symbolic-link target link)
        (let ((nskk-study-file link))
          (nskk-prolog-test-with-isolated-db
            (nskk-prolog-retract-all 'study-association 3)
            (nskk-prolog-assert '((study-association "old" "reading" "candidate")))
            (nskk-study-load)
            (should (nskk-prolog-holds-p '(study-association "old" "reading" "candidate")))
            (should-not
              (nskk-prolog-holds-p '(study-association "new" "reading" "candidate"))))))
      (delete-directory directory t))))

(ert-deftest
  nskk-study-test/load-detects-post-stat-symlink-race
  ()
  "A file becoming a symbolic link after stat must not be loaded."
  (let ((file (make-temp-file "nskk-study-race-"))
        (symlink-checks 0))
    (unwind-protect (progn
        (with-temp-file file (prin1 '(("new" "reading" "candidate")) (current-buffer)))
        (let ((nskk-study-file file))
          (nskk-prolog-test-with-isolated-db
            (nskk-prolog-retract-all 'study-association 3)
            (nskk-prolog-assert '((study-association "old" "reading" "candidate")))
            (cl-letf
              (((symbol-function 'file-symlink-p)
                  (lambda (_file)
                    (setq symlink-checks (1+ symlink-checks))
                    (= symlink-checks 2))))
              (nskk-study-load))
            (should (nskk-prolog-holds-p '(study-association "old" "reading" "candidate")))
            (should-not
              (nskk-prolog-holds-p '(study-association "new" "reading" "candidate"))))))
      (delete-file file))))

(ert-deftest
  nskk-study-test/load-rejects-file-replaced-during-read
  ()
  "A file replaced during reading must not update existing facts."
  (let ((file (make-temp-file "nskk-study-replaced-"))
        (replacement (make-temp-file "nskk-study-replacement-"))
        (real-insert (symbol-function (quote insert-file-contents))))
    (unwind-protect (progn
        (with-temp-file
          file
          (prin1 (quote (("new" "reading" "candidate"))) (current-buffer)))
        (with-temp-file
          replacement
          (prin1 (quote (("attacker" "reading" "candidate"))) (current-buffer)))
        (let ((nskk-study-file file))
          (nskk-prolog-test-with-isolated-db
            (nskk-prolog-retract-all (quote study-association) 3)
            (nskk-prolog-assert (quote ((study-association "old" "reading" "candidate"))))
            (cl-letf
              (((symbol-function (quote insert-file-contents))
                  (lambda (&rest arguments)
                    (prog1
                      (apply real-insert arguments)
                      (rename-file replacement file t)))))
              (nskk-study-load))
            (should
              (nskk-prolog-holds-p (quote (study-association "old" "reading" "candidate"))))
            (should-not
              (nskk-prolog-holds-p (quote (study-association "new" "reading" "candidate"))))
            (should-not
              (nskk-prolog-holds-p
                (quote (study-association "attacker" "reading" "candidate")))))))
      (when (file-exists-p file)
        (delete-file file))
      (when (file-exists-p replacement)
        (delete-file replacement)))))

(ert-deftest
  nskk-study-test/load-rejects-non-regular-file
  ()
  "A non-regular study file must be rejected before any read."
  (unless (executable-find "mkfifo")
    (ert-skip "mkfifo is unavailable"))
  (let ((fifo
        (make-temp-name (expand-file-name "nskk-study-fifo-" temporary-file-directory)))
        (read-called nil))
    (unwind-protect (progn
        (should (zerop (call-process "mkfifo" nil nil nil fifo)))
        (let ((nskk-study-file fifo))
          (nskk-prolog-test-with-isolated-db
            (nskk-prolog-retract-all 'study-association 3)
            (nskk-prolog-assert '((study-association "old" "reading" "candidate")))
            (cl-letf
              (((symbol-function 'insert-file-contents)
                  (lambda (&rest _arguments)
                    (setq read-called t)
                    (error "FIFO read attempted"))))
              (nskk-study-load))
            (should-not read-called)
            (should (nskk-prolog-holds-p '(study-association "old" "reading" "candidate"))))))
      (when (file-exists-p fifo)
        (delete-file fifo)))))

(ert-deftest nskk-study-test/load-rolls-back-publication-error ()
    "Publication errors restore the exact previous predicate storage."
    (let ((file (make-temp-file "nskk-study-publication-error-"))
          (real-apply
           (symbol-function 'nskk-dict-transaction-apply-predicate-snapshot)))
      (unwind-protect
          (progn
            (with-temp-file file
              (prin1 '(("new" "reading" "candidate")) (current-buffer)))
            (let ((nskk-study-file file))
              (nskk-prolog-test-with-isolated-db
               (let ((key (nskk-prolog-clause-key 'study-association 3))
                     (rollback-called 0)
                     rollback-inhibited)
                 (nskk-prolog-retract-all 'study-association 3)
                 (nskk-prolog-assert
                  '((study-association "old" "reading" "candidate")))
                 (let ((before (nskk-dict-transaction-predicate-snapshot key)))
                   (cl-letf
                       (((symbol-function 'nskk-prolog-assert)
                         (lambda (&rest _arguments)
                           (error "original publication error")))
                        ((symbol-function
                          'nskk-dict-transaction-apply-predicate-snapshot)
                         (lambda (snapshot)
                           (cl-incf rollback-called)
                           (setq rollback-inhibited inhibit-quit)
                           (funcall real-apply snapshot))))
                     (nskk-study-load))
                   (should (= rollback-called 1))
                   (should rollback-inhibited)
                   (should
                    (equal before (nskk-dict-transaction-predicate-snapshot key)))
                   (should
                    (nskk-prolog-holds-p
                     '(study-association "old" "reading" "candidate")))
                   (should-not
                    (nskk-prolog-holds-p
                     '(study-association "new" "reading" "candidate"))))))))
        (ignore-errors (delete-file file)))))

  (ert-deftest nskk-study-test/load-rolls-back-before-resignaling-quit ()
    "Rollback completes under inhibited quit before the original quit is signaled."
    (let ((file (make-temp-file "nskk-study-publication-quit-"))
          (real-apply
           (symbol-function 'nskk-dict-transaction-apply-predicate-snapshot)))
      (unwind-protect
          (progn
            (with-temp-file file
              (prin1 '(("new" "reading" "candidate")) (current-buffer)))
            (let ((nskk-study-file file))
              (nskk-prolog-test-with-isolated-db
               (let ((key (nskk-prolog-clause-key 'study-association 3))
                     (rollback-called 0)
                     rollback-inhibited)
                 (nskk-prolog-retract-all 'study-association 3)
                 (nskk-prolog-assert
                  '((study-association "old" "reading" "candidate")))
                 (let ((before (nskk-dict-transaction-predicate-snapshot key)))
                   (cl-letf
                       (((symbol-function 'nskk-prolog-assert)
                         (lambda (&rest _arguments)
                           (signal
                            'quit
                            '("original publication quit" payload))))
                        ((symbol-function
                          'nskk-dict-transaction-apply-predicate-snapshot)
                         (lambda (snapshot)
                           (cl-incf rollback-called)
                           (setq rollback-inhibited inhibit-quit)
                           (funcall real-apply snapshot))))
                     (let ((condition
                            (condition-case condition
                                (progn
                                  (nskk-study-load)
                                  'returned)
                              (quit condition))))
                       (should
                        (equal condition
                               '(quit "original publication quit" payload)))))
                   (should (= rollback-called 1))
                   (should rollback-inhibited)
                   (should
                    (equal before (nskk-dict-transaction-predicate-snapshot key)))
                   (should
                    (nskk-prolog-holds-p
                     '(study-association "old" "reading" "candidate")))
                   (should-not
                    (nskk-prolog-holds-p
                     '(study-association "new" "reading" "candidate"))))))))
        (ignore-errors (delete-file file)))))

(defun nskk-study-test--assert-load-rejects-time-race (time-index)
  "Assert that a TIME-INDEX metadata race aborts a study load."
  (let ((file (make-temp-file "nskk-study-time-race-")))
    (unwind-protect
        (progn
          (with-temp-file file
            (prin1 '(("new" "reading" "candidate")) (current-buffer)))
          (let* ((attributes-before (file-attributes file 'integer))
 (resolved-file (file-truename file))
 (attribute-reads 0)
 (read-called nil)
 (publish-called nil)
 (real-attributes (symbol-function 'file-attributes))
 (real-retract (symbol-function 'nskk-prolog-retract-all)))
            (let ((nskk-study-file file))
              (nskk-prolog-test-with-isolated-db
                (let ((key (nskk-prolog-clause-key
                            'study-association 3)))
                  (nskk-prolog-retract-all 'study-association 3)
                  (nskk-prolog-assert
                   '((study-association "old" "reading" "candidate")))
                  (let ((before (nskk-dict-transaction-predicate-snapshot key)))
                    (cl-letf
                        (((symbol-function 'file-attributes)
                          (lambda (filename &optional id-format)
                            (should (eq id-format 'integer))
                            (if (or (equal filename file) (equal filename resolved-file))
                                (pcase (cl-incf attribute-reads)
                                  (1 attributes-before)
                                  (2 (funcall real-attributes
                                              filename id-format))
                                  (3
                                   (let ((changed
                                          (copy-tree
                                           (funcall real-attributes
                                                    filename id-format))))
                                     (setf (nth time-index changed)
                                           (time-add
                                            (nth time-index changed) 1))
                                     changed))
                                  (_ (error
                                      "Unexpected source attribute read")))
                              (funcall real-attributes
                                       filename id-format))))
                         ((symbol-function 'read)
                          (lambda (&rest _arguments)
                            (setq read-called t)
                            (error "Reader reached")))
                         ((symbol-function 'nskk-prolog-retract-all)
                          (lambda (&rest arguments)
                            (setq publish-called t)
                            (apply real-retract arguments))))
                      (nskk-study-load))
                    (should (= attribute-reads 3))
                    (should-not read-called)
                    (should-not publish-called)
                    (should
                     (equal before
                            (nskk-dict-transaction-predicate-snapshot key)))
                    (should
                     (nskk-prolog-holds-p
                      '(study-association
                        "old" "reading" "candidate")))
                    (should-not
                     (nskk-prolog-holds-p
                      '(study-association
                        "new" "reading" "candidate")))))))))
      (delete-file file))))

(ert-deftest
  nskk-study-test/load-rejects-modification-time-change-during-read
  ()
  "An mtime-only race must not update existing facts."
  (nskk-study-test--assert-load-rejects-time-race 5))

(ert-deftest
  nskk-study-test/load-rejects-status-change-time-change-during-read
  ()
  "A ctime-only race must not update existing facts."
  (nskk-study-test--assert-load-rejects-time-race 6))

(provide (quote nskk-study-test))

;;; nskk-study-test.el ends here
