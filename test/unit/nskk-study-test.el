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

(nskk-describe "nskk--study-recent-words"
  (nskk-it "returns words in most-recent-first order"
    (let ((nskk--study-kakutei-ring nil)
          (nskk-study-search-times 5))
      (nskk--study-push-kakutei "A" 1 (current-buffer))
      (nskk--study-push-kakutei "B" 2 (current-buffer))
      (should (equal (nskk--study-recent-words) '("B" "A"))))))

;;;
;;; Distance Check
;;;

(nskk-describe "nskk--study-distance-ok-p"
  (nskk-it "returns t when no max-distance set"
    (let ((nskk-study-max-distance nil)
          (nskk--study-kakutei-ring
           (list (list :word "雨" :point 10 :buffer (current-buffer)))))
      (should (nskk--study-distance-ok-p 100 (current-buffer)))))

  (nskk-it "returns t when ring is empty"
    (let ((nskk-study-max-distance 30)
          (nskk--study-kakutei-ring nil))
      (should (nskk--study-distance-ok-p 100 (current-buffer)))))

  (nskk-it "returns t when within max-distance in same buffer"
    (let ((nskk-study-max-distance 30)
          (nskk--study-kakutei-ring
           (list (list :word "雨" :point 10 :buffer (current-buffer)))))
      (should (nskk--study-distance-ok-p 35 (current-buffer)))))

  (nskk-it "returns nil when beyond max-distance"
    (let ((nskk-study-max-distance 30)
          (nskk--study-kakutei-ring
           (list (list :word "雨" :point 10 :buffer (current-buffer)))))
      (should-not (nskk--study-distance-ok-p 50 (current-buffer)))))

  (nskk-it "returns nil when in different buffer"
    (let ((nskk-study-max-distance 30))
      (with-temp-buffer
        (let ((other-buf (current-buffer)))
          (with-temp-buffer
            (let ((nskk--study-kakutei-ring
                   (list (list :word "雨" :point 10 :buffer other-buf))))
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
          (delete-file nskk-study-file))))))

;;;
;;; nskk-study-load validation tests
;;;

(nskk-describe "nskk-study-load validation"
  (nskk-it "rejects non-list data and does not crash"
    (let ((tmpfile (make-temp-file "nskk-test-study")))
      (unwind-protect
          (progn
            (with-temp-file tmpfile
              (prin1 "not-a-list" (current-buffer)))
            (let ((nskk-study-file tmpfile))
              (nskk-prolog-test-with-isolated-db
                (nskk-study-load)
                (should-not (nskk-prolog-holds-p '(study-association \?_ \?_ \?_))))))
        (delete-file tmpfile))))

  (nskk-it "loads valid list data without error"
    (let ((tmpfile (make-temp-file "nskk-test-study")))
      (unwind-protect
          (progn
            (with-temp-file tmpfile
              (prin1 '(("prev" "reading" "cand")) (current-buffer)))
            (let ((nskk-study-file tmpfile))
              (nskk-prolog-test-with-isolated-db
                (nskk-study-load)
                (should (nskk-prolog-holds-p
                         '(study-association "prev" "reading" "cand"))))))
        (delete-file tmpfile)))))

(provide 'nskk-study-test)

;;; nskk-study-test.el ends here
