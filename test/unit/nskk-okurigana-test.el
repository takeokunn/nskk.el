;;; nskk-okurigana-test.el --- Comprehensive okurigana tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

;;; Commentary:

;; Comprehensive tests for okurigana detection, state storage, and Prolog
;; predicates.  Covers all uppercase letters A-Z exhaustively, state
;; roundtrip semantics, PBT properties, and table-driven cases for the
;; 14 standard okurigana consonants.

;;; Code:

(require 'ert)
(require 'nskk-henkan)
(require 'nskk-state)
(require 'nskk-prolog)
(require 'nskk-test-framework)
(require 'nskk-test-macros)
(require 'nskk-pbt-generators)

;;;
;;; Detection Tests: Uppercase A-Z
;;;

(nskk-deftest-unit okurigana-detect-uppercase-a-to-z
  "Test that nskk-detect-okurigana-char returns lowercase for every A-Z."
  (dolist (c (number-sequence ?A ?Z))
    (let ((result (nskk-detect-okurigana-char c)))
      (should result)
      (should (equal result (downcase c))))))

(nskk-deftest-unit okurigana-detect-uppercase-a
  "Test uppercase A is detected and maps to lowercase a."
  (should (equal (nskk-detect-okurigana-char ?A) ?a)))

(nskk-deftest-unit okurigana-detect-uppercase-z
  "Test uppercase Z is detected and maps to lowercase z."
  (should (equal (nskk-detect-okurigana-char ?Z) ?z)))

(nskk-deftest-unit okurigana-detect-uppercase-k
  "Test uppercase K maps to lowercase k."
  (should (equal (nskk-detect-okurigana-char ?K) ?k)))

(nskk-deftest-unit okurigana-detect-uppercase-s
  "Test uppercase S maps to lowercase s."
  (should (equal (nskk-detect-okurigana-char ?S) ?s)))

(nskk-deftest-unit okurigana-detect-uppercase-t
  "Test uppercase T maps to lowercase t."
  (should (equal (nskk-detect-okurigana-char ?T) ?t)))

(nskk-deftest-unit okurigana-detect-uppercase-n
  "Test uppercase N maps to lowercase n."
  (should (equal (nskk-detect-okurigana-char ?N) ?n)))

(nskk-deftest-unit okurigana-detect-uppercase-h
  "Test uppercase H maps to lowercase h."
  (should (equal (nskk-detect-okurigana-char ?H) ?h)))

(nskk-deftest-unit okurigana-detect-uppercase-m
  "Test uppercase M maps to lowercase m."
  (should (equal (nskk-detect-okurigana-char ?M) ?m)))

(nskk-deftest-unit okurigana-detect-uppercase-r
  "Test uppercase R maps to lowercase r."
  (should (equal (nskk-detect-okurigana-char ?R) ?r)))

(nskk-deftest-unit okurigana-detect-uppercase-w
  "Test uppercase W maps to lowercase w."
  (should (equal (nskk-detect-okurigana-char ?W) ?w)))

;;;
;;; Detection Tests: Lowercase a-z Return nil
;;;

(nskk-deftest-unit okurigana-detect-lowercase-a-to-z-nil
  "Test that all lowercase letters a-z return nil."
  (dolist (c (number-sequence ?a ?z))
    (should-not (nskk-detect-okurigana-char c))))

(nskk-deftest-unit okurigana-detect-lowercase-a-nil
  "Test lowercase a returns nil."
  (should-not (nskk-detect-okurigana-char ?a)))

(nskk-deftest-unit okurigana-detect-lowercase-k-nil
  "Test lowercase k returns nil."
  (should-not (nskk-detect-okurigana-char ?k)))

(nskk-deftest-unit okurigana-detect-lowercase-z-nil
  "Test lowercase z returns nil."
  (should-not (nskk-detect-okurigana-char ?z)))

;;;
;;; Detection Tests: Digits 0-9 Return nil
;;;

(nskk-deftest-unit okurigana-detect-digits-nil
  "Test that all digits 0-9 return nil."
  (dolist (c (number-sequence ?0 ?9))
    (should-not (nskk-detect-okurigana-char c))))

;;;
;;; Detection Tests: Non-Character Inputs Return nil
;;;

(nskk-deftest-unit okurigana-detect-nil-input-nil
  "Test that nil input returns nil."
  (should-not (nskk-detect-okurigana-char nil)))

(nskk-deftest-unit okurigana-detect-string-input-nil
  "Test that a string input returns nil (not a character)."
  (should-not (nskk-detect-okurigana-char "K")))

(nskk-deftest-unit okurigana-detect-symbol-input-nil
  "Test that a symbol input returns nil."
  (should-not (nskk-detect-okurigana-char 'symbol)))

(nskk-deftest-unit okurigana-detect-space-nil
  "Test that space character returns nil."
  (should-not (nskk-detect-okurigana-char ?\s)))

(nskk-deftest-unit okurigana-detect-period-nil
  "Test that period character returns nil."
  (should-not (nskk-detect-okurigana-char ?.)))

(nskk-deftest-unit okurigana-detect-at-sign-nil
  "Test that @ character (below uppercase range) returns nil."
  (should-not (nskk-detect-okurigana-char ?@)))

(nskk-deftest-unit okurigana-detect-bracket-nil
  "Test that [ character (above uppercase range) returns nil."
  (should-not (nskk-detect-okurigana-char ?\[)))

;;;
;;; Exhaustive Property Test: All A-Z
;;;

(nskk-property-test-exhaustive okurigana-all-uppercase-map-to-downcase
  (number-sequence ?A ?Z)
  (equal (nskk-detect-okurigana-char item)
         (downcase item)))

;;;
;;; State Storage Tests: Set / Get Roundtrip
;;;

(nskk-deftest-unit okurigana-state-initial-nil
  "Test that okurigana is nil on a freshly created state."
  (let ((state (nskk-state-create)))
    (should (null (nskk-state-get-okurigana state)))))

(nskk-deftest-unit okurigana-state-set-and-get-k
  "Test set/get roundtrip for consonant k."
  (let ((state (nskk-state-create)))
    (nskk-state-set-okurigana state ?k)
    (should (eq (nskk-state-get-okurigana state) ?k))))

(nskk-deftest-unit okurigana-state-set-and-get-s
  "Test set/get roundtrip for consonant s."
  (let ((state (nskk-state-create)))
    (nskk-state-set-okurigana state ?s)
    (should (eq (nskk-state-get-okurigana state) ?s))))

(nskk-deftest-unit okurigana-state-set-and-get-t
  "Test set/get roundtrip for consonant t."
  (let ((state (nskk-state-create)))
    (nskk-state-set-okurigana state ?t)
    (should (eq (nskk-state-get-okurigana state) ?t))))

(nskk-deftest-unit okurigana-state-set-and-get-n
  "Test set/get roundtrip for consonant n."
  (let ((state (nskk-state-create)))
    (nskk-state-set-okurigana state ?n)
    (should (eq (nskk-state-get-okurigana state) ?n))))

(nskk-deftest-unit okurigana-state-set-and-get-h
  "Test set/get roundtrip for consonant h."
  (let ((state (nskk-state-create)))
    (nskk-state-set-okurigana state ?h)
    (should (eq (nskk-state-get-okurigana state) ?h))))

(nskk-deftest-unit okurigana-state-set-and-get-m
  "Test set/get roundtrip for consonant m."
  (let ((state (nskk-state-create)))
    (nskk-state-set-okurigana state ?m)
    (should (eq (nskk-state-get-okurigana state) ?m))))

(nskk-deftest-unit okurigana-state-set-and-get-y
  "Test set/get roundtrip for consonant y."
  (let ((state (nskk-state-create)))
    (nskk-state-set-okurigana state ?y)
    (should (eq (nskk-state-get-okurigana state) ?y))))

(nskk-deftest-unit okurigana-state-set-and-get-r
  "Test set/get roundtrip for consonant r."
  (let ((state (nskk-state-create)))
    (nskk-state-set-okurigana state ?r)
    (should (eq (nskk-state-get-okurigana state) ?r))))

(nskk-deftest-unit okurigana-state-set-and-get-w
  "Test set/get roundtrip for consonant w."
  (let ((state (nskk-state-create)))
    (nskk-state-set-okurigana state ?w)
    (should (eq (nskk-state-get-okurigana state) ?w))))

(nskk-deftest-unit okurigana-state-set-and-get-g
  "Test set/get roundtrip for consonant g."
  (let ((state (nskk-state-create)))
    (nskk-state-set-okurigana state ?g)
    (should (eq (nskk-state-get-okurigana state) ?g))))

(nskk-deftest-unit okurigana-state-set-and-get-z
  "Test set/get roundtrip for consonant z."
  (let ((state (nskk-state-create)))
    (nskk-state-set-okurigana state ?z)
    (should (eq (nskk-state-get-okurigana state) ?z))))

(nskk-deftest-unit okurigana-state-set-and-get-d
  "Test set/get roundtrip for consonant d."
  (let ((state (nskk-state-create)))
    (nskk-state-set-okurigana state ?d)
    (should (eq (nskk-state-get-okurigana state) ?d))))

(nskk-deftest-unit okurigana-state-set-and-get-b
  "Test set/get roundtrip for consonant b."
  (let ((state (nskk-state-create)))
    (nskk-state-set-okurigana state ?b)
    (should (eq (nskk-state-get-okurigana state) ?b))))

(nskk-deftest-unit okurigana-state-set-and-get-p
  "Test set/get roundtrip for consonant p."
  (let ((state (nskk-state-create)))
    (nskk-state-set-okurigana state ?p)
    (should (eq (nskk-state-get-okurigana state) ?p))))

(nskk-deftest-unit okurigana-state-overwrite
  "Test that setting okurigana overwrites previous value."
  (let ((state (nskk-state-create)))
    (nskk-state-set-okurigana state ?k)
    (should (eq (nskk-state-get-okurigana state) ?k))
    (nskk-state-set-okurigana state ?s)
    (should (eq (nskk-state-get-okurigana state) ?s))
    (nskk-state-set-okurigana state ?t)
    (should (eq (nskk-state-get-okurigana state) ?t))))

(nskk-deftest-unit okurigana-state-independent-instances
  "Test that two state objects maintain independent okurigana values."
  (let ((state1 (nskk-state-create))
        (state2 (nskk-state-create)))
    (nskk-state-set-okurigana state1 ?k)
    (nskk-state-set-okurigana state2 ?s)
    (should (eq (nskk-state-get-okurigana state1) ?k))
    (should (eq (nskk-state-get-okurigana state2) ?s))))

;;;
;;; Prolog Predicate Tests: okurigana-char/2
;;;

(nskk-deftest-unit okurigana-prolog-char-a-to-a
  "Test Prolog okurigana-char maps uppercase A to lowercase a."
  (should (equal (nskk-prolog-query-value `(okurigana-char ,?A \?lc) '\?lc) ?a)))

(nskk-deftest-unit okurigana-prolog-char-k-to-k
  "Test Prolog okurigana-char maps uppercase K to lowercase k."
  (should (equal (nskk-prolog-query-value `(okurigana-char ,?K \?lc) '\?lc) ?k)))

(nskk-deftest-unit okurigana-prolog-char-s-to-s
  "Test Prolog okurigana-char maps uppercase S to lowercase s."
  (should (equal (nskk-prolog-query-value `(okurigana-char ,?S \?lc) '\?lc) ?s)))

(nskk-deftest-unit okurigana-prolog-char-t-to-t
  "Test Prolog okurigana-char maps uppercase T to lowercase t."
  (should (equal (nskk-prolog-query-value `(okurigana-char ,?T \?lc) '\?lc) ?t)))

(nskk-deftest-unit okurigana-prolog-char-z-to-z
  "Test Prolog okurigana-char maps uppercase Z to lowercase z."
  (should (equal (nskk-prolog-query-value `(okurigana-char ,?Z \?lc) '\?lc) ?z)))

(nskk-deftest-unit okurigana-prolog-char-lowercase-not-mapped
  "Test that lowercase letters are not in okurigana-char."
  (should-not (nskk-prolog-query-value `(okurigana-char ,?a \?lc) '\?lc))
  (should-not (nskk-prolog-query-value `(okurigana-char ,?k \?lc) '\?lc))
  (should-not (nskk-prolog-query-value `(okurigana-char ,?z \?lc) '\?lc)))

(nskk-deftest-unit okurigana-prolog-char-all-uppercase-exhaustive
  "Test that all uppercase A-Z are in okurigana-char and map to downcased version."
  (dolist (c (number-sequence ?A ?Z))
    (let ((result (nskk-prolog-query-value `(okurigana-char ,c \?lc) '\?lc)))
      (should result)
      (should (equal result (downcase c))))))

;;;
;;; Prolog Predicate Tests: okurigana-trigger/1
;;;

(nskk-deftest-unit okurigana-prolog-trigger-k-succeeds
  "Test that okurigana-trigger succeeds for uppercase K."
  (should (nskk-prolog-query-one `(okurigana-trigger ,?K))))

(nskk-deftest-unit okurigana-prolog-trigger-s-succeeds
  "Test that okurigana-trigger succeeds for uppercase S."
  (should (nskk-prolog-query-one `(okurigana-trigger ,?S))))

(nskk-deftest-unit okurigana-prolog-trigger-t-succeeds
  "Test that okurigana-trigger succeeds for uppercase T."
  (should (nskk-prolog-query-one `(okurigana-trigger ,?T))))

(nskk-deftest-unit okurigana-prolog-trigger-n-succeeds
  "Test that okurigana-trigger succeeds for uppercase N."
  (should (nskk-prolog-query-one `(okurigana-trigger ,?N))))

(nskk-deftest-unit okurigana-prolog-trigger-all-uppercase-succeed
  "Test that okurigana-trigger succeeds for all uppercase A-Z."
  (dolist (c (number-sequence ?A ?Z))
    (should (nskk-prolog-query-one `(okurigana-trigger ,c)))))

(nskk-deftest-unit okurigana-prolog-trigger-lowercase-fails
  "Test that okurigana-trigger fails for lowercase letters."
  (dolist (c (number-sequence ?a ?z))
    (should-not (nskk-prolog-query-one `(okurigana-trigger ,c)))))

(nskk-deftest-unit okurigana-prolog-trigger-digit-fails
  "Test that okurigana-trigger fails for digit characters."
  (dolist (c (number-sequence ?0 ?9))
    (should-not (nskk-prolog-query-one `(okurigana-trigger ,c)))))

;;;
;;; Table-Driven Tests: 14 Standard Okurigana Consonants
;;;

(nskk-deftest-table okurigana-consonant-mapping
  :columns (uppercase expected-lowercase)
  :rows ((?K ?k)
         (?S ?s)
         (?T ?t)
         (?N ?n)
         (?H ?h)
         (?M ?m)
         (?Y ?y)
         (?R ?r)
         (?W ?w)
         (?G ?g)
         (?Z ?z)
         (?D ?d)
         (?B ?b)
         (?P ?p))
  :description "Standard okurigana consonants map to their lowercase equivalents"
  :body (should (equal (nskk-detect-okurigana-char uppercase) expected-lowercase)))

(nskk-deftest-table okurigana-prolog-char-mapping
  :columns (uppercase expected-lowercase)
  :rows ((?K ?k)
         (?S ?s)
         (?T ?t)
         (?N ?n)
         (?H ?h)
         (?M ?m)
         (?Y ?y)
         (?R ?r)
         (?W ?w)
         (?G ?g)
         (?Z ?z)
         (?D ?d)
         (?B ?b)
         (?P ?p))
  :description "Standard okurigana consonants in Prolog okurigana-char/2 predicate"
  :body (should (equal (nskk-prolog-query-value `(okurigana-char ,uppercase \?lc) '\?lc)
                       expected-lowercase)))

;;;
;;; Property-Based Tests
;;;

;; PBT: uppercase okurigana consonant chars always return their lowercase via detect
(nskk-property-test-seeded okurigana-pbt-uppercase-returns-lowercase
  ((char okurigana-consonant-char))
  (equal (nskk-detect-okurigana-char char) (downcase char))
  100 42)

;; PBT: state roundtrip — set then get returns same value
(nskk-property-test-seeded okurigana-pbt-state-roundtrip
  ((char okurigana-consonant-char))
  (let* ((state (nskk-state-create))
         (lower-char (downcase char)))
    (nskk-state-set-okurigana state lower-char)
    (eq (nskk-state-get-okurigana state) lower-char))
  100 42)

;; PBT: lowercase letters always return nil from detect
(nskk-property-test-seeded okurigana-pbt-lowercase-returns-nil
  ((char okurigana-consonant-char))
  (null (nskk-detect-okurigana-char (downcase char)))
  100 42)

;; PBT: okurigana-consonant-char generator always yields chars in A-Z
(nskk-property-test-seeded okurigana-pbt-generator-yields-uppercase
  ((char okurigana-consonant-char))
  (and (characterp char)
       (>= char ?A)
       (<= char ?Z))
  100 42)

;; PBT: Prolog okurigana-char result equals downcase of input
(nskk-property-test-seeded okurigana-pbt-prolog-maps-to-downcase
  ((char okurigana-consonant-char))
  (equal (nskk-prolog-query-value `(okurigana-char ,char \?lc) '\?lc)
         (downcase char))
  100 42)

;;;
;;; API Existence Tests
;;;

(nskk-deftest-unit okurigana-api-detect-okurigana-char-defined
  "Test that nskk-detect-okurigana-char is defined."
  (should (fboundp 'nskk-detect-okurigana-char)))

(nskk-deftest-unit okurigana-api-process-okurigana-input-defined
  "Test that nskk-process-okurigana-input is defined."
  (should (fboundp 'nskk-process-okurigana-input)))

(nskk-deftest-unit okurigana-api-state-set-okurigana-defined
  "Test that nskk-state-set-okurigana is defined."
  (should (fboundp 'nskk-state-set-okurigana)))

(nskk-deftest-unit okurigana-api-state-get-okurigana-defined
  "Test that nskk-state-get-okurigana is defined."
  (should (fboundp 'nskk-state-get-okurigana)))

;;;
;;; Test Runner
;;;

(defun nskk-okurigana-test-run-all ()
  "Run all okurigana tests."
  (interactive)
  (ert-run-tests-interactively "^nskk-.*okurigana"))

(provide 'nskk-okurigana-test)

;;; nskk-okurigana-test.el ends here
