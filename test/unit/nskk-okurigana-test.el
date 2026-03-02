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

(nskk-describe "okurigana character detection"
  (nskk-context "uppercase letter detection"
    (nskk-it "returns lowercase for every uppercase letter A-Z"
      (dolist (c (number-sequence ?A ?Z))
        (let ((result (nskk-detect-okurigana-char c)))
          (should result)
          (should (equal result (downcase c))))))

    (nskk-it "maps uppercase A to lowercase a"
      (should (equal (nskk-detect-okurigana-char ?A) ?a)))

    (nskk-it "maps uppercase Z to lowercase z"
      (should (equal (nskk-detect-okurigana-char ?Z) ?z))))

  (nskk-context "standard okurigana consonants"
    (nskk-it "maps uppercase K to lowercase k"
      (should (equal (nskk-detect-okurigana-char ?K) ?k)))

    (nskk-it "maps uppercase S to lowercase s"
      (should (equal (nskk-detect-okurigana-char ?S) ?s)))

    (nskk-it "maps uppercase T to lowercase t"
      (should (equal (nskk-detect-okurigana-char ?T) ?t)))

    (nskk-it "maps uppercase N to lowercase n"
      (should (equal (nskk-detect-okurigana-char ?N) ?n)))

    (nskk-it "maps uppercase H to lowercase h"
      (should (equal (nskk-detect-okurigana-char ?H) ?h)))

    (nskk-it "maps uppercase M to lowercase m"
      (should (equal (nskk-detect-okurigana-char ?M) ?m)))

    (nskk-it "maps uppercase R to lowercase r"
      (should (equal (nskk-detect-okurigana-char ?R) ?r)))

    (nskk-it "maps uppercase W to lowercase w"
      (should (equal (nskk-detect-okurigana-char ?W) ?w))))

  (nskk-context "lowercase letter rejection"
    (nskk-it "returns nil for all lowercase letters a-z"
      (dolist (c (number-sequence ?a ?z))
        (should-not (nskk-detect-okurigana-char c))))

    (nskk-it "returns nil for lowercase a"
      (should-not (nskk-detect-okurigana-char ?a)))

    (nskk-it "returns nil for lowercase k"
      (should-not (nskk-detect-okurigana-char ?k)))

    (nskk-it "returns nil for lowercase z"
      (should-not (nskk-detect-okurigana-char ?z))))

  (nskk-context "digit rejection"
    (nskk-it "returns nil for all digits 0-9"
      (dolist (c (number-sequence ?0 ?9))
        (should-not (nskk-detect-okurigana-char c)))))

  (nskk-context "non-character input rejection"
    (nskk-it "returns nil for nil input"
      (should-not (nskk-detect-okurigana-char nil)))

    (nskk-it "returns nil for string input (not a character)"
      (should-not (nskk-detect-okurigana-char "K")))

    (nskk-it "returns nil for symbol input"
      (should-not (nskk-detect-okurigana-char 'symbol)))

    (nskk-it "returns nil for space character"
      (should-not (nskk-detect-okurigana-char ?\s)))

    (nskk-it "returns nil for period character"
      (should-not (nskk-detect-okurigana-char ?.)))

    (nskk-it "returns nil for @ (below uppercase range)"
      (should-not (nskk-detect-okurigana-char ?@)))

    (nskk-it "returns nil for [ (above uppercase range)"
      (should-not (nskk-detect-okurigana-char ?\[)))))

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

(nskk-describe "okurigana state storage"
  (nskk-context "initial state"
    (nskk-it "okurigana is nil on a freshly created state"
      (let ((state (nskk-state-create)))
        (should (null (nskk-state-get-okurigana state))))))

  (nskk-context "set and get roundtrip"
    (nskk-it "set/get roundtrip works for consonant k"
      (let ((state (nskk-state-create)))
        (nskk-state-set-okurigana state ?k)
        (should (eq (nskk-state-get-okurigana state) ?k))))

    (nskk-it "set/get roundtrip works for consonant s"
      (let ((state (nskk-state-create)))
        (nskk-state-set-okurigana state ?s)
        (should (eq (nskk-state-get-okurigana state) ?s))))

    (nskk-it "set/get roundtrip works for consonant t"
      (let ((state (nskk-state-create)))
        (nskk-state-set-okurigana state ?t)
        (should (eq (nskk-state-get-okurigana state) ?t))))

    (nskk-it "set/get roundtrip works for consonant n"
      (let ((state (nskk-state-create)))
        (nskk-state-set-okurigana state ?n)
        (should (eq (nskk-state-get-okurigana state) ?n))))

    (nskk-it "set/get roundtrip works for consonant h"
      (let ((state (nskk-state-create)))
        (nskk-state-set-okurigana state ?h)
        (should (eq (nskk-state-get-okurigana state) ?h))))

    (nskk-it "set/get roundtrip works for consonant m"
      (let ((state (nskk-state-create)))
        (nskk-state-set-okurigana state ?m)
        (should (eq (nskk-state-get-okurigana state) ?m))))

    (nskk-it "set/get roundtrip works for consonant y"
      (let ((state (nskk-state-create)))
        (nskk-state-set-okurigana state ?y)
        (should (eq (nskk-state-get-okurigana state) ?y))))

    (nskk-it "set/get roundtrip works for consonant r"
      (let ((state (nskk-state-create)))
        (nskk-state-set-okurigana state ?r)
        (should (eq (nskk-state-get-okurigana state) ?r))))

    (nskk-it "set/get roundtrip works for consonant w"
      (let ((state (nskk-state-create)))
        (nskk-state-set-okurigana state ?w)
        (should (eq (nskk-state-get-okurigana state) ?w))))

    (nskk-it "set/get roundtrip works for consonant g"
      (let ((state (nskk-state-create)))
        (nskk-state-set-okurigana state ?g)
        (should (eq (nskk-state-get-okurigana state) ?g))))

    (nskk-it "set/get roundtrip works for consonant z"
      (let ((state (nskk-state-create)))
        (nskk-state-set-okurigana state ?z)
        (should (eq (nskk-state-get-okurigana state) ?z))))

    (nskk-it "set/get roundtrip works for consonant d"
      (let ((state (nskk-state-create)))
        (nskk-state-set-okurigana state ?d)
        (should (eq (nskk-state-get-okurigana state) ?d))))

    (nskk-it "set/get roundtrip works for consonant b"
      (let ((state (nskk-state-create)))
        (nskk-state-set-okurigana state ?b)
        (should (eq (nskk-state-get-okurigana state) ?b))))

    (nskk-it "set/get roundtrip works for consonant p"
      (let ((state (nskk-state-create)))
        (nskk-state-set-okurigana state ?p)
        (should (eq (nskk-state-get-okurigana state) ?p)))))

  (nskk-context "overwrite behavior"
    (nskk-it "setting okurigana overwrites the previous value"
      (let ((state (nskk-state-create)))
        (nskk-state-set-okurigana state ?k)
        (should (eq (nskk-state-get-okurigana state) ?k))
        (nskk-state-set-okurigana state ?s)
        (should (eq (nskk-state-get-okurigana state) ?s))
        (nskk-state-set-okurigana state ?t)
        (should (eq (nskk-state-get-okurigana state) ?t)))))

  (nskk-context "independence between instances"
    (nskk-it "two state objects maintain independent okurigana values"
      (let ((state1 (nskk-state-create))
            (state2 (nskk-state-create)))
        (nskk-state-set-okurigana state1 ?k)
        (nskk-state-set-okurigana state2 ?s)
        (should (eq (nskk-state-get-okurigana state1) ?k))
        (should (eq (nskk-state-get-okurigana state2) ?s))))))

;;;
;;; Prolog Predicate Tests: okurigana-char/2
;;;

(nskk-describe "Prolog okurigana-char/2 predicate"
  (nskk-context "uppercase to lowercase mapping"
    (nskk-it "maps uppercase A to lowercase a"
      (should (equal (nskk-prolog-query-value `(okurigana-char ,?A \?lc) '\?lc) ?a)))

    (nskk-it "maps uppercase K to lowercase k"
      (should (equal (nskk-prolog-query-value `(okurigana-char ,?K \?lc) '\?lc) ?k)))

    (nskk-it "maps uppercase S to lowercase s"
      (should (equal (nskk-prolog-query-value `(okurigana-char ,?S \?lc) '\?lc) ?s)))

    (nskk-it "maps uppercase T to lowercase t"
      (should (equal (nskk-prolog-query-value `(okurigana-char ,?T \?lc) '\?lc) ?t)))

    (nskk-it "maps uppercase Z to lowercase z"
      (should (equal (nskk-prolog-query-value `(okurigana-char ,?Z \?lc) '\?lc) ?z))))

  (nskk-context "lowercase rejection"
    (nskk-it "lowercase letters a, k, z are not in okurigana-char"
      (should-not (nskk-prolog-query-value `(okurigana-char ,?a \?lc) '\?lc))
      (should-not (nskk-prolog-query-value `(okurigana-char ,?k \?lc) '\?lc))
      (should-not (nskk-prolog-query-value `(okurigana-char ,?z \?lc) '\?lc))))

  (nskk-context "exhaustive uppercase coverage"
    (nskk-it "all uppercase A-Z are in okurigana-char and map to their downcased version"
      (dolist (c (number-sequence ?A ?Z))
        (let ((result (nskk-prolog-query-value `(okurigana-char ,c \?lc) '\?lc)))
          (should result)
          (should (equal result (downcase c))))))))

;;;
;;; Prolog Predicate Tests: okurigana-trigger/1
;;;

(nskk-describe "Prolog okurigana-trigger/1 predicate"
  (nskk-context "uppercase letter triggering"
    (nskk-it "okurigana-trigger succeeds for uppercase K"
      (should (nskk-prolog-query-one `(okurigana-trigger ,?K))))

    (nskk-it "okurigana-trigger succeeds for uppercase S"
      (should (nskk-prolog-query-one `(okurigana-trigger ,?S))))

    (nskk-it "okurigana-trigger succeeds for uppercase T"
      (should (nskk-prolog-query-one `(okurigana-trigger ,?T))))

    (nskk-it "okurigana-trigger succeeds for uppercase N"
      (should (nskk-prolog-query-one `(okurigana-trigger ,?N))))

    (nskk-it "okurigana-trigger succeeds for all uppercase A-Z"
      (dolist (c (number-sequence ?A ?Z))
        (should (nskk-prolog-query-one `(okurigana-trigger ,c))))))

  (nskk-context "non-uppercase rejection"
    (nskk-it "okurigana-trigger fails for all lowercase letters"
      (dolist (c (number-sequence ?a ?z))
        (should-not (nskk-prolog-query-one `(okurigana-trigger ,c)))))

    (nskk-it "okurigana-trigger fails for digit characters"
      (dolist (c (number-sequence ?0 ?9))
        (should-not (nskk-prolog-query-one `(okurigana-trigger ,c)))))))

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

(nskk-describe "okurigana API existence"
  (nskk-it "nskk-detect-okurigana-char is defined"
    (should (fboundp 'nskk-detect-okurigana-char)))

  (nskk-it "nskk-process-okurigana-input is defined"
    (should (fboundp 'nskk-process-okurigana-input)))

  (nskk-it "nskk-state-set-okurigana is defined"
    (should (fboundp 'nskk-state-set-okurigana)))

  (nskk-it "nskk-state-get-okurigana is defined"
    (should (fboundp 'nskk-state-get-okurigana))))

;;;
;;; Regression Tests: Pending Romaji Discard on Okurigana Trigger
;;;
;;
;; Bug (fixed in nskk-henkan.el): when a pending incomplete romaji consonant
;; (e.g. "k", "sh") was in nskk--romaji-buffer when an okurigana trigger
;; (uppercase letter) arrived, the raw consonant was inserted into the buffer
;; before the * okurigana marker, producing e.g. "▽かk*" instead of "▽か*".
;;
;; The fix discards :incomplete romaji (anything where nskk-converter-convert
;; returns (:incomplete . ...) or nil) and only emits successfully-converted kana
;; or a standalone "n" (→ "ん" at word boundary).

(nskk-describe "okurigana input flush behaviour"
  (nskk-context "pending consonant is discarded (not inserted)"
    (nskk-it "pending k is discarded: buffer does not contain k before the * marker"
      ;; T-U1: "k" in romaji buffer + uppercase K trigger → "k" must NOT appear before *
      (with-temp-buffer
        (let ((nskk-current-state (nskk-state-create 'hiragana)))
          ;; Insert preedit reading so far: ▽か
          (insert "\u25BD\u304B")
          ;; Set conversion-start marker at buffer start (as henkan-on does)
          (nskk--set-conversion-start-marker (point-min))
          ;; Put the state into henkan-on phase (reading in progress)
          (nskk-state-set-henkan-phase nskk-current-state 'on)
          ;; Simulate a pending incomplete romaji consonant "k"
          (setq nskk--romaji-buffer "k")
          ;; Fire okurigana trigger (uppercase K)
          (nskk-process-okurigana-input ?K)
          ;; The buffer should NOT contain "k" adjacent to the "*" marker.
          ;; Check both orderings: "k*" (consonant before marker, the actual bug)
          ;; and "*k" (consonant after marker), to catch both insertion orders.
          (let ((content (buffer-string)))
            (should-not (string-match-p "k\\*\\|\\*k" content))
            ;; The * okurigana marker must be present
            (should (string-match-p "\\*" content))))))

    (nskk-it "pending multi-char sh is discarded: buffer does not contain sh before *"
      ;; T-U2: "sh" (multi-char incomplete) + uppercase K → "sh" must NOT appear before *
      (with-temp-buffer
        (let ((nskk-current-state (nskk-state-create 'hiragana)))
          (insert "\u25BD\u304B")
          (nskk--set-conversion-start-marker (point-min))
          (nskk-state-set-henkan-phase nskk-current-state 'on)
          ;; "sh" is an incomplete romaji prefix (needs vowel to complete shi/sha/shu etc.)
          (setq nskk--romaji-buffer "sh")
          (nskk-process-okurigana-input ?K)
          (let ((content (buffer-string)))
            (should-not (string-match-p "sh" content))
            (should (string-match-p "\\*" content))))))

    (nskk-it "pending n is converted to ん and inserted before *"
      ;; T-U3: "n" (standalone n at word boundary) + uppercase K → "ん" IS inserted before *
      (with-temp-buffer
        (let ((nskk-current-state (nskk-state-create 'hiragana)))
          (insert "\u25BD\u304B")
          (nskk--set-conversion-start-marker (point-min))
          (nskk-state-set-henkan-phase nskk-current-state 'on)
          ;; "n" alone is the ん exception: it should flush as ん before the marker
          (setq nskk--romaji-buffer "n")
          (nskk-process-okurigana-input ?K)
          (let ((content (buffer-string)))
            ;; ん must appear in the buffer before the * marker
            (should (string-match-p "\u3093" content))
            (should (string-match-p "\\*" content))
            ;; And the raw "n" character must NOT appear as ASCII
            (should-not (string-match-p "[nN]\\*\\|\\*[nN]" content))))))

    (nskk-it "empty romaji buffer with uppercase K trigger inserts no extra char before *"
      ;; T-U4: empty romaji buffer + uppercase K → only * inserted, no spurious chars
      (with-temp-buffer
        (let ((nskk-current-state (nskk-state-create 'hiragana)))
          (insert "\u25BD\u304B")
          (nskk--set-conversion-start-marker (point-min))
          (nskk-state-set-henkan-phase nskk-current-state 'on)
          ;; No pending romaji
          (setq nskk--romaji-buffer "")
          (nskk-process-okurigana-input ?K)
          (let ((content (buffer-string)))
            ;; * marker must be present
            (should (string-match-p "\\*" content))
            ;; No raw ASCII consonant should appear adjacent to *
            (should-not (string-match-p "[a-z]\\*\\|\\*[a-z]" content))))))))

;;;
;;; Test Runner
;;;

(defun nskk-okurigana-test-run-all ()
  "Run all okurigana tests."
  (interactive)
  (ert-run-tests-interactively "^nskk-.*okurigana"))

(provide 'nskk-okurigana-test)

;;; nskk-okurigana-test.el ends here
