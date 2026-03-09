;;; nskk-dictionary-integration-pbt-test.el --- Dictionary integration PBT tests -*- lexical-binding: t; -*-

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

;; Property-based tests for dictionary loading, parsing, and lookup.
;;
;; This file tests invariants of the dictionary subsystem:
;; - Loading from fixture files
;; - Lookup returns correct candidates for known keys
;; - Unknown keys return nil
;; - Save and reload roundtrip preserves entries
;; - Line parsing handles valid and invalid formats
;;
;; Properties tested:
;; - dict-load-fixture: Loading test dictionary yields accessible entries
;; - dict-lookup-returns-candidates: Known keys always return non-nil
;; - dict-lookup-unknown-returns-nil: Unknown keys return nil
;; - dict-save-load-roundtrip: Save then reload preserves entries
;; - dict-parse-line-valid: Valid SKK format lines parse correctly
;; - dict-parse-line-invalid: Comments and invalid lines return nil

;;; Code:

(require 'ert)
(require 'nskk-test-framework)
(require 'nskk-test-macros)
(require 'nskk-pbt-generators)
(require 'nskk-dictionary)


;;;;
;;;; Helper Functions
;;;;

(defun nskk-pbt--fixture-dict-path ()
  "Return path to test fixture dictionary."
  (let ((base (or (and load-file-name
                       (file-name-directory load-file-name))
                  default-directory)))
    ;; If loaded from test/integration/, go up to project root
    (when (string-match-p "test/integration/?$" base)
      (setq base (file-name-directory
                  (directory-file-name
                   (file-name-directory
                    (directory-file-name base))))))
    ;; If loaded from test/, go up one level
    (when (string-match-p "test/?$" base)
      (setq base (file-name-directory (directory-file-name base))))
    (expand-file-name "test/fixtures/test-dict.skk" base)))

(defconst nskk-pbt--known-dict-keys
  '("かんじ" "にほん" "にほんご" "ひらがな" "かたかな"
    "へんかん" "にゅうりょく" "もじ" "かな" "さくら"
    "やま" "かわ" "そら" "はな" "つき" "ほし" "うみ"
    "かぜ" "あめ" "ゆき")
  "Keys known to exist in the test fixture dictionary.")

(defconst nskk-pbt--known-dict-expected
  '(("かんじ" . ("漢字" "感じ" "幹事"))
    ("にほん" . ("日本" "二本"))
    ("にほんご" . ("日本語"))
    ("さくら" . ("桜"))
    ("やま" . ("山"))
    ("かわ" . ("川" "河")))
  "Known key -> candidates mappings from fixture dictionary.")

(defun nskk-pbt--generate-unknown-key ()
  "Generate a key that is unlikely to exist in the fixture dictionary."
  (let ((prefixes '("zzz" "xxx" "qqq" "www"))
        (suffixes '("ああ" "いい" "うう" "ええ" "おお")))
    (concat (nskk-pbt--random-choice prefixes)
            (nskk-pbt--random-choice suffixes))))

(defconst nskk-pbt--test-pred 'nskk-pbt-test-dict-entry
  "Prolog predicate used in fixture-loading integration tests.")


;;;;
;;;; Property 1: Dictionary Load Fixture
;;;;

(ert-deftest nskk-state-machine-dict-load-fixture ()
  "Loading test dictionary yields accessible entries via Prolog."
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-clear-database)
    (let* ((dict-path (nskk-pbt--fixture-dict-path))
           (pred (nskk-dict-load-file dict-path nil nskk-pbt--test-pred)))
      ;; nskk-dict-load-file returns the predicate symbol on success
      (should (symbolp pred))
      (should (eq pred nskk-pbt--test-pred))
      ;; Create an index struct from the predicate
      (let ((index (make-nskk-dict-index :predicate pred)))
        (should (nskk-dict-index-p index))
        ;; Should have multiple entries in the Prolog database
        (should (> (nskk-dict--struct-entry-count index nil) 10)))
      ;; All known keys should be present via Prolog query
      (let ((failures nil))
        (dolist (key nskk-pbt--known-dict-keys)
          (let ((result (nskk-prolog-query-value
                         `(,nskk-pbt--test-pred ,key \?c) '\?c)))
            (unless result
              (push key failures))))
        (when failures
          (ert-fail (format "Missing keys in loaded dictionary: %S" failures)))))))


;;;;
;;;; Property 2: Dictionary Lookup Returns Candidates
;;;;

(ert-deftest nskk-state-machine-dict-lookup-returns-candidates ()
  "Known keys always return non-nil candidates from fixture dictionary."
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-clear-database)
    (let* ((dict-path (nskk-pbt--fixture-dict-path))
           (pred (nskk-dict-load-file dict-path nil nskk-pbt--test-pred))
           (index (make-nskk-dict-index :predicate pred))
           (runs 50)
           (failures nil))
      (dotimes (_ runs)
        (let* ((key (nskk-pbt--random-choice nskk-pbt--known-dict-keys))
               (result (nskk-dict--struct-lookup index key)))
          (unless (and result
                       (nskk-dict-entry-p result)
                       (nskk-dict-entry-candidates result)
                       (> (length (nskk-dict-entry-candidates result)) 0))
            (push (list :key key :result result) failures))))
      (when failures
        (ert-fail (format "Lookup failed for %d known keys:\n%S"
                          (length failures)
                          (take 5 failures)))))))


;;;;
;;;; Property 3: Dictionary Lookup Unknown Returns Nil
;;;;

(ert-deftest nskk-state-machine-dict-lookup-unknown-returns-nil ()
  "Random unknown keys return nil from fixture dictionary."
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-clear-database)
    (let* ((dict-path (nskk-pbt--fixture-dict-path))
           (pred (nskk-dict-load-file dict-path nil nskk-pbt--test-pred))
           (index (make-nskk-dict-index :predicate pred))
           (runs 50)
           (failures nil))
      (dotimes (_ runs)
        (let* ((key (nskk-pbt--generate-unknown-key))
               (result (nskk-dict--struct-lookup index key)))
          (when result
            (push (list :key key :result result) failures))))
      (when failures
        (ert-fail (format "Unknown key returned non-nil for %d cases:\n%S"
                          (length failures)
                          (take 5 failures)))))))


;;;;
;;;; Property 4: Dictionary Save-Load Roundtrip
;;;;

(ert-deftest nskk-state-machine-dict-save-load-roundtrip ()
  "Save user dict then reload: entries are preserved."
  (nskk-prolog-test-with-isolated-db
    (let ((runs 50)
          (failures nil))
      (dotimes (_ runs)
        (let* ((key (nskk-pbt--random-choice nskk-pbt--known-dict-keys))
               (candidates (nskk-pbt--random-choice
                            '(("候補1" "候補2") ("テスト") ("漢字" "感じ"))))
               (temp-file (make-temp-file "nskk-test-dict-" nil ".skk")))
          (unwind-protect
              (progn
                ;; Set up user dict entries via Prolog
                ;; Use targeted retract-all instead of clear-database to preserve
                ;; module-level facts (valid-henkan-transition, cache-type, etc.)
                (nskk-prolog-retract-all 'user-dict-entry 2)
                (nskk-prolog-set-index 'user-dict-entry 2 :trie)
                (nskk-prolog-assert (list (list 'user-dict-entry key candidates)))
                ;; Save user dictionary to temp file
                (let ((nskk--user-dict-index 'user)
                      (nskk-dict-user-dictionary-file temp-file))
                  (nskk-dict-save-user-dictionary)
                  ;; Reload the saved file under a separate predicate
                  (let* ((reload-pred 'nskk-pbt-reload-dict)
                         (_ (nskk-prolog-retract-all reload-pred 2))
                         (loaded (nskk-dict-load-file temp-file nil reload-pred)))
                    (if (not (symbolp loaded))
                        (push (list :key key :error "reload returned non-symbol") failures)
                      ;; Check that the key's candidates are present
                      (let ((reloaded-candidates
                             (nskk-prolog-query-value
                              `(,reload-pred ,key \?c) '\?c)))
                        (dolist (c candidates)
                          (unless (member c reloaded-candidates)
                            (push (list :key key
                                        :original candidates
                                        :reloaded reloaded-candidates
                                        :missing c)
                                  failures))))))))
            (when (file-exists-p temp-file)
              (delete-file temp-file)))))
      (when failures
        (ert-fail (format "Save-load roundtrip failed for %d cases:\n%S"
                          (length failures)
                          (take 5 failures)))))))


;;;;
;;;; Property 5: Dictionary Parse Line Valid
;;;;

(ert-deftest nskk-state-machine-dict-parse-line-valid ()
  "Valid SKK format lines parse correctly."
  (let ((runs 50)
        (failures nil)
        (valid-lines '("かんじ /漢字/感じ/幹事/"
                       "にほん /日本/二本/"
                       "さくら /桜/"
                       "やま /山/"
                       "かわ /川/河/"
                       "あめ /雨/飴/"
                       "すし /寿司/鮨/"
                       "てんぷら /天ぷら/天婦羅/")))
    (dotimes (_ runs)
      (let* ((line (nskk-pbt--random-choice valid-lines))
             (parsed (nskk-dict-parse-line line)))
        (unless (and parsed
                     (consp parsed)
                     (stringp (car parsed))
                     (> (length (car parsed)) 0)
                     (listp (cdr parsed))
                     (> (length (cdr parsed)) 0))
          (push (list :line line :parsed parsed) failures))))
    (when failures
      (ert-fail (format "Valid line parsing failed for %d cases:\n%S"
                        (length failures)
                        (take 5 failures))))))


;;;;
;;;; Property 6: Dictionary Parse Line Invalid
;;;;

(ert-deftest nskk-state-machine-dict-parse-line-invalid ()
  "Comments and invalid lines return nil."
  (let ((runs 50)
        (failures nil)
        (invalid-lines '(";; This is a comment"
                         ";; -*- coding: utf-8 -*-"
                         ""
                         "invalid line without slash"
                         "   "
                         ";; okuri-nasi entries.")))
    (dotimes (_ runs)
      (let* ((line (nskk-pbt--random-choice invalid-lines))
             (parsed (nskk-dict-parse-line line)))
        (when parsed
          (push (list :line line :parsed parsed) failures))))
    (when failures
      (ert-fail (format "Invalid line parsing returned non-nil for %d cases:\n%S"
                        (length failures)
                        (take 5 failures))))))


;;;
;;; Property-Based Tests (nskk-property-test / nskk-deftest-cases)
;;;

(nskk-property-test dict-lookup-returns-list-or-nil
  ((q search-query))
  (nskk-with-mock-dict nil
    (let ((result (nskk-dict-lookup q)))
      (should (or (null result) (listp result)))))
  40)

(nskk-property-test dict-lookup-known-key-returns-non-nil
  ((q search-query))
  ;; Register the reading first, then look it up — always returns the registered word
  (nskk-prolog-test-with-isolated-db
    (let ((nskk--user-dict-index 'user)
          (nskk-dict-modified nil))
      (nskk-prolog-set-index 'user-dict-entry 2 :trie)
      (nskk-dict-register-word q "テスト")
      (should (member "テスト" (nskk-dict-lookup q)))))
  20)

(nskk-deftest-cases dict-known-entries
  (("かんじ" . "漢字")
   ("うみ"   . "海"))
  :body (nskk-with-mock-dict (list (cons input (list expected)))
          (should (member expected (nskk-dict-lookup input)))))


(provide 'nskk-dictionary-integration-pbt-test)

;;; nskk-dictionary-integration-pbt-test.el ends here
