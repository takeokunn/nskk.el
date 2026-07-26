;;; nskk-tutorial-test.el --- Unit tests for nskk-tutorial  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: i18n, testing

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Unit tests for the NSKK tutorial module.

;;; Code:

(require 'ert)
(require 'nskk-test-framework)
(require 'nskk-tutorial)


;;;;
;;;; Lesson Data Integrity
;;;;

(ert-deftest nskk-tutorial-test/lessons-not-empty ()
  "Tutorial should have at least one lesson."
  (should (> (length nskk-tutorial--lessons) 0)))

(ert-deftest nskk-tutorial-test/lessons-count ()
  "Tutorial should have exactly 15 lessons."
  (should (= (length nskk-tutorial--lessons) 15)))

(ert-deftest nskk-tutorial-test/lessons-have-required-keys ()
  "Every lesson must have :title, :explanation, and :exercises."
  (dolist (lesson nskk-tutorial--lessons)
    (should (plist-get lesson :title))
    (should (plist-get lesson :explanation))
    (should (plist-get lesson :exercises))))

(ert-deftest nskk-tutorial-test/exercises-have-required-keys ()
  "Every exercise must have :instruction, :hint, and either :expected or :validator."
  (dolist (lesson nskk-tutorial--lessons)
    (dolist (ex (plist-get lesson :exercises))
      (should (plist-get ex :instruction))
      (should (plist-get ex :hint))
      (should (or (plist-get ex :expected)
                  (plist-get ex :validator))))))

(ert-deftest nskk-tutorial-test/lesson-titles-are-strings ()
  "All lesson titles should be strings."
  (dolist (lesson nskk-tutorial--lessons)
    (should (stringp (plist-get lesson :title)))))

(ert-deftest nskk-tutorial-test/lesson-explanations-are-strings ()
  "All lesson explanations should be strings."
  (dolist (lesson nskk-tutorial--lessons)
    (should (stringp (plist-get lesson :explanation)))))

(ert-deftest nskk-tutorial-test/exercises-are-lists ()
  "All lesson exercises should be non-empty lists."
  (dolist (lesson nskk-tutorial--lessons)
    (let ((exercises (plist-get lesson :exercises)))
      (should (listp exercises))
      (should (> (length exercises) 0)))))


;;;;
;;;; Mini Dictionary Integrity
;;;;

(ert-deftest nskk-tutorial-test/mini-dict-not-empty ()
  "Mini dictionary should have entries."
  (should (> (length nskk-tutorial--mini-dict) 0)))

(ert-deftest nskk-tutorial-test/mini-dict-entries-are-valid ()
  "Each mini dictionary entry should be a cons of (string . list-of-strings)."
  (dolist (entry nskk-tutorial--mini-dict)
    (should (consp entry))
    (should (stringp (car entry)))
    (should (listp (cdr entry)))
    (should (> (length (cdr entry)) 0))
    (dolist (candidate (cdr entry))
      (should (stringp candidate)))))

(ert-deftest nskk-tutorial-test/mini-dict-covers-basic-exercises ()
  "Mini dictionary should contain entries for basic conversion exercises."
  (let ((keys (mapcar #'car nskk-tutorial--mini-dict)))
    (should (member "かんじ" keys))
    (should (member "へんかん" keys))
    (should (member "にほんご" keys))
    (should (member "さくら" keys))
    (should (member "うみ" keys))))

(ert-deftest nskk-tutorial-test/mini-dict-covers-okurigana ()
  "Mini dictionary should contain okurigana entries for lesson 6."
  (let ((keys (mapcar #'car nskk-tutorial--mini-dict)))
    (should (member "かk" keys))
    (should (member "よm" keys))
    (should (member "みr" keys))))

(ert-deftest nskk-tutorial-test/mini-dict-covers-dcomp ()
  "Mini dictionary should contain dcomp prefix entries for lesson 10."
  (let ((keys (mapcar #'car nskk-tutorial--mini-dict)))
    (should (member "かんけい" keys))
    (should (member "かんきょう" keys))
    (should (member "かんたん" keys))))

(ert-deftest nskk-tutorial-test/mini-dict-covers-candidate-list ()
  "Mini dictionary should have enough candidates for lesson 11."
  (let ((entry (assoc "こうえん" nskk-tutorial--mini-dict)))
    (should entry)
    (should (>= (length (cdr entry)) 7))))

(ert-deftest nskk-tutorial-test/mini-dict-covers-advanced ()
  "Mini dictionary should contain entries for advanced lessons 12-14."
  (let ((keys (mapcar #'car nskk-tutorial--mini-dict)))
    (should (member "でんわ" keys))
    (should (member "がっこう" keys))
    (should (member "ぶんしょう" keys))
    (should (member "さくせい" keys))
    (should (member "かんせい" keys))))

(ert-deftest nskk-tutorial-test/mini-dict-kanji-has-multiple-candidates ()
  "The かんじ entry should have multiple candidates for lesson 5."
  (let ((entry (assoc "かんじ" nskk-tutorial--mini-dict)))
    (should entry)
    (should (>= (length (cdr entry)) 3))))


;;;;
;;;; Validator Functions
;;;;

(ert-deftest nskk-tutorial-test/validate-hiragana-mode-when-active ()
    "Hiragana mode validator should return t when in hiragana mode."
    (with-temp-buffer
      (nskk-with-mock-dict nil
        (nskk-mode 1)
        (nskk--set-mode 'hiragana)
        (should (nskk-tutorial--validate-hiragana-mode))
        (ignore-errors (nskk-mode -1)))))

  (defun nskk-tutorial-test--make-dict-state ()
    "Return a dictionary state graph with representative shared references."
    (let* ((clauses (list '((outside "reading" ("candidate")))))
           (tail (last clauses))
           (database (make-hash-table :test 'equal))
           (tails (make-hash-table :test 'equal))
           (index-config (make-hash-table :test 'equal))
           (hash-indices (make-hash-table :test 'equal))
           (trie-indices (make-hash-table :test 'equal))
           (bucket-tail-cache (make-hash-table :test 'equal))
           (hash-index (make-hash-table :test 'equal))
           (trie-index (nskk-trie-create))
           (buckets (make-hash-table :test 'equal))
           (cache-entry (vector :hash hash-index buckets))
           (active-mutation-keys (list "outside/2")))
      (puthash "outside/2" clauses database)
      (puthash "outside/2" tail tails)
      (puthash "outside/2" :hash index-config)
      (puthash "reading" clauses hash-index)
      (nskk-trie-insert trie-index "reading" clauses)
      (puthash "outside/2" hash-index hash-indices)
      (puthash "outside/2" trie-index trie-indices)
      (puthash "reading" (vector clauses tail) buckets)
      (puthash "outside/2" cache-entry bucket-tail-cache)
      (vector database tails index-config hash-indices trie-indices
              bucket-tail-cache 37 active-mutation-keys
              hash-index trie-index)))

(ert-deftest nskk-tutorial-test/validate-hiragana-mode-when-ascii ()
  "Hiragana mode validator should return nil when in ascii mode."
  (with-temp-buffer
    (nskk-with-mock-dict nil
      (nskk-mode 1)
      (nskk--set-mode 'ascii)
      (should-not (nskk-tutorial--validate-hiragana-mode))
      (ignore-errors (nskk-mode -1)))))

(ert-deftest nskk-tutorial-test/validate-hiragana-mode-when-no-nskk ()
  "Hiragana mode validator should return nil when nskk-mode is off."
  (with-temp-buffer
    (should-not (nskk-tutorial--validate-hiragana-mode))))


;;;;
;;;; Face Definitions
;;;;

(ert-deftest nskk-tutorial-test/faces-defined ()
  "All tutorial faces should be defined."
  (should (facep 'nskk-tutorial-header-face))
  (should (facep 'nskk-tutorial-instruction-face))
  (should (facep 'nskk-tutorial-input-area-face))
  (should (facep 'nskk-tutorial-success-face))
  (should (facep 'nskk-tutorial-hint-face)))


;;;;
;;;; Prolog DB Isolation
;;;;

(ert-deftest nskk-tutorial-test/copy-object-graph-preserves-sharing ()
  "Graph copying should isolate mutable objects and preserve shared edges."
  (let* ((state (nskk-tutorial-test--make-dict-state))
         (copy (nskk-tutorial--copy-object-graph
                state (make-hash-table :test 'eq)))
         (original-clauses (gethash "outside/2" (aref state 0)))
         (original-tail (gethash "outside/2" (aref state 1)))
         (original-trie (aref state 9))
         (copied-clauses (gethash "outside/2" (aref copy 0)))
         (copied-tail (gethash "outside/2" (aref copy 1)))
         (copied-trie (aref copy 9))
         (cache-entry (gethash "outside/2" (aref copy 5)))
         (buckets (aref cache-entry 2))
         (bucket-entry (gethash "reading" buckets)))
    (should-not (eq copy state))
    (dolist (index '(0 1 2 3 4 5 7 8 9))
      (should-not (eq (aref copy index) (aref state index))))
    (should (= (aref copy 6) 37))
    (should (eq copied-tail (last copied-clauses)))
    (should (eq (aref copy 8)
                (gethash "outside/2" (aref copy 3))))
    (should (eq copied-trie
                (gethash "outside/2" (aref copy 4))))
    (should (eq (nskk-trie-lookup copied-trie "reading")
                copied-clauses))
    (should (eq (aref cache-entry 1) (aref copy 8)))
    (should (eq (aref bucket-entry 0) copied-clauses))
    (should (eq (aref bucket-entry 1) copied-tail))
    (setcdr copied-tail (list 'copied-only))
    (puthash "copied-only" t (aref copy 8))
    (nskk-trie-insert copied-trie "copied-only" t)
    (should-not (cdr original-tail))
    (should-not (gethash "copied-only" (aref state 8)))
    (should-not (nskk-trie-lookup original-trie "copied-only"))
    (should (eq original-tail (last original-clauses)))))

(ert-deftest nskk-tutorial-test/copy-object-graph-preserves-cycles ()
  "Graph copying should preserve cycles, shared edges, and copied hash keys."
  (let* ((node (cons 'node nil))
         (table (make-hash-table :test 'eq))
         (root (vector node node table)))
    (setcdr node node)
    (puthash node node table)
    (let* ((copy (nskk-tutorial--copy-object-graph
                  root (make-hash-table :test 'eq)))
           (copied-node (aref copy 0))
           (copied-table (aref copy 2)))
      (should-not (eq copy root))
      (should-not (eq copied-node node))
      (should (eq copied-node (aref copy 1)))
      (should (eq (cdr copied-node) copied-node))
      (should (eq (gethash copied-node copied-table) copied-node))
      (should-not (gethash node copied-table)))))

(ert-deftest nskk-tutorial-test/copy-object-graph-copies-string-properties ()
  "Graph copying should copy string property values and preserve intervals."
  (let ((multibyte (copy-sequence "aあbc"))
        (unibyte (unibyte-string ?a 255 ?b ?c)))
    (should (multibyte-string-p multibyte))
    (should-not (multibyte-string-p unibyte))
    (dolist (text (list multibyte unibyte))
      (let* ((payload (list 'original))
             (root (vector text payload)))
        (put-text-property 0 1 'node text text)
        (put-text-property 1 3 'node payload text)
        (put-text-property 3 4 'node 'tail text)
        (put-text-property 0 4 'face 'bold text)
        (let* ((copy (nskk-tutorial--copy-object-graph
                      root (make-hash-table :test 'eq)))
               (copied-text (aref copy 0))
               (copied-payload (aref copy 1)))
          (should-not (eq copied-text text))
          (should (string= copied-text text))
          (should (eq (multibyte-string-p copied-text)
                      (multibyte-string-p text)))
          (should (eq (get-text-property 0 'node copied-text) copied-text))
          (should (eq (get-text-property 1 'node copied-text)
                      copied-payload))
          (should (eq (get-text-property 2 'node copied-text)
                      copied-payload))
          (should-not (eq copied-payload payload))
          (should (eq (get-text-property 3 'node copied-text) 'tail))
          (should (eq (get-text-property 0 'face copied-text) 'bold))
          (should (= (next-property-change 0 copied-text 4) 1))
          (should (= (next-property-change 1 copied-text 4) 3))
          (should (= (next-property-change 3 copied-text 4) 4))
          (setcar copied-payload 'copied)
          (aset copied-text 0 ?z)
          (should (eq (car payload) 'original))
          (should (= (aref text 0) ?a))
          (should (eq (get-text-property 0 'node text) text)))))))

(ert-deftest nskk-tutorial-test/copy-object-graph-snapshots-weak-hash-entries ()
  "Graph copying should retain weak entries observed during discovery."
  (let* ((memo (make-hash-table :test 'eq))
         (weak-table (make-hash-table
                      :test 'eq
                      :size 17
                      :rehash-size 2.0
                      :rehash-threshold 0.7
                      :weakness 'key))
         (original-gethash (symbol-function 'gethash))
         (missing (make-symbol "missing"))
         (probes 0)
         copy)
    (let ((key (make-symbol "ephemeral-key")))
      (puthash key 'value weak-table))
    (should (= (hash-table-count weak-table) 1))
    (let ((gc-cons-threshold most-positive-fixnum))
      (cl-letf (((symbol-function 'gethash)
                 (lambda (key table &optional default)
                   (when (and (eq key weak-table) (eq table memo))
                     (setq probes (1+ probes))
                     (when (= probes 2)
                       (garbage-collect)))
                   (funcall original-gethash key table default))))
        (setq copy (nskk-tutorial--copy-object-graph weak-table memo)))
      (should (= probes 3))
      (should-not (eq copy weak-table))
      (should (eq (hash-table-test copy) (hash-table-test weak-table)))
      (should (eq (hash-table-weakness copy)
                  (hash-table-weakness weak-table)))
      (should (= (hash-table-size copy) (hash-table-size weak-table)))
      (should (equal (hash-table-rehash-size copy)
                     (hash-table-rehash-size weak-table)))
      (should (equal (hash-table-rehash-threshold copy)
                     (hash-table-rehash-threshold weak-table)))
      (should (= (hash-table-count weak-table) 1))
      (should (= (hash-table-count copy) 1))
      (let (copied-key copied-value)
        (maphash
         (lambda (key value)
           (setq copied-key key
                 copied-value value))
         copy)
        (should copied-key)
        (should (eq copied-value 'value))
        (should (eq (gethash copied-key weak-table missing) copied-value)))
      (puthash 'copy-only t copy)
      (should (eq (gethash 'copy-only weak-table missing) missing)))))

(ert-deftest nskk-tutorial-test/copy-object-graph-is-stack-safe ()
  "Graph copying should handle a cyclic list deeper than Lisp evaluation depth."
  (let* ((depth (+ max-lisp-eval-depth 100))
         (head (cons 0 nil))
         (tail head))
    (dotimes (index depth)
      (let ((node (cons (1+ index) nil)))
        (setcdr tail node)
        (setq tail node)))
    (setcdr tail head)
    (let ((copy (nskk-tutorial--copy-object-graph
                 head (make-hash-table :test 'eq))))
      (should-not (eq copy head))
      (let ((cursor copy))
        (dotimes (index (1+ depth))
          (should (= (car cursor) index))
          (setq cursor (cdr cursor)))
        (should (eq cursor copy))))))

(defun nskk-tutorial-test--assert-char-table-extra-slot-copy
    (actual-count reported-count)
  "Assert saved copies use ACTUAL-COUNT despite subtype REPORTED-COUNT."
  (let* ((state (nskk-tutorial-test--make-dict-state))
         (purpose (make-symbol "nskk-tutorial-copy-extra-slots"))
         (property (quote char-table-extra-slots))
         (property-present-p
          (plist-member (symbol-plist purpose) property))
         (old-property (get purpose property))
         (key "tutorial-copy-extra-slots/1")
         (slot-values (make-vector actual-count nil)))
    (dotimes (index actual-count)
      (aset slot-values index (list index)))
    (unwind-protect
        (progn
          (put purpose property actual-count)
          (let ((table (make-char-table purpose nil)))
            (dotimes (index actual-count)
              (set-char-table-extra-slot
               table index (aref slot-values index)))
            (put purpose property reported-count)
            (let ((clause
                   (list
                    (list (quote tutorial-copy-extra-slots) table)))
                  (canonical-clause nil))
              (nskk-tutorial-test--call-with-dict-state
               state
               (lambda ()
                 (nskk-prolog-assert clause)
                 (progn
                   (setq canonical-clause
                         (car (gethash key nskk--prolog-database)))
                   (should (equal canonical-clause clause))
                   (should-not (eq canonical-clause clause)))
                 (with-temp-buffer
                   (nskk-tutorial--save-dict-state)
                   (unwind-protect
                       (let* ((working-clause
                               (car
                                (gethash key nskk--prolog-database)))
                              (copied-table
                               (nth 1 (car working-clause))))
                         (nskk-tutorial-test--should-saved-state-eq
                          state)
                         (should
                          (eq
                           (car
                            (gethash
                             key nskk-tutorial--saved-prolog-db))
                           canonical-clause))
                         (should-not (eq working-clause canonical-clause))
                         (should-not (eq copied-table table))
                         (should
                          (= (get purpose property) reported-count))
                         (dotimes (index actual-count)
                           (let ((original-value
                                  (aref slot-values index))
                                 (copied-value
                                  (char-table-extra-slot
                                   copied-table index)))
                             (should-not
                              (eq copied-value original-value))
                             (should (equal copied-value original-value))
                             (setcar copied-value (quote copied))
                             (should
                              (eq
                               (char-table-extra-slot table index)
                               original-value))
                             (should (= (car original-value) index))))
                         (should-error
                          (char-table-extra-slot
                           copied-table actual-count)
                          :type (quote args-out-of-range)))
                     (when nskk-tutorial--dict-state-saved-p
                       (nskk-tutorial--restore-dict-state)))
                   (nskk-tutorial-test--should-current-state-eq
                    state)))))))
      (if property-present-p
          (put purpose property old-property)
        (cl-remprop purpose property)))))

(ert-deftest nskk-tutorial-test/save-dict-state-uses-actual-extra-slot-count-when-property-is-smaller ()
  "Saving must copy slots omitted by a reduced subtype property."
  (nskk-tutorial-test--assert-char-table-extra-slot-copy 2 1))

(ert-deftest nskk-tutorial-test/save-dict-state-uses-actual-extra-slot-count-when-property-is-larger ()
  "Saving must not read nonexistent slots added only to a subtype property."
  (nskk-tutorial-test--assert-char-table-extra-slot-copy 1 3))

(ert-deftest nskk-tutorial-test/save-dict-state-copies-bool-vector-and-char-table ()
  "Saving asserted data should isolate bool vectors and char table graphs."
  (let* ((state (nskk-tutorial-test--make-dict-state))
         (purpose (make-symbol "nskk-tutorial-copy-table"))
         (key "tutorial-copy-composites/2")
         (bits (make-bool-vector 4 nil))
         (parent-default (list 'parent-default))
         (parent-entry (list 'parent-entry))
         (shared (list 'shared)))
    (put purpose 'char-table-extra-slots 2)
    (let* ((parent (make-char-table purpose nil))
           (table (make-char-table purpose nil))
           (peer (make-char-table purpose nil)))
      (aset bits 1 t)
      (set-char-table-range parent nil parent-default)
      (set-char-table-range parent ?p parent-entry)
      (set-char-table-range parent ?r parent-default)
      (set-char-table-parent table parent)
      (set-char-table-range table '(?a . ?c) shared)
      (set-char-table-range table ?x peer)
      (set-char-table-range table ?z table)
      (set-char-table-range peer ?y table)
      (set-char-table-extra-slot table 0 shared)
      (set-char-table-extra-slot table 1 table)
      (let ((clause
             (list
              (list 'tutorial-copy-composites bits table)))
            (canonical-clause nil))
        (nskk-tutorial-test--call-with-dict-state
         state
         (lambda ()
           (nskk-prolog-assert clause)
           (progn
             (setq canonical-clause
                   (car (gethash key nskk--prolog-database)))
             (should (equal canonical-clause clause))
             (should-not (eq canonical-clause clause)))
           (with-temp-buffer
             (nskk-tutorial--save-dict-state)
             (unwind-protect
                 (let* ((working-clause
                         (car (gethash key nskk--prolog-database)))
                        (working-head (car working-clause))
                        (copied-bits (nth 1 working-head))
                        (copied-table (nth 2 working-head))
                        (copied-parent (char-table-parent copied-table))
                        (copied-peer (char-table-range copied-table ?x))
                        (copied-shared
                         (char-table-range copied-table ?a))
                        (copied-parent-entry
                         (char-table-range copied-parent ?p))
                        (copied-default
                         (char-table-range copied-parent nil)))
                   (nskk-tutorial-test--should-saved-state-eq state)
                   (should (eq
                            (car (gethash
                                  key nskk-tutorial--saved-prolog-db))
                            canonical-clause))
                   (should-not (eq working-clause canonical-clause))
                   (should-not (eq copied-bits bits))
                   (should (equal copied-bits bits))
                   (should-not (aref copied-bits 0))
                   (should (aref copied-bits 1))
                   (should-not (eq copied-table table))
                   (should (eq (char-table-subtype copied-table) purpose))
                   (should (= (get (char-table-subtype copied-table)
                                   'char-table-extra-slots)
                              2))
                   (should-not (eq copied-parent parent))
                   (should (eq (char-table-subtype copied-parent) purpose))
                   (should-not (eq copied-peer peer))
                   (should (eq (char-table-subtype copied-peer) purpose))
                   (should-not (char-table-range copied-table nil))
                   (should-not (eq copied-shared shared))
                   (should (eq (char-table-range copied-table ?b)
                               copied-shared))
                   (should (eq (char-table-range copied-table ?c)
                               copied-shared))
                   (should (eq (char-table-extra-slot copied-table 0)
                               copied-shared))
                   (should (eq (char-table-range copied-table ?z)
                               copied-table))
                   (should (eq (char-table-extra-slot copied-table 1)
                               copied-table))
                   (should (eq (char-table-range copied-peer ?y)
                               copied-table))
                   (should-not (eq copied-parent-entry parent-entry))
                   (should (eq (aref copied-table ?p)
                               copied-parent-entry))
                   (should-not (eq copied-default parent-default))
                   (should (eq (aref copied-table ?q) copied-default))
                   (should (eq (char-table-range copied-parent ?r) copied-default))
                   (should (eq (aref copied-table ?r) copied-default))
                   (aset copied-bits 0 t)
                   (setcar copied-shared 'working-shared)
                   (setcar copied-parent-entry 'working-entry)
                   (setcar copied-default 'working-default)
                   (should (eq (aref copied-table ?p)
                               copied-parent-entry))
                   (should (eq (aref copied-table ?q)
                               copied-default))
                   (let ((new-entry (list 'new-entry))
                         (new-default (list 'new-default)))
                     (set-char-table-range copied-parent ?p new-entry)
                     (set-char-table-range copied-parent nil new-default)
                     (should (eq (aref copied-table ?p) new-entry))
                     (should (eq (aref copied-table ?q) new-default))
                     (should (eq (aref copied-table ?r) copied-default)))
                   (should-not (aref bits 0))
                   (should (aref bits 1))
                   (should (eq (car shared) 'shared))
                   (should (eq (car parent-entry) 'parent-entry))
                   (should (eq (car parent-default) 'parent-default))
                   (should (eq (char-table-range parent ?r) parent-default))
                   (should (eq (char-table-range table ?z) table))
                   (should (eq (char-table-range peer ?y) table))
                   (should (eq (char-table-extra-slot table 1) table)))
               (when nskk-tutorial--dict-state-saved-p
                 (nskk-tutorial--restore-dict-state)))
             (nskk-tutorial-test--should-current-state-eq state))))))))

;;;;
;;;; Buffer Name
;;;;

(ert-deftest nskk-tutorial-test/buffer-name ()
  "Tutorial buffer should be named *NSKK Tutorial*."
  (should (string= nskk-tutorial--buffer-name "*NSKK Tutorial*")))


;;;;
;;;; Header Line
;;;;

(ert-deftest nskk-tutorial-test/header-line-format ()
  "Header line should include lesson number and title."
  (with-temp-buffer
    (setq-local nskk-tutorial--current-lesson 0)
    (let ((header (nskk-tutorial--header-line)))
      (should (string-match-p "レッスン 1/" header))
      (should (string-match-p "はじめに" header)))))

(ert-deftest nskk-tutorial-test/advanced-lessons-exist ()
  "Lessons 9-15 should exist with expected titles."
  (let ((titles (mapcar (lambda (l) (plist-get l :title))
                        nskk-tutorial--lessons)))
    (should (string-match-p "Abbrev" (nth 8 titles)))
    (should (string-match-p "動的補完" (nth 9 titles)))
    (should (string-match-p "候補リスト" (nth 10 titles)))
    (should (string-match-p "Sticky" (nth 11 titles)))
    (should (string-match-p "数値変換" (nth 12 titles)))
    (should (string-match-p "AZIK" (nth 13 titles)))
    (should (string-match-p "総合練習" (nth 14 titles)))))


(define-error 'nskk-tutorial-test-injected-error
    "Injected tutorial startup error")
  (define-error 'nskk-tutorial-test-restore-error
    "Injected tutorial restore failure")

(ert-deftest nskk-tutorial-test/restores-save-inhibition-on-normal-kill ()
    "Killing the tutorial buffer should restore the prior save inhibition."
    (dolist (initial-value '(nil t))
      (let ((nskk--dict-save-inhibited initial-value)
            (buffer (generate-new-buffer " *nskk-tutorial-kill-test*")))
        (unwind-protect
            (progn
              (with-current-buffer buffer
                (nskk-tutorial-mode)
                (nskk-tutorial--save-dict-state))
              (should nskk--dict-save-inhibited)
              (kill-buffer buffer)
              (should (eq nskk--dict-save-inhibited initial-value)))
          (when (buffer-live-p buffer)
            (kill-buffer buffer))))))

(ert-deftest nskk-tutorial-test/startup-failure-restores-state-and-kills-buffer ()
    "Startup errors should roll back all tutorial-owned state."
    (dolist (initial-value '(nil t))
      (let* ((nskk--dict-save-inhibited initial-value)
             (sentinel-reading
              (format "tutorial-startup-sentinel-%s" initial-value))
             (sentinel-head
              `(user-dict-entry ,sentinel-reading ("original"))))
        (nskk-prolog-assert (list sentinel-head))
        (unwind-protect
            (cl-letf (((symbol-function 'nskk-mode)
                       (lambda (&rest _)
                         (signal 'nskk-tutorial-test-injected-error
                                 '(original-payload)))))
              (let ((error-data
                     (should-error
                      (nskk-tutorial)
                      :type 'nskk-tutorial-test-injected-error)))
                (should
                 (equal error-data
                        '(nskk-tutorial-test-injected-error
                          original-payload))))
              (should-not (get-buffer nskk-tutorial--buffer-name))
              (should (eq nskk--dict-save-inhibited initial-value))
              (should
               (equal
                (nskk-prolog-query-value
                 `(user-dict-entry ,sentinel-reading \?candidates)
                 '\?candidates)
                '("original"))))
          (when-let* ((buffer (get-buffer nskk-tutorial--buffer-name)))
            (kill-buffer buffer))
          (nskk-prolog-retract sentinel-head)))))

(ert-deftest nskk-tutorial-test/ownership-rejects-concurrent-tutorial ()
    "Only one live tutorial transaction may own process state."
    (let ((nskk-tutorial--owner nil)
          (nskk--active-buffers nil)
          (owner (generate-new-buffer " *nskk-tutorial-owner*"))
          (challenger (generate-new-buffer " *nskk-tutorial-challenger*")))
      (unwind-protect
          (progn
            (with-current-buffer owner
              (nskk-tutorial--acquire-ownership))
            (with-current-buffer challenger
              (should-error (nskk-tutorial--acquire-ownership)
                            :type 'user-error))
            (should (eq nskk-tutorial--owner owner)))
        (when (buffer-live-p owner)
          (with-current-buffer owner
            (nskk-tutorial--release-ownership))
          (kill-buffer owner))
        (when (buffer-live-p challenger)
          (kill-buffer challenger)))))

  (ert-deftest nskk-tutorial-test/ownership-rejects-other-active-nskk-buffer ()
    "Tutorial startup must not overlap another active NSKK buffer."
    (let ((nskk-tutorial--owner nil)
          (active (generate-new-buffer " *nskk-active*"))
          (tutorial (generate-new-buffer " *nskk-tutorial-candidate*")))
      (unwind-protect
          (let ((nskk--active-buffers (list active)))
            (with-current-buffer tutorial
              (should-error (nskk-tutorial--acquire-ownership)
                            :type 'user-error))
            (should-not nskk-tutorial--owner))
        (when (buffer-live-p active)
          (kill-buffer active))
        (when (buffer-live-p tutorial)
          (kill-buffer tutorial)))))

  (ert-deftest nskk-tutorial-test/ownership-rejects-nskk-enable-in-other-buffer ()
    "Tutorial ownership must reject NSKK activation in another buffer."
    (let ((nskk-tutorial--owner nil)
          (nskk--active-buffers nil)
          (owner (generate-new-buffer " *nskk-tutorial-enable-owner*"))
          (challenger (generate-new-buffer " *nskk-tutorial-enable-challenger*")))
      (unwind-protect
          (progn
            (with-current-buffer owner
              (nskk-tutorial--acquire-ownership))
            (with-current-buffer challenger
              (should-error (nskk-mode 1) :type 'user-error)
              (should-not nskk-mode)
              (should-not (bound-and-true-p nskk-current-state)))
            (should (eq nskk-tutorial--owner owner))
            (should-not nskk--active-buffers))
        (when (buffer-live-p owner)
          (with-current-buffer owner
            (nskk-tutorial--release-ownership))
          (kill-buffer owner))
        (when (buffer-live-p challenger)
          (with-current-buffer challenger
            (when nskk-mode
              (nskk-mode -1)))
          (kill-buffer challenger)))))

  (ert-deftest nskk-tutorial-test/major-mode-change-rolls-back-transaction ()
    "Changing tutorial major mode restores guards and ownership."
    (let ((nskk-tutorial--owner nil)
          (nskk--active-buffers nil)
          (nskk--dict-save-inhibited nil)
          (nskk--persistence-inhibited 'previous)
          (buffer (generate-new-buffer " *nskk-tutorial-mode-change*")))
      (unwind-protect
          (with-current-buffer buffer
            (nskk-tutorial-mode)
            (nskk-tutorial--acquire-ownership)
            (nskk-tutorial--save-dict-state)
            (should nskk--dict-save-inhibited)
            (should (eq nskk--persistence-inhibited t))
            (fundamental-mode)
            (should-not nskk-tutorial--owner)
            (should-not nskk--dict-save-inhibited)
            (should (eq nskk--persistence-inhibited 'previous)))
        (when (buffer-live-p buffer)
          (kill-buffer buffer)))))

  (ert-deftest nskk-tutorial-test/rollback-completes-after-quit ()
    "Rollback must release all resources before re-signaling QUIT."
    (let ((nskk-tutorial--owner nil)
      (nskk--active-buffers nil)
      (nskk--dict-save-inhibited nil)
      (nskk--persistence-inhibited (quote previous))
      (nskk--candidate-show-hook-owned nil)
      (nskk--candidate-hide-hook-owned nil)
      (nskk--candidate-select-function-owned nil)
      (nskk--saved-candidate-select-function nil))
  (with-temp-buffer
    (nskk-tutorial-mode)
    (nskk-tutorial--acquire-ownership)
    (nskk-tutorial--save-dict-state)
    (nskk-mode 1)
    (setq nskk--show-mode-overlay (make-overlay (point) (point))
          nskk--show-mode-timer (run-with-timer 60 nil (function ignore)))
    (let ((timer nskk--show-mode-timer)
          condition-data)
      (add-hook
        (quote nskk-mode-off-hook)
        (lambda ()
          (signal (quote quit) (quote (original-payload))))
        nil
        t)
      (setq condition-data (condition-case
          data
          (progn
            (nskk-tutorial--rollback)
            nil)
          (quit data)))
      (should (equal condition-data (quote (quit original-payload))))
      (should-not nskk-current-state)
      (should-not nskk--show-mode-overlay)
      (should-not nskk--show-mode-timer)
      (should-not (memq timer timer-list))
      (should-not (memq (current-buffer) nskk--active-buffers))
      (should-not nskk-tutorial--owner)
      (should-not nskk-tutorial--owns-transaction)
      (should-not nskk--dict-save-inhibited)
      (should (eq nskk--persistence-inhibited (quote previous)))))))
  (defun nskk-tutorial-test--current-dict-state ()
  "Return the currently published dictionary roots."
  (vector nskk--prolog-database
          nskk--prolog-database-tails
          nskk--prolog-index-config
          nskk--prolog-hash-indices
          nskk--prolog-trie-indices
          nskk--prolog-index-bucket-tail-cache
          nskk--prolog-var-counter
          nskk--prolog-active-mutation-keys
          nskk--user-dict-index
          nskk--system-dict-index))

(defun nskk-tutorial-test--saved-dict-state ()
  "Return the dictionary roots retained in the tutorial snapshot."
  (vector nskk-tutorial--saved-prolog-db
          nskk-tutorial--saved-prolog-tails
          nskk-tutorial--saved-prolog-idx
          nskk-tutorial--saved-prolog-hash
          nskk-tutorial--saved-prolog-trie
          nskk-tutorial--saved-prolog-bucket-tail-cache
          nskk-tutorial--saved-prolog-counter
          nskk-tutorial--saved-prolog-active-mutation-keys
          nskk-tutorial--saved-user-dict
          nskk-tutorial--saved-system-dict))

(defun nskk-tutorial-test--call-with-dict-state (state function)
  "Call FUNCTION with dictionary STATE and sentinel transaction values."
  (cl-progv
      '(nskk--prolog-database
        nskk--prolog-database-tails
        nskk--prolog-index-config
        nskk--prolog-hash-indices
        nskk--prolog-trie-indices
        nskk--prolog-index-bucket-tail-cache
        nskk--prolog-var-counter
        nskk--prolog-active-mutation-keys
        nskk--user-dict-index
        nskk--system-dict-index
        nskk--input-initialized
        nskk--state-prolog-initialized
        nskk--henkan-initialized
        nskk--kana-initialized
        nskk--converter-initialized
        nskk--candidate-key-facts-initialized
        nskk--annotation-initialized
        nskk--dict-save-inhibited
        nskk--persistence-inhibited)
      (list (aref state 0)
            (aref state 1)
            (aref state 2)
            (aref state 3)
            (aref state 4)
            (aref state 5)
            (aref state 6)
            (aref state 7)
            (aref state 8)
            (aref state 9)
            'input-before
            'state-prolog-before
            'henkan-before
            'kana-before
            'converter-before
            'candidate-key-before
            'annotation-before
            'dict-before
            'persistence-before)
    (funcall function)))

(defun nskk-tutorial-test--should-current-state-eq (state)
  "Assert that every published dictionary root is identical to STATE."
  (let ((current (nskk-tutorial-test--current-dict-state)))
    (dotimes (index (length state))
      (should (eq (aref current index) (aref state index))))))

(defun nskk-tutorial-test--should-saved-state-eq (state)
  "Assert that every saved dictionary root is identical to STATE."
  (let ((saved (nskk-tutorial-test--saved-dict-state)))
    (dotimes (index (length state))
      (should (eq (aref saved index) (aref state index))))))

(ert-deftest nskk-tutorial-test/save-install-and-restore-exact-state ()
  "Tutorial state must isolate mutations and restore every exact reference."
  (let* ((state (nskk-tutorial-test--make-dict-state))
         (outside-clauses (gethash "outside/2" (aref state 0)))
         (outside-tail (gethash "outside/2" (aref state 1)))
         (outside-hash-index (aref state 8)))
    (nskk-tutorial-test--call-with-dict-state
     state
     (lambda ()
       (with-temp-buffer
         (nskk-tutorial--save-dict-state)
         (let* ((working (nskk-tutorial-test--current-dict-state))
                (working-clauses (gethash "outside/2" (aref working 0)))
                (working-tail (gethash "outside/2" (aref working 1)))
                (cache-entry (gethash "outside/2" (aref working 5)))
                (bucket-entry
                 (gethash "reading" (aref cache-entry 2))))
           (dolist (index '(0 1 2 3 4 5 7 8 9))
             (should-not (eq (aref working index) (aref state index))))
           (should (= (aref working 6) (aref state 6)))
           (should (eq working-tail (last working-clauses)))
           (should (eq (aref working 8)
                       (gethash "outside/2" (aref working 3))))
           (should (eq (aref working 9)
                       (gethash "outside/2" (aref working 4))))
           (should (eq (aref cache-entry 1) (aref working 8)))
           (should (eq (aref bucket-entry 0) working-clauses))
           (should (eq (aref bucket-entry 1) working-tail))
           (should (eq nskk--input-initialized 'input-before))
           (should (eq nskk--state-prolog-initialized
                       'state-prolog-before))
           (should (eq nskk--henkan-initialized 'henkan-before))
           (should (eq nskk--kana-initialized 'kana-before))
           (should (eq nskk--converter-initialized 'converter-before))
           (should (eq nskk--candidate-key-facts-initialized
                       'candidate-key-before))
           (should (eq nskk--annotation-initialized
                       'annotation-before))
           (should nskk--dict-save-inhibited)
           (should nskk--persistence-inhibited)
           (nskk-tutorial--install-mini-dict)
           (should (gethash "user-dict-entry/2"
                            nskk--prolog-database))
           (should-not (gethash "user-dict-entry/2" (aref state 0)))
           (should (= (hash-table-count (aref state 0)) 1))
           (should-not (cdr outside-tail))
           (should (eq outside-tail (last outside-clauses)))
           (should (eq (gethash "reading" outside-hash-index)
                       outside-clauses))
           (should (eq (gethash "outside/2" (aref state 1))
                       outside-tail))
           (should (eq (aref (gethash "outside/2" (aref state 5)) 1)
                       outside-hash-index))
           (nskk-tutorial--restore-dict-state)
           (nskk-tutorial-test--should-current-state-eq state)
           (should (eq nskk--input-initialized 'input-before))
           (should (eq nskk--state-prolog-initialized
                       'state-prolog-before))
           (should (eq nskk--henkan-initialized 'henkan-before))
           (should (eq nskk--kana-initialized 'kana-before))
           (should (eq nskk--converter-initialized 'converter-before))
           (should (eq nskk--candidate-key-facts-initialized
                       'candidate-key-before))
           (should (eq nskk--annotation-initialized
                       'annotation-before))
           (should (eq nskk--dict-save-inhibited 'dict-before))
           (should (eq nskk--persistence-inhibited
                       'persistence-before))
           (should-not nskk-tutorial--dict-state-saved-p)
           (let ((saved (nskk-tutorial-test--saved-dict-state)))
             (dotimes (index (length saved))
               (should-not (aref saved index))))))))))

(ert-deftest nskk-tutorial-test/save-preparation-failure-does-not-publish ()
  "Preparation ERROR or QUIT must leave all process globals untouched."
  (dolist (condition '(nskk-tutorial-test-injected-error quit))
    (let ((state (nskk-tutorial-test--make-dict-state))
          published
          condition-data)
      (nskk-tutorial-test--call-with-dict-state
       state
       (lambda ()
         (with-temp-buffer
           (setq condition-data
                 (cl-letf
                     (((symbol-function
                        'nskk-tutorial--copy-object-graph)
                       (lambda (&rest _)
                         (signal condition '(copy-payload))))
                      ((symbol-function
                        'nskk-tutorial--publish-dict-state)
                       (lambda (&rest _)
                         (setq published t))))
                   (condition-case data
                       (progn
                         (nskk-tutorial--save-dict-state)
                         nil)
                     ((error quit) data))))
           (should
            (equal condition-data (list condition 'copy-payload)))
           (should-not published)
           (nskk-tutorial-test--should-current-state-eq state)
           (should (eq nskk--input-initialized 'input-before))
           (should (eq nskk--state-prolog-initialized
                       'state-prolog-before))
           (should (eq nskk--henkan-initialized 'henkan-before))
           (should (eq nskk--kana-initialized 'kana-before))
           (should (eq nskk--converter-initialized
                       'converter-before))
           (should (eq nskk--candidate-key-facts-initialized
                       'candidate-key-before))
           (should (eq nskk--annotation-initialized
                       'annotation-before))
           (should (eq nskk--dict-save-inhibited 'dict-before))
           (should (eq nskk--persistence-inhibited
                       'persistence-before))
           (should-not nskk-tutorial--dict-state-saved-p)
           (let ((saved (nskk-tutorial-test--saved-dict-state)))
             (dotimes (index (length saved))
               (should-not (aref saved index))))))))))

(ert-deftest nskk-tutorial-test/save-publication-failure-rolls-back ()
  "Publication ERROR or QUIT must restore exact state and clear stale diagnostics."
  (dolist (condition '(nskk-tutorial-test-injected-error quit))
    (let ((state (nskk-tutorial-test--make-dict-state))
          (publish-function
           (symbol-function 'nskk-tutorial--publish-dict-state))
          (publish-calls 0)
          condition-data)
      (nskk-tutorial-test--call-with-dict-state
       state
       (lambda ()
         (with-temp-buffer
           (setq nskk-tutorial--dict-rollback-diagnostic
                 '(:primary stale-primary :rollback stale-rollback)
                 condition-data
                 (cl-letf
                     (((symbol-function
                        'nskk-tutorial--publish-dict-state)
                       (lambda (published-state init-flags
                                dict-save-inhibited persistence-inhibited)
                         (setq publish-calls (1+ publish-calls))
                         (if (= publish-calls 1)
                             (progn
                               (setq nskk--prolog-database
                                     (make-hash-table :test 'equal)
                                     nskk--input-initialized
                                     'partially-published)
                               (signal condition '(publish-payload)))
                           (funcall publish-function
                                    published-state init-flags
                                    dict-save-inhibited
                                    persistence-inhibited)))))
                   (condition-case data
                       (progn
                         (nskk-tutorial--save-dict-state)
                         nil)
                     ((error quit) data))))
           (should (= publish-calls 2))
           (should
            (equal condition-data (list condition 'publish-payload)))
           (nskk-tutorial-test--should-current-state-eq state)
           (should-not nskk-tutorial--dict-state-saved-p)
           (should-not nskk-tutorial--dict-rollback-diagnostic)
           (let ((saved (nskk-tutorial-test--saved-dict-state)))
             (dotimes (index (length saved))
               (should-not (aref saved index))))
           (nskk-tutorial--save-dict-state)
           (should nskk-tutorial--dict-state-saved-p)
           (nskk-tutorial--restore-dict-state)
           (nskk-tutorial-test--should-current-state-eq state)
           (should-not nskk-tutorial--dict-state-saved-p)
           (should-not nskk-tutorial--dict-rollback-diagnostic)))))))

  (ert-deftest nskk-tutorial-test/save-publication-rollback-failure-keeps-snapshot ()
  "Primary condition survives rollback and warning ERROR or QUIT."
  (dolist (rollback-condition '(nskk-tutorial-test-restore-error quit))
    (dolist (warning-condition '(nskk-tutorial-test-restore-error quit))
      (let* ((state (nskk-tutorial-test--make-dict-state))
             (primary-condition
              '(nskk-tutorial-test-injected-error publish-payload))
             (rollback-data
              (list rollback-condition 'rollback-payload))
             (publish-calls 0)
             warnings
             condition-data)
        (nskk-tutorial-test--call-with-dict-state
         state
         (lambda ()
           (with-temp-buffer
             (setq condition-data
                   (cl-letf
                       (((symbol-function
                          'nskk-tutorial--publish-dict-state)
                         (lambda (&rest _)
                           (setq publish-calls (1+ publish-calls)
                                 nskk--prolog-database
                                 (make-hash-table :test 'equal)
                                 nskk--input-initialized
                                 'partially-published)
                           (if (= publish-calls 1)
                               (signal
                                'nskk-tutorial-test-injected-error
                                '(publish-payload))
                             (signal rollback-condition
                                     '(rollback-payload)))))
                        ((symbol-function 'display-warning)
                         (lambda (category message
                                  &optional level buffer-name)
                           (push (list category message level buffer-name)
                                 warnings)
                           (signal warning-condition
                                   '(warning-payload)))))
                     (condition-case data
                         (progn
                           (nskk-tutorial--save-dict-state)
                           nil)
                       ((error quit) data))))
             (should (= publish-calls 2))
             (should (equal condition-data primary-condition))
             (should
              (equal nskk-tutorial--dict-rollback-diagnostic
                     (list :primary primary-condition
                           :rollback rollback-data)))
             (should (= (length warnings) 1))
             (let ((warning (car warnings)))
               (should (eq (nth 0 warning) 'nskk-tutorial))
               (should (eq (nth 2 warning) :error))
               (should
                (string-match-p
                 (regexp-quote (format "%S" primary-condition))
                 (nth 1 warning)))
               (should
                (string-match-p
                 (regexp-quote (format "%S" rollback-data))
                 (nth 1 warning))))
             (should nskk-tutorial--dict-state-saved-p)
             (nskk-tutorial-test--should-saved-state-eq state)
             (let ((restore-condition-data
                    (cl-letf
                        (((symbol-function
                           'nskk-tutorial--publish-dict-state)
                          (lambda (&rest _)
                            (setq nskk--prolog-database
                                  (make-hash-table :test 'equal))
                            (signal warning-condition
                                    '(restore-payload)))))
                      (condition-case data
                          (progn
                            (nskk-tutorial--restore-dict-state)
                            nil)
                        ((error quit) data)))))
               (should
                (equal restore-condition-data
                       (list warning-condition 'restore-payload))))
             (should nskk-tutorial--dict-state-saved-p)
             (nskk-tutorial-test--should-saved-state-eq state)
             (should
              (equal nskk-tutorial--dict-rollback-diagnostic
                     (list :primary primary-condition
                           :rollback rollback-data)))
             (nskk-tutorial--restore-dict-state)
             (nskk-tutorial-test--should-current-state-eq state)
             (should-not nskk-tutorial--dict-state-saved-p)
             (should-not
              nskk-tutorial--dict-rollback-diagnostic))))))))

  (ert-deftest nskk-tutorial-test/restore-failure-retains-retryable-snapshot ()
    "Restore ERROR or QUIT must retain exact roots until a retry succeeds."
    (dolist (condition '(nskk-tutorial-test-restore-error quit))
      (let ((state (nskk-tutorial-test--make-dict-state))
            condition-data)
        (nskk-tutorial-test--call-with-dict-state
         state
         (lambda ()
           (with-temp-buffer
             (nskk-tutorial--save-dict-state)
             (setq condition-data
                   (cl-letf
                       (((symbol-function
                          'nskk-tutorial--publish-dict-state)
                         (lambda (&rest _)
                           (setq nskk--prolog-database
                                 (make-hash-table :test 'equal))
                           (signal condition '(restore-payload)))))
                     (condition-case data
                         (progn
                           (nskk-tutorial--restore-dict-state)
                           nil)
                       ((error quit) data))))
             (should
              (equal condition-data (list condition 'restore-payload)))
             (should nskk-tutorial--dict-state-saved-p)
             (nskk-tutorial-test--should-saved-state-eq state)
             (should
              (equal
               nskk-tutorial--saved-init-flags
               '((nskk--input-initialized . input-before)
                 (nskk--state-prolog-initialized . state-prolog-before)
                 (nskk--henkan-initialized . henkan-before)
                 (nskk--kana-initialized . kana-before)
                 (nskk--converter-initialized . converter-before)
                 (nskk--candidate-key-facts-initialized
                  . candidate-key-before)
                 (nskk--annotation-initialized . annotation-before))))
             (should
              (eq nskk-tutorial--saved-dict-save-inhibited
                  'dict-before))
             (should
              (eq nskk-tutorial--saved-persistence-inhibited
                  'persistence-before))
             (should nskk--dict-save-inhibited)
             (should nskk--persistence-inhibited)
             (nskk-tutorial--restore-dict-state)
             (nskk-tutorial-test--should-current-state-eq state)
             (should (eq nskk--input-initialized 'input-before))
             (should (eq nskk--state-prolog-initialized
                         'state-prolog-before))
             (should (eq nskk--henkan-initialized 'henkan-before))
             (should (eq nskk--kana-initialized 'kana-before))
             (should (eq nskk--converter-initialized 'converter-before))
             (should (eq nskk--candidate-key-facts-initialized
                         'candidate-key-before))
             (should (eq nskk--annotation-initialized
                         'annotation-before))
             (should (eq nskk--dict-save-inhibited 'dict-before))
             (should (eq nskk--persistence-inhibited
                         'persistence-before))
             (should-not nskk-tutorial--dict-state-saved-p)))))))


  (ert-deftest nskk-tutorial-test/startup-quit-restores-state-and-buffer ()
    "Startup QUIT must preserve its payload and roll back tutorial state."
    (let ((nskk-tutorial--owner nil)
          (nskk--active-buffers nil)
          (nskk--dict-save-inhibited nil)
          (nskk--persistence-inhibited 'previous)
          condition-data)
      (unwind-protect
          (cl-letf (((symbol-function 'nskk-mode)
                     (lambda (&rest _)
                       (signal 'quit '(startup-payload)))))
            (setq condition-data
                  (condition-case data
                      (progn (nskk-tutorial) nil)
                    (quit data))))
        (when-let* ((buffer (get-buffer nskk-tutorial--buffer-name)))
          (kill-buffer buffer)))
      (should (equal condition-data '(quit startup-payload)))
      (should-not (get-buffer nskk-tutorial--buffer-name))
      (should-not nskk-tutorial--owner)
      (should-not nskk--dict-save-inhibited)
      (should (eq nskk--persistence-inhibited 'previous))))

  (ert-deftest nskk-tutorial-test/startup-restore-failure-preserves-retry-state ()
    "Restore ERROR or QUIT must retain the retryable tutorial transaction."
    (dolist (restore-condition '(nskk-tutorial-test-restore-error quit))
      (let ((nskk-tutorial--owner nil)
            (nskk--active-buffers nil)
            (nskk--dict-save-inhibited nil)
            (nskk--persistence-inhibited 'previous)
            (install-function
             (symbol-function 'nskk-tutorial--install-mini-dict))
            installed
            restore-calls
            condition-data)
        (unwind-protect
            (progn
              (cl-letf (((symbol-function 'nskk-tutorial--install-mini-dict)
                         (lambda ()
                           (setq installed t)
                           (funcall install-function)))
                        ((symbol-function 'nskk-mode)
                         (lambda (&rest _)
                           (signal 'nskk-tutorial-test-injected-error
                                   '(original-payload))))
                        ((symbol-function 'nskk-tutorial--restore-dict-state)
                         (lambda ()
                           (setq restore-calls (1+ (or restore-calls 0)))
                           (signal restore-condition '(restore-payload)))))
                (setq condition-data
                      (condition-case data
                          (progn (nskk-tutorial) nil)
                        (nskk-tutorial-test-injected-error data))))
              (let ((buffer (get-buffer nskk-tutorial--buffer-name)))
                (should installed)
                (should (= restore-calls 1))
                (should (equal condition-data
                               '(nskk-tutorial-test-injected-error
                                 original-payload)))
                (should (buffer-live-p buffer))
                (should (eq nskk-tutorial--owner buffer))
                (with-current-buffer buffer
                  (should nskk-tutorial--dict-state-saved-p)
                  (should nskk-tutorial--owns-transaction)
                  (should (memq #'nskk-tutorial--on-kill
                                kill-buffer-hook))
                  (should (memq #'nskk-tutorial--on-major-mode-change
                                change-major-mode-hook)))
                (should nskk--dict-save-inhibited)
                (should nskk--persistence-inhibited)
                (kill-buffer buffer)
                (should-not (buffer-live-p buffer))
                (should-not nskk-tutorial--owner)
                (should-not nskk--dict-save-inhibited)
                (should (eq nskk--persistence-inhibited 'previous))))
          (when-let* ((buffer (get-buffer nskk-tutorial--buffer-name)))
            (condition-case nil
                (kill-buffer buffer)
              ((error quit) nil)))))))

  ;;; nskk-tutorial-test.el ends here
