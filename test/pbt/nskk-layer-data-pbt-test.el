;;; nskk-layer-data-pbt-test.el --- Property-Based Tests for Data Access Layer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

;; Author: NSKK Contributors
;; Maintainer: takeokunn <bararararatty@gmail.com>
;; URL: https://github.com/takeokunn/nskk.el
;; Keywords: i18n, testing, property-based

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Property-Based Tests for the Data Access layer.
;;
;; This file tests data access layer properties including trie operations,
;; cache behavior, and dictionary search using property-based testing
;; techniques to ensure correctness across a wide range of inputs.
;;
;; Test Categories:
;; - Trie Properties: Insert, lookup, prefix search, and delete operations
;; - Cache Properties: Hit/miss behavior, LRU eviction, and size bounds
;; - Search Properties: Exact, prefix, and fuzzy search behaviors

;;; Code:

(require 'ert)
(require 'nskk-test-framework)
(require 'nskk-test-macros)
(require 'nskk-pbt-generators)
(require 'nskk-trie)
(require 'nskk-cache)
(require 'nskk-search)

;;;
;;; Register Additional Generators for Data Layer Tests
;;;

;; These generators use Emacs' built-in (random) which is seeded by the
;; nskk-property-test-seeded macro. We always use (random LIMIT) with
;; a positive limit to ensure non-negative values.
;;
;; IMPORTANT: The nskk-property-test-seeded macro uses (random seed) to
;; initialize the RNG, which fails with negative seeds on some Emacs versions.
;; We define a helper function and use it in the SEED argument.

(defun nskk-pbt--positive-seed ()
  "Return a guaranteed positive random seed."
  (let ((s (random)))
    (if (< s 0) (- s) (1+ s))))

;; Initialize random state with a positive seed
(random (random 1000000))

;; Generator for cache keys (strings)
(nskk-register-generator 'pbt-cache-key
  (lambda ()
    (format "key-%d-%d" (random 1000) (random 1000))))

;; Generator for cache values (various types)
(nskk-register-generator 'pbt-cache-value
  (lambda ()
    (let ((type (random 4)))
      (pcase type
        (0 (format "value-%d" (random 100)))
        (1 (random 1000))
        (2 (cl-loop for i from 1 to (1+ (random 5))
                    collect (format "item-%d" i)))
        (3 (list :type 'entry :data (format "data-%d" (random 100))))))))

;; Generator for trie keys (hiragana strings)
(nskk-register-generator 'pbt-trie-key
  (lambda ()
    (let* ((hiragana '("あ" "い" "う" "え" "お"
                       "か" "き" "く" "け" "こ"
                       "さ" "し" "す" "せ" "そ"
                       "た" "ち" "つ" "て" "と"
                       "な" "に" "ぬ" "ね" "の"
                       "は" "ひ" "ふ" "へ" "ほ"
                       "ま" "み" "む" "め" "も"
                       "や" "ゆ" "よ"
                       "ら" "り" "る" "れ" "ろ"
                       "わ" "を" "ん"))
           (len (1+ (random 5))))  ; 1-6 characters
      (mapconcat #'identity
                 (cl-loop repeat len
                          collect (nth (random (length hiragana)) hiragana))
                 ""))))

;; Generator for trie values
(nskk-register-generator 'pbt-trie-value
  (lambda ()
    (let ((type (random 3)))
      (pcase type
        (0 (format "entry-%d" (random 100)))
        (1 (list :candidates (cl-loop for i from 1 to (1+ (random 3))
                                      collect (format "候補%d" i))))
        (2 (list '("漢字" "文字" "変換")))))))

;; Generator for positive integers (cache capacity, etc.)
(nskk-register-generator 'pbt-positive-int
  (lambda ()
    (+ 2 (random 18))))  ; Range: 2 to 19

;; Generator for small positive integers
(nskk-register-generator 'pbt-small-int
  (lambda ()
    (1+ (random 9))))  ; Range: 1 to 9

;; Generator for cache types
(nskk-register-generator 'pbt-cache-type
  (lambda ()
    (nth (random 2) '(lru lfu))))

;;;
;;; Helper Functions
;;;

(defun nskk-pbt--trie-contains-p (trie key)
  "Check if TRIE contains KEY."
  (nskk-trie-has-key-p trie key))

(defun nskk-pbt--make-test-trie (entries)
  "Create a test trie with ENTRIES (list of (key . value) pairs)."
  (let ((trie (nskk-trie-create)))
    (dolist (entry entries)
      (nskk-trie-insert trie (car entry) (cdr entry)))
    trie))

(defun nskk-pbt--cache-size-valid-p (cache)
  "Check if CACHE size is within bounds."
  (let ((stats (nskk-cache-stats cache)))
    (and (<= (plist-get stats :size) (plist-get stats :capacity))
         (>= (plist-get stats :size) 0))))

;;;
;;; Trie Properties
;;;

;;;### Trie Property 1: Insert-Lookup
(nskk-property-test-seeded trie-insert-lookup
  ((key pbt-trie-key)
   (value pbt-trie-value))
  (let ((trie (nskk-trie-create)))
    (nskk-trie-insert trie key value)
    (let ((result (nskk-trie-lookup trie key)))
      (and (consp result)
           (cdr result)  ; found flag is t
           (equal (car result) value))))
  50 (nskk-pbt--positive-seed))

;;;### Trie Property 2: Prefix Search
(nskk-property-test-seeded trie-prefix-search
  ((prefix pbt-trie-key)
   (suffix1 pbt-trie-key)
   (suffix2 pbt-trie-key))
  (let* ((trie (nskk-trie-create))
         (key1 (concat prefix suffix1))
         (key2 (concat prefix suffix2))
         ;; Ensure keys are different and have the prefix
         (key1 (if (string= key1 prefix) (concat key1 "あ") key1))
         (key2 (if (string= key2 prefix) (concat key2 "い") key2))
         ;; Ensure key1 and key2 are different from each other
         (key2 (if (string= key1 key2) (concat key2 "う") key2)))
    (nskk-trie-insert trie key1 "value1")
    (nskk-trie-insert trie key2 "value2")
    (let ((results (nskk-trie-prefix-search trie prefix)))
      (and (listp results)
           (>= (length results) 2)
           ;; All results should start with prefix
           (cl-every (lambda (r) (string-prefix-p prefix (car r))) results))))
  50 (nskk-pbt--positive-seed))

;;;### Trie Property 3: Delete Removes
(nskk-property-test-seeded trie-delete-removes
  ((key pbt-trie-key)
   (value pbt-trie-value))
  (let ((trie (nskk-trie-create)))
    (nskk-trie-insert trie key value)
    (and (nskk-trie-has-key-p trie key)
         (progn
           (nskk-trie-delete trie key)
           (not (nskk-trie-has-key-p trie key)))))
  50 (nskk-pbt--positive-seed))

;;;### Trie Property 4: Insert Is Idempotent
(nskk-property-test-seeded trie-insert-idempotent
  ((key pbt-trie-key)
   (value pbt-trie-value))
  (let ((trie (nskk-trie-create)))
    (nskk-trie-insert trie key value)
    (let ((size-after-first (nskk-trie-size trie)))
      (nskk-trie-insert trie key value)
      (nskk-trie-insert trie key value)
      (and (= (nskk-trie-size trie) size-after-first)
           (equal (nskk-trie-lookup-values trie key) value))))
  50 (nskk-pbt--positive-seed))

;;;### Trie Property 5: Multiple Keys
(nskk-property-test-seeded trie-multiple-keys
  ((key1 pbt-trie-key)
   (key2 pbt-trie-key)
   (val1 pbt-trie-value)
   (val2 pbt-trie-value))
  ;; Ensure keys are different
  (let* ((key2 (if (string= key1 key2) (concat key2 "あ") key2))
         (trie (nskk-trie-create)))
    (nskk-trie-insert trie key1 val1)
    (nskk-trie-insert trie key2 val2)
    (and (equal (nskk-trie-lookup-values trie key1) val1)
         (equal (nskk-trie-lookup-values trie key2) val2)))
  50 (nskk-pbt--positive-seed))

;;;
;;; Cache Properties
;;;

;;;### Cache Property 1: Hit/Miss Behavior
(nskk-property-test-seeded cache-hit-miss
  ((key pbt-cache-key)
   (value pbt-cache-value)
   (cache-type pbt-cache-type))
  (let ((cache (nskk-cache-create cache-type 100)))
    (and (null (nskk-cache-get cache key))  ; Miss on non-existent key
         (progn
           (nskk-cache-put cache key value)  ; Put the value
           (equal (nskk-cache-get cache key) value))))  ; Hit on existing key
  50 (nskk-pbt--positive-seed))

;;;### Cache Property 2: LRU Eviction
(nskk-property-test-seeded cache-eviction-lru
  ((key1 pbt-cache-key)
   (key2 pbt-cache-key)
   (key3 pbt-cache-key)
   (val1 pbt-cache-value)
   (val2 pbt-cache-value)
   (val3 pbt-cache-value))
  ;; Ensure all keys are different
  (let* ((key2 (if (string= key1 key2) (concat key2 "-x") key2))
         (key3 (if (or (string= key1 key3) (string= key2 key3)) (concat key3 "-y") key3))
         (cache (nskk-cache-create 'lru 2)))
    ;; Insert 2 items (at capacity)
    (nskk-cache-put cache key1 val1)
    (nskk-cache-put cache key2 val2)
    (and (= (nskk-cache-size cache) 2)
         ;; Insert 3rd item - should evict key1 (oldest)
         (progn
           (nskk-cache-put cache key3 val3)
           (and (= (nskk-cache-size cache) 2)
                (null (nskk-cache-get cache key1))  ; key1 evicted
                (nskk-cache-get cache key2)          ; key2 still there
                (nskk-cache-get cache key3)))))      ; key3 still there
  50 (nskk-pbt--positive-seed))

;;;### Cache Property 3: Size Bound
(nskk-property-test-seeded cache-size-bound
  ((capacity pbt-small-int))
  (let ((cache (nskk-cache-create 'lru capacity)))
    ;; Insert more items than capacity
    (cl-loop for i from 1 to (* capacity 2)
             do (nskk-cache-put cache (format "key-%d" i) (format "value-%d" i)))
    (<= (nskk-cache-size cache) capacity))
  50 (nskk-pbt--positive-seed))

;;;### Cache Property 4: Clear Empties
(nskk-property-test-seeded cache-clear-empties
  ((key pbt-cache-key)
   (value pbt-cache-value)
   (cache-type pbt-cache-type))
  (let ((cache (nskk-cache-create cache-type 100)))
    (nskk-cache-put cache key value)
    (and (> (nskk-cache-size cache) 0)
         (progn
           (nskk-cache-clear cache)
           (and (= (nskk-cache-size cache) 0)
                (null (nskk-cache-get cache key))))))
  50 (nskk-pbt--positive-seed))

;;;### Cache Property 5: Invalidate Removes
(nskk-property-test-seeded cache-invalidate-removes
  ((key pbt-cache-key)
   (value pbt-cache-value)
   (cache-type pbt-cache-type))
  (let ((cache (nskk-cache-create cache-type 100)))
    (nskk-cache-put cache key value)
    (and (nskk-cache-get cache key)
         (progn
           (nskk-cache-invalidate cache key)
           (null (nskk-cache-get cache key)))))
  50 (nskk-pbt--positive-seed))

;;;### Cache Property 6: Stats Consistency
(nskk-property-test-seeded cache-stats-consistency
  ((key pbt-cache-key)
   (value pbt-cache-value)
   (cache-type pbt-cache-type))
  (let ((cache (nskk-cache-create cache-type 100)))
    ;; Initial stats check and put/get operations combined
    (let ((stats1 (nskk-cache-stats cache)))
      (and (<= (plist-get stats1 :size) (plist-get stats1 :capacity))
           (= (plist-get stats1 :hits) 0)
           (= (plist-get stats1 :misses) 0)
           ;; Put and get
           (progn
             (nskk-cache-put cache key value)
             (nskk-cache-get cache key)   ; hit
             (nskk-cache-get cache "nonexistent")  ; miss
             (let ((stats2 (nskk-cache-stats cache)))
               (and (<= (plist-get stats2 :size) (plist-get stats2 :capacity))
                    (>= (plist-get stats2 :hits) 1)
                    (>= (plist-get stats2 :misses) 1)
                    (<= (plist-get stats2 :hit-rate) 1.0)
                    (>= (plist-get stats2 :hit-rate) 0.0)))))))
  50 (nskk-pbt--positive-seed))

;;;
;;; Search Properties
;;;

;;; Note: For search tests, we create simple mock indexes or use trie directly
;;; since nskk-dict-index requires additional dependencies.

;;;### Search Property 1: Trie Exact Search
(nskk-property-test-seeded search-exact-finds
  ((key pbt-trie-key)
   (value pbt-trie-value))
  (let ((trie (nskk-trie-create)))
    (nskk-trie-insert trie key value)
    (let ((result (nskk-trie-lookup trie key)))
      (and result
           (cdr result)  ; found
           (equal (car result) value))))
  50 (nskk-pbt--positive-seed))

;;;### Search Property 2: Trie Prefix Search Finds All Prefixes
(nskk-property-test-seeded search-prefix-finds-prefixes
  ((base pbt-trie-key)
   (suffixes (key-sequence 3)))
  (let* ((trie (nskk-trie-create))
         (prefix-keys nil))
    ;; Build keys with common prefix
    (dolist (suffix suffixes)
      (let ((key (concat base (substring (format "%s" suffix) 0 1))))
        (unless (member key prefix-keys)
          (push key prefix-keys)
          (nskk-trie-insert trie key (format "val-%s" key)))))
    ;; Also insert the base if not empty
    (when (and (> (length base) 0)
               (not (member base prefix-keys)))
      (nskk-trie-insert trie base "base-value"))
    ;; Search with prefix
    (let ((results (nskk-trie-prefix-search trie base)))
      (and (listp results)
           ;; All results start with prefix
           (cl-every (lambda (r) (string-prefix-p base (car r))) results))))
  50 (nskk-pbt--positive-seed))

;;;### Search Property 3: Search Returns List
(nskk-property-test-seeded search-returns-list
  ((prefix pbt-trie-key))
  (let ((trie (nskk-trie-create)))
    (and (listp (nskk-trie-prefix-search trie prefix))  ; Empty trie
         ;; With some data
         (progn
           (nskk-trie-insert trie "あいうえお" "test1")
           (listp (nskk-trie-prefix-search trie prefix)))))
  50 (nskk-pbt--positive-seed))

;;;### Search Property 4: Fuzzy Distance (Levenshtein)
(nskk-property-test-seeded search-fuzzy-distance
  ((s1 pbt-trie-key)
   (s2 pbt-trie-key))
  (let ((dist (nskk-search--levenshtein-distance s1 s2)))
    ;; Distance is always non-negative
    (and (integerp dist)
         (>= dist 0)
         ;; Distance of string to itself is 0
         (= (nskk-search--levenshtein-distance s1 s1) 0)
         ;; Distance is symmetric
         (= dist (nskk-search--levenshtein-distance s2 s1))
         ;; Distance is at most the length of the longer string
         (<= dist (max (length s1) (length s2)))))
  50 (nskk-pbt--positive-seed))

;;;### Search Property 5: Empty Trie Search
(nskk-property-test-seeded search-empty-trie
  ((query pbt-trie-key))
  (let ((trie (nskk-trie-create)))
    (and (null (nskk-trie-lookup-values trie query))
         (listp (nskk-trie-prefix-search trie query))
         (= (length (nskk-trie-prefix-search trie query)) 0)))
  50 (nskk-pbt--positive-seed))

;;;### Search Property 6: Trie Serialization Roundtrip
(nskk-property-test-seeded trie-serialize-roundtrip
  ((key1 pbt-trie-key)
   (key2 pbt-trie-key)
   (val1 pbt-trie-value)
   (val2 pbt-trie-value))
  (let* ((key2 (if (string= key1 key2) (concat key2 "x") key2))
         (trie1 (nskk-trie-create)))
    (nskk-trie-insert trie1 key1 val1)
    (nskk-trie-insert trie1 key2 val2)
    (let* ((data (nskk-trie-serialize trie1))
           (trie2 (nskk-trie-deserialize data)))
      (and (equal (nskk-trie-lookup-values trie2 key1) val1)
           (equal (nskk-trie-lookup-values trie2 key2) val2)
           (= (nskk-trie-size trie1) (nskk-trie-size trie2)))))
  50 (nskk-pbt--positive-seed))

;;;
;;; Additional Cache Invariant Properties
;;;

;;;### Cache Property 7: Update Preserves Count
(nskk-property-test-seeded cache-update-preserves-count
  ((key pbt-cache-key)
   (val1 pbt-cache-value)
   (val2 pbt-cache-value)
   (cache-type pbt-cache-type))
  (let ((cache (nskk-cache-create cache-type 100)))
    (nskk-cache-put cache key val1)
    (let ((size-after-first (nskk-cache-size cache)))
      (nskk-cache-put cache key val2)  ; Update existing key
      (and (= (nskk-cache-size cache) size-after-first)
           (equal (nskk-cache-get cache key) val2))))
  50 (nskk-pbt--positive-seed))

;;;### Cache Property 8: Hit Rate Calculation
(nskk-property-test-seeded cache-hit-rate-calculation
  ((key pbt-cache-key)
   (value pbt-cache-value)
   (cache-type pbt-cache-type))
  (let ((cache (nskk-cache-create cache-type 100)))
    ;; Miss first
    (nskk-cache-get cache key)
    ;; Put and hit
    (nskk-cache-get cache key)  ; another miss (not yet put)
    (nskk-cache-put cache key value)
    (nskk-cache-get cache key)  ; hit
    (let ((stats (nskk-cache-stats cache)))
      (and (>= (plist-get stats :hits) 1)
           (>= (plist-get stats :misses) 1)
           (> (+ (plist-get stats :hits) (plist-get stats :misses)) 0)
           (<= (plist-get stats :hit-rate) 1.0)
           (>= (plist-get stats :hit-rate) 0.0))))
  50 (nskk-pbt--positive-seed))

;;;
;;; Trie Statistics Properties
;;;

;;;### Trie Property 6: Statistics Accuracy
(nskk-property-test-seeded trie-statistics-accuracy
  ((key1 pbt-trie-key)
   (key2 pbt-trie-key)
   (val1 pbt-trie-value)
   (val2 pbt-trie-value))
  (let* ((key2 (if (string= key1 key2) (concat key2 "あ") key2))
         (trie (nskk-trie-create)))
    (nskk-trie-insert trie key1 val1)
    (nskk-trie-insert trie key2 val2)
    (let ((stats (nskk-trie-statistics trie)))
      (and (= (plist-get stats :size) 2)
           (>= (plist-get stats :node-count) 2)
           (> (plist-get stats :max-depth) 0)
           (>= (plist-get stats :avg-depth) 0.0))))
  50 (nskk-pbt--positive-seed))

;;;### Trie Property 7: Keys Function
(nskk-property-test-seeded trie-keys-function
  ((key1 pbt-trie-key)
   (key2 pbt-trie-key)
   (key3 pbt-trie-key))
  (let* ((key2 (if (string= key1 key2) (concat key2 "い") key2))
         (key3 (if (or (string= key1 key3) (string= key2 key3)) (concat key3 "う") key3))
         (trie (nskk-trie-create)))
    (nskk-trie-insert trie key1 "v1")
    (nskk-trie-insert trie key2 "v2")
    (nskk-trie-insert trie key3 "v3")
    (let ((keys (nskk-trie-keys trie)))
      (and (listp keys)
           (= (length keys) 3)
           (member key1 keys)
           (member key2 keys)
           (member key3 keys))))
  50 (nskk-pbt--positive-seed))

(provide 'nskk-layer-data-pbt-test)

;;; nskk-layer-data-pbt-test.el ends here
