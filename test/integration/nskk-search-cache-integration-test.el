;;; nskk-search-cache-integration-test.el --- Search+cache integration tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test

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

;; Search+cache integration tests.

;;; Code:

(require 'ert)
(require 'nskk-cache)
(require 'nskk-search)
(require 'nskk-dictionary)
(require 'nskk-state)
(require 'nskk-test-framework)
(require 'nskk-test-macros)


;;;
;;; Group 1: Cache Basic Operations
;;;

(nskk-describe "cache basic operations"

  (nskk-it "creating LRU cache with capacity 10 yields empty stats"
    (let ((cache (nskk-cache-create :type 'lru :capacity 10)))
      (nskk-then
        (should (nskk-cache-p cache))
        (should (= (nskk-cache-size cache) 0))
        (let ((stats (nskk-cache-stats cache)))
          (should (= (plist-get stats :hits) 0))
          (should (= (plist-get stats :misses) 0))
          (should (= (plist-get stats :size) 0))
          (should (= (plist-get stats :capacity) 10))
          (should (eq (plist-get stats :type) 'lru))))))

  (nskk-it "put and get records a hit and a miss correctly"
    (let ((cache (nskk-cache-create :type 'lru :capacity 10)))
      (nskk-given (nskk-cache-put cache "test" '("a" "b")))
      (nskk-then
        (nskk-should-equal '("a" "b") (nskk-cache-get cache "test"))
        (should (null (nskk-cache-get cache "nonexistent")))
        (let ((stats (nskk-cache-stats cache)))
          (should (= (plist-get stats :hits) 1))
          (should (= (plist-get stats :misses) 1))))))

  (nskk-it "hit-rate is 0.75 after 3 hits and 1 miss"
    (let ((cache (nskk-cache-create :type 'lru :capacity 10)))
      (nskk-given
        (nskk-cache-put cache "k1" "v1")
        (nskk-cache-put cache "k2" "v2")
        (nskk-cache-put cache "k3" "v3"))
      (nskk-when
        (nskk-cache-get cache "k1")
        (nskk-cache-get cache "k2")
        (nskk-cache-get cache "k3")
        (nskk-cache-get cache "absent"))
      (nskk-then
        (nskk-assert-approx-equal (nskk-cache-hit-rate cache) 0.75 0.001))))

  (nskk-it "LRU cache with capacity 2 evicts the least-recently-used entry"
    (let ((cache (nskk-cache-create :type 'lru :capacity 2)))
      (nskk-given
        (nskk-cache-put cache "key1" "val1")
        (nskk-cache-put cache "key2" "val2"))
      (nskk-when
        (nskk-cache-put cache "key3" "val3"))
      (nskk-then
        (should (null (nskk-cache-get cache "key1")))
        (nskk-should-equal "val2" (nskk-cache-get cache "key2"))
        (nskk-should-equal "val3" (nskk-cache-get cache "key3")))))

  (nskk-it "invalidating a specific key removes it and decrements size"
    (let ((cache (nskk-cache-create :type 'lru :capacity 10)))
      (nskk-given
        (nskk-cache-put cache "alpha" '("x"))
        (nskk-cache-put cache "beta" '("y")))
      (nskk-when
        (should (eq (nskk-cache-invalidate cache "alpha") t)))
      (nskk-then
        (should (= (nskk-cache-size cache) 1))
        (should (null (nskk-cache-get cache "alpha")))
        (nskk-should-equal '("y") (nskk-cache-get cache "beta")))))

  (nskk-it "pattern invalidation removes matching keys and leaves others intact"
    (nskk-prolog-test-with-isolated-db
      (let ((cache (nskk-cache-create :type 'lru :capacity 20)))
        (nskk-given
          (nskk-cache-put cache "かんじ:exact:nil" '("漢字"))
          (nskk-cache-put cache "かんじ:prefix:nil" '("漢字" "感じ"))
          (nskk-cache-put cache "さくら:exact:nil" '("桜")))
        (nskk-when
          (let ((deleted (nskk-cache-invalidate-pattern cache "^かんじ:")))
            (should (= (length deleted) 2))))
        (nskk-then
          (should (= (nskk-cache-size cache) 1))
          (should (null (nskk-cache-get cache "かんじ:exact:nil")))
          (should (null (nskk-cache-get cache "かんじ:prefix:nil")))
          (nskk-should-equal '("桜") (nskk-cache-get cache "さくら:exact:nil"))))))

  (nskk-it "clearing a cache removes all entries and resets size to zero"
    (let ((cache (nskk-cache-create :type 'lru :capacity 10)))
      (nskk-given
        (nskk-cache-put cache "a" 1)
        (nskk-cache-put cache "b" 2)
        (nskk-cache-put cache "c" 3))
      (nskk-then (should (= (nskk-cache-size cache) 3)))
      (nskk-when (nskk-cache-clear cache))
      (nskk-then (should (= (nskk-cache-size cache) 0))))))


;;;
;;; Group 2: Search with Mock Dictionary
;;;

(nskk-describe "search with mock dictionary"

  (nskk-it "exact match search returns expected candidates from mock dictionary"
    (nskk-with-mock-dict nil
      (let ((entry (nskk-search-exact (nskk-dict-system-index) "かんじ" nil)))
        (nskk-then
          (should (nskk-dict-entry-p entry))
          (let ((candidates (nskk-dict-entry-candidates entry)))
            (should (listp candidates))
            (should candidates)
            (should (member "漢字" candidates)))))))

  (nskk-it "exact match search returns nil for a key not in the dictionary"
    (nskk-with-mock-dict nil
      (let ((entry (nskk-search-exact (nskk-dict-system-index) "ほげほげほげ" nil)))
        (nskk-then (should (null entry))))))

  (nskk-it "first search is a cache miss; second is a cache hit"
    (nskk-with-mock-dict nil
      (let* ((cache (nskk-cache-create :type 'lru :capacity 100))
             (result1 (nskk-search-with-cache cache (nskk-dict-system-index) "かんじ")))
        (nskk-then
          (should (nskk-dict-entry-p result1))
          (let ((stats (nskk-cache-stats cache)))
            (should (= (plist-get stats :size) 1))
            (should (= (plist-get stats :misses) 1))
            (should (= (plist-get stats :hits) 0))))
        (let ((result2 (nskk-search-with-cache cache (nskk-dict-system-index) "かんじ")))
          (nskk-then
            (nskk-should-equal result1 result2)
            (let ((stats (nskk-cache-stats cache)))
              (should (= (plist-get stats :size) 1))
              (should (= (plist-get stats :misses) 1))
              (should (= (plist-get stats :hits) 1))))))))

  (nskk-it "search-with-cache returns nil for a missing key and does not cache nil"
    (nskk-with-mock-dict nil
      (let* ((cache (nskk-cache-create :type 'lru :capacity 100))
             (result (nskk-search-with-cache cache (nskk-dict-system-index) "ほげほげほげ")))
        (nskk-then
          (should (null result))
          (should (= (nskk-cache-size cache) 0))))))

  (nskk-it "independent keys are cached separately without interference"
    (nskk-with-mock-dict nil
      (let ((cache (nskk-cache-create :type 'lru :capacity 100)))
        (let ((e1 (nskk-search-with-cache cache (nskk-dict-system-index) "かんじ"))
              (e2 (nskk-search-with-cache cache (nskk-dict-system-index) "さくら")))
          (nskk-then
            (should (nskk-dict-entry-p e1))
            (should (nskk-dict-entry-p e2))
            (should (= (nskk-cache-size cache) 2))
            (should (member "漢字" (nskk-dict-entry-candidates e1)))
            (should (member "桜" (nskk-dict-entry-candidates e2)))))))))


;;;
;;; Group 3: Search Learning
;;;

(nskk-describe "search learning"
  (nskk-it "increments frequency count"
    (nskk-prolog-test-with-isolated-db
      (nskk-given
        (should (= 0 (nskk--search-candidate-score "よみ" "候補"))))
      (nskk-when
        (nskk-search-learn "よみ" "候補"))
      (nskk-then
        (should (= 1 (nskk--search-candidate-score "よみ" "候補"))))
      (nskk-when
        (nskk-search-learn "よみ" "候補"))
      (nskk-then
        (should (= 2 (nskk--search-candidate-score "よみ" "候補"))))))

  (nskk-it "score affects result ordering"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'learning-score 3)
      (nskk-with-mock-dict '(("かんじ" "漢字")
                             ("かんたん" "簡単"))
        (let* ((nskk-search-sort-method 'frequency)
               (before
                (nskk-search-prefix
                 (nskk-dict-system-index) "かん" nil nil))
               (target (car (last before)))
               (reading (car target))
               (candidate
                (car (nskk-dict-entry-candidates (cdr target)))))
          (nskk-given
            (should (= (length before) 2))
            (should-not (equal reading (caar before))))
          (nskk-when
            (nskk-search-learn reading candidate))
          (nskk-then
            (let ((after
                   (nskk-search-prefix
                    (nskk-dict-system-index) "かん" nil nil)))
              (should (equal reading (caar after)))))))))

  (nskk-it "learning flushes cached prefix results and reapplies learned ordering"
    (nskk-prolog-test-with-isolated-db
      (nskk-with-mock-dict nil
        (let* ((nskk-search-sort-method 'frequency)
               (cache (nskk-cache-create :type 'lru :capacity 100))
               (before
                (nskk-search-with-cache
                 cache (nskk-dict-system-index) "に" 'prefix nil))
               (target (car (last before)))
               (reading (car target))
               (candidate
                (car (nskk-dict-entry-candidates (cdr target)))))
          (nskk-given
            (should (> (length before) 1))
            (should (= (nskk-cache-size cache) 1))
            (should-not (equal reading (caar before))))
          (nskk-when
            (nskk-search-learn reading candidate))
          (nskk-then
            (should (= (nskk-cache-size cache) 0))
            (let ((after
                   (nskk-search-with-cache
                    cache (nskk-dict-system-index) "に" 'prefix nil)))
              (should (equal reading (caar after)))
              (should (= (nskk-cache-size cache) 1)))))))))


;;;
;;; Group 4: Cache Type Validation
;;;

(nskk-describe "cache type validation"

  (nskk-it "LFU cache supports basic put/get and tracks stats by type"
    (let ((cache (nskk-cache-create :type 'lfu :capacity 100)))
      (nskk-given
        (should (nskk-cache-p cache))
        (should (= (nskk-cache-size cache) 0)))
      (nskk-when
        (nskk-cache-put cache "test-key" '("value1" "value2")))
      (nskk-then
        (nskk-should-equal '("value1" "value2") (nskk-cache-get cache "test-key"))
        (should (null (nskk-cache-get cache "absent-key")))
        (let ((stats (nskk-cache-stats cache)))
          (should (eq (plist-get stats :type) 'lfu))
          (should (= (plist-get stats :hits) 1))
          (should (= (plist-get stats :misses) 1))))))

  (nskk-it "LFU cache evicts the least-frequently-used entry on overflow"
    (let ((cache (nskk-cache-create :type 'lfu :capacity 2)))
      (nskk-given
        (nskk-cache-put cache "rare" "r")
        (nskk-cache-put cache "freq" "f"))
      (nskk-when
        (nskk-cache-get cache "freq")
        (nskk-cache-get cache "freq")
        (nskk-cache-put cache "new" "n"))
      (nskk-then
        (should (null (nskk-cache-get cache "rare")))
        (nskk-should-equal "f" (nskk-cache-get cache "freq"))
        (nskk-should-equal "n" (nskk-cache-get cache "new")))))

  (nskk-it "requesting an unknown cache type signals a user-error"
    (nskk-then
      (should-error
       (nskk-cache-create :type 'invalid-type)
       :type 'user-error)))

  (nskk-it "invalidating a non-existent key returns nil"
    (let ((cache (nskk-cache-create :type 'lru :capacity 10)))
      (nskk-then
        (should (null (nskk-cache-invalidate cache "no-such-key"))))))

  (nskk-it "creating a cache with no arguments uses default strategy and capacity"
    (let ((cache (nskk-cache-create)))
      (nskk-then
        (should (nskk-cache-p cache))
        (let ((stats (nskk-cache-stats cache)))
          (should (eq (plist-get stats :type) nskk-cache-strategy))
          (should (= (plist-get stats :capacity) nskk-cache-default-capacity))))))

  (nskk-it "keyword-argument creation respects :type and :capacity"
    (let ((cache (nskk-cache-create :type 'lfu :capacity 42)))
      (nskk-then
        (should (nskk-cache-p cache))
        (let ((stats (nskk-cache-stats cache)))
          (should (eq (plist-get stats :type) 'lfu))
          (should (= (plist-get stats :capacity) 42)))))))


;;;
;;; Group 5: Search Dispatcher Integration
;;;

(nskk-describe "search dispatcher integration"

  (nskk-it "nskk-search dispatches to exact-match search and returns an entry"
    (nskk-with-mock-dict nil
      (let ((result (nskk-search (nskk-dict-system-index) "かんじ" 'exact)))
        (nskk-then
          (should (nskk-dict-entry-p result))
          (should (member "漢字" (nskk-dict-entry-candidates result)))))))

  (nskk-it "nskk-search signals nskk-dict-search-invalid-query for unknown type"
    (nskk-with-mock-dict nil
      (nskk-then
        (should-error
         (nskk-search (nskk-dict-system-index) "かんじ" 'bogus-type)
         :type 'nskk-dict-search-invalid-query))))

  (nskk-it "nskk-search signals nskk-dict-search-invalid-index for a non-index argument"
    (nskk-then
      (should-error
       (nskk-search "not-an-index" "かんじ" 'exact)
       :type 'nskk-dict-search-invalid-index)))

  (nskk-it "nskk-search signals nskk-dict-search-invalid-query for an empty query"
    (nskk-with-mock-dict nil
      (nskk-then
        (should-error
         (nskk-search (nskk-dict-system-index) "" 'exact)
         :type 'nskk-dict-search-invalid-query))))

  (nskk-it "nskk-search-with-cache signals wrong-type-argument for a non-cache object"
    (nskk-with-mock-dict nil
      (nskk-then
        (should-error
         (nskk-search-with-cache "not-a-cache" (nskk-dict-system-index) "かんじ")
         :type 'wrong-type-argument)))))


;;;
;;; Group 6: Cache Invariants (PBT)
;;;

(nskk-describe "cache invariants (PBT)"

  (nskk-property-test cache-hit-rate-monotonically-non-decreasing
    ((key   search-query)
     (value candidate-list))
    (let* ((cache    (nskk-cache-create :type 'lru :capacity 50))
           (num-puts (+ 3 (random 8)))   ; 3..10 random puts
           (num-hits (+ 2 (random 8)))   ; 2..9 guaranteed hits
           (put-keys nil))
      (dotimes (i num-puts)
        (let ((k (format "%s-%d" key i))
              (v (nskk-generate 'candidate-list)))
          (push k put-keys)
          (nskk-cache-put cache k v)))
      (let ((prev-rate 0.0)
            (ok t))
        (dotimes (j num-hits)
          (let ((hit-key (nth (mod j (length put-keys)) put-keys)))
            (nskk-cache-get cache hit-key)
            (let ((rate (nskk-cache-hit-rate cache)))
              (when (< rate (- prev-rate 1e-10))
                (setq ok nil))
              (setq prev-rate rate))))
        ok))
    20)

  (nskk-property-test cache-lru-capacity-never-exceeded
    ((key search-query))
    (let* ((capacity 3)
           (cache    (nskk-cache-create :type 'lru :capacity capacity))
           (num-puts (+ 4 (random 10)))  ; 4..13, always > capacity
           (ok t))
      (dotimes (i num-puts)
        (let ((k (format "%s-%d" key i)))
          (nskk-cache-put cache k (list k))
          (when (> (nskk-cache-size cache) capacity)
            (setq ok nil))))
      ok)
    20)

  (nskk-property-test cache-lfu-evicts-least-frequently-used
    ((key search-query))
    (let* ((cache (nskk-cache-create :type 'lfu :capacity 3))
           (k-a   (format "%s-a" key))
           (k-b   (format "%s-b" key))
           (k-c   (format "%s-c" key))
           (k-d   (format "%s-d" key)))
      (nskk-cache-put cache k-a "val-a")
      (nskk-cache-put cache k-b "val-b")
      (nskk-cache-put cache k-c "val-c")
      (nskk-cache-get cache k-b)
      (nskk-cache-get cache k-c)
      (nskk-cache-get cache k-c)
      (nskk-cache-put cache k-d "val-d")
      (and (null  (nskk-cache-get cache k-a))
           (equal "val-b" (nskk-cache-get cache k-b))
           (equal "val-c" (nskk-cache-get cache k-c))
           (equal "val-d" (nskk-cache-get cache k-d))))
    20))


(provide 'nskk-search-cache-integration-test)

;;; nskk-search-cache-integration-test.el ends here
