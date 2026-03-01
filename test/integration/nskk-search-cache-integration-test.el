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

;; Integration tests for nskk-search.el and nskk-cache.el.
;; Tests the full search+cache pipeline: cache creation, hit/miss,
;; eviction policies, pattern invalidation, and search-with-cache.

;;; Code:

(require 'ert)
(require 'nskk-cache)
(require 'nskk-search)
(require 'nskk-dictionary)
(require 'nskk-state)
(require 'nskk-test-framework)


;;;
;;; Group 1: Cache Basic Operations
;;;

(nskk-deftest-integration cache-create-lru
  "Create LRU cache with capacity 10 and verify initial state."
  (let ((cache (nskk-cache-create 'lru 10)))
    (should (nskk-cache-p cache))
    (should (= (nskk-cache-size cache) 0))
    (let ((stats (nskk-cache-stats cache)))
      (should (= (plist-get stats :hits) 0))
      (should (= (plist-get stats :misses) 0))
      (should (= (plist-get stats :size) 0))
      (should (= (plist-get stats :capacity) 10))
      (should (eq (plist-get stats :type) 'lru)))))

(nskk-deftest-integration cache-put-and-get
  "Put a key/value pair into LRU cache and retrieve it; verify hit/miss stats."
  (let ((cache (nskk-cache-create 'lru 10)))
    (nskk-cache-put cache "test" '("a" "b"))
    ;; Hit: existing key
    (nskk-should-equal '("a" "b") (nskk-cache-get cache "test"))
    ;; Miss: absent key
    (should (null (nskk-cache-get cache "nonexistent")))
    ;; Verify stats after 1 hit and 1 miss
    (let ((stats (nskk-cache-stats cache)))
      (should (= (plist-get stats :hits) 1))
      (should (= (plist-get stats :misses) 1)))))

(nskk-deftest-integration cache-hit-rate-tracking
  "Verify hit-rate calculation after a known pattern of hits and misses."
  (let ((cache (nskk-cache-create 'lru 10)))
    ;; Populate 3 entries
    (nskk-cache-put cache "k1" "v1")
    (nskk-cache-put cache "k2" "v2")
    (nskk-cache-put cache "k3" "v3")
    ;; 3 hits
    (nskk-cache-get cache "k1")
    (nskk-cache-get cache "k2")
    (nskk-cache-get cache "k3")
    ;; 1 miss
    (nskk-cache-get cache "absent")
    ;; hit-rate should be 3/4 = 0.75
    (nskk-assert-approx-equal (nskk-cache-hit-rate cache) 0.75 0.001)))

(nskk-deftest-integration cache-lru-eviction
  "LRU cache with capacity 2 evicts the least-recently-used entry on overflow."
  (let ((cache (nskk-cache-create 'lru 2)))
    (nskk-cache-put cache "key1" "val1")
    (nskk-cache-put cache "key2" "val2")
    ;; Adding key3 should evict key1 (the LRU entry)
    (nskk-cache-put cache "key3" "val3")
    ;; key1 must have been evicted (returns nil, recording a miss)
    (should (null (nskk-cache-get cache "key1")))
    ;; key2 and key3 must still be present
    (nskk-should-equal "val2" (nskk-cache-get cache "key2"))
    (nskk-should-equal "val3" (nskk-cache-get cache "key3"))))

(nskk-deftest-integration cache-invalidate-key
  "Invalidating a specific key removes it from the cache."
  (let ((cache (nskk-cache-create 'lru 10)))
    (nskk-cache-put cache "alpha" '("x"))
    (nskk-cache-put cache "beta" '("y"))
    ;; Invalidate one key; should return t
    (should (eq (nskk-cache-invalidate cache "alpha") t))
    ;; Size must have decremented
    (should (= (nskk-cache-size cache) 1))
    ;; Invalidated key returns nil on get
    (should (null (nskk-cache-get cache "alpha")))
    ;; Remaining key is still accessible
    (nskk-should-equal '("y") (nskk-cache-get cache "beta"))))

(nskk-deftest-integration cache-invalidate-pattern
  "Pattern invalidation removes only matching keys, leaving others intact."
  (nskk-prolog-test-with-isolated-db
    (let ((cache (nskk-cache-create 'lru 20)))
      (nskk-cache-put cache "かんじ:exact:nil" '("漢字"))
      (nskk-cache-put cache "かんじ:prefix:nil" '("漢字" "感じ"))
      (nskk-cache-put cache "さくら:exact:nil" '("桜"))
      ;; Invalidate all keys beginning with "かんじ:"
      (let ((deleted (nskk-cache-invalidate-pattern cache "^かんじ:")))
        (should (= (length deleted) 2)))
      ;; Only the さくら entry should remain
      (should (= (nskk-cache-size cache) 1))
      (should (null (nskk-cache-get cache "かんじ:exact:nil")))
      (should (null (nskk-cache-get cache "かんじ:prefix:nil")))
      (nskk-should-equal '("桜") (nskk-cache-get cache "さくら:exact:nil")))))

(nskk-deftest-integration cache-clear
  "Clearing a cache removes all entries and resets size to zero."
  (let ((cache (nskk-cache-create 'lru 10)))
    (nskk-cache-put cache "a" 1)
    (nskk-cache-put cache "b" 2)
    (nskk-cache-put cache "c" 3)
    (should (= (nskk-cache-size cache) 3))
    (nskk-cache-clear cache)
    (should (= (nskk-cache-size cache) 0))))


;;;
;;; Group 2: Search with Mock Dictionary
;;;

(nskk-deftest-integration search-exact-with-mock-dict
  "Exact match search returns expected candidates from the mock dictionary."
  (nskk-with-mock-dict nil
    (let ((entry (nskk-search-exact nskk--system-dict-index "かんじ" nil)))
      (should (nskk-dict-entry-p entry))
      (let ((candidates (nskk-dict-entry-candidates entry)))
        (should (listp candidates))
        (should (> (length candidates) 0))
        (should (member "漢字" candidates))))))

(nskk-deftest-integration search-returns-nil-for-unknown-key
  "Exact match search returns nil for a key not present in the dictionary."
  (nskk-with-mock-dict nil
    (let ((entry (nskk-search-exact nskk--system-dict-index "ほげほげほげ" nil)))
      (should (null entry)))))

(nskk-deftest-integration search-with-cache-miss-then-hit
  "First search is a cache miss; second search for the same key is a cache hit."
  (nskk-with-mock-dict nil
    (let* ((cache (nskk-cache-create 'lru 100))
           (cache-key "かんじ:exact:none")
           ;; First call: cache miss, result fetched from Prolog
           (result1 (nskk-search-with-cache cache nskk--system-dict-index "かんじ")))
      (should (nskk-dict-entry-p result1))
      ;; After the miss the key should now be in the cache
      (should (not (null (nskk-cache-get cache cache-key))))
      ;; Stats: 1 miss from the first nskk-search-with-cache call, 1 hit from
      ;; our direct nskk-cache-get above
      (let ((stats (nskk-cache-stats cache)))
        (should (>= (plist-get stats :misses) 1)))
      ;; Second call: cache hit, same object returned
      (let ((result2 (nskk-search-with-cache cache nskk--system-dict-index "かんじ")))
        (nskk-should-equal result1 result2))
      ;; After the second call the hit counter must have incremented
      (let ((stats (nskk-cache-stats cache)))
        (should (>= (plist-get stats :hits) 1))))))

(nskk-deftest-integration search-with-cache-nil-for-missing-key
  "Search-with-cache returns nil for a key absent from the dictionary and does not cache it."
  (nskk-with-mock-dict nil
    (let* ((cache (nskk-cache-create 'lru 100))
           (result (nskk-search-with-cache cache nskk--system-dict-index "ほげほげほげ")))
      (should (null result))
      ;; nil results must not be stored in the cache
      (should (= (nskk-cache-size cache) 0)))))

(nskk-deftest-integration search-with-cache-multiple-keys
  "Independent keys are cached separately and do not interfere with each other."
  (nskk-with-mock-dict nil
    (let ((cache (nskk-cache-create 'lru 100)))
      ;; Populate cache with two different keys
      (let ((e1 (nskk-search-with-cache cache nskk--system-dict-index "かんじ"))
            (e2 (nskk-search-with-cache cache nskk--system-dict-index "さくら")))
        (should (nskk-dict-entry-p e1))
        (should (nskk-dict-entry-p e2))
        ;; Both entries should now be in the cache
        (should (= (nskk-cache-size cache) 2))
        ;; Candidates should be distinct
        (should (member "漢字" (nskk-dict-entry-candidates e1)))
        (should (member "桜" (nskk-dict-entry-candidates e2)))))))


;;;
;;; Group 3: Search Learning
;;;

(nskk-deftest-integration search-learn-ranking
  "Learning increments the Prolog learning-score fact for a candidate."
  (nskk-prolog-test-with-isolated-db
    (nskk-with-mock-dict nil
      ;; Verify no prior learning score exists for this pair
      (let ((score-before
             (nskk-prolog-query-value
              '(learning-score "かんじ" "感じ" \?s) '\?s)))
        (should (null score-before)))
      ;; Record one selection
      (nskk-search-learn "かんじ" "感じ")
      ;; Now the score must be 1
      (let ((score-after
             (nskk-prolog-query-value
              '(learning-score "かんじ" "感じ" \?s) '\?s)))
        (should (= score-after 1))))))

(nskk-deftest-integration search-learn-increments-score
  "Repeated learning calls increment the score each time."
  (nskk-prolog-test-with-isolated-db
    (nskk-with-mock-dict nil
      (nskk-search-learn "かんじ" "漢字")
      (nskk-search-learn "かんじ" "漢字")
      (nskk-search-learn "かんじ" "漢字")
      (let ((score
             (nskk-prolog-query-value
              '(learning-score "かんじ" "漢字" \?s) '\?s)))
        (should (= score 3))))))

(nskk-deftest-integration search-learn-dirty-flag
  "Learning sets the dirty flag so auto-save knows data changed."
  (nskk-prolog-test-with-isolated-db
    (nskk-with-mock-dict nil
      (setq nskk-search--dirty-flag nil)
      (nskk-search-learn "にほん" "日本")
      (should (eq nskk-search--dirty-flag t)))))


;;;
;;; Group 4: Cache Type Validation
;;;

(nskk-deftest-integration cache-lfu-creation
  "Create LFU cache and verify basic put/get operations work correctly."
  (let ((cache (nskk-cache-create 'lfu 100)))
    (should (nskk-cache-p cache))
    (should (= (nskk-cache-size cache) 0))
    ;; Basic put and get
    (nskk-cache-put cache "test-key" '("value1" "value2"))
    (nskk-should-equal '("value1" "value2") (nskk-cache-get cache "test-key"))
    ;; Miss for absent key
    (should (null (nskk-cache-get cache "absent-key")))
    ;; Stats reflect type
    (let ((stats (nskk-cache-stats cache)))
      (should (eq (plist-get stats :type) 'lfu))
      (should (= (plist-get stats :hits) 1))
      (should (= (plist-get stats :misses) 1)))))

(nskk-deftest-integration cache-lfu-eviction
  "LFU cache evicts the least-frequently-used entry when capacity is reached."
  (let ((cache (nskk-cache-create 'lfu 2)))
    (nskk-cache-put cache "rare" "r")
    (nskk-cache-put cache "freq" "f")
    ;; Access "freq" multiple times to raise its frequency
    (nskk-cache-get cache "freq")
    (nskk-cache-get cache "freq")
    ;; Adding a third entry should evict "rare" (frequency 1) over "freq"
    (nskk-cache-put cache "new" "n")
    (should (null (nskk-cache-get cache "rare")))
    (nskk-should-equal "f" (nskk-cache-get cache "freq"))
    (nskk-should-equal "n" (nskk-cache-get cache "new"))))

(nskk-deftest-integration cache-invalid-type
  "Requesting an unknown cache type signals a user-error."
  (should-error
   (nskk-cache-create 'invalid-type)
   :type 'user-error))

(nskk-deftest-integration cache-lru-invalidate-nonexistent-returns-nil
  "Invalidating a key that does not exist in the cache returns nil."
  (let ((cache (nskk-cache-create 'lru 10)))
    (should (null (nskk-cache-invalidate cache "no-such-key")))))

(nskk-deftest-integration cache-default-creation
  "Creating a cache with no arguments uses the default strategy and capacity."
  (let ((cache (nskk-cache-create)))
    (should (nskk-cache-p cache))
    (let ((stats (nskk-cache-stats cache)))
      (should (eq (plist-get stats :type) nskk-cache-strategy))
      (should (= (plist-get stats :capacity) nskk-cache-default-capacity)))))

(nskk-deftest-integration cache-keyword-args-creation
  "Creating a cache via keyword arguments respects :type and :capacity."
  (let ((cache (nskk-cache-create :type 'lfu :capacity 42)))
    (should (nskk-cache-p cache))
    (let ((stats (nskk-cache-stats cache)))
      (should (eq (plist-get stats :type) 'lfu))
      (should (= (plist-get stats :capacity) 42)))))


;;;
;;; Group 5: Search Dispatcher Integration
;;;

(nskk-deftest-integration search-dispatcher-exact
  "nskk-search dispatches to exact-match search and returns an entry."
  (nskk-with-mock-dict nil
    (let ((result (nskk-search nskk--system-dict-index "かんじ" 'exact)))
      (should (nskk-dict-entry-p result))
      (should (member "漢字" (nskk-dict-entry-candidates result))))))

(nskk-deftest-integration search-dispatcher-invalid-type
  "nskk-search signals nskk-dict-search-invalid-query for an unknown search type."
  (nskk-with-mock-dict nil
    (should-error
     (nskk-search nskk--system-dict-index "かんじ" 'bogus-type)
     :type 'nskk-dict-search-invalid-query)))

(nskk-deftest-integration search-dispatcher-invalid-index
  "nskk-search signals nskk-dict-search-invalid-index for a non-index argument."
  (should-error
   (nskk-search "not-an-index" "かんじ" 'exact)
   :type 'nskk-dict-search-invalid-index))

(nskk-deftest-integration search-dispatcher-empty-query
  "nskk-search signals nskk-dict-search-invalid-query for an empty query string."
  (nskk-with-mock-dict nil
    (should-error
     (nskk-search nskk--system-dict-index "" 'exact)
     :type 'nskk-dict-search-invalid-query)))

(nskk-deftest-integration search-with-cache-invalid-cache
  "nskk-search-with-cache signals wrong-type-argument when cache is not a cache object."
  (nskk-with-mock-dict nil
    (should-error
     (nskk-search-with-cache "not-a-cache" nskk--system-dict-index "かんじ")
     :type 'wrong-type-argument)))


(provide 'nskk-search-cache-integration-test)

;;; nskk-search-cache-integration-test.el ends here
