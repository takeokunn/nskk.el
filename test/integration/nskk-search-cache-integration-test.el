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
(require 'nskk-test-macros)


;;;
;;; Group 1: Cache Basic Operations
;;;

(nskk-describe "cache basic operations"

  (nskk-it "creating LRU cache with capacity 10 yields empty stats"
    (let ((cache (nskk-cache-create 'lru 10)))
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
    (let ((cache (nskk-cache-create 'lru 10)))
      (nskk-given (nskk-cache-put cache "test" '("a" "b")))
      (nskk-then
        ;; Hit: existing key
        (nskk-should-equal '("a" "b") (nskk-cache-get cache "test"))
        ;; Miss: absent key
        (should (null (nskk-cache-get cache "nonexistent")))
        ;; Verify stats after 1 hit and 1 miss
        (let ((stats (nskk-cache-stats cache)))
          (should (= (plist-get stats :hits) 1))
          (should (= (plist-get stats :misses) 1))))))

  (nskk-it "hit-rate is 0.75 after 3 hits and 1 miss"
    (let ((cache (nskk-cache-create 'lru 10)))
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
    (let ((cache (nskk-cache-create 'lru 2)))
      (nskk-given
        (nskk-cache-put cache "key1" "val1")
        (nskk-cache-put cache "key2" "val2"))
      (nskk-when
        ;; Adding key3 should evict key1 (the LRU entry)
        (nskk-cache-put cache "key3" "val3"))
      (nskk-then
        (should (null (nskk-cache-get cache "key1")))
        (nskk-should-equal "val2" (nskk-cache-get cache "key2"))
        (nskk-should-equal "val3" (nskk-cache-get cache "key3")))))

  (nskk-it "invalidating a specific key removes it and decrements size"
    (let ((cache (nskk-cache-create 'lru 10)))
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
      (let ((cache (nskk-cache-create 'lru 20)))
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
    (let ((cache (nskk-cache-create 'lru 10)))
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
      (let ((entry (nskk-search-exact nskk--system-dict-index "かんじ" nil)))
        (nskk-then
          (should (nskk-dict-entry-p entry))
          (let ((candidates (nskk-dict-entry-candidates entry)))
            (should (listp candidates))
            (should (> (length candidates) 0))
            (should (member "漢字" candidates)))))))

  (nskk-it "exact match search returns nil for a key not in the dictionary"
    (nskk-with-mock-dict nil
      (let ((entry (nskk-search-exact nskk--system-dict-index "ほげほげほげ" nil)))
        (nskk-then (should (null entry))))))

  (nskk-it "first search is a cache miss; second is a cache hit"
    (nskk-with-mock-dict nil
      (let* ((cache (nskk-cache-create 'lru 100))
             (cache-key "かんじ:exact:none")
             ;; First call: cache miss, result fetched from Prolog
             (result1 (nskk-search-with-cache cache nskk--system-dict-index "かんじ")))
        (nskk-then
          (should (nskk-dict-entry-p result1))
          ;; After the miss the key should now be in the cache
          (should (not (null (nskk-cache-get cache cache-key))))
          ;; Stats: 1 miss from the first nskk-search-with-cache call
          (let ((stats (nskk-cache-stats cache)))
            (should (>= (plist-get stats :misses) 1))))
        ;; Second call: cache hit, same object returned
        (let ((result2 (nskk-search-with-cache cache nskk--system-dict-index "かんじ")))
          (nskk-then
            (nskk-should-equal result1 result2)
            ;; After the second call the hit counter must have incremented
            (let ((stats (nskk-cache-stats cache)))
              (should (>= (plist-get stats :hits) 1))))))))

  (nskk-it "search-with-cache returns nil for a missing key and does not cache nil"
    (nskk-with-mock-dict nil
      (let* ((cache (nskk-cache-create 'lru 100))
             (result (nskk-search-with-cache cache nskk--system-dict-index "ほげほげほげ")))
        (nskk-then
          (should (null result))
          (should (= (nskk-cache-size cache) 0))))))

  (nskk-it "independent keys are cached separately without interference"
    (nskk-with-mock-dict nil
      (let ((cache (nskk-cache-create 'lru 100)))
        (let ((e1 (nskk-search-with-cache cache nskk--system-dict-index "かんじ"))
              (e2 (nskk-search-with-cache cache nskk--system-dict-index "さくら")))
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

  (nskk-it "learning increments the Prolog learning-score fact for a candidate"
    (nskk-prolog-test-with-isolated-db
      (nskk-with-mock-dict nil
        (nskk-given
          ;; Verify no prior learning score exists for this pair
          (let ((score-before
                 (nskk-prolog-query-value
                  '(learning-score "かんじ" "感じ" \?s) '\?s)))
            (should (null score-before))))
        (nskk-when
          (nskk-search-learn "かんじ" "感じ"))
        (nskk-then
          (let ((score-after
                 (nskk-prolog-query-value
                  '(learning-score "かんじ" "感じ" \?s) '\?s)))
            (should (= score-after 1)))))))

  (nskk-it "repeated learning calls increment the score each time"
    (nskk-prolog-test-with-isolated-db
      (nskk-with-mock-dict nil
        (nskk-when
          (nskk-search-learn "かんじ" "漢字")
          (nskk-search-learn "かんじ" "漢字")
          (nskk-search-learn "かんじ" "漢字"))
        (nskk-then
          (let ((score
                 (nskk-prolog-query-value
                  '(learning-score "かんじ" "漢字" \?s) '\?s)))
            (should (= score 3)))))))

  (nskk-it "learning sets the dirty flag so auto-save knows data changed"
    (nskk-prolog-test-with-isolated-db
      (nskk-with-mock-dict nil
        (nskk-given (setq nskk-search--dirty-flag nil))
        (nskk-when  (nskk-search-learn "にほん" "日本"))
        (nskk-then  (should (eq nskk-search--dirty-flag t)))))))


;;;
;;; Group 4: Cache Type Validation
;;;

(nskk-describe "cache type validation"

  (nskk-it "LFU cache supports basic put/get and tracks stats by type"
    (let ((cache (nskk-cache-create 'lfu 100)))
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
    (let ((cache (nskk-cache-create 'lfu 2)))
      (nskk-given
        (nskk-cache-put cache "rare" "r")
        (nskk-cache-put cache "freq" "f"))
      (nskk-when
        ;; Access "freq" multiple times to raise its frequency
        (nskk-cache-get cache "freq")
        (nskk-cache-get cache "freq")
        ;; Adding a third entry should evict "rare" (frequency 1)
        (nskk-cache-put cache "new" "n"))
      (nskk-then
        (should (null (nskk-cache-get cache "rare")))
        (nskk-should-equal "f" (nskk-cache-get cache "freq"))
        (nskk-should-equal "n" (nskk-cache-get cache "new")))))

  (nskk-it "requesting an unknown cache type signals a user-error"
    (nskk-then
      (should-error
       (nskk-cache-create 'invalid-type)
       :type 'user-error)))

  (nskk-it "invalidating a non-existent key returns nil"
    (let ((cache (nskk-cache-create 'lru 10)))
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
      (let ((result (nskk-search nskk--system-dict-index "かんじ" 'exact)))
        (nskk-then
          (should (nskk-dict-entry-p result))
          (should (member "漢字" (nskk-dict-entry-candidates result)))))))

  (nskk-it "nskk-search signals nskk-dict-search-invalid-query for unknown type"
    (nskk-with-mock-dict nil
      (nskk-then
        (should-error
         (nskk-search nskk--system-dict-index "かんじ" 'bogus-type)
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
         (nskk-search nskk--system-dict-index "" 'exact)
         :type 'nskk-dict-search-invalid-query))))

  (nskk-it "nskk-search-with-cache signals wrong-type-argument for a non-cache object"
    (nskk-with-mock-dict nil
      (nskk-then
        (should-error
         (nskk-search-with-cache "not-a-cache" nskk--system-dict-index "かんじ")
         :type 'wrong-type-argument)))))


(provide 'nskk-search-cache-integration-test)

;;; nskk-search-cache-integration-test.el ends here
