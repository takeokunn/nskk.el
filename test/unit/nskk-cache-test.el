;;; nskk-cache-test.el --- Cache implementation tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, cache, test

;; This file is part of NSKK.

;;; Commentary:

;; Tests for nskk-cache.el covering:
;; - Prolog fact registration (cache-type/1, cache-dispatch-fn/3, cache-field-fn/3)
;; - nskk-cache--type-of helper
;; - LRU cache operations and eviction
;; - LFU cache operations and eviction
;; - Statistics collection
;; - Unified interface
;; - Cache management (clear, invalidate)

;;; Code:

(require 'ert)
(require 'nskk-cache)
(require 'nskk-test-framework)

(nskk-describe "Prolog integration"
  (nskk-it "cache-type/1 Prolog facts are asserted at load time"
    (nskk-prolog-test-with-isolated-db
      (should (nskk-prolog-query-one '(cache-type lru)))
      (should (nskk-prolog-query-one '(cache-type lfu)))
      (should (null (nskk-prolog-query-one '(cache-type unknown))))))

  (nskk-it "cache-eviction-policy/2 Prolog facts are asserted at load time"
    (nskk-prolog-test-with-isolated-db
      (should (nskk-prolog-query-one '(cache-eviction-policy lru least-recently-used)))
      (should (nskk-prolog-query-one '(cache-eviction-policy lfu least-frequently-used)))))

  (nskk-it "cache-dispatch-fn/3 facts exist for all LRU and LFU operations"
    (nskk-prolog-test-with-isolated-db
      (dolist (op '(get put invalidate clear size))
        (should (nskk-prolog-query-one `(cache-dispatch-fn lru ,op \?fn)))
        (should (nskk-prolog-query-one `(cache-dispatch-fn lfu ,op \?fn))))))

  (nskk-it "cache-field-fn/3 facts exist for all LRU and LFU fields"
    (nskk-prolog-test-with-isolated-db
      (dolist (field '(capacity size hits misses hash))
        (should (nskk-prolog-query-one `(cache-field-fn lru ,field \?fn)))
        (should (nskk-prolog-query-one `(cache-field-fn lfu ,field \?fn))))))

  (nskk-it "cache-dispatch-fn/3 resolves to expected function symbols"
    (nskk-prolog-test-with-isolated-db
      (should (eq (nskk-prolog-query-value '(cache-dispatch-fn lru get \?fn) '\?fn)
                  'nskk-cache-lru-get))
      (should (eq (nskk-prolog-query-value '(cache-dispatch-fn lru put \?fn) '\?fn)
                  'nskk-cache-lru-put))
      (should (eq (nskk-prolog-query-value '(cache-dispatch-fn lfu get \?fn) '\?fn)
                  'nskk-cache-lfu-get))
      (should (eq (nskk-prolog-query-value '(cache-dispatch-fn lfu clear \?fn) '\?fn)
                  'nskk-cache-lfu-clear)))))

(nskk-describe "nskk-cache--type-of"
  (nskk-it "returns lru for LRU caches"
    (let ((cache (nskk-cache-lru-create 10)))
      (should (eq (nskk-cache--type-of cache) 'lru))))

  (nskk-it "returns lfu for LFU caches"
    (let ((cache (nskk-cache-lfu-create 10)))
      (should (eq (nskk-cache--type-of cache) 'lfu))))

  (nskk-it "signals an error for non-cache values"
    (should-error (nskk-cache--type-of nil))
    (should-error (nskk-cache--type-of "not-a-cache"))
    (should-error (nskk-cache--type-of 42))))

(nskk-describe "LRU cache creation"
  (nskk-it "creates cache with basic capacity"
    (let ((cache (nskk-cache-lru-create 100)))
      (should (nskk-cache-lru-p cache))
      (should (= (nskk-cache-lru-capacity cache) 100))
      (should (= (nskk-cache-lru-size cache) 0))))

  (nskk-it "creates cache with custom size"
    (let ((cache (nskk-cache-lru-create 50)))
      (should (nskk-cache-lru-p cache))
      (should (= (nskk-cache-lru-capacity cache) 50)))))

(nskk-describe "LRU cache basic operations"
  (nskk-it "stores and retrieves a single entry"
    (let ((cache (nskk-cache-lru-create 100)))
      (nskk-cache-lru-put cache "key1" "value1")
      (should (string= (nskk-cache-lru-get cache "key1") "value1"))))

  (nskk-it "stores and retrieves multiple entries"
    (let ((cache (nskk-cache-lru-create 100)))
      (nskk-cache-lru-put cache "key1" "value1")
      (nskk-cache-lru-put cache "key2" "value2")
      (nskk-cache-lru-put cache "key3" "value3")
      (should (string= (nskk-cache-lru-get cache "key1") "value1"))
      (should (string= (nskk-cache-lru-get cache "key2") "value2"))
      (should (string= (nskk-cache-lru-get cache "key3") "value3"))))

  (nskk-it "returns nil for non-existent key"
    (let ((cache (nskk-cache-lru-create 100)))
      (nskk-cache-lru-put cache "key1" "value1")
      (should (null (nskk-cache-lru-get cache "key2")))))

  (nskk-it "updates existing entry"
    (let ((cache (nskk-cache-lru-create 100)))
      (nskk-cache-lru-put cache "key1" "value1")
      (should (string= (nskk-cache-lru-get cache "key1") "value1"))
      (nskk-cache-lru-put cache "key1" "value2")
      (should (string= (nskk-cache-lru-get cache "key1") "value2"))))

  (nskk-it "invalidates a specific entry"
    (let ((cache (nskk-cache-lru-create 100)))
      (nskk-cache-lru-put cache "key1" "value1")
      (nskk-cache-lru-put cache "key2" "value2")
      (nskk-cache-lru-invalidate cache "key1")
      (should (null (nskk-cache-lru-get cache "key1")))
      (should (string= (nskk-cache-lru-get cache "key2") "value2"))))

  (nskk-it "invalidating non-existent key does not error"
    (let ((cache (nskk-cache-lru-create 100)))
      (nskk-cache-lru-put cache "key1" "value1")
      (nskk-cache-lru-invalidate cache "key2")
      (should (string= (nskk-cache-lru-get cache "key1") "value1")))))

(nskk-describe "LRU cache eviction"
  (nskk-it "evicts least recently used entry when cache is full"
    (let ((cache (nskk-cache-lru-create 3)))
      (nskk-cache-lru-put cache "key1" "value1")
      (nskk-cache-lru-put cache "key2" "value2")
      (nskk-cache-lru-put cache "key3" "value3")
      (should (string= (nskk-cache-lru-get cache "key1") "value1"))
      (should (string= (nskk-cache-lru-get cache "key2") "value2"))
      (should (string= (nskk-cache-lru-get cache "key3") "value3"))
      ;; Adding 4th item should evict key1 (least recently used)
      (nskk-cache-lru-put cache "key4" "value4")
      (should (null (nskk-cache-lru-get cache "key1")))
      (should (string= (nskk-cache-lru-get cache "key2") "value2"))
      (should (string= (nskk-cache-lru-get cache "key3") "value3"))
      (should (string= (nskk-cache-lru-get cache "key4") "value4"))))

  (nskk-it "respects access order when evicting"
    (let ((cache (nskk-cache-lru-create 3)))
      (nskk-cache-lru-put cache "key1" "value1")
      (nskk-cache-lru-put cache "key2" "value2")
      (nskk-cache-lru-put cache "key3" "value3")
      ;; Access key1 to make it most recently used
      (nskk-cache-lru-get cache "key1")
      ;; Add key4, should evict key2 (now least recently used)
      (nskk-cache-lru-put cache "key4" "value4")
      (should (string= (nskk-cache-lru-get cache "key1") "value1"))
      (should (null (nskk-cache-lru-get cache "key2")))
      (should (string= (nskk-cache-lru-get cache "key3") "value3"))
      (should (string= (nskk-cache-lru-get cache "key4") "value4"))))

  (nskk-it "evicts from MRU to LRU correctly across multiple accesses"
    (let ((cache (nskk-cache-lru-create 5)))
      (nskk-cache-lru-put cache "key1" "value1")
      (nskk-cache-lru-put cache "key2" "value2")
      (nskk-cache-lru-put cache "key3" "value3")
      (nskk-cache-lru-put cache "key4" "value4")
      (nskk-cache-lru-put cache "key5" "value5")
      ;; Access in order: key3, key1, key4
      (nskk-cache-lru-get cache "key3")
      (nskk-cache-lru-get cache "key1")
      (nskk-cache-lru-get cache "key4")
      ;; Add key6, should evict key2 (least recently used)
      (nskk-cache-lru-put cache "key6" "value6")
      (should (string= (nskk-cache-lru-get cache "key1") "value1"))
      (should (null (nskk-cache-lru-get cache "key2")))
      (should (string= (nskk-cache-lru-get cache "key3") "value3"))
      (should (string= (nskk-cache-lru-get cache "key4") "value4"))
      (should (string= (nskk-cache-lru-get cache "key5") "value5"))
      (should (string= (nskk-cache-lru-get cache "key6") "value6")))))

(nskk-describe "LRU cache statistics"
  (nskk-it "tracks hits, misses, size, and hit-rate"
    (let ((cache (nskk-cache-lru-create 100)))
      (nskk-cache-lru-put cache "key1" "value1")
      (nskk-cache-lru-put cache "key2" "value2")
      (nskk-cache-lru-get cache "key1")  ; hit
      (nskk-cache-lru-get cache "key3")  ; miss
      (nskk-cache-lru-get cache "key2")  ; hit
      (nskk-cache-lru-get cache "key3")  ; miss
      (let ((stats (nskk-cache-stats cache)))
        (should (eq (plist-get stats :type) 'lru))
        (should (= (plist-get stats :size) 2))
        (should (= (plist-get stats :hits) 2))
        (should (= (plist-get stats :misses) 2))
        (should (= (plist-get stats :hit-rate) 0.5)))))

  (nskk-it "reflects correct size after eviction"
    (let ((cache (nskk-cache-lru-create 2)))
      (nskk-cache-lru-put cache "key1" "value1")
      (nskk-cache-lru-put cache "key2" "value2")
      (nskk-cache-lru-put cache "key3" "value3")  ; evicts key1
      (should (= (nskk-cache-lru-size cache) 2))
      (should (null (nskk-cache-lru-get cache "key1"))))))

(nskk-describe "LFU cache creation"
  (nskk-it "creates cache with basic capacity"
    (let ((cache (nskk-cache-lfu-create 100)))
      (should (nskk-cache-lfu-p cache))
      (should (= (nskk-cache-lfu-capacity cache) 100))
      (should (= (nskk-cache-lfu-size cache) 0))))

  (nskk-it "creates cache with custom size"
    (let ((cache (nskk-cache-lfu-create 50)))
      (should (nskk-cache-lfu-p cache))
      (should (= (nskk-cache-lfu-capacity cache) 50)))))

(nskk-describe "LFU cache basic operations"
  (nskk-it "stores and retrieves a single entry"
    (let ((cache (nskk-cache-lfu-create 100)))
      (nskk-cache-lfu-put cache "key1" "value1")
      (should (string= (nskk-cache-lfu-get cache "key1") "value1"))))

  (nskk-it "stores and retrieves multiple entries"
    (let ((cache (nskk-cache-lfu-create 100)))
      (nskk-cache-lfu-put cache "key1" "value1")
      (nskk-cache-lfu-put cache "key2" "value2")
      (nskk-cache-lfu-put cache "key3" "value3")
      (should (string= (nskk-cache-lfu-get cache "key1") "value1"))
      (should (string= (nskk-cache-lfu-get cache "key2") "value2"))
      (should (string= (nskk-cache-lfu-get cache "key3") "value3"))))

  (nskk-it "returns nil for non-existent key"
    (let ((cache (nskk-cache-lfu-create 100)))
      (nskk-cache-lfu-put cache "key1" "value1")
      (should (null (nskk-cache-lfu-get cache "key2")))))

  (nskk-it "invalidates a specific entry"
    (let ((cache (nskk-cache-lfu-create 100)))
      (nskk-cache-lfu-put cache "key1" "value1")
      (nskk-cache-lfu-put cache "key2" "value2")
      (nskk-cache-lfu-invalidate cache "key1")
      (should (null (nskk-cache-lfu-get cache "key1")))
      (should (string= (nskk-cache-lfu-get cache "key2") "value2")))))

(nskk-describe "LFU cache eviction"
  (nskk-it "evicts least frequently used entry when cache is full"
    (let ((cache (nskk-cache-lfu-create 3)))
      (nskk-cache-lfu-put cache "key1" "value1")
      (nskk-cache-lfu-put cache "key2" "value2")
      (nskk-cache-lfu-put cache "key3" "value3")
      ;; Access key2 and key3 to increase their frequency
      (nskk-cache-lfu-get cache "key2")  ; freq: 2
      (nskk-cache-lfu-get cache "key3")  ; freq: 2
      (nskk-cache-lfu-get cache "key2")  ; freq: 3
      ;; Add key4, should evict key1 (frequency 1)
      (nskk-cache-lfu-put cache "key4" "value4")
      (should (null (nskk-cache-lfu-get cache "key1")))
      (should (string= (nskk-cache-lfu-get cache "key2") "value2"))
      (should (string= (nskk-cache-lfu-get cache "key3") "value3"))
      (should (string= (nskk-cache-lfu-get cache "key4") "value4"))))

  (nskk-it "evicts first inserted entry on frequency tie"
    (let ((cache (nskk-cache-lfu-create 3)))
      (nskk-cache-lfu-put cache "key1" "value1")
      (nskk-cache-lfu-put cache "key2" "value2")
      (nskk-cache-lfu-put cache "key3" "value3")
      ;; All have frequency 1, should evict key1 (first inserted)
      (nskk-cache-lfu-put cache "key4" "value4")
      (should (null (nskk-cache-lfu-get cache "key1")))
      (should (string= (nskk-cache-lfu-get cache "key2") "value2"))
      (should (string= (nskk-cache-lfu-get cache "key3") "value3"))
      (should (string= (nskk-cache-lfu-get cache "key4") "value4")))))

(nskk-describe "LFU cache statistics"
  (nskk-it "tracks hits, misses, size, and hit-rate"
    (let ((cache (nskk-cache-lfu-create 100)))
      (nskk-cache-lfu-put cache "key1" "value1")
      (nskk-cache-lfu-put cache "key2" "value2")
      (nskk-cache-lfu-get cache "key1")  ; hit, freq becomes 2
      (nskk-cache-lfu-get cache "key3")  ; miss
      (nskk-cache-lfu-get cache "key2")  ; hit, freq becomes 2
      (nskk-cache-lfu-get cache "key1")  ; hit, freq becomes 3
      (nskk-cache-lfu-get cache "key3")  ; miss
      (let ((stats (nskk-cache-stats cache)))
        (should (eq (plist-get stats :type) 'lfu))
        (should (= (plist-get stats :size) 2))
        (should (= (plist-get stats :hits) 3))
        (should (= (plist-get stats :misses) 2))
        (should (= (plist-get stats :hit-rate) 0.6)))))

  (nskk-it "reflects correct size after eviction"
    (let ((cache (nskk-cache-lfu-create 2)))
      (nskk-cache-lfu-put cache "key1" "value1")
      (nskk-cache-lfu-put cache "key2" "value2")
      (nskk-cache-lfu-put cache "key3" "value3")  ; evicts one
      (should (= (nskk-cache-lfu-size cache) 2)))))

(nskk-describe "cache management"
  (nskk-it "clears LRU cache completely"
    (let ((cache (nskk-cache-lru-create 100)))
      (nskk-cache-lru-put cache "key1" "value1")
      (nskk-cache-lru-put cache "key2" "value2")
      (nskk-cache-lru-clear cache)
      (should (null (nskk-cache-lru-get cache "key1")))
      (should (null (nskk-cache-lru-get cache "key2")))
      (should (= (nskk-cache-lru-size cache) 0))))

  (nskk-it "clears LFU cache completely"
    (let ((cache (nskk-cache-lfu-create 100)))
      (nskk-cache-lfu-put cache "key1" "value1")
      (nskk-cache-lfu-put cache "key2" "value2")
      (nskk-cache-lfu-clear cache)
      (should (null (nskk-cache-lfu-get cache "key1")))
      (should (null (nskk-cache-lfu-get cache "key2")))
      (should (= (nskk-cache-lfu-size cache) 0))))

  (nskk-it "multiple independent caches do not interfere"
    (let ((cache1 (nskk-cache-lru-create 2))
          (cache2 (nskk-cache-lfu-create 2)))
      (nskk-cache-lru-put cache1 "key" "value1")
      (nskk-cache-lfu-put cache2 "key" "value2")
      (should (string= (nskk-cache-lru-get cache1 "key") "value1"))
      (should (string= (nskk-cache-lfu-get cache2 "key") "value2"))
      ;; Eviction in cache1 should not affect cache2
      (nskk-cache-lru-put cache1 "key2" "value1-2")
      (nskk-cache-lru-put cache1 "key3" "value1-3")  ; evicts from cache1
      (should (string= (nskk-cache-lfu-get cache2 "key") "value2")))))

(nskk-describe "unified cache interface"
  (nskk-it "creates LRU and LFU caches by type"
    (let ((lru-cache (nskk-cache-create 'lru 100))
          (lfu-cache (nskk-cache-create 'lfu 100)))
      (should (nskk-cache-lru-p lru-cache))
      (should (nskk-cache-lfu-p lfu-cache))))

  (nskk-it "signals error for unknown type"
    (should-error (nskk-cache-create 'bogus 100)))

  (nskk-it "signals user-error for unregistered cache type"
    (should-error (nskk-cache-create 'unknown-type 100) :type 'user-error))

  (nskk-it "provides get/put operations through unified interface"
    (let ((cache (nskk-cache-create 'lru 100)))
      (nskk-cache-put cache "key1" "value1")
      (should (string= (nskk-cache-get cache "key1") "value1"))
      (should (null (nskk-cache-get cache "key2")))))

  (nskk-it "can be cleared and reused"
    (let ((cache (nskk-cache-lru-create 100)))
      (nskk-cache-lru-put cache "key1" "value1")
      (nskk-cache-clear cache)
      (should (null (nskk-cache-get cache "key1")))))

  (nskk-it "tracks size through unified interface"
    (let ((cache (nskk-cache-create 'lru 100)))
      (should (= (nskk-cache-size cache) 0))
      (nskk-cache-put cache "k1" "v1")
      (nskk-cache-put cache "k2" "v2")
      (should (= (nskk-cache-size cache) 2))))

  (nskk-it "correctly dispatches to LFU implementation"
    (let ((cache (nskk-cache-create 'lfu 100)))
      (nskk-cache-put cache "key1" "value1")
      (should (string= (nskk-cache-get cache "key1") "value1"))
      (should (nskk-cache-lfu-p cache)))))

(nskk-describe "cache performance"
  (nskk-it "LRU put and get 1000 entries within time budget"
    (let ((cache (nskk-cache-lru-create 1000)))
      (let ((start-time (current-time)))
        (dotimes (i 1000)
          (nskk-cache-lru-put cache (format "key%d" i) (format "value%d" i)))
        (let ((put-time (float-time (time-subtract (current-time) start-time))))
          (message "[Performance] LRU put 1000 entries: %.3fms" (* 1000 put-time))
          (should (< put-time 0.1))))  ; < 100ms total
      (let ((start-time (current-time)))
        (dotimes (i 1000)
          (nskk-cache-lru-get cache (format "key%d" i)))
        (let ((get-time (float-time (time-subtract (current-time) start-time))))
          (message "[Performance] LRU get 1000 entries: %.3fms" (* 1000 get-time))
          (should (< get-time 0.05))))))  ; < 50ms total

  (nskk-it "LFU put and get 1000 entries within time budget"
    (let ((cache (nskk-cache-lfu-create 1000)))
      (let ((start-time (current-time)))
        (dotimes (i 1000)
          (nskk-cache-lfu-put cache (format "key%d" i) (format "value%d" i)))
        (let ((put-time (float-time (time-subtract (current-time) start-time))))
          (message "[Performance] LFU put 1000 entries: %.3fms" (* 1000 put-time))
          (should (< put-time 0.1))))  ; < 100ms total
      (let ((start-time (current-time)))
        (dotimes (i 1000)
          (nskk-cache-lfu-get cache (format "key%d" i)))
        (let ((get-time (float-time (time-subtract (current-time) start-time))))
          (message "[Performance] LFU get 1000 entries: %.3fms" (* 1000 get-time))
          (should (< get-time 0.05))))))  ; < 50ms total

  (nskk-it "LRU eviction of 900 entries stays within time budget"
    (let ((cache (nskk-cache-lru-create 100)))
      (let ((start-time (current-time)))
        (dotimes (i 1000)
          (nskk-cache-lru-put cache (format "key%d" i) (format "value%d" i)))
        (let ((eviction-time (float-time (time-subtract (current-time) start-time))))
          (message "[Performance] LRU with 900 evictions: %.3fms" (* 1000 eviction-time))
          (should (< eviction-time 0.2))))))  ; < 200ms total

  (nskk-it "unified interface Prolog dispatch overhead stays within budget"
    (let ((cache (nskk-cache-create 'lru 1000)))
      (dotimes (i 500)
        (nskk-cache-put cache (format "key%d" i) (format "value%d" i)))
      (let ((start-time (current-time)))
        (dotimes (i 500)
          (nskk-cache-get cache (format "key%d" i)))
        (let ((elapsed (float-time (time-subtract (current-time) start-time))))
          (message "[Performance] Unified get 500 (Prolog dispatch): %.3fms"
                   (* 1000 elapsed))
          ;; Each call: ~20us Prolog + LRU O(1) => 500 * 0.1ms budget = 50ms max
          (should (< elapsed 0.05)))))))

(nskk-describe "cache integration workflows"
  (nskk-it "LRU cache simulates dictionary lookup caching"
    (let ((cache (nskk-cache-lru-create 5)))
      ;; Simulate dictionary lookup caching
      (nskk-cache-lru-put cache "かんじ" "漢字")
      (nskk-cache-lru-put cache "にほんご" "日本語")
      (nskk-cache-lru-put cache "いぬ" "犬")
      (should (string= (nskk-cache-lru-get cache "かんじ") "漢字"))
      (should (string= (nskk-cache-lru-get cache "にほんご") "日本語"))
      ;; Add more entries, trigger eviction
      (nskk-cache-lru-put cache "ねこ" "猫")
      (nskk-cache-lru-put cache "とり" "鳥")
      (nskk-cache-lru-put cache "さかな" "魚")
      (should (string= (nskk-cache-lru-get cache "ねこ") "猫"))
      ;; Check stats
      (let ((stats (nskk-cache-stats cache)))
        (should (= (plist-get stats :size) 5)))))

  (nskk-it "LFU cache retains frequently accessed entries"
    (let ((cache (nskk-cache-lfu-create 5)))
      ;; Simulate frequently accessed entries
      (nskk-cache-lfu-put cache "common1" "value1")
      (nskk-cache-lfu-put cache "common2" "value2")
      (nskk-cache-lfu-put cache "rare1" "value3")
      ;; Access common entries multiple times
      (dotimes (_ 10)
        (nskk-cache-lfu-get cache "common1"))
      (dotimes (_ 5)
        (nskk-cache-lfu-get cache "common2"))
      ;; Add more entries
      (nskk-cache-lfu-put cache "rare2" "value4")
      (nskk-cache-lfu-put cache "rare3" "value5")
      (nskk-cache-lfu-put cache "rare4" "value6")  ; should evict rare entry
      ;; Common entries should still be present
      (should (string= (nskk-cache-lfu-get cache "common1") "value1"))
      (should (string= (nskk-cache-lfu-get cache "common2") "value2"))
      ;; Check statistics
      (let ((stats (nskk-cache-stats cache)))
        (should (> (plist-get stats :hits) 15))
        (should (= (plist-get stats :size) 5))))))

(provide 'nskk-cache-test)

;;; nskk-cache-test.el ends here
