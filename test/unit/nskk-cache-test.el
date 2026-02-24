;;; nskk-cache-test.el --- Cache implementation tests -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Authors

;; Author: NSKK Developers
;; Keywords: japanese, cache, test

;; This file is part of NSKK.

;;; Commentary:

;; Comprehensive tests for nskk-cache.el covering:
;; - LRU cache operations
;; - LFU cache operations
;; - Cache eviction policies
;; - Statistics collection
;; - Cache management (clear)

;;; Code:

(require 'ert)
(require 'nskk-cache)
(require 'nskk-test-framework)

;;;
;;; LRU Cache Creation Tests
;;;

(nskk-deftest-unit cache-lru-create-basic
  "Test basic LRU cache creation."
  (let ((cache (nskk-cache-lru-create 100)))
    (should (nskk-cache-lru-p cache))
    (should (= (nskk-cache-lru-capacity cache) 100))
    (should (= (nskk-cache-lru-size cache) 0))))

(nskk-deftest-unit cache-lru-create-with-size
  "Test LRU cache creation with custom size."
  (let ((cache (nskk-cache-lru-create 50)))
    (should (nskk-cache-lru-p cache))
    (should (= (nskk-cache-lru-capacity cache) 50))))

;;;
;;; LRU Cache Basic Operations Tests
;;;

(nskk-deftest-unit cache-lru-put-get-basic
  "Test basic LRU put and get operations."
  (let ((cache (nskk-cache-lru-create 100)))
    (nskk-cache-lru-put cache "key1" "value1")
    (should (string= (nskk-cache-lru-get cache "key1") "value1"))))

(nskk-deftest-unit cache-lru-put-get-multiple
  "Test LRU with multiple entries."
  (let ((cache (nskk-cache-lru-create 100)))
    (nskk-cache-lru-put cache "key1" "value1")
    (nskk-cache-lru-put cache "key2" "value2")
    (nskk-cache-lru-put cache "key3" "value3")
    (should (string= (nskk-cache-lru-get cache "key1") "value1"))
    (should (string= (nskk-cache-lru-get cache "key2") "value2"))
    (should (string= (nskk-cache-lru-get cache "key3") "value3"))))

(nskk-deftest-unit cache-lru-get-nonexistent
  "Test LRU get with non-existent key."
  (let ((cache (nskk-cache-lru-create 100)))
    (nskk-cache-lru-put cache "key1" "value1")
    (should (null (nskk-cache-lru-get cache "key2")))))

(nskk-deftest-unit cache-lru-update-existing
  "Test updating existing LRU entry."
  (let ((cache (nskk-cache-lru-create 100)))
    (nskk-cache-lru-put cache "key1" "value1")
    (should (string= (nskk-cache-lru-get cache "key1") "value1"))
    (nskk-cache-lru-put cache "key1" "value2")
    (should (string= (nskk-cache-lru-get cache "key1") "value2"))))

(nskk-deftest-unit cache-lru-delete-basic
  "Test LRU delete (invalidate) operation."
  (let ((cache (nskk-cache-lru-create 100)))
    (nskk-cache-lru-put cache "key1" "value1")
    (nskk-cache-lru-put cache "key2" "value2")
    (nskk-cache-lru-invalidate cache "key1")
    (should (null (nskk-cache-lru-get cache "key1")))
    (should (string= (nskk-cache-lru-get cache "key2") "value2"))))

(nskk-deftest-unit cache-lru-delete-nonexistent
  "Test LRU delete with non-existent key (should not error)."
  (let ((cache (nskk-cache-lru-create 100)))
    (nskk-cache-lru-put cache "key1" "value1")
    (nskk-cache-lru-invalidate cache "key2")
    (should (string= (nskk-cache-lru-get cache "key1") "value1"))))

;;;
;;; LRU Cache Eviction Tests
;;;

(nskk-deftest-unit cache-lru-eviction-basic
  "Test LRU eviction when cache is full."
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

(nskk-deftest-unit cache-lru-eviction-with-access
  "Test LRU eviction respects access order."
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

(nskk-deftest-unit cache-lru-eviction-mru-to-lru
  "Test LRU eviction from MRU to LRU."
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

    ;; Check remaining keys
    (should (string= (nskk-cache-lru-get cache "key1") "value1"))
    (should (null (nskk-cache-lru-get cache "key2")))
    (should (string= (nskk-cache-lru-get cache "key3") "value3"))
    (should (string= (nskk-cache-lru-get cache "key4") "value4"))
    (should (string= (nskk-cache-lru-get cache "key5") "value5"))
    (should (string= (nskk-cache-lru-get cache "key6") "value6"))))

;;;
;;; LRU Cache Statistics Tests
;;;

(nskk-deftest-unit cache-lru-statistics-basic
  "Test LRU cache statistics."
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

(nskk-deftest-unit cache-lru-statistics-eviction-count
  "Test LRU statistics after eviction."
  (let ((cache (nskk-cache-lru-create 2)))
    (nskk-cache-lru-put cache "key1" "value1")
    (nskk-cache-lru-put cache "key2" "value2")
    (nskk-cache-lru-put cache "key3" "value3")  ; evicts key1

    (should (= (nskk-cache-lru-size cache) 2))
    (should (null (nskk-cache-lru-get cache "key1")))))

;;;
;;; LFU Cache Creation Tests
;;;

(nskk-deftest-unit cache-lfu-create-basic
  "Test basic LFU cache creation."
  (let ((cache (nskk-cache-lfu-create 100)))
    (should (nskk-cache-lfu-p cache))
    (should (= (nskk-cache-lfu-capacity cache) 100))
    (should (= (nskk-cache-lfu-size cache) 0))))

(nskk-deftest-unit cache-lfu-create-with-size
  "Test LFU cache creation with custom size."
  (let ((cache (nskk-cache-lfu-create 50)))
    (should (nskk-cache-lfu-p cache))
    (should (= (nskk-cache-lfu-capacity cache) 50))))

;;;
;;; LFU Cache Basic Operations Tests
;;;

(nskk-deftest-unit cache-lfu-put-get-basic
  "Test basic LFU put and get operations."
  (let ((cache (nskk-cache-lfu-create 100)))
    (nskk-cache-lfu-put cache "key1" "value1")
    (should (string= (nskk-cache-lfu-get cache "key1") "value1"))))

(nskk-deftest-unit cache-lfu-put-get-multiple
  "Test LFU with multiple entries."
  (let ((cache (nskk-cache-lfu-create 100)))
    (nskk-cache-lfu-put cache "key1" "value1")
    (nskk-cache-lfu-put cache "key2" "value2")
    (nskk-cache-lfu-put cache "key3" "value3")
    (should (string= (nskk-cache-lfu-get cache "key1") "value1"))
    (should (string= (nskk-cache-lfu-get cache "key2") "value2"))
    (should (string= (nskk-cache-lfu-get cache "key3") "value3"))))

(nskk-deftest-unit cache-lfu-get-nonexistent
  "Test LFU get with non-existent key."
  (let ((cache (nskk-cache-lfu-create 100)))
    (nskk-cache-lfu-put cache "key1" "value1")
    (should (null (nskk-cache-lfu-get cache "key2")))))

(nskk-deftest-unit cache-lfu-delete-basic
  "Test LFU delete operation."
  (let ((cache (nskk-cache-lfu-create 100)))
    (nskk-cache-lfu-put cache "key1" "value1")
    (nskk-cache-lfu-put cache "key2" "value2")
    (nskk-cache-lfu-invalidate cache "key1")
    (should (null (nskk-cache-lfu-get cache "key1")))
    (should (string= (nskk-cache-lfu-get cache "key2") "value2"))))

;;;
;;; LFU Cache Eviction Tests
;;;

(nskk-deftest-unit cache-lfu-eviction-basic
  "Test LFU eviction when cache is full."
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

(nskk-deftest-unit cache-lfu-eviction-frequency-tie
  "Test LFU eviction with frequency ties."
  (let ((cache (nskk-cache-lfu-create 3)))
    (nskk-cache-lfu-put cache "key1" "value1")
    (nskk-cache-lfu-put cache "key2" "value2")
    (nskk-cache-lfu-put cache "key3" "value3")

    ;; All have frequency 1, should evict key1 (first inserted)
    (nskk-cache-lfu-put cache "key4" "value4")

    (should (null (nskk-cache-lfu-get cache "key1")))
    (should (string= (nskk-cache-lfu-get cache "key2") "value2"))
    (should (string= (nskk-cache-lfu-get cache "key3") "value3"))
    (should (string= (nskk-cache-lfu-get cache "key4") "value4"))))

;;;
;;; LFU Cache Statistics Tests
;;;

(nskk-deftest-unit cache-lfu-statistics-basic
  "Test LFU cache statistics."
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

(nskk-deftest-unit cache-lfu-statistics-eviction-count
  "Test LFU statistics after eviction."
  (let ((cache (nskk-cache-lfu-create 2)))
    (nskk-cache-lfu-put cache "key1" "value1")
    (nskk-cache-lfu-put cache "key2" "value2")
    (nskk-cache-lfu-put cache "key3" "value3")  ; evicts one

    (should (= (nskk-cache-lfu-size cache) 2))))

;;;
;;; Cache Management Tests
;;;

(nskk-deftest-unit cache-clear-lru
  "Test clearing LRU cache."
  (let ((cache (nskk-cache-lru-create 100)))
    (nskk-cache-lru-put cache "key1" "value1")
    (nskk-cache-lru-put cache "key2" "value2")
    (nskk-cache-lru-clear cache)
    (should (null (nskk-cache-lru-get cache "key1")))
    (should (null (nskk-cache-lru-get cache "key2")))
    (should (= (nskk-cache-lru-size cache) 0))))

(nskk-deftest-unit cache-clear-lfu
  "Test clearing LFU cache."
  (let ((cache (nskk-cache-lfu-create 100)))
    (nskk-cache-lfu-put cache "key1" "value1")
    (nskk-cache-lfu-put cache "key2" "value2")
    (nskk-cache-lfu-clear cache)
    (should (null (nskk-cache-lfu-get cache "key1")))
    (should (null (nskk-cache-lfu-get cache "key2")))
    (should (= (nskk-cache-lfu-size cache) 0))))

(nskk-deftest-unit cache-multiple-caches
  "Test multiple independent caches."
  (let ((cache1 (nskk-cache-lru-create 2))
        (cache2 (nskk-cache-lfu-create 2)))
    (nskk-cache-lru-put cache1 "key" "value1")
    (nskk-cache-lfu-put cache2 "key" "value2")

    (should (string= (nskk-cache-lru-get cache1 "key") "value1"))
    (should (string= (nskk-cache-lfu-get cache2 "key") "value2"))

    ;; Eviction in cache1 should not affect cache2
    (nskk-cache-lru-put cache1 "key2" "value1-2")
    (nskk-cache-lru-put cache1 "key3" "value1-3")  ; evicts from cache1

    (should (string= (nskk-cache-lfu-get cache2 "key") "value2"))))

;;;
;;; Unified Interface Tests
;;;

(nskk-deftest-unit cache-unified-create
  "Test unified cache creation."
  (let ((lru-cache (nskk-cache-create 'lru 100))
        (lfu-cache (nskk-cache-create 'lfu 100)))
    (should (nskk-cache-lru-p lru-cache))
    (should (nskk-cache-lfu-p lfu-cache))))

(nskk-deftest-unit cache-unified-operations
  "Test unified cache get/put operations."
  (let ((cache (nskk-cache-create 'lru 100)))
    (nskk-cache-put cache "key1" "value1")
    (should (string= (nskk-cache-get cache "key1") "value1"))
    (should (null (nskk-cache-get cache "key2")))))

(nskk-deftest-unit cache-destroy-basic
  "Test clearing then re-using cache."
  (let ((cache (nskk-cache-lru-create 100)))
    (nskk-cache-lru-put cache "key1" "value1")
    (nskk-cache-clear cache)
    (should (null (nskk-cache-get cache "key1")))))

;;;
;;; Performance Tests
;;;

(nskk-deftest-performance cache-performance-lru-operations
  "Test LRU cache performance."
  (let ((cache (nskk-cache-lru-create 1000)))
    (let ((start-time (current-time)))
      (dotimes (i 1000)
        (nskk-cache-lru-put cache (format "key%d" i) (format "value%d" i)))
      (let ((put-time (float-time (time-subtract (current-time) start-time))))
        (message "[Performance] LRU put 1000 entries: %.3fms" (* 1000 put-time))
        (should (< put-time 0.1))))  ; Should complete in < 100ms

    (let ((start-time (current-time)))
      (dotimes (i 1000)
        (nskk-cache-lru-get cache (format "key%d" i)))
      (let ((get-time (float-time (time-subtract (current-time) start-time))))
        (message "[Performance] LRU get 1000 entries: %.3fms" (* 1000 get-time))
        (should (< get-time 0.05))))))  ; Should complete in < 50ms

(nskk-deftest-performance cache-performance-lfu-operations
  "Test LFU cache performance."
  (let ((cache (nskk-cache-lfu-create 1000)))
    (let ((start-time (current-time)))
      (dotimes (i 1000)
        (nskk-cache-lfu-put cache (format "key%d" i) (format "value%d" i)))
      (let ((put-time (float-time (time-subtract (current-time) start-time))))
        (message "[Performance] LFU put 1000 entries: %.3fms" (* 1000 put-time))
        (should (< put-time 0.1))))  ; Should complete in < 100ms

    (let ((start-time (current-time)))
      (dotimes (i 1000)
        (nskk-cache-lfu-get cache (format "key%d" i)))
      (let ((get-time (float-time (time-subtract (current-time) start-time))))
        (message "[Performance] LFU get 1000 entries: %.3fms" (* 1000 get-time))
        (should (< get-time 0.05))))))  ; Should complete in < 50ms

(nskk-deftest-performance cache-performance-eviction
  "Test cache eviction performance."
  (let ((cache (nskk-cache-lru-create 100)))
    (let ((start-time (current-time)))
      (dotimes (i 1000)
        (nskk-cache-lru-put cache (format "key%d" i) (format "value%d" i)))
      (let ((eviction-time (float-time (time-subtract (current-time) start-time))))
        (message "[Performance] LRU with 900 evictions: %.3fms" (* 1000 eviction-time))
        (should (< eviction-time 0.2))))))  ; Should complete in < 200ms

;;;
;;; Integration Tests
;;;

(nskk-deftest-integration cache-lru-workflow
  "Test realistic LRU cache workflow."
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
    (nskk-cache-lru-put cache "さかな" "魚")  ; evicts "いぬ" (least recently used after gets above)

    (should (string= (nskk-cache-lru-get cache "ねこ") "猫"))

    ;; Check stats
    (let ((stats (nskk-cache-stats cache)))
      (should (= (plist-get stats :size) 5)))))

(nskk-deftest-integration cache-lfu-workflow
  "Test realistic LFU cache workflow."
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
      (should (= (plist-get stats :size) 5)))))

(provide 'nskk-cache-test)

;;; nskk-cache-test.el ends here
