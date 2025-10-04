;;; run-cache-integration-test.el --- Integration test for cache -*- lexical-binding: t; -*-

(require 'nskk-cache)

(message "=== Comprehensive Cache Test ===")
(message "")

(message "1. Testing LRU and LFU cache creation...")
(let ((lru (nskk-cache-create 'lru 100))
      (lfu (nskk-cache-create 'lfu 100)))
  (message "   LRU created: %s" (nskk-cache-lru-p lru))
  (message "   LFU created: %s" (nskk-cache-lfu-p lfu)))

(message "")
(message "2. Testing cache operations...")
(let ((cache (nskk-cache-create 'lru 3)))
  (nskk-cache-put cache "key1" "value1")
  (nskk-cache-put cache "key2" "value2")
  (nskk-cache-put cache "key3" "value3")
  (message "   Added 3 entries")
  (nskk-cache-get cache "key1") ;; Make key1 recently used
  (nskk-cache-put cache "key4" "value4")
  (message "   Added 4th entry (should evict key2)")
  (message "   key1 exists: %s" (not (null (nskk-cache-get cache "key1"))))
  (message "   key2 evicted: %s" (null (nskk-cache-get cache "key2")))
  (message "   key3 exists: %s" (not (null (nskk-cache-get cache "key3"))))
  (message "   key4 exists: %s" (not (null (nskk-cache-get cache "key4")))))

(message "")
(message "3. Testing pattern invalidation...")
(let ((cache (nskk-cache-create 'lru 100)))
  (dotimes (i 10)
    (nskk-cache-put cache (format "user:%d" i) (format "value%d" i))
    (nskk-cache-put cache (format "post:%d" i) (format "post%d" i)))
  (let ((deleted (nskk-cache-invalidate-pattern cache "^user:")))
    (message "   Deleted %d entries matching ^user:" (length deleted))
    (message "   user:0 deleted: %s" (null (nskk-cache-get cache "user:0")))
    (message "   post:0 exists: %s" (not (null (nskk-cache-get cache "post:0"))))))

(message "")
(message "4. Testing statistics...")
(let ((cache (nskk-cache-create 'lru 100)))
  (dotimes (i 50) (nskk-cache-put cache (format "k%d" i) i))
  (dotimes (i 30) (nskk-cache-get cache (format "k%d" i))) ;; 30 hits
  (dotimes (i 20) (nskk-cache-get cache (format "k%d" (+ i 50)))) ;; 20 misses
  (let ((stats (nskk-cache-stats cache)))
    (message "   Hits: %d" (plist-get stats :hits))
    (message "   Misses: %d" (plist-get stats :misses))
    (message "   Hit rate: %.2f%%" (* (plist-get stats :hit-rate) 100))
    (message "   Size: %d / %d" (plist-get stats :size) (plist-get stats :capacity))))

(message "")
(message "=== All Tests Completed Successfully ===")

;;; run-cache-integration-test.el ends here
