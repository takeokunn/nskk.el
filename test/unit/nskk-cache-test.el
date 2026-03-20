;;; nskk-cache-test.el --- Cache implementation tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, cache, test

;; This file is part of NSKK.

;;; Commentary:

;; Tests for nskk-cache.el covering:
;; - Prolog fact registration (cache-type/1, cache-dispatch-fn/3, cache-field-fn/3)
;; - nskk--cache-type-of and nskk-cache-p helpers
;; - LRU cache operations and eviction
;; - LFU cache operations and eviction
;; - Statistics collection and hit-rate
;; - Unified interface (create, get, put, clear, invalidate, invalidate-pattern)
;; - Cache management (clear, invalidate, invalidate-pattern)
;; - Property-based invariants (size bounds, hit-rate range, get-after-put)

;;; Code:

(require 'ert)
(require 'nskk-cache)
(require 'nskk-test-framework)
(require 'nskk-test-macros)

;;; ─────────────────────────────────────────────────────────────────────────
;;; Shared test fixtures
;;; ─────────────────────────────────────────────────────────────────────────

(defconst nskk-cache-test--cache-ops '(get put invalidate clear size)
  "Canonical operation names for cache-dispatch-fn/3 coverage tests.")

(defconst nskk-cache-test--fields '(capacity size hits misses hash)
  "Canonical field names for field-fn coverage tests.")

(defconst nskk--test-default-cache-capacity 100
  "Default cache capacity used in cache unit tests.")

;;; ─────────────────────────────────────────────────────────────────────────
;;; Prolog integration
;;; ─────────────────────────────────────────────────────────────────────────

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
      (dolist (op nskk-cache-test--cache-ops)
        (should (nskk-prolog-query-one `(cache-dispatch-fn lru ,op \?fn)))
        (should (nskk-prolog-query-one `(cache-dispatch-fn lfu ,op \?fn))))))

  (nskk-it "cache-field-fn/3 facts exist for all LRU and LFU fields"
    (nskk-prolog-test-with-isolated-db
      (dolist (field nskk-cache-test--fields)
        (should (nskk-prolog-query-one `(cache-field-fn lru ,field \?fn)))
        (should (nskk-prolog-query-one `(cache-field-fn lfu ,field \?fn))))))

  (nskk-it "cache-dispatch-fn/3 resolves to expected function symbols"
    (nskk-prolog-test-with-isolated-db
      (nskk-deftest-table cache-dispatch-fn-resolution
        :columns (type op expected-fn)
        :rows    ((lru get   nskk-cache-lru-get)
                  (lru put   nskk-cache-lru-put)
                  (lfu get   nskk-cache-lfu-get)
                  (lfu clear nskk-cache-lfu-clear))
        :body
        (should (eq (nskk-prolog-query-value
                     `(cache-dispatch-fn ,type ,op \?fn) '\?fn)
                    expected-fn)))))

  (nskk-it "cache-constructor/2 maps each type to its constructor function"
    (nskk-prolog-test-with-isolated-db
      (nskk-deftest-table cache-constructor-dispatch-spot-check
        :columns (type expected-fn)
        :rows ((lru nskk-cache-lru-create)
               (lfu nskk-cache-lfu-create))
        :body
        (should (eq expected-fn
                    (nskk-prolog-query-value
                     `(cache-constructor ,type \?fn) '\?fn)))))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Cache predicate and type detection
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-describe "nskk-cache-p"
  (nskk-it "returns non-nil for LRU caches"
    (should (nskk-cache-p (nskk-cache-lru-create 10))))

  (nskk-it "returns non-nil for LFU caches"
    (should (nskk-cache-p (nskk-cache-lfu-create 10))))

  (nskk-it "returns nil for non-cache values"
    (should-not (nskk-cache-p nil))
    (should-not (nskk-cache-p "not-a-cache"))
    (should-not (nskk-cache-p 42))
    (should-not (nskk-cache-p '(lru . fake)))))

(nskk-describe "nskk--cache-type-of"
  (nskk-it "returns lru for LRU caches"
    (let ((cache (nskk-cache-lru-create 10)))
      (should (eq (nskk--cache-type-of cache) 'lru))))

  (nskk-it "returns lfu for LFU caches"
    (let ((cache (nskk-cache-lfu-create 10)))
      (should (eq (nskk--cache-type-of cache) 'lfu))))

  (nskk-it "signals an error for non-cache values"
    (should-error (nskk--cache-type-of nil))
    (should-error (nskk--cache-type-of "not-a-cache"))
    (should-error (nskk--cache-type-of 42))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; LRU cache: creation
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-describe "LRU cache creation"
  (nskk-it "creates a valid LRU cache with the given capacity"
    (nskk-deftest-table lru-creation
      :columns (capacity)
      :rows    ((10) (50) (100) (1000))
      :body
      (let ((cache (nskk-cache-lru-create capacity)))
        (should (nskk-cache-lru-p cache))
        (should (= (nskk-cache-lru-capacity cache) capacity))
        (should (= (nskk-cache-lru-size cache) 0))
        (should (= (nskk-cache-lru-hits cache) 0))
        (should (= (nskk-cache-lru-misses cache) 0)))))

  (nskk-it "initializes head/tail sentinel nodes as a doubly-linked pair"
    (let* ((cache (nskk-cache-lru-create 10))
           (head  (nskk-cache-lru-head cache))
           (tail  (nskk-cache-lru-tail cache)))
      (should (eq (nskk-cache-lru-node-next head) tail))
      (should (eq (nskk-cache-lru-node-prev tail) head))
      (should (null (nskk-cache-lru-node-prev head)))
      (should (null (nskk-cache-lru-node-next tail))))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; LRU cache: basic operations
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-describe "LRU cache basic operations"
  (nskk-it "stores and retrieves a single entry"
    (let ((cache (nskk-cache-lru-create nskk--test-default-cache-capacity)))
      (nskk-given (nskk-cache-lru-put cache "key1" "value1"))
      (nskk-then  (should (string= (nskk-cache-lru-get cache "key1") "value1")))))

  (nskk-it "stores and retrieves multiple entries"
    (let ((cache (nskk-cache-lru-create nskk--test-default-cache-capacity)))
      (nskk-given
       (nskk-cache-lru-put cache "key1" "value1")
       (nskk-cache-lru-put cache "key2" "value2")
       (nskk-cache-lru-put cache "key3" "value3"))
      (nskk-then
       (should (string= (nskk-cache-lru-get cache "key1") "value1"))
       (should (string= (nskk-cache-lru-get cache "key2") "value2"))
       (should (string= (nskk-cache-lru-get cache "key3") "value3")))))

  (nskk-it "returns nil on a cache miss"
    (let ((cache (nskk-cache-lru-create nskk--test-default-cache-capacity)))
      (nskk-given (nskk-cache-lru-put cache "key1" "value1"))
      (nskk-then  (should (null (nskk-cache-lru-get cache "missing-key"))))))

  (nskk-it "updates value in place without changing size"
    (let ((cache (nskk-cache-lru-create nskk--test-default-cache-capacity)))
      (nskk-given
       (nskk-cache-lru-put cache "key1" "value1")
       (nskk-cache-lru-put cache "key1" "value2"))
      (nskk-then
       (should (string= (nskk-cache-lru-get cache "key1") "value2"))
       (should (= (nskk-cache-lru-size cache) 1)))))

  (nskk-it "invalidate removes a specific entry and returns t"
    (let ((cache (nskk-cache-lru-create nskk--test-default-cache-capacity)))
      (nskk-given
       (nskk-cache-lru-put cache "key1" "value1")
       (nskk-cache-lru-put cache "key2" "value2"))
      (nskk-when
       (should (eq t (nskk-cache-lru-invalidate cache "key1"))))
      (nskk-then
       (should (null (nskk-cache-lru-get cache "key1")))
       (should (string= (nskk-cache-lru-get cache "key2") "value2")))))

  (nskk-it "invalidate returns nil for a missing key"
    (let ((cache (nskk-cache-lru-create nskk--test-default-cache-capacity)))
      (nskk-given (nskk-cache-lru-put cache "key1" "value1"))
      (nskk-then  (should (null (nskk-cache-lru-invalidate cache "missing")))))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; LRU cache: eviction
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-describe "LRU cache eviction"
  (nskk-it "evicts the least recently used entry when full"
    (let ((cache (nskk-cache-lru-create 3)))
      (nskk-given
       ;; Insert three entries; access order: key1, key2, key3
       (nskk-cache-lru-put cache "key1" "value1")
       (nskk-cache-lru-put cache "key2" "value2")
       (nskk-cache-lru-put cache "key3" "value3"))
      (nskk-when
       ;; key4 causes eviction of key1 (LRU)
       (nskk-cache-lru-put cache "key4" "value4"))
      (nskk-then
       (should (null  (nskk-cache-lru-get cache "key1")))
       (should (string= (nskk-cache-lru-get cache "key2") "value2"))
       (should (string= (nskk-cache-lru-get cache "key3") "value3"))
       (should (string= (nskk-cache-lru-get cache "key4") "value4")))))

  (nskk-it "respects access order: a get promotes an entry past the LRU"
    (let ((cache (nskk-cache-lru-create 3)))
      (nskk-given
       (nskk-cache-lru-put cache "key1" "value1")
       (nskk-cache-lru-put cache "key2" "value2")
       (nskk-cache-lru-put cache "key3" "value3")
       ;; Promote key1 to MRU; key2 is now LRU
       (nskk-cache-lru-get cache "key1"))
      (nskk-when
       (nskk-cache-lru-put cache "key4" "value4"))
      (nskk-then
       (should (string= (nskk-cache-lru-get cache "key1") "value1"))
       (should (null  (nskk-cache-lru-get cache "key2")))
       (should (string= (nskk-cache-lru-get cache "key3") "value3"))
       (should (string= (nskk-cache-lru-get cache "key4") "value4")))))

  (nskk-it "evicts correct entry after multiple mixed accesses"
    (let ((cache (nskk-cache-lru-create 5)))
      (nskk-given
       (nskk-cache-lru-put cache "key1" "value1")
       (nskk-cache-lru-put cache "key2" "value2")
       (nskk-cache-lru-put cache "key3" "value3")
       (nskk-cache-lru-put cache "key4" "value4")
       (nskk-cache-lru-put cache "key5" "value5")
       ;; Access order: key3, key1, key4 -> key2 and key5 become candidates
       ;; Most-recently-used: key4 > key1 > key3 > key5 > key2 (LRU)
       (nskk-cache-lru-get cache "key3")
       (nskk-cache-lru-get cache "key1")
       (nskk-cache-lru-get cache "key4"))
      (nskk-when
       (nskk-cache-lru-put cache "key6" "value6"))
      (nskk-then
       (should (string= (nskk-cache-lru-get cache "key1") "value1"))
       (should (null  (nskk-cache-lru-get cache "key2")))
       (should (string= (nskk-cache-lru-get cache "key3") "value3"))
       (should (string= (nskk-cache-lru-get cache "key4") "value4"))
       (should (string= (nskk-cache-lru-get cache "key5") "value5"))
       (should (string= (nskk-cache-lru-get cache "key6") "value6"))))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; LRU cache: statistics
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-describe "LRU cache statistics"
  (nskk-it "tracks hits, misses, size, and hit-rate accurately"
    (let ((cache (nskk-cache-lru-create nskk--test-default-cache-capacity)))
      (nskk-given
       (nskk-cache-lru-put cache "key1" "value1")
       (nskk-cache-lru-put cache "key2" "value2"))
      (nskk-when
       (nskk-cache-lru-get cache "key1")  ; hit
       (nskk-cache-lru-get cache "key3")  ; miss
       (nskk-cache-lru-get cache "key2")  ; hit
       (nskk-cache-lru-get cache "key3")) ; miss
      (nskk-then
       (let ((stats (nskk-cache-stats cache)))
         (should (eq   (plist-get stats :type)     'lru))
         (should (=    (plist-get stats :size)     2))
         (should (=    (plist-get stats :hits)     2))
         (should (=    (plist-get stats :misses)   2))
         (should (=    (plist-get stats :hit-rate) 0.5))))))

  (nskk-it "reflects correct size after eviction"
    (let ((cache (nskk-cache-lru-create 2)))
      (nskk-given
       (nskk-cache-lru-put cache "key1" "value1")
       (nskk-cache-lru-put cache "key2" "value2")
       (nskk-cache-lru-put cache "key3" "value3")) ; evicts key1
      (nskk-then
       (should (= (nskk-cache-lru-size cache) 2))
       (should (null (nskk-cache-lru-get cache "key1"))))))

  (nskk-it "resets statistics after clear"
    (let ((cache (nskk-cache-lru-create nskk--test-default-cache-capacity)))
      (nskk-given
       (nskk-cache-lru-put cache "key1" "value1")
       (nskk-cache-lru-get cache "key1")
       (nskk-cache-lru-get cache "missing"))
      (nskk-when
       (nskk-cache-lru-clear cache))
      (nskk-then
       (should (= (nskk-cache-lru-size   cache) 0))
       (should (= (nskk-cache-lru-hits   cache) 0))
       (should (= (nskk-cache-lru-misses cache) 0)))))

  (nskk-it "reconnects head/tail sentinels correctly after clear"
    (let* ((cache (nskk-cache-lru-create 10))
           (head  (nskk-cache-lru-head cache))
           (tail  (nskk-cache-lru-tail cache)))
      ;; Add entries then clear
      (nskk-cache-lru-put cache "k1" "v1")
      (nskk-cache-lru-put cache "k2" "v2")
      (nskk-cache-lru-clear cache)
      ;; Sentinel nodes must be directly linked again
      (should (eq (nskk-cache-lru-node-next head) tail))
      (should (eq (nskk-cache-lru-node-prev tail) head)))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; LFU cache: creation
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-describe "LFU cache creation"
  (nskk-it "creates a valid LFU cache with the given capacity"
    (nskk-deftest-table lfu-creation
      :columns (capacity)
      :rows    ((10) (50) (100) (1000))
      :body
      (let ((cache (nskk-cache-lfu-create capacity)))
        (should (nskk-cache-lfu-p cache))
        (should (= (nskk-cache-lfu-capacity cache) capacity))
        (should (= (nskk-cache-lfu-size cache) 0))
        (should (= (nskk-cache-lfu-hits cache) 0))
        (should (= (nskk-cache-lfu-misses cache) 0))))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; LFU cache: basic operations
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-describe "LFU cache basic operations"
  (nskk-it "stores and retrieves a single entry"
    (let ((cache (nskk-cache-lfu-create nskk--test-default-cache-capacity)))
      (nskk-given (nskk-cache-lfu-put cache "key1" "value1"))
      (nskk-then  (should (string= (nskk-cache-lfu-get cache "key1") "value1")))))

  (nskk-it "stores and retrieves multiple entries"
    (let ((cache (nskk-cache-lfu-create nskk--test-default-cache-capacity)))
      (nskk-given
       (nskk-cache-lfu-put cache "key1" "value1")
       (nskk-cache-lfu-put cache "key2" "value2")
       (nskk-cache-lfu-put cache "key3" "value3"))
      (nskk-then
       (should (string= (nskk-cache-lfu-get cache "key1") "value1"))
       (should (string= (nskk-cache-lfu-get cache "key2") "value2"))
       (should (string= (nskk-cache-lfu-get cache "key3") "value3")))))

  (nskk-it "returns nil on a cache miss"
    (let ((cache (nskk-cache-lfu-create nskk--test-default-cache-capacity)))
      (nskk-given (nskk-cache-lfu-put cache "key1" "value1"))
      (nskk-then  (should (null (nskk-cache-lfu-get cache "missing-key"))))))

  (nskk-it "invalidate removes a specific entry and returns t"
    (let ((cache (nskk-cache-lfu-create nskk--test-default-cache-capacity)))
      (nskk-given
       (nskk-cache-lfu-put cache "key1" "value1")
       (nskk-cache-lfu-put cache "key2" "value2"))
      (nskk-when
       (should (eq t (nskk-cache-lfu-invalidate cache "key1"))))
      (nskk-then
       (should (null (nskk-cache-lfu-get cache "key1")))
       (should (string= (nskk-cache-lfu-get cache "key2") "value2")))))

  (nskk-it "invalidate returns nil for a missing key"
    (let ((cache (nskk-cache-lfu-create nskk--test-default-cache-capacity)))
      (nskk-given (nskk-cache-lfu-put cache "key1" "value1"))
      (nskk-then  (should (null (nskk-cache-lfu-invalidate cache "missing"))))))

  (nskk-it "updates value in place without changing size"
    (let ((cache (nskk-cache-lfu-create nskk--test-default-cache-capacity)))
      (nskk-given
       (nskk-cache-lfu-put cache "key1" "value1")
       (nskk-cache-lfu-put cache "key1" "value2"))
      (nskk-then
       (should (string= (nskk-cache-lfu-get cache "key1") "value2"))
       (should (= (nskk-cache-lfu-size cache) 1)))))

  (nskk-it "increments frequency when updating an existing entry"
    (let ((cache (nskk-cache-lfu-create nskk--test-default-cache-capacity)))
      (nskk-given
       (nskk-cache-lfu-put cache "key1" "value1")
       ;; Put same key again → frequency should be 2 before any get
       (nskk-cache-lfu-put cache "key1" "value2"))
      (nskk-then
       (let* ((entry (gethash "key1" (nskk-cache-lfu-hash cache)))
              (freq  (nskk-cache-lfu-entry-frequency entry)))
         ;; After initial put (freq=1) + update put (freq=2)
         (should (= freq 2)))))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; LFU cache: internal helpers
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-describe "LFU cache internal helpers"

  (nskk-describe "nskk-cache-lfu--bucket-any-key/k"
    (nskk-it "calls on-found with a key from a non-empty bucket"
      (let ((bucket (make-hash-table :test 'equal)))
        (puthash "k1" t bucket)
        (puthash "k2" t bucket)
        (let ((result nil))
          (nskk-cache-lfu--bucket-any-key/k bucket
            (lambda (k) (setq result k))
            (lambda () (setq result :not-found)))
          ;; Any key in the bucket is acceptable
          (should (member result '("k1" "k2"))))))

    (nskk-it "calls on-not-found for an empty bucket"
      (let ((bucket (make-hash-table :test 'equal))
            (called nil))
        (nskk-cache-lfu--bucket-any-key/k bucket
          (lambda (_k) (setq called :found))
          (lambda ()   (setq called :not-found)))
        (should (eq called :not-found)))))

  (nskk-describe "nskk-cache-lfu--evict-min-freq"
    (nskk-it "evicts one entry and decrements size"
      (let ((cache (nskk-cache-lfu-create 2)))
        (nskk-cache-lfu-put cache "key1" "v1")
        (nskk-cache-lfu-put cache "key2" "v2")
        ;; Both at freq 1; evict one
        (nskk-cache-lfu--evict-min-freq cache)
        (should (= (nskk-cache-lfu-size cache) 1))
        ;; Exactly one of the two keys survives
        (let ((surviving (cl-count-if #'identity
                           (list (nskk-cache-lfu-get cache "key1")
                                 (nskk-cache-lfu-get cache "key2")))))
          (should (= surviving 1)))))

    (nskk-it "is a no-op when the cache is empty"
      (let ((cache (nskk-cache-lfu-create 10)))
        (nskk-cache-lfu--evict-min-freq cache)
        (should (= (nskk-cache-lfu-size cache) 0))))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; LFU cache: eviction
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-describe "LFU cache eviction"
  (nskk-it "evicts the least frequently used entry when full"
    (let ((cache (nskk-cache-lfu-create 3)))
      (nskk-given
       (nskk-cache-lfu-put cache "key1" "value1")
       (nskk-cache-lfu-put cache "key2" "value2")
       (nskk-cache-lfu-put cache "key3" "value3")
       ;; Raise frequency of key2 (→3) and key3 (→2); key1 stays at 1
       (nskk-cache-lfu-get cache "key2")
       (nskk-cache-lfu-get cache "key3")
       (nskk-cache-lfu-get cache "key2"))
      (nskk-when
       ;; key4 causes eviction of key1 (lowest frequency = 1)
       (nskk-cache-lfu-put cache "key4" "value4"))
      (nskk-then
       (should (null  (nskk-cache-lfu-get cache "key1")))
       (should (string= (nskk-cache-lfu-get cache "key2") "value2"))
       (should (string= (nskk-cache-lfu-get cache "key3") "value3"))
       (should (string= (nskk-cache-lfu-get cache "key4") "value4")))))

  (nskk-it "capacity-1 cache: second put evicts the first entry"
    (let ((cache (nskk-cache-lfu-create 1)))
      (nskk-given (nskk-cache-lfu-put cache "key1" "value1"))
      (nskk-when  (nskk-cache-lfu-put cache "key2" "value2"))
      (nskk-then
       (should (= (nskk-cache-lfu-size cache) 1))
       (should (null  (nskk-cache-lfu-get cache "key1")))
       (should (string= (nskk-cache-lfu-get cache "key2") "value2")))))

  (nskk-it "min-freq advances when all entries at min-freq are promoted by get"
    ;; Two entries at freq 1.  After getting both, the freq-1 bucket empties.
    ;; min-freq must advance to 2; a new entry inserted afterward correctly
    ;; sets min-freq back to 1 and the promoted entries survive eviction.
    (let ((cache (nskk-cache-lfu-create 2)))
      (nskk-given
       (nskk-cache-lfu-put cache "key1" "value1")   ; freq 1
       (nskk-cache-lfu-put cache "key2" "value2")   ; freq 1
       (nskk-cache-lfu-get cache "key1")             ; key1 freq → 2
       (nskk-cache-lfu-get cache "key2"))            ; key2 freq → 2; min-freq=2
      (nskk-then
       (should (= (nskk-cache-lfu-min-freq cache) 2)))
      (nskk-when
       ;; Inserting key3 triggers eviction at min-freq=2 (key1 or key2),
       ;; then resets min-freq to 1.
       (nskk-cache-lfu-put cache "key3" "value3"))
      (nskk-then
       (should (= (nskk-cache-lfu-size cache) 2))
       (should (= (nskk-cache-lfu-min-freq cache) 1))
       (should (string= (nskk-cache-lfu-get cache "key3") "value3")))))

  (nskk-it "get-promoted entries survive when a lower-frequency entry is evicted"
    ;; key1 stays at freq 1; key2 gets promoted to freq 2.
    ;; Inserting key3 must evict key1, not key2.
    (let ((cache (nskk-cache-lfu-create 2)))
      (nskk-given
       (nskk-cache-lfu-put cache "key1" "value1")   ; freq 1
       (nskk-cache-lfu-put cache "key2" "value2")   ; freq 1
       (nskk-cache-lfu-get cache "key2"))            ; key2 freq → 2
      (nskk-when
       (nskk-cache-lfu-put cache "key3" "value3"))   ; evicts key1 (min-freq=1)
      (nskk-then
       (should (null  (nskk-cache-lfu-get cache "key1")))
       (should (string= (nskk-cache-lfu-get cache "key2") "value2"))
       (should (string= (nskk-cache-lfu-get cache "key3") "value3")))))

  (nskk-it "evicts any entry at the minimum frequency when frequencies are equal"
    ;; Frequency buckets are now hash-tables (O(1) add/remove), so eviction order
    ;; within equal-frequency entries is arbitrary (not FIFO).  The contract is:
    ;; exactly one of the min-freq entries is evicted; the new entry is present.
    (let ((cache (nskk-cache-lfu-create 3)))
      (nskk-given
       ;; All entries have frequency 1
       (nskk-cache-lfu-put cache "key1" "value1")
       (nskk-cache-lfu-put cache "key2" "value2")
       (nskk-cache-lfu-put cache "key3" "value3"))
      (nskk-when
       (nskk-cache-lfu-put cache "key4" "value4"))
      (nskk-then
       ;; key4 is always present; exactly 2 of {key1,key2,key3} survive
       (should (= (nskk-cache-lfu-size cache) 3))
       (should (string= (nskk-cache-lfu-get cache "key4") "value4"))
       (let ((surviving (cl-count-if #'identity
                          (list (nskk-cache-lfu-get cache "key1")
                                (nskk-cache-lfu-get cache "key2")
                                (nskk-cache-lfu-get cache "key3")))))
         (should (= surviving 2)))))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; LFU cache: statistics
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-describe "LFU cache statistics"
  (nskk-it "tracks hits, misses, size, and hit-rate accurately"
    (let ((cache (nskk-cache-lfu-create nskk--test-default-cache-capacity)))
      (nskk-given
       (nskk-cache-lfu-put cache "key1" "value1")
       (nskk-cache-lfu-put cache "key2" "value2"))
      (nskk-when
       (nskk-cache-lfu-get cache "key1")  ; hit  (freq → 2)
       (nskk-cache-lfu-get cache "key3")  ; miss
       (nskk-cache-lfu-get cache "key2")  ; hit  (freq → 2)
       (nskk-cache-lfu-get cache "key1")  ; hit  (freq → 3)
       (nskk-cache-lfu-get cache "key3")) ; miss
      (nskk-then
       (let ((stats (nskk-cache-stats cache)))
         (should (eq   (plist-get stats :type)     'lfu))
         (should (=    (plist-get stats :size)     2))
         (should (=    (plist-get stats :hits)     3))
         (should (=    (plist-get stats :misses)   2))
         (should (=    (plist-get stats :hit-rate) 0.6))))))

  (nskk-it "reflects correct size after eviction"
    (let ((cache (nskk-cache-lfu-create 2)))
      (nskk-given
       (nskk-cache-lfu-put cache "key1" "value1")
       (nskk-cache-lfu-put cache "key2" "value2")
       (nskk-cache-lfu-put cache "key3" "value3")) ; evicts one
      (nskk-then
       (should (= (nskk-cache-lfu-size cache) 2)))))

  (nskk-it "resets statistics after clear"
    (let ((cache (nskk-cache-lfu-create nskk--test-default-cache-capacity)))
      (nskk-given
       (nskk-cache-lfu-put cache "key1" "value1")
       (nskk-cache-lfu-get cache "key1")
       (nskk-cache-lfu-get cache "missing"))
      (nskk-when
       (nskk-cache-lfu-clear cache))
      (nskk-then
       (should (= (nskk-cache-lfu-size     cache) 0))
       (should (= (nskk-cache-lfu-min-freq cache) 0))
       (should (= (nskk-cache-lfu-hits     cache) 0))
       (should (= (nskk-cache-lfu-misses   cache) 0))))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Cache management: clear and invalidate-pattern
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-describe "cache management"
  (nskk-it "clear removes all entries from an LRU cache"
    (let ((cache (nskk-cache-lru-create nskk--test-default-cache-capacity)))
      (nskk-given
       (nskk-cache-lru-put cache "key1" "value1")
       (nskk-cache-lru-put cache "key2" "value2"))
      (nskk-when (nskk-cache-lru-clear cache))
      (nskk-then
       (should (null (nskk-cache-lru-get cache "key1")))
       (should (null (nskk-cache-lru-get cache "key2")))
       (should (= (nskk-cache-lru-size cache) 0)))))

  (nskk-it "clear removes all entries from an LFU cache"
    (let ((cache (nskk-cache-lfu-create nskk--test-default-cache-capacity)))
      (nskk-given
       (nskk-cache-lfu-put cache "key1" "value1")
       (nskk-cache-lfu-put cache "key2" "value2"))
      (nskk-when (nskk-cache-lfu-clear cache))
      (nskk-then
       (should (null (nskk-cache-lfu-get cache "key1")))
       (should (null (nskk-cache-lfu-get cache "key2")))
       (should (= (nskk-cache-lfu-size cache) 0)))))

  (nskk-it "multiple independent caches do not interfere"
    (let ((cache1 (nskk-cache-lru-create 2))
          (cache2 (nskk-cache-lfu-create 2)))
      (nskk-given
       (nskk-cache-lru-put cache1 "key" "value1")
       (nskk-cache-lfu-put cache2 "key" "value2"))
      (nskk-then
       (should (string= (nskk-cache-lru-get cache1 "key") "value1"))
       (should (string= (nskk-cache-lfu-get cache2 "key") "value2")))
      ;; Eviction in cache1 must not affect cache2
      (nskk-when
       (nskk-cache-lru-put cache1 "key2" "v1-2")
       (nskk-cache-lru-put cache1 "key3" "v1-3"))
      (nskk-then
       (should (string= (nskk-cache-lfu-get cache2 "key") "value2"))))))

(nskk-describe "cache invalidate-pattern"
  (nskk-it "removes all keys matching a pattern from LRU cache"
    (let ((cache (nskk-cache-lru-create nskk--test-default-cache-capacity)))
      (nskk-given
       (nskk-cache-lru-put cache "dict:ka"   "か")
       (nskk-cache-lru-put cache "dict:ki"   "き")
       (nskk-cache-lru-put cache "other:foo" "bar"))
      (nskk-when
       (let ((deleted (nskk-cache-invalidate-pattern cache "^dict:")))
         (nskk-then
          ;; All dict: keys returned
          (should (member "dict:ka" deleted))
          (should (member "dict:ki" deleted))
          (should-not (member "other:foo" deleted))
          ;; Actual entries removed
          (should (null (nskk-cache-lru-get cache "dict:ka")))
          (should (null (nskk-cache-lru-get cache "dict:ki")))
          (should (string= (nskk-cache-lru-get cache "other:foo") "bar"))
          ;; Size updated
          (should (= (nskk-cache-lru-size cache) 1)))))))

  (nskk-it "removes all keys matching a pattern from LFU cache"
    (let ((cache (nskk-cache-lfu-create nskk--test-default-cache-capacity)))
      (nskk-given
       (nskk-cache-lfu-put cache "prefix:a" "va")
       (nskk-cache-lfu-put cache "prefix:b" "vb")
       (nskk-cache-lfu-put cache "other"    "vc"))
      (nskk-when
       (let ((deleted (nskk-cache-invalidate-pattern cache "^prefix:")))
         (nskk-then
          (should (member "prefix:a" deleted))
          (should (member "prefix:b" deleted))
          (should-not (member "other" deleted))
          (should (null (nskk-cache-lfu-get cache "prefix:a")))
          (should (null (nskk-cache-lfu-get cache "prefix:b")))
          (should (string= (nskk-cache-lfu-get cache "other") "vc")))))))

  (nskk-it "returns empty list when no keys match"
    (let ((cache (nskk-cache-lru-create nskk--test-default-cache-capacity)))
      (nskk-given (nskk-cache-lru-put cache "key1" "value1"))
      (nskk-then
       (should (null (nskk-cache-invalidate-pattern cache "^no-such-prefix:")))
       (should (string= (nskk-cache-lru-get cache "key1") "value1")))))

  (nskk-it "signals invalid-regexp for a malformed pattern"
    (let ((cache (nskk-cache-lru-create 10)))
      (nskk-cache-lru-put cache "key1" "v1")
      (should-error (nskk-cache-invalidate-pattern cache "[invalid")
                    :type 'invalid-regexp))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Unified interface
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-describe "unified cache interface"
  (nskk-it "creates an LRU cache with keyword args :type 'lru"
    (nskk-then
     (should (nskk-cache-lru-p (nskk-cache-create :type 'lru :capacity nskk--test-default-cache-capacity)))))

  (nskk-it "creates an LFU cache with keyword args :type 'lfu"
    (nskk-then
     (should (nskk-cache-lfu-p (nskk-cache-create :type 'lfu :capacity nskk--test-default-cache-capacity)))))

  (nskk-it "creates a cache with keyword arguments"
    (let ((cache (nskk-cache-create :type 'lru :capacity 50)))
      (should (nskk-cache-lru-p cache))
      (should (= (nskk-cache-lru-capacity cache) 50))))

  (nskk-it "creates a cache with no arguments using defaults"
    (let ((nskk-cache-strategy 'lru)
          (nskk-cache-default-capacity 200))
      (let ((cache (nskk-cache-create)))
        (should (nskk-cache-lru-p cache))
        (should (= (nskk-cache-lru-capacity cache) 200)))))

  (nskk-it ":size takes precedence over :capacity when both are supplied"
    (let ((cache (nskk-cache-create :type 'lru :capacity 10 :size 20)))
      (should (= (nskk-cache-lru-capacity cache) 20))))

  (nskk-it ":size alone sets capacity without :capacity"
    (let ((cache (nskk-cache-create :type 'lfu :size 30)))
      (should (= (nskk-cache-lfu-capacity cache) 30))))

  (nskk-it "defaults to nskk-cache-strategy type when :type is omitted"
    (let* ((nskk-cache-strategy 'lfu)
           (cache (nskk-cache-create :capacity 5)))
      (should (nskk-cache-lfu-p cache))
      (should (= (nskk-cache-lfu-capacity cache) 5))))

  (nskk-it "signals user-error for an unregistered cache type"
    (should-error (nskk-cache-create :type 'bogus :capacity nskk--test-default-cache-capacity)     :type 'user-error)
    (should-error (nskk-cache-create :type 'unknown-type)  :type 'user-error))

  (nskk-it "provides get/put through the unified interface"
    (let ((cache (nskk-cache-create :type 'lru :capacity nskk--test-default-cache-capacity)))
      (nskk-given (nskk-cache-put cache "key1" "value1"))
      (nskk-then
       (should (string= (nskk-cache-get cache "key1") "value1"))
       (should (null    (nskk-cache-get cache "key2"))))))

  (nskk-it "clears a cache through the unified interface"
    (let ((cache (nskk-cache-lru-create nskk--test-default-cache-capacity)))
      (nskk-given (nskk-cache-lru-put cache "key1" "value1"))
      (nskk-when  (nskk-cache-clear cache))
      (nskk-then  (should (null (nskk-cache-get cache "key1"))))))

  (nskk-it "tracks size through the unified interface"
    (let ((cache (nskk-cache-create :type 'lru :capacity nskk--test-default-cache-capacity)))
      (should (= (nskk-cache-size cache) 0))
      (nskk-cache-put cache "k1" "v1")
      (nskk-cache-put cache "k2" "v2")
      (should (= (nskk-cache-size cache) 2))))

  (nskk-it "dispatches get/put correctly to the LFU implementation"
    (let ((cache (nskk-cache-create :type 'lfu :capacity nskk--test-default-cache-capacity)))
      (nskk-given (nskk-cache-put cache "key1" "value1"))
      (nskk-then
       (should (string= (nskk-cache-get cache "key1") "value1"))
       (should (nskk-cache-lfu-p cache))))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; nskk-cache-create/k: CPS wrapper
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-describe "nskk-cache-create/k CPS wrapper"
  (nskk-it "calls on-done with an LRU cache when type is lru"
    (let ((result nil))
      (nskk-cache-create/k
       (lambda (c) (setq result c))
       nil
       :type 'lru :capacity nskk--test-default-cache-capacity)
      (should (nskk-cache-lru-p result))
      (should (= (nskk-cache-lru-capacity result) nskk--test-default-cache-capacity))))

  (nskk-it "calls on-done with an LFU cache when type is lfu"
    (let ((result nil))
      (nskk-cache-create/k
       (lambda (c) (setq result c))
       nil
       :type 'lfu :capacity 50)
      (should (nskk-cache-lfu-p result))
      (should (= (nskk-cache-lfu-capacity result) 50))))

  (nskk-it "never calls _on-not-found for a valid type"
    (let ((not-found-called nil))
      (nskk-cache-create/k
       #'identity
       (lambda () (setq not-found-called t))
       :type 'lru :capacity 10)
      (should-not not-found-called)))

  (nskk-it "signals user-error for an unknown type (propagates from nskk-cache-create)"
    (should-error
     (nskk-cache-create/k #'identity nil :type 'bogus :capacity 10)
     :type 'user-error)))

;;; ─────────────────────────────────────────────────────────────────────────
;;; nskk-cache-get/k: CPS interface and falsy-value correctness
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-describe "nskk-cache-get/k falsy-value correctness"
  (nskk-it "calls on-found for a stored nil value, not on-not-found"
    (let ((cache (nskk-cache-lru-create 10))
          (found-called nil)
          (not-found-called nil))
      (nskk-cache-lru-put cache "key" nil)
      (nskk-cache-get/k
       cache "key"
       (lambda (v) (setq found-called t) v)
       (lambda () (setq not-found-called t)))
      (should found-called)
      (should-not not-found-called)))

  (nskk-it "calls on-found for a stored 0 (integer zero)"
    (let ((cache (nskk-cache-lru-create 10))
          (found-val :sentinel))
      (nskk-cache-lru-put cache "key" 0)
      (nskk-cache-get/k
       cache "key"
       (lambda (v) (setq found-val v))
       (lambda () (setq found-val :miss)))
      (should (= found-val 0))))

  (nskk-it "calls on-found for a stored empty string"
    (let ((cache (nskk-cache-lru-create 10))
          (found-val :sentinel))
      (nskk-cache-lru-put cache "key" "")
      (nskk-cache-get/k
       cache "key"
       (lambda (v) (setq found-val v))
       (lambda () (setq found-val :miss)))
      (should (string= found-val ""))))

  (nskk-it "calls on-found for a stored empty list"
    (let ((cache (nskk-cache-lru-create 10))
          (found-val :sentinel))
      (nskk-cache-lru-put cache "key" '())
      (nskk-cache-get/k
       cache "key"
       (lambda (v) (setq found-val v))
       (lambda () (setq found-val :miss)))
      (should (null found-val))
      ;; Verify the found-val was set by on-found, not left as sentinel
      ;; by checking on-not-found was never called via side effect
      (should (not (eq found-val :sentinel)))))

  (nskk-it "calls on-not-found for a genuine cache miss"
    (let ((cache (nskk-cache-lru-create 10))
          (not-found-called nil))
      (nskk-cache-get/k
       cache "missing-key"
       (lambda (_v) (error "on-found should not be called for a miss"))
       (lambda () (setq not-found-called t)))
      (should not-found-called)))

  (nskk-it "sync nskk-cache-get returns nil for both stored nil and miss (documented limitation)"
    ;; This documents the known limitation: sync get cannot distinguish
    ;; a stored nil from a miss.  Use nskk-cache-get/k to distinguish them.
    (let ((cache (nskk-cache-lru-create 10)))
      (nskk-cache-lru-put cache "nil-key" nil)
      (should (null (nskk-cache-get cache "nil-key")))   ; stored nil → nil
      (should (null (nskk-cache-get cache "miss-key")))))  ; miss → nil

  (nskk-it "works correctly for LFU caches too"
    (let ((cache (nskk-cache-lfu-create 10))
          (found-called nil))
      (nskk-cache-lfu-put cache "key" nil)
      (nskk-cache-get/k
       cache "key"
       (lambda (_v) (setq found-called t))
       (lambda () (error "on-not-found should not be called")))
      (should found-called))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Direct /k variant calling convention tests
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-describe "LRU /k variants: direct continuation tests"
  (nskk-it "nskk-cache-lru-get/k calls on-found with the stored value"
    (nskk-given "an LRU cache with one entry"
      (let ((cache (nskk-cache-create :type 'lru :capacity 10))
            (result nil))
        (nskk-cache-lru-put cache "k" "v")
        (nskk-when "get/k is called for that key"
          (nskk-cache-lru-get/k cache "k"
            (lambda (val) (setq result val))
            (lambda () (ert-fail "on-not-found called unexpectedly")))
          (nskk-then "on-found receives the stored value"
            (should (equal result "v")))))))

  (nskk-it "nskk-cache-lru-get/k calls on-not-found on miss"
    (let ((cache (nskk-cache-create :type 'lru :capacity 10))
          (missed nil))
      (nskk-cache-lru-get/k cache "absent"
        (lambda (_) (ert-fail "on-found called unexpectedly"))
        (lambda () (setq missed t)))
      (should missed)))

  (nskk-it "nskk-cache-lru-get/k calls on-found even when stored value is nil"
    (let ((cache (nskk-cache-create :type 'lru :capacity 10))
          (found nil))
      (nskk-cache-lru-put cache "nilkey" nil)
      (nskk-cache-lru-get/k cache "nilkey"
        (lambda (val) (setq found (list :found val)))
        (lambda () (ert-fail "on-not-found called — nil value confused with miss")))
      (should (equal found '(:found nil)))))

  (nskk-it "nskk-cache-lru-invalidate/k calls on-found with t for an existing key"
    (let ((cache (nskk-cache-create :type 'lru :capacity 10))
          (result nil))
      (nskk-cache-lru-put cache "k" "v")
      (nskk-cache-lru-invalidate/k cache "k"
        (lambda (val) (setq result val))
        (lambda () (ert-fail "on-not-found called unexpectedly")))
      (should (eq result t))))

  (nskk-it "nskk-cache-lru-invalidate/k calls on-not-found for a missing key"
    (let ((cache (nskk-cache-create :type 'lru :capacity 10))
          (missed nil))
      (nskk-cache-lru-invalidate/k cache "absent"
        (lambda (_) (ert-fail "on-found called unexpectedly"))
        (lambda () (setq missed t)))
      (should missed))))

(nskk-describe "LFU /k variants: direct continuation tests"
  (nskk-it "nskk-cache-lfu-get/k calls on-found with the stored value"
    (let ((cache (nskk-cache-create :type 'lfu :capacity 10))
          (result nil))
      (nskk-cache-lfu-put cache "k" "v")
      (nskk-cache-lfu-get/k cache "k"
        (lambda (val) (setq result val))
        (lambda () (ert-fail "on-not-found called unexpectedly")))
      (should (equal result "v"))))

  (nskk-it "nskk-cache-lfu-get/k calls on-not-found on miss"
    (let ((cache (nskk-cache-create :type 'lfu :capacity 10))
          (missed nil))
      (nskk-cache-lfu-get/k cache "absent"
        (lambda (_) (ert-fail "on-found called unexpectedly"))
        (lambda () (setq missed t)))
      (should missed)))

  (nskk-it "nskk-cache-lfu-get/k calls on-found even when stored value is nil"
    (let ((cache (nskk-cache-create :type 'lfu :capacity 10))
          (found nil))
      (nskk-cache-lfu-put cache "nilkey" nil)
      (nskk-cache-lfu-get/k cache "nilkey"
        (lambda (val) (setq found (list :found val)))
        (lambda () (ert-fail "on-not-found called — nil value confused with miss")))
      (should (equal found '(:found nil)))))

  (nskk-it "nskk-cache-lfu-invalidate/k calls on-found with t for an existing key"
    (let ((cache (nskk-cache-create :type 'lfu :capacity 10))
          (result nil))
      (nskk-cache-lfu-put cache "k" "v")
      (nskk-cache-lfu-invalidate/k cache "k"
        (lambda (val) (setq result val))
        (lambda () (ert-fail "on-not-found called unexpectedly")))
      (should (eq result t))))

  (nskk-it "nskk-cache-lfu-invalidate/k calls on-not-found for a missing key"
    (let ((cache (nskk-cache-create :type 'lfu :capacity 10))
          (missed nil))
      (nskk-cache-lfu-invalidate/k cache "absent"
        (lambda (_) (ert-fail "on-found called unexpectedly"))
        (lambda () (setq missed t)))
      (should missed))))

(nskk-describe "Unified /k variants: direct continuation tests"
  (nskk-it "nskk-cache-invalidate/k calls on-not-found for a missing key"
    (let ((cache (nskk-cache-create :type 'lru :capacity 10))
          (missed nil))
      (nskk-cache-invalidate/k cache "never-inserted"
        (lambda (_) (ert-fail "on-found called unexpectedly"))
        (lambda () (setq missed t)))
      (should missed)))

  (nskk-it "nskk-cache-invalidate-pattern/k calls on-found with the deleted key list"
    (let ((cache (nskk-cache-create :type 'lru :capacity 10))
          (result :unset))
      (nskk-cache-put cache "abc" 1)
      (nskk-cache-put cache "abd" 2)
      (nskk-cache-put cache "xyz" 3)
      (nskk-cache-invalidate-pattern/k cache "^ab"
        (lambda (keys) (setq result keys))
        (lambda () (ert-fail "on-not-found called unexpectedly")))
      (should (equal (sort result #'string<) '("abc" "abd")))))

  (nskk-it "nskk-cache-p/k calls on-found for a valid LRU cache"
    (let ((cache (nskk-cache-create :type 'lru :capacity 10))
          (found nil))
      (nskk-cache-p/k cache
        (lambda (v) (setq found v))
        (lambda () (ert-fail "on-not-found called unexpectedly")))
      (should (eq found t))))

  (nskk-it "nskk-cache-p/k calls on-not-found for a non-cache value"
    (let ((missed nil))
      (nskk-cache-p/k "not-a-cache"
        (lambda (_) (ert-fail "on-found called unexpectedly"))
        (lambda () (setq missed t)))
      (should missed)))

  (nskk-it "nskk-cache-hit-rate/k calls on-found with a float"
    (let ((cache (nskk-cache-create :type 'lru :capacity 10))
          (result nil))
      (nskk-cache-put cache "k" "v")
      (nskk-cache-get cache "k")    ; hit
      (nskk-cache-get cache "miss") ; miss
      (nskk-cache-hit-rate/k cache
        (lambda (r) (setq result r))
        (lambda () (ert-fail "on-not-found called unexpectedly")))
      (should (floatp result))
      (should (= result 0.5))))

  (nskk-it "nskk-cache-stats/k calls on-found with the stats plist"
    (let ((cache (nskk-cache-create :type 'lru :capacity 42))
          (result nil))
      (nskk-cache-stats/k cache
        (lambda (s) (setq result s))
        (lambda () (ert-fail "on-not-found called unexpectedly")))
      (should (plist-get result :type))
      (should (= (plist-get result :capacity) 42))
      (should (= (plist-get result :size) 0))
      (should (= (plist-get result :hits) 0))
      (should (= (plist-get result :misses) 0))
      (should (= (plist-get result :hit-rate) 0.0)))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Hit rate
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-describe "nskk-cache-hit-rate"
  (nskk-it "returns 0.0 with no accesses"
    (let ((cache (nskk-cache-lru-create 10)))
      (should (= (nskk-cache-hit-rate cache) 0.0))))

  (nskk-it "returns 1.0 when every access is a hit"
    (let ((cache (nskk-cache-lru-create 10)))
      (nskk-cache-lru-put cache "k" "v")
      (nskk-cache-lru-get cache "k")
      (nskk-cache-lru-get cache "k")
      (should (= (nskk-cache-hit-rate cache) 1.0))))

  (nskk-it "returns correct fractional hit rate"
    (let ((cache (nskk-cache-lru-create 10)))
      (nskk-cache-lru-put cache "k" "v")
      (nskk-cache-lru-get cache "k")     ; hit
      (nskk-cache-lru-get cache "k")     ; hit
      (nskk-cache-lru-get cache "miss1") ; miss
      (nskk-cache-lru-get cache "miss2") ; miss
      (should (= (nskk-cache-hit-rate cache) 0.5)))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Performance benchmarks
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-deftest-performance cache-performance-lru-put-get-1000
  "LRU put and get 1000 entries must each complete within their time budget."
  (let ((cache (nskk-cache-lru-create 1000)))
    (let ((start (current-time)))
      (dotimes (i 1000)
        (nskk-cache-lru-put cache (format "key%d" i) (format "value%d" i)))
      (let ((put-ms (* 1000 (float-time (time-subtract (current-time) start)))))
        (message "[Performance] LRU put 1000 entries: %.3fms" put-ms)
        (should (< put-ms 200))))   ; < 200 ms total (relaxed for CI)
    (let ((start (current-time)))
      (dotimes (i 1000)
        (nskk-cache-lru-get cache (format "key%d" i)))
      (let ((get-ms (* 1000 (float-time (time-subtract (current-time) start)))))
        (message "[Performance] LRU get 1000 entries: %.3fms" get-ms)
        (should (< get-ms 50))))))  ; < 50 ms total

(nskk-deftest-performance cache-performance-lfu-put-get-1000
  "LFU put and get 1000 entries must each complete within their time budget."
  (let ((cache (nskk-cache-lfu-create 1000)))
    (let ((start (current-time)))
      (dotimes (i 1000)
        (nskk-cache-lfu-put cache (format "key%d" i) (format "value%d" i)))
      (let ((put-ms (* 1000 (float-time (time-subtract (current-time) start)))))
        (message "[Performance] LFU put 1000 entries: %.3fms" put-ms)
        (should (< put-ms 100))))   ; < 100 ms total
    (let ((start (current-time)))
      (dotimes (i 1000)
        (nskk-cache-lfu-get cache (format "key%d" i)))
      (let ((get-ms (* 1000 (float-time (time-subtract (current-time) start)))))
        (message "[Performance] LFU get 1000 entries: %.3fms" get-ms)
        (should (< get-ms 50))))))

(nskk-deftest-performance cache-performance-lru-eviction-900
  "LRU eviction of 900 entries (capacity 100, insert 1000) stays within time budget."
  (let ((cache (nskk-cache-lru-create nskk--test-default-cache-capacity)))
    (let ((start (current-time)))
      (dotimes (i 1000)
        (nskk-cache-lru-put cache (format "key%d" i) (format "value%d" i)))
      (let ((ms (* 1000 (float-time (time-subtract (current-time) start)))))
        (message "[Performance] LRU with 900 evictions: %.3fms" ms)
        (should (< ms 200))))))    ; < 200 ms total

(nskk-deftest-performance cache-performance-unified-prolog-dispatch-500
  "Unified interface Prolog dispatch overhead for 500 gets stays within budget."
  (let ((cache (nskk-cache-create :type 'lru :capacity 1000)))
    (dotimes (i 500)
      (nskk-cache-put cache (format "key%d" i) (format "value%d" i)))
    (let ((start (current-time)))
      (dotimes (i 500)
        (nskk-cache-get cache (format "key%d" i)))
      (let ((ms (* 1000 (float-time (time-subtract (current-time) start)))))
        (message "[Performance] Unified get 500 (Prolog dispatch): %.3fms" ms)
        (should (< ms 50))))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Property-based tests: invariants that must hold across all inputs
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-describe "PBT: LRU cache invariants"
  (nskk-it "get-after-put returns the stored value (LRU)"
    (nskk-property-test lru-get-after-put
      ((key   romaji-string)
       (value romaji-string))
      (let ((cache (nskk-cache-lru-create nskk--test-default-cache-capacity)))
        (nskk-cache-lru-put cache key value)
        (equal (nskk-cache-lru-get cache key) value))
      50))

  (nskk-it "0 <= size <= capacity after random inserts (LRU)"
    (nskk-property-test lru-size-invariant-random
      ((key (nskk-gen-romaji-string)))
      (let ((cache (nskk-cache-create :type 'lru :capacity 10)))
        (nskk-cache-put cache key "v")
        (cl-loop for i from 1 to 20 do
                 (nskk-cache-put cache (format "key%d" i) i))
        (<= 0 (nskk-cache-size cache) 10))
      30))

  (nskk-it "hit rate is always in [0.0, 1.0] (LRU)"
    (nskk-property-test lru-hit-rate-range
      ((key romaji-string))
      (let ((cache (nskk-cache-lru-create 10)))
        (nskk-cache-lru-put cache key "v")
        (nskk-cache-lru-get cache key)
        (nskk-cache-lru-get cache "missing")
        (let ((rate (nskk-cache-hit-rate cache)))
          (and (>= rate 0.0) (<= rate 1.0))))
      30))

  (nskk-it "clear always yields size 0 (LRU)"
    (nskk-property-test lru-clear-empty
      ((n romaji-string))
      (let ((cache (nskk-cache-lru-create 50)))
        (dotimes (i 30)
          (nskk-cache-lru-put cache (format "key%d" i) "v"))
        (nskk-cache-lru-clear cache)
        (= (nskk-cache-lru-size cache) 0))
      20))

  (nskk-it "invalidate removes key and decrements size (LRU)"
    (nskk-property-test lru-invalidate-removes-key
      ((key (nskk-gen-romaji-string)))
      (let ((cache (nskk-cache-create :type 'lru :capacity 100)))
        (nskk-cache-put cache key "val")
        (let ((size-before (nskk-cache-size cache)))
          (nskk-cache-invalidate cache key)
          (and (null (nskk-cache-get cache key))
               (= (nskk-cache-size cache) (1- size-before)))))
      30)))

(nskk-describe "PBT: LFU cache invariants"
  (nskk-it "get-after-put returns the stored value (LFU)"
    (nskk-property-test lfu-get-after-put
      ((key   romaji-string)
       (value romaji-string))
      (let ((cache (nskk-cache-lfu-create nskk--test-default-cache-capacity)))
        (nskk-cache-lfu-put cache key value)
        (equal (nskk-cache-lfu-get cache key) value))
      50))

  (nskk-it "0 <= size <= capacity after random inserts (LFU)"
    (nskk-property-test lfu-size-invariant-random
      ((key (nskk-gen-romaji-string)))
      (let ((cache (nskk-cache-create :type 'lfu :capacity 10)))
        (nskk-cache-put cache key "v")
        (cl-loop for i from 1 to 20 do
                 (nskk-cache-put cache (format "key%d" i) i))
        (<= 0 (nskk-cache-size cache) 10))
      30))

  (nskk-it "hit rate is always in [0.0, 1.0] (LFU)"
    (nskk-property-test lfu-hit-rate-range
      ((key romaji-string))
      (let ((cache (nskk-cache-lfu-create 10)))
        (nskk-cache-lfu-put cache key "v")
        (nskk-cache-lfu-get cache key)
        (nskk-cache-lfu-get cache "missing")
        (let ((rate (nskk-cache-hit-rate cache)))
          (and (>= rate 0.0) (<= rate 1.0))))
      30))

  (nskk-it "clear always yields size 0 (LFU)"
    (nskk-property-test lfu-clear-empty
      ((n romaji-string))
      (let ((cache (nskk-cache-lfu-create 50)))
        (dotimes (i 30)
          (nskk-cache-lfu-put cache (format "key%d" i) "v"))
        (nskk-cache-lfu-clear cache)
        (= (nskk-cache-lfu-size cache) 0))
      20))

  (nskk-it "invalidate removes key and decrements size (LFU)"
    (nskk-property-test lfu-invalidate-removes-key
      ((key (nskk-gen-romaji-string)))
      (let ((cache (nskk-cache-create :type 'lfu :capacity 100)))
        (nskk-cache-put cache key "val")
        (let ((size-before (nskk-cache-size cache)))
          (nskk-cache-invalidate cache key)
          (and (null (nskk-cache-get cache key))
               (= (nskk-cache-size cache) (1- size-before)))))
      30)))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Integration workflows
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-describe "cache integration workflows"
  (nskk-it "LRU cache simulates Japanese dictionary lookup caching"
    (let ((cache (nskk-cache-lru-create 5)))
      (nskk-given
       (nskk-cache-lru-put cache "かんじ"   "漢字")
       (nskk-cache-lru-put cache "にほんご" "日本語")
       (nskk-cache-lru-put cache "いぬ"     "犬"))
      (nskk-then
       (should (string= (nskk-cache-lru-get cache "かんじ")   "漢字"))
       (should (string= (nskk-cache-lru-get cache "にほんご") "日本語")))
      ;; Add more entries, trigger eviction
      (nskk-when
       (nskk-cache-lru-put cache "ねこ"   "猫")
       (nskk-cache-lru-put cache "とり"   "鳥")
       (nskk-cache-lru-put cache "さかな" "魚"))
      (nskk-then
       (should (string= (nskk-cache-lru-get cache "ねこ") "猫"))
       (should (= (plist-get (nskk-cache-stats cache) :size) 5)))))

  (nskk-it "LFU cache retains frequently accessed entries under pressure"
    (let ((cache (nskk-cache-lfu-create 5)))
      (nskk-given
       (nskk-cache-lfu-put cache "common1" "value1")
       (nskk-cache-lfu-put cache "common2" "value2")
       (nskk-cache-lfu-put cache "rare1"   "value3")
       ;; Raise frequency of common entries
       (dotimes (_ 10) (nskk-cache-lfu-get cache "common1"))
       (dotimes (_ 5)  (nskk-cache-lfu-get cache "common2")))
      (nskk-when
       (nskk-cache-lfu-put cache "rare2" "value4")
       (nskk-cache-lfu-put cache "rare3" "value5")
       (nskk-cache-lfu-put cache "rare4" "value6"))
      (nskk-then
       (should (string= (nskk-cache-lfu-get cache "common1") "value1"))
       (should (string= (nskk-cache-lfu-get cache "common2") "value2"))
       (let ((stats (nskk-cache-stats cache)))
         (should (> (plist-get stats :hits) 15))
         (should (= (plist-get stats :size) 5)))))))

;;;
;;; nskk-cache-invalidate (top-level single-key removal)
;;;

(nskk-describe "nskk-cache-invalidate"
  (nskk-it "removes a key from an LRU cache and returns t"
    (let ((cache (nskk-cache-create :type 'lru :capacity 8)))
      (nskk-cache-put cache "key" "value")
      (should (nskk-cache-invalidate cache "key"))
      (should (null (nskk-cache-get cache "key")))))

  (nskk-it "returns nil when the key does not exist"
    (let ((cache (nskk-cache-create :type 'lru :capacity 8)))
      (should (null (nskk-cache-invalidate cache "missing")))))

  (nskk-it "removes a key from an LFU cache and returns t"
    (let ((cache (nskk-cache-create :type 'lfu :capacity 8)))
      (nskk-cache-put cache "key" "value")
      (should (nskk-cache-invalidate cache "key"))
      (should (null (nskk-cache-get cache "key")))))

  (nskk-it "does not affect other keys when one is invalidated"
    (let ((cache (nskk-cache-create :type 'lru :capacity 8)))
      (nskk-cache-put cache "a" "1")
      (nskk-cache-put cache "b" "2")
      (nskk-cache-invalidate cache "a")
      (should (null   (nskk-cache-get cache "a")))
      (should (equal "2" (nskk-cache-get cache "b"))))))

;;;
;;; nskk-cache-dispatch (macro) / nskk--cache-dispatch-prolog
;;;

(nskk-describe "nskk-cache-dispatch"
  (nskk-it "is a macro"
    (should (macrop 'nskk-cache-dispatch))))

(nskk-describe "nskk--cache-dispatch-prolog"
  (nskk-it "dispatches 'get to the lru-get function"
    (let ((cache (nskk-cache-lru-create 10)))
      (nskk-cache-lru-put cache "key" "value")
      (should (equal (nskk--cache-dispatch-prolog cache 'get "key") "value"))))

  (nskk-it "dispatches 'put to the lru-put function"
    (let ((cache (nskk-cache-lru-create 10)))
      (nskk--cache-dispatch-prolog cache 'put "key" "val")
      (should (equal (nskk-cache-lru-get cache "key") "val"))))

  (nskk-it "signals error for an unknown operation type"
    (let ((cache (nskk-cache-lru-create 10)))
      (should-error (nskk--cache-dispatch-prolog cache 'unknown-op-xyz "key")))))

;;;
;;; nskk-cache-field (macro) / nskk--cache-field-prolog
;;;

(nskk-describe "nskk-cache-field"
  (nskk-it "is a macro"
    (should (macrop 'nskk-cache-field))))

(nskk-describe "nskk--cache-field-prolog"
  (nskk-it "reads the capacity field from an LRU cache"
    (let ((cache (nskk-cache-lru-create 42)))
      (should (= (nskk--cache-field-prolog cache 'capacity 0) 42))))

  (nskk-it "reads the capacity field from an LFU cache"
    (let ((cache (nskk-cache-lfu-create 16)))
      (should (= (nskk--cache-field-prolog cache 'capacity 0) 16))))

  (nskk-it "returns default when field is unknown"
    (let ((cache (nskk-cache-lru-create 10)))
      (should (= (nskk--cache-field-prolog cache 'nonexistent-field 99) 99)))))

;;;
;;; LRU doubly-linked list internals
;;;

(nskk-describe "nskk-cache-lru--add-to-head"
  (nskk-it "inserts a node immediately after the dummy head"
    (let* ((cache (nskk-cache-lru-create 10))
           (node (nskk-cache-lru-node--create :key "k" :value "v")))
      (nskk-cache-lru--add-to-head cache node)
      (let ((head (nskk-cache-lru-head cache)))
        (should (eq (nskk-cache-lru-node-next head) node))
        (should (eq (nskk-cache-lru-node-prev node) head)))))

  (nskk-it "links the new node to the previous first node"
    (let* ((cache (nskk-cache-lru-create 10))
           (node1 (nskk-cache-lru-node--create :key "k1" :value "v1"))
           (node2 (nskk-cache-lru-node--create :key "k2" :value "v2")))
      (nskk-cache-lru--add-to-head cache node1)
      (nskk-cache-lru--add-to-head cache node2)
      ;; node2 should be at head, node1 second
      (let ((head (nskk-cache-lru-head cache)))
        (should (eq (nskk-cache-lru-node-next head) node2))
        (should (eq (nskk-cache-lru-node-next node2) node1))))))

(nskk-describe "nskk-cache-lru--remove-node"
  (nskk-it "removes a node from the doubly-linked list"
    (let* ((cache (nskk-cache-lru-create 10))
           (node (nskk-cache-lru-node--create :key "k" :value "v")))
      (nskk-cache-lru--add-to-head cache node)
      (nskk-cache-lru--remove-node node)
      ;; After removal, head and tail should point directly to each other
      (let ((head (nskk-cache-lru-head cache))
            (tail (nskk-cache-lru-tail cache)))
        (should (eq (nskk-cache-lru-node-next head) tail))
        (should (eq (nskk-cache-lru-node-prev tail) head))))))

(nskk-describe "nskk-cache-lru--move-to-head"
  (nskk-it "moves an existing node to the most-recently-used position"
    (let* ((cache (nskk-cache-lru-create 10))
           (node1 (nskk-cache-lru-node--create :key "k1" :value "v1"))
           (node2 (nskk-cache-lru-node--create :key "k2" :value "v2")))
      ;; Add node1 then node2; node2 is at MRU, node1 is second
      (nskk-cache-lru--add-to-head cache node1)
      (nskk-cache-lru--add-to-head cache node2)
      ;; Move node1 to MRU position
      (nskk-cache-lru--move-to-head cache node1)
      (let ((head (nskk-cache-lru-head cache)))
        (should (eq (nskk-cache-lru-node-next head) node1))))))

(nskk-describe "nskk-cache-lru--remove-tail"
  (nskk-it "removes and returns the least-recently-used node"
    (let* ((cache (nskk-cache-lru-create 10))
           (node1 (nskk-cache-lru-node--create :key "k1" :value "v1"))
           (node2 (nskk-cache-lru-node--create :key "k2" :value "v2")))
      ;; node1 added first → becomes LRU; node2 added second → MRU
      (nskk-cache-lru--add-to-head cache node1)
      (nskk-cache-lru--add-to-head cache node2)
      (let ((removed (nskk-cache-lru--remove-tail cache)))
        (should (eq removed node1))))))

;;;
;;; nskk-cache-lfu--update-freq
;;;

(nskk-describe "nskk-cache-lfu--update-freq"
  (nskk-it "promotes an entry from one frequency bucket to the next"
    (let ((cache (nskk-cache-lfu-create 10)))
      (nskk-cache-lfu-put cache "key" "value")
      ;; Entry starts at frequency 1
      (let* ((entry (gethash "key" (nskk-cache-lfu-hash cache)))
             (old-freq (nskk-cache-lfu-entry-frequency entry)))
        (cl-incf (nskk-cache-lfu-entry-frequency entry))
        (nskk-cache-lfu--update-freq cache entry old-freq)
        ;; Entry should now be in frequency bucket 2 (bucket is now a hash-table)
        (let ((freq-2-bucket (gethash 2 (nskk-cache-lfu-freq cache))))
          (should (hash-table-p freq-2-bucket))
          (should (gethash "key" freq-2-bucket))))))

  (nskk-it "handles nil old-freq (new-entry path — no bucket removal)"
    (let* ((cache (nskk-cache-lfu-create 10))
           (entry (nskk-cache-lfu-entry--create :key "k" :value "v" :frequency 1)))
      ;; old-freq nil means first insertion — should not error
      (nskk-cache-lfu--update-freq cache entry nil)
      ;; entry should be in bucket 1
      (let* ((bucket (gethash 1 (nskk-cache-lfu-freq cache))))
        (should bucket)
        (should (gethash "k" bucket)))))

  (nskk-it "advances min-freq when emptied bucket was the minimum"
    (let* ((cache (nskk-cache-lfu-create 10))
           ;; manually set up: one entry at freq 1
           (entry (nskk-cache-lfu-entry--create :key "k" :value "v" :frequency 2)))
      (puthash "k" entry (nskk-cache-lfu-hash cache))
      ;; create bucket at freq 1 with just this key, set min-freq=1
      (let ((b1 (make-hash-table :test 'equal :size 4)))
        (puthash "k" t b1)
        (puthash 1 b1 (nskk-cache-lfu-freq cache)))
      (setf (nskk-cache-lfu-min-freq cache) 1)
      ;; update-freq: remove from freq 1, add to freq 2
      (nskk-cache-lfu--update-freq cache entry 1)
      ;; freq 1 bucket should be gone, min-freq should advance to 2
      (should-not (gethash 1 (nskk-cache-lfu-freq cache)))
      (should (= 2 (nskk-cache-lfu-min-freq cache)))))

  (nskk-it "removing one entry does not destroy a multi-entry bucket"
    (let* ((cache (nskk-cache-lfu-create 10))
           (e1 (nskk-cache-lfu-entry--create :key "k1" :value 1 :frequency 2))
           (_e2 (nskk-cache-lfu-entry--create :key "k2" :value 2 :frequency 2)))
      ;; set up freq 1 bucket with both k1 and k2
      (let ((b1 (make-hash-table :test 'equal :size 4)))
        (puthash "k1" t b1)
        (puthash "k2" t b1)
        (puthash 1 b1 (nskk-cache-lfu-freq cache)))
      (setf (nskk-cache-lfu-min-freq cache) 1)
      ;; promote k1: remove from freq 1, add to freq 2
      (nskk-cache-lfu--update-freq cache e1 1)
      ;; freq 1 bucket should still exist (k2 is still there)
      (let ((b1-after (gethash 1 (nskk-cache-lfu-freq cache))))
        (should b1-after)
        (should (gethash "k2" b1-after))
        (should-not (gethash "k1" b1-after))))))

(provide 'nskk-cache-test)

;;; nskk-cache-test.el ends here
