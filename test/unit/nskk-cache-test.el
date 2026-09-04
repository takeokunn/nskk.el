;;; nskk-cache-test.el --- Cache implementation tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, cache, test

;; This file is part of NSKK.

;;; Commentary:

;; Cache implementation tests.

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

(defconst nskk--test-default-cache-capacity 100
  "Default cache capacity used in cache unit tests.")

(defun nskk-cache-test--call (type suffix &rest args)
  "Apply ARGS to the TYPE-specific cache function named SUFFIX."
  (apply (intern (format "nskk-cache-%s-%s" type suffix)) args))

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
        (should (nskk-prolog-query-one `(cache-dispatch-fn lfu ,op \?fn)))))))

(nskk-deftest-table cache-dispatch-fn-resolution
  :columns (type op expected-fn)
  :rows    ((lru get        nskk-cache-lru-get)
            (lru put        nskk-cache-lru-put)
            (lru invalidate nskk-cache-lru-invalidate)
            (lru clear      nskk-cache-lru-clear)
            (lru size       nskk-cache-lru-size)
            (lfu get        nskk-cache-lfu-get)
            (lfu put        nskk-cache-lfu-put)
            (lfu invalidate nskk-cache-lfu-invalidate)
            (lfu clear      nskk-cache-lfu-clear)
            (lfu size       nskk-cache-lfu-size))
  :body
  (nskk-prolog-test-with-isolated-db
    (should (eq (nskk-prolog-query-value
                 `(cache-dispatch-fn ,type ,op \?fn) '\?fn)
                expected-fn))))

(nskk-deftest-table cache-constructor-dispatch-spot-check
  :columns (type expected-fn)
  :rows ((lru nskk-cache-lru-create)
         (lfu nskk-cache-lfu-create))
  :body
  (nskk-prolog-test-with-isolated-db
    (should (eq expected-fn
                (nskk-prolog-query-value
                 `(cache-constructor ,type \?fn) '\?fn)))))

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
  (nskk-it "initializes head/tail sentinel nodes as a doubly-linked pair"
    (let* ((cache (nskk-cache-lru-create 10))
           (head  (nskk-cache-lru-head cache))
           (tail  (nskk-cache-lru-tail cache)))
      (should (eq (nskk-cache-lru-node-next head) tail))
      (should (eq (nskk-cache-lru-node-prev tail) head))
      (should (null (nskk-cache-lru-node-prev head)))
      (should (null (nskk-cache-lru-node-next tail))))))

(nskk-deftest-table cache-create-rejects-invalid-capacity
  :columns (type)
  :rows    ((lru) (lfu))
  :body
  (dolist (capacity '(0 -1 nil 1.5 "10" t))
    (should-error (nskk-cache-test--call type "create" capacity) :type 'user-error)))

(nskk-deftest-table lru-creation
  :columns (capacity)
  :rows    ((10) (50) (100) (1000))
  :body
  (let ((cache (nskk-cache-lru-create capacity)))
    (should (nskk-cache-lru-p cache))
    (should (= (nskk-cache-lru-capacity cache) capacity))
    (should (= (nskk-cache-lru-size cache) 0))
    (should (= (nskk-cache-lru-hits cache) 0))
    (should (= (nskk-cache-lru-misses cache) 0))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; LRU cache: basic operations
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-deftest-table cache-basic-store-retrieve-single
  :columns (type)
  :rows    ((lru) (lfu))
  :body
  (let ((cache (nskk-cache-test--call type "create" nskk--test-default-cache-capacity)))
    (nskk-cache-test--call type "put" cache "key1" "value1")
    (should (string= (nskk-cache-test--call type "get" cache "key1") "value1"))))

(nskk-deftest-table cache-basic-store-retrieve-multiple
  :columns (type)
  :rows    ((lru) (lfu))
  :body
  (let ((cache (nskk-cache-test--call type "create" nskk--test-default-cache-capacity)))
    (nskk-cache-test--call type "put" cache "key1" "value1")
    (nskk-cache-test--call type "put" cache "key2" "value2")
    (nskk-cache-test--call type "put" cache "key3" "value3")
    (should (string= (nskk-cache-test--call type "get" cache "key1") "value1"))
    (should (string= (nskk-cache-test--call type "get" cache "key2") "value2"))
    (should (string= (nskk-cache-test--call type "get" cache "key3") "value3"))))

(nskk-deftest-table cache-basic-miss-returns-nil
  :columns (type)
  :rows    ((lru) (lfu))
  :body
  (let ((cache (nskk-cache-test--call type "create" nskk--test-default-cache-capacity)))
    (nskk-cache-test--call type "put" cache "key1" "value1")
    (should (null (nskk-cache-test--call type "get" cache "missing-key")))))

(nskk-deftest-table cache-basic-update-in-place-keeps-size
  :columns (type)
  :rows    ((lru) (lfu))
  :body
  (let ((cache (nskk-cache-test--call type "create" nskk--test-default-cache-capacity)))
    (nskk-cache-test--call type "put" cache "key1" "value1")
    (nskk-cache-test--call type "put" cache "key1" "value2")
    (should (string= (nskk-cache-test--call type "get" cache "key1") "value2"))
    (should (= (nskk-cache-test--call type "size" cache) 1))))

(nskk-deftest-table cache-basic-invalidate-removes-and-returns-t
  :columns (type)
  :rows    ((lru) (lfu))
  :body
  (let ((cache (nskk-cache-test--call type "create" nskk--test-default-cache-capacity)))
    (nskk-cache-test--call type "put" cache "key1" "value1")
    (nskk-cache-test--call type "put" cache "key2" "value2")
    (should (eq t (nskk-cache-test--call type "invalidate" cache "key1")))
    (should (null (nskk-cache-test--call type "get" cache "key1")))
    (should (string= (nskk-cache-test--call type "get" cache "key2") "value2"))))

(nskk-deftest-table cache-basic-invalidate-missing-returns-nil
  :columns (type)
  :rows    ((lru) (lfu))
  :body
  (let ((cache (nskk-cache-test--call type "create" nskk--test-default-cache-capacity)))
    (nskk-cache-test--call type "put" cache "key1" "value1")
    (should (null (nskk-cache-test--call type "invalidate" cache "missing")))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; LRU cache: eviction
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-describe "LRU cache eviction"
  (nskk-it "evicts the least recently used entry when full"
    (let ((cache (nskk-cache-lru-create 3)))
      (nskk-given
       (nskk-cache-lru-put cache "key1" "value1")
       (nskk-cache-lru-put cache "key2" "value2")
       (nskk-cache-lru-put cache "key3" "value3"))
      (nskk-when
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
      (nskk-cache-lru-put cache "k1" "v1")
      (nskk-cache-lru-put cache "k2" "v2")
      (nskk-cache-lru-clear cache)
      (should (eq (nskk-cache-lru-node-next head) tail))
      (should (eq (nskk-cache-lru-node-prev tail) head)))))

(nskk-describe "nskk-cache-stats: fresh cache"
  (nskk-it "returns an all-zero stats plist for a cache with no accesses"
    (let* ((cache (nskk-cache-create :type 'lru :capacity 42))
           (result (nskk-cache-stats cache)))
      (should (plist-get result :type))
      (should (= (plist-get result :capacity) 42))
      (should (= (plist-get result :size) 0))
      (should (= (plist-get result :hits) 0))
      (should (= (plist-get result :misses) 0))
      (should (= (plist-get result :hit-rate) 0.0)))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; LFU cache: creation
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-deftest-table lfu-creation
  :columns (capacity)
  :rows    ((10) (50) (100) (1000))
  :body
  (let ((cache (nskk-cache-lfu-create capacity)))
    (should (nskk-cache-lfu-p cache))
    (should (= (nskk-cache-lfu-capacity cache) capacity))
    (should (= (nskk-cache-lfu-size cache) 0))
    (should (= (nskk-cache-lfu-hits cache) 0))
    (should (= (nskk-cache-lfu-misses cache) 0))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; LFU cache: basic operations
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-describe "LFU cache basic operations"
  (nskk-it "increments frequency when updating an existing entry"
    (let ((cache (nskk-cache-lfu-create nskk--test-default-cache-capacity)))
      (nskk-given
       (nskk-cache-lfu-put cache "key1" "value1")
       (nskk-cache-lfu-put cache "key1" "value2"))
      (nskk-then
       (let* ((entry (gethash "key1" (nskk-cache-lfu-hash cache)))
              (freq  (nskk-cache-lfu-entry-frequency entry)))
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
          (should (member result '("k1" "k2"))))))

    (nskk-it "calls on-not-found for an empty bucket"
      (let ((bucket (make-hash-table :test 'equal))
            (called nil))
        (nskk-cache-lfu--bucket-any-key/k bucket
          (lambda (_k) (setq called :found))
          (lambda () (setq called :not-found)))
        (should (eq called :not-found))))

    (nskk-it "stops after one callback in a large same-frequency bucket"
      (let* ((entry-count 4096)
             (cache (nskk-cache-lfu-create entry-count))
             (callback-count 0)
             (original-maphash (symbol-function 'maphash)))
        (dotimes (key entry-count)
          (nskk-cache-lfu-put cache key key))
        (should
         (= (hash-table-count
             (gethash 1 (nskk-cache-lfu-freq cache)))
            entry-count))
        (cl-letf
            (((symbol-function 'maphash)
              (lambda (function table)
                (funcall
                 original-maphash
                 (lambda (key value)
                   (cl-incf callback-count)
                   (funcall function key value))
                 table))))
          (nskk-cache-lfu--evict-min-freq cache))
        (should (= callback-count 1))
        (should (= (nskk-cache-lfu-size cache) (1- entry-count))))))

  (nskk-describe "nskk-cache-lfu--evict-min-freq"
    (nskk-it "evicts one entry and decrements size"
      (let ((cache (nskk-cache-lfu-create 2)))
        (nskk-cache-lfu-put cache "key1" "v1")
        (nskk-cache-lfu-put cache "key2" "v2")
        (nskk-cache-lfu--evict-min-freq cache)
        (should (= (nskk-cache-lfu-size cache) 1))
        (let ((surviving
               (cl-count-if
                #'identity
                (list
                 (nskk-cache-lfu-get cache "key1")
                 (nskk-cache-lfu-get cache "key2")))))
          (should (= surviving 1)))))

    (nskk-it "is a no-op when the cache is empty"
      (let ((cache (nskk-cache-lfu-create 10)))
        (nskk-cache-lfu--evict-min-freq cache)
        (should (= (nskk-cache-lfu-size cache) 0)))))

  (nskk-it "treats nil as a found key in a non-empty bucket"
    (let ((bucket (make-hash-table :test 'equal))
          (result :unset))
      (puthash nil t bucket)
      (nskk-cache-lfu--bucket-any-key/k bucket
        (lambda (key) (setq result (list :found key)))
        (lambda () (setq result '(:not-found))))
      (should (equal result '(:found nil))))))

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
       (nskk-cache-lfu-get cache "key2")
       (nskk-cache-lfu-get cache "key3")
       (nskk-cache-lfu-get cache "key2"))
      (nskk-when
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
    (let ((cache (nskk-cache-lfu-create 2)))
      (nskk-given
       (nskk-cache-lfu-put cache "key1" "value1")   ; freq 1
       (nskk-cache-lfu-put cache "key2" "value2")   ; freq 1
       (nskk-cache-lfu-get cache "key1")             ; key1 freq → 2
       (nskk-cache-lfu-get cache "key2"))            ; key2 freq → 2; min-freq=2
      (nskk-then
       (should (= (nskk-cache-lfu-min-freq cache) 2)))
      (nskk-when
       (nskk-cache-lfu-put cache "key3" "value3"))
      (nskk-then
       (should (= (nskk-cache-lfu-size cache) 2))
       (should (= (nskk-cache-lfu-min-freq cache) 1))
       (should (string= (nskk-cache-lfu-get cache "key3") "value3")))))

  (nskk-it "get-promoted entries survive when a lower-frequency entry is evicted"
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
    (let ((cache (nskk-cache-lfu-create 3)))
      (nskk-given
       (nskk-cache-lfu-put cache "key1" "value1")
       (nskk-cache-lfu-put cache "key2" "value2")
       (nskk-cache-lfu-put cache "key3" "value3"))
      (nskk-when
       (nskk-cache-lfu-put cache "key4" "value4"))
      (nskk-then
       (should (= (nskk-cache-lfu-size cache) 3))
       (should (string= (nskk-cache-lfu-get cache "key4") "value4"))
       (let ((surviving (cl-count-if #'identity
                          (list (nskk-cache-lfu-get cache "key1")
                                (nskk-cache-lfu-get cache "key2")
                                (nskk-cache-lfu-get cache "key3")))))
         (should (= surviving 2))))))

  (nskk-it "capacity-1 cache evicts a nil key for a non-nil key"
    (let ((cache (nskk-cache-lfu-create 1)))
      (nskk-cache-lfu-put cache nil "nil-value")
      (should (<= (nskk-cache-lfu-size cache)
                  (nskk-cache-lfu-capacity cache)))
      (nskk-cache-lfu-put cache "next" "next-value")
      (should (= (nskk-cache-lfu-size cache) 1))
      (should (<= (nskk-cache-lfu-size cache)
                  (nskk-cache-lfu-capacity cache)))
      (should (= (hash-table-count (nskk-cache-lfu-hash cache)) 1))
      (should (eq (gethash nil (nskk-cache-lfu-hash cache) :missing)
                  :missing))
      (should (equal (nskk-cache-lfu-get cache "next") "next-value"))))

  (nskk-it "capacity-1 cache evicts a non-nil key for a nil key"
    (let ((cache (nskk-cache-lfu-create 1)))
      (nskk-cache-lfu-put cache "first" "first-value")
      (should (<= (nskk-cache-lfu-size cache)
                  (nskk-cache-lfu-capacity cache)))
      (nskk-cache-lfu-put cache nil "nil-value")
      (should (= (nskk-cache-lfu-size cache) 1))
      (should (<= (nskk-cache-lfu-size cache)
                  (nskk-cache-lfu-capacity cache)))
      (should (= (hash-table-count (nskk-cache-lfu-hash cache)) 1))
      (should (eq (gethash "first" (nskk-cache-lfu-hash cache) :missing)
                  :missing))
      (should (equal (nskk-cache-lfu-get cache nil) "nil-value")))))

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

(nskk-deftest-table cache-clear-removes-all-entries
  :columns (type)
  :rows    ((lru) (lfu))
  :body
  (let ((cache (nskk-cache-test--call type "create" nskk--test-default-cache-capacity)))
    (nskk-cache-test--call type "put" cache "key1" "value1")
    (nskk-cache-test--call type "put" cache "key2" "value2")
    (nskk-cache-test--call type "clear" cache)
    (should (null (nskk-cache-test--call type "get" cache "key1")))
    (should (null (nskk-cache-test--call type "get" cache "key2")))
    (should (= (nskk-cache-test--call type "size" cache) 0))))

(nskk-describe "cache management"
  (nskk-it "multiple independent caches do not interfere"
    (let ((cache1 (nskk-cache-lru-create 2))
          (cache2 (nskk-cache-lfu-create 2)))
      (nskk-given
       (nskk-cache-lru-put cache1 "key" "value1")
       (nskk-cache-lfu-put cache2 "key" "value2"))
      (nskk-then
       (should (string= (nskk-cache-lru-get cache1 "key") "value1"))
       (should (string= (nskk-cache-lfu-get cache2 "key") "value2")))
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
          (should (member "dict:ka" deleted))
          (should (member "dict:ki" deleted))
          (should-not (member "other:foo" deleted))
          (should (null (nskk-cache-lru-get cache "dict:ka")))
          (should (null (nskk-cache-lru-get cache "dict:ki")))
          (should (string= (nskk-cache-lru-get cache "other:foo") "bar"))
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

  (nskk-it "matches only string keys in LRU and LFU caches"
    (dolist (cache (list (nskk-cache-lru-create 8)
                         (nskk-cache-lfu-create 8)))
      (nskk-cache-put cache nil "nil-value")
      (nskk-cache-put cache 'symbol-key "symbol-value")
      (nskk-cache-put cache "dict:match" "matched-value")
      (nskk-cache-put cache "other:string" "other-value")
      (let ((deleted (nskk-cache-invalidate-pattern cache "^dict:")))
        (should (equal deleted '("dict:match")))
        (should (equal (nskk-cache-get cache nil) "nil-value"))
        (should (equal (nskk-cache-get cache 'symbol-key) "symbol-value"))
        (should (equal (nskk-cache-get cache "other:string") "other-value"))
        (should (null (nskk-cache-get cache "dict:match")))
        (should (= (nskk-cache-size cache) 3)))))

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
;;; Vector cache keys
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-deftest-table cache-vector-key-structural-equality
  :columns (type)
  :rows    ((lru) (lfu))
  :body
  (let ((cache (nskk-cache-test--call type "create" 10))
        (key (vector "a" "b" 3))
        (lookup (vector "a" "b" 3)))
    (should-not (eq key lookup))
    (nskk-cache-test--call type "put" cache key "vector-value")
    (should (equal (nskk-cache-test--call type "get" cache lookup) "vector-value"))))

(nskk-deftest-table cache-vector-key-different-lengths
  :columns (type)
  :rows    ((lru) (lfu))
  :body
  (let ((cache (nskk-cache-test--call type "create" 10)))
    (nskk-cache-test--call type "put" cache (vector 1 2) "short")
    (nskk-cache-test--call type "put" cache (vector 1 2 3) "long")
    (should (equal (nskk-cache-test--call type "get" cache (vector 1 2)) "short"))
    (should (equal (nskk-cache-test--call type "get" cache (vector 1 2 3)) "long"))
    (should (= (nskk-cache-test--call type "size" cache) 2))))

(nskk-deftest-table cache-vector-key-cyclic-self-reference
  :columns (type)
  :rows    ((lru) (lfu))
  :body
  (let* ((cache (nskk-cache-test--call type "create" 10))
         (key (make-vector 2 nil))
         (lookup (make-vector 2 nil)))
    (aset key 0 "self")
    (aset key 1 key)
    (aset lookup 0 "self")
    (aset lookup 1 lookup)
    (nskk-cache-test--call type "put" cache key "cyclic-vector-value")
    (should (equal (nskk-cache-test--call type "get" cache lookup)
                    "cyclic-vector-value"))))

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
       (should (nskk-cache-lfu-p cache)))))

  (nskk-it "rejects invalid :capacity and :size values"
    (dolist (type '(lru lfu))
      (dolist (capacity '(0 -1 nil 1.5 "10" t))
        (should-error
         (nskk-cache-create :type type :capacity capacity)
         :type 'user-error)
        (should-error
         (nskk-cache-create :type type :size capacity)
         :type 'user-error)))))

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

(nskk-describe "nskk-cache-get-prepared sync wrapper"
  (nskk-it "returns the preparer's result on a hit"
    (let ((cache (nskk-cache-lru-create 10)))
      (nskk-cache-lru-put cache "key" "stored")
      (should (equal (nskk-cache-get-prepared cache "key" #'upcase) "STORED"))))

  (nskk-it "returns nil without calling the preparer on a miss"
    (let ((cache (nskk-cache-lru-create 10)))
      (should (null (nskk-cache-get-prepared
                     cache "missing"
                     (lambda (_v) (error "preparer should not be called"))))))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Direct /k variant calling convention tests
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-deftest-table cache-k-get-hit
  :columns (type)
  :rows    ((lru) (lfu))
  :body
  (let ((cache (nskk-cache-create :type type :capacity 10))
        (result nil))
    (nskk-cache-test--call type "put" cache "k" "v")
    (nskk-cache-test--call type "get/k" cache "k"
      (lambda (val) (setq result val))
      (lambda () (ert-fail "on-not-found called unexpectedly")))
    (should (equal result "v"))))

(nskk-deftest-table cache-k-get-miss
  :columns (type)
  :rows    ((lru) (lfu))
  :body
  (let ((cache (nskk-cache-create :type type :capacity 10))
        (missed nil))
    (nskk-cache-test--call type "get/k" cache "absent"
      (lambda (_) (ert-fail "on-found called unexpectedly"))
      (lambda () (setq missed t)))
    (should missed)))

(nskk-deftest-table cache-k-get-nil-value
  :columns (type)
  :rows    ((lru) (lfu))
  :body
  (let ((cache (nskk-cache-create :type type :capacity 10))
        (found nil))
    (nskk-cache-test--call type "put" cache "nilkey" nil)
    (nskk-cache-test--call type "get/k" cache "nilkey"
      (lambda (val) (setq found (list :found val)))
      (lambda () (ert-fail "on-not-found called — nil value confused with miss")))
    (should (equal found '(:found nil)))))

(nskk-deftest-table cache-k-invalidate-hit
  :columns (type)
  :rows    ((lru) (lfu))
  :body
  (let ((cache (nskk-cache-create :type type :capacity 10))
        (result nil))
    (nskk-cache-test--call type "put" cache "k" "v")
    (nskk-cache-test--call type "invalidate/k" cache "k"
      (lambda (val) (setq result val))
      (lambda () (ert-fail "on-not-found called unexpectedly")))
    (should (eq result t))))

(nskk-deftest-table cache-k-invalidate-miss
  :columns (type)
  :rows    ((lru) (lfu))
  :body
  (let ((cache (nskk-cache-create :type type :capacity 10))
        (missed nil))
    (nskk-cache-test--call type "invalidate/k" cache "absent"
      (lambda (_) (ert-fail "on-found called unexpectedly"))
      (lambda () (setq missed t)))
    (should missed)))

(nskk-describe "Unified /k variants: direct continuation tests"
  (nskk-it "nskk-cache-invalidate/k calls on-not-found for a missing key"
    (let ((cache (nskk-cache-create :type 'lru :capacity 10))
          (missed nil))
      (nskk-cache-invalidate/k cache "never-inserted"
        (lambda (_) (ert-fail "on-found called unexpectedly"))
        (lambda () (setq missed t)))
      (should missed))))

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
;;; Property-based tests: invariants that must hold across all inputs
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-property-test lru-get-after-put
  ((key   romaji-string)
   (value romaji-string))
  (let ((cache (nskk-cache-lru-create nskk--test-default-cache-capacity)))
    (nskk-cache-lru-put cache key value)
    (equal (nskk-cache-lru-get cache key) value))
  50)

(nskk-property-test lru-size-invariant-random
  ((key (nskk-gen-romaji-string)))
  (let ((cache (nskk-cache-create :type 'lru :capacity 10)))
    (nskk-cache-put cache key "v")
    (cl-loop for i from 1 to 20 do
             (nskk-cache-put cache (format "key%d" i) i))
    (<= 0 (nskk-cache-size cache) 10))
  30)

(nskk-property-test lru-hit-rate-range
  ((key romaji-string))
  (let ((cache (nskk-cache-lru-create 10)))
    (nskk-cache-lru-put cache key "v")
    (nskk-cache-lru-get cache key)
    (nskk-cache-lru-get cache "missing")
    (let ((rate (nskk-cache-hit-rate cache)))
      (and (>= rate 0.0) (<= rate 1.0))))
  30)

(nskk-property-test lru-clear-empty
  ((n romaji-string))
  (let ((cache (nskk-cache-lru-create 50)))
    (dotimes (i 30)
      (nskk-cache-lru-put cache (format "key%d" i) "v"))
    (nskk-cache-lru-clear cache)
    (= (nskk-cache-lru-size cache) 0))
  20)

(nskk-property-test lru-invalidate-removes-key
  ((key (nskk-gen-romaji-string)))
  (let ((cache (nskk-cache-create :type 'lru :capacity 100)))
    (nskk-cache-put cache key "val")
    (let ((size-before (nskk-cache-size cache)))
      (nskk-cache-invalidate cache key)
      (and (null (nskk-cache-get cache key))
           (= (nskk-cache-size cache) (1- size-before)))))
  30)

(nskk-property-test lfu-get-after-put
  ((key   romaji-string)
   (value romaji-string))
  (let ((cache (nskk-cache-lfu-create nskk--test-default-cache-capacity)))
    (nskk-cache-lfu-put cache key value)
    (equal (nskk-cache-lfu-get cache key) value))
  50)

(nskk-property-test lfu-size-invariant-random
  ((key (nskk-gen-romaji-string)))
  (let ((cache (nskk-cache-create :type 'lfu :capacity 10)))
    (nskk-cache-put cache key "v")
    (cl-loop for i from 1 to 20 do
             (nskk-cache-put cache (format "key%d" i) i))
    (<= 0 (nskk-cache-size cache) 10))
  30)

(nskk-property-test lfu-hit-rate-range
  ((key romaji-string))
  (let ((cache (nskk-cache-lfu-create 10)))
    (nskk-cache-lfu-put cache key "v")
    (nskk-cache-lfu-get cache key)
    (nskk-cache-lfu-get cache "missing")
    (let ((rate (nskk-cache-hit-rate cache)))
      (and (>= rate 0.0) (<= rate 1.0))))
  30)

(nskk-property-test lfu-clear-empty
  ((n romaji-string))
  (let ((cache (nskk-cache-lfu-create 50)))
    (dotimes (i 30)
      (nskk-cache-lfu-put cache (format "key%d" i) "v"))
    (nskk-cache-lfu-clear cache)
    (= (nskk-cache-lfu-size cache) 0))
  20)

(nskk-property-test lfu-invalidate-removes-key
  ((key (nskk-gen-romaji-string)))
  (let ((cache (nskk-cache-create :type 'lfu :capacity 100)))
    (nskk-cache-put cache key "val")
    (let ((size-before (nskk-cache-size cache)))
      (nskk-cache-invalidate cache key)
      (and (null (nskk-cache-get cache key))
           (= (nskk-cache-size cache) (1- size-before)))))
  30)

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

(nskk-deftest-table cache-invalidate-removes-key-and-returns-t
  :columns (type)
  :rows    ((lru) (lfu))
  :body
  (let ((cache (nskk-cache-create :type type :capacity 8)))
    (nskk-cache-put cache "key" "value")
    (should (nskk-cache-invalidate cache "key"))
    (should (null (nskk-cache-get cache "key")))))

(nskk-describe "nskk-cache-invalidate"
  (nskk-it "returns nil when the key does not exist"
    (let ((cache (nskk-cache-create :type 'lru :capacity 8)))
      (should (null (nskk-cache-invalidate cache "missing")))))

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
      (let ((head (nskk-cache-lru-head cache)))
        (should (eq (nskk-cache-lru-node-next head) node2))
        (should (eq (nskk-cache-lru-node-next node2) node1))))))

(nskk-describe "nskk-cache-lru--remove-node"
  (nskk-it "removes a node from the doubly-linked list"
    (let* ((cache (nskk-cache-lru-create 10))
           (node (nskk-cache-lru-node--create :key "k" :value "v")))
      (nskk-cache-lru--add-to-head cache node)
      (nskk-cache-lru--remove-node node)
      (let ((head (nskk-cache-lru-head cache))
            (tail (nskk-cache-lru-tail cache)))
        (should (eq (nskk-cache-lru-node-next head) tail))
        (should (eq (nskk-cache-lru-node-prev tail) head))))))

(nskk-describe "nskk-cache-lru--move-to-head"
  (nskk-it "moves an existing node to the most-recently-used position"
    (let* ((cache (nskk-cache-lru-create 10))
           (node1 (nskk-cache-lru-node--create :key "k1" :value "v1"))
           (node2 (nskk-cache-lru-node--create :key "k2" :value "v2")))
      (nskk-cache-lru--add-to-head cache node1)
      (nskk-cache-lru--add-to-head cache node2)
      (nskk-cache-lru--move-to-head cache node1)
      (let ((head (nskk-cache-lru-head cache)))
        (should (eq (nskk-cache-lru-node-next head) node1))))))

(nskk-describe "nskk-cache-lru--remove-tail"
  (nskk-it "removes and returns the least-recently-used node"
    (let* ((cache (nskk-cache-lru-create 10))
           (node1 (nskk-cache-lru-node--create :key "k1" :value "v1"))
           (node2 (nskk-cache-lru-node--create :key "k2" :value "v2")))
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
      (let* ((entry (gethash "key" (nskk-cache-lfu-hash cache)))
             (old-freq (nskk-cache-lfu-entry-frequency entry)))
        (cl-incf (nskk-cache-lfu-entry-frequency entry))
        (nskk-cache-lfu--update-freq cache entry old-freq)
        (let ((freq-2-bucket (gethash 2 (nskk-cache-lfu-freq cache))))
          (should (hash-table-p freq-2-bucket))
          (should (gethash "key" freq-2-bucket))))))

  (nskk-it "handles nil old-freq (new-entry path — no bucket removal)"
    (let* ((cache (nskk-cache-lfu-create 10))
           (entry (nskk-cache-lfu-entry--create :key "k" :value "v" :frequency 1)))
      (nskk-cache-lfu--update-freq cache entry nil)
      (let* ((bucket (gethash 1 (nskk-cache-lfu-freq cache))))
        (should bucket)
        (should (gethash "k" bucket)))))

  (nskk-it "advances min-freq when emptied bucket was the minimum"
    (let* ((cache (nskk-cache-lfu-create 10))
           (entry (nskk-cache-lfu-entry--create :key "k" :value "v" :frequency 2)))
      (puthash "k" entry (nskk-cache-lfu-hash cache))
      (let ((b1 (make-hash-table :test 'equal :size 4)))
        (puthash "k" t b1)
        (puthash 1 b1 (nskk-cache-lfu-freq cache)))
      (setf (nskk-cache-lfu-min-freq cache) 1)
      (nskk-cache-lfu--update-freq cache entry 1)
      (should-not (gethash 1 (nskk-cache-lfu-freq cache)))
      (should (= 2 (nskk-cache-lfu-min-freq cache)))))

  (nskk-it "removing one entry does not destroy a multi-entry bucket"
    (let* ((cache (nskk-cache-lfu-create 10))
           (e1 (nskk-cache-lfu-entry--create :key "k1" :value 1 :frequency 2))
           (_e2 (nskk-cache-lfu-entry--create :key "k2" :value 2 :frequency 2)))
      (let ((b1 (make-hash-table :test 'equal :size 4)))
        (puthash "k1" t b1)
        (puthash "k2" t b1)
        (puthash 1 b1 (nskk-cache-lfu-freq cache)))
      (setf (nskk-cache-lfu-min-freq cache) 1)
      (nskk-cache-lfu--update-freq cache e1 1)
      (let ((b1-after (gethash 1 (nskk-cache-lfu-freq cache))))
        (should b1-after)
        (should (gethash "k2" b1-after))
        (should-not (gethash "k1" b1-after))))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Cache snapshot capture and restore
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-describe "nskk-cache-capture-snapshot and nskk-cache-restore-snapshot"
  (nskk-it "restores LRU entries, size, hits, misses, and list endpoints; hash/head/tail are the same objects"
    (let* ((cache (nskk-cache-lru-create 10)))
      (nskk-cache-put cache "a" 1)
      (nskk-cache-put cache "b" 2)
      (nskk-cache-get cache "a")
      (let* ((table (nskk-cache-lru-hash cache))
             (head (nskk-cache-lru-head cache))
             (tail (nskk-cache-lru-tail cache))
             (head-next (nskk-cache-lru-node-next head))
             (tail-prev (nskk-cache-lru-node-prev tail))
             (snapshot (nskk-cache-capture-snapshot cache)))
        (nskk-cache-put cache "c" 3)
        (nskk-cache-invalidate cache "b")
        (nskk-cache-get cache "missing")
        (nskk-cache-restore-snapshot snapshot)
        (should (eq (nskk-cache-lru-hash cache) table))
        (should (eq (nskk-cache-lru-head cache) head))
        (should (eq (nskk-cache-lru-tail cache) tail))
        (should (eq (nskk-cache-lru-node-next head) head-next))
        (should (eq (nskk-cache-lru-node-prev tail) tail-prev))
        (should (= 2 (nskk-cache-lru-size cache)))
        (should (= 1 (nskk-cache-lru-hits cache)))
        (should (= 0 (nskk-cache-lru-misses cache)))
        (should (= 2 (hash-table-count table)))
        (should-not (gethash "c" table))
        (should (equal (nskk-cache-get cache "a") 1))
        (should (equal (nskk-cache-get cache "b") 2)))))

  (nskk-it "restores LFU entries, size, hits, misses, min-freq, and the freq table's top-level entries"
    (let* ((cache (nskk-cache-lfu-create 10)))
      (nskk-cache-put cache "a" 1)
      (nskk-cache-get cache "a")
      (let* ((table (nskk-cache-lfu-hash cache))
             (freq-table (nskk-cache-lfu-freq cache))
             (bucket-2 (gethash 2 freq-table))
             (snapshot (nskk-cache-capture-snapshot cache)))
        (nskk-cache-put cache "b" 2)
        (nskk-cache-restore-snapshot snapshot)
        (should (eq (nskk-cache-lfu-hash cache) table))
        (should (eq (nskk-cache-lfu-freq cache) freq-table))
        (should (= 2 (nskk-cache-lfu-min-freq cache)))
        (should (= 1 (nskk-cache-lfu-size cache)))
        (should (= 1 (nskk-cache-lfu-hits cache)))
        (should (= 0 (nskk-cache-lfu-misses cache)))
        (should (= 1 (hash-table-count table)))
        (should-not (gethash 1 freq-table))
        (should (eq (gethash 2 freq-table) bucket-2))
        (should (equal (nskk-cache-get cache "a") 1))
        (should-not (nskk-cache-get cache "b")))))

  (nskk-it "undoes an eviction: a key evicted after capture is retrievable again once restored"
    (let* ((cache (nskk-cache-lru-create 2)))
      (nskk-cache-put cache "a" 1)
      (nskk-cache-put cache "b" 2)
      (let ((snapshot (nskk-cache-capture-snapshot cache)))
        (nskk-cache-put cache "c" 3)
        (should-not (nskk-cache-get cache "a"))
        (nskk-cache-restore-snapshot snapshot)
        (should (equal (nskk-cache-get cache "a") 1)))))

  (nskk-it "does not roll back a key overwrite: put on an existing key mutates the captured entry in place"
    (let* ((cache (nskk-cache-lru-create 10)))
      (nskk-cache-put cache "a" 1)
      (let ((snapshot (nskk-cache-capture-snapshot cache)))
        (nskk-cache-put cache "a" 2)
        (nskk-cache-restore-snapshot snapshot)
        (should (equal (nskk-cache-get cache "a") 2))))
    (let* ((cache (nskk-cache-lfu-create 10)))
      (nskk-cache-put cache "a" 1)
      (let ((snapshot (nskk-cache-capture-snapshot cache)))
        (nskk-cache-put cache "a" 2)
        (nskk-cache-restore-snapshot snapshot)
        (should (equal (nskk-cache-get cache "a") 2))))))

(nskk-describe "nskk-cache-capture-metadata-snapshot and nskk-cache-restore-metadata-snapshot"
  (nskk-it "restores LRU capacity, size, hits, misses, and hash/head/tail identities"
    (let* ((cache (nskk-cache-lru-create 10)))
      (nskk-cache-put cache "a" 1)
      (nskk-cache-get cache "a")
      (let* ((table (nskk-cache-lru-hash cache))
             (head (nskk-cache-lru-head cache))
             (tail (nskk-cache-lru-tail cache))
             (snapshot (nskk-cache-capture-metadata-snapshot cache)))
        (nskk-cache-put cache "b" 2)
        (nskk-cache-get cache "missing")
        (nskk-cache-restore-metadata-snapshot snapshot)
        (should (= 10 (nskk-cache-lru-capacity cache)))
        (should (= 1 (nskk-cache-lru-size cache)))
        (should (= 1 (nskk-cache-lru-hits cache)))
        (should (= 0 (nskk-cache-lru-misses cache)))
        (should (eq (nskk-cache-lru-hash cache) table))
        (should (eq (nskk-cache-lru-head cache) head))
        (should (eq (nskk-cache-lru-tail cache) tail)))))

  (nskk-it "restores LFU capacity, size, hits, misses, and hash/freq identities"
    (let* ((cache (nskk-cache-lfu-create 10)))
      (nskk-cache-put cache "a" 1)
      (nskk-cache-get cache "a")
      (let* ((table (nskk-cache-lfu-hash cache))
             (freq (nskk-cache-lfu-freq cache))
             (min-freq (nskk-cache-lfu-min-freq cache))
             (snapshot (nskk-cache-capture-metadata-snapshot cache)))
        (nskk-cache-put cache "b" 2)
        (nskk-cache-get cache "missing")
        (nskk-cache-restore-metadata-snapshot snapshot)
        (should (= 10 (nskk-cache-lfu-capacity cache)))
        (should (= 1 (nskk-cache-lfu-size cache)))
        (should (= 1 (nskk-cache-lfu-hits cache)))
        (should (= 0 (nskk-cache-lfu-misses cache)))
        (should (= min-freq (nskk-cache-lfu-min-freq cache)))
        (should (eq (nskk-cache-lfu-hash cache) table))
        (should (eq (nskk-cache-lfu-freq cache) freq)))))

  (nskk-it "does not roll back hash-table contents, unlike the full snapshot"
    (let* ((cache (nskk-cache-lru-create 10)))
      (nskk-cache-put cache "a" 1)
      (let ((snapshot (nskk-cache-capture-metadata-snapshot cache)))
        (nskk-cache-put cache "b" 2)
        (nskk-cache-restore-metadata-snapshot snapshot)
        (should (= 1 (nskk-cache-lru-size cache)))
        (should (= 2 (hash-table-count (nskk-cache-lru-hash cache))))
        (should (equal (nskk-cache-get cache "b") 2))))))

(defun nskk-cache-test--expect-signal (condition thunk)
  "Call THUNK and require it to signal CONDITION."
  (let ((caught
         (condition-case err
             (progn
               (funcall thunk)
               nil)
           (error err)
           (quit err))))
    (should (consp caught))
    (should (eq (car caught) condition))))

(defun nskk-cache-test--fault-after-call (symbol condition thunk)
  "Call THUNK with SYMBOL signaling CONDITION after its real mutation."
  (let ((original (symbol-function symbol)))
    (cl-letf (((symbol-function symbol)
               (lambda (&rest args)
                 (prog1 (apply original args)
                   (signal condition nil)))))
      (nskk-cache-test--expect-signal condition thunk))))

(defun nskk-cache-test--only-key (table)
  "Return the sole physical key in TABLE."
  (let (keys)
    (maphash (lambda (key _value) (push key keys)) table)
    (should (= 1 (length keys)))
    (car keys)))

(defun nskk-cache-test--primary-table (cache)
  "Return CACHE's primary key table."
  (if (nskk-cache-lru-p cache)
      (nskk-cache-lru-hash cache)
    (nskk-cache-lfu-hash cache)))

(defun nskk-cache-test--stored-key (cache)
  "Return the sole physical key owned by CACHE."
  (let* ((table (nskk-cache-test--primary-table cache))
         (record (gethash (nskk-cache-test--only-key table) table)))
    (if (nskk-cache-lru-p cache)
        (nskk-cache-lru-node-key record)
      (nskk-cache-lfu-entry-key record))))

(defun nskk-cache-test--make-transaction-fixture (strategy)
  "Create a two-entry STRATEGY cache and capture its exact object graph."
  (let* ((cache (nskk-cache-create :type strategy :capacity 2))
         (a-value (list :a strategy))
         (b-value (list :b strategy)))
    (nskk-cache-put cache "a" a-value)
    (nskk-cache-put cache "b" b-value)
    (if (eq strategy 'lru)
        (list :cache cache
              :a-record (gethash "a" (nskk-cache-lru-hash cache))
              :b-record (gethash "b" (nskk-cache-lru-hash cache))
              :a-value a-value
              :b-value b-value)
      (list :cache cache
            :a-record (gethash "a" (nskk-cache-lfu-hash cache))
            :b-record (gethash "b" (nskk-cache-lfu-hash cache))
            :bucket (gethash 1 (nskk-cache-lfu-freq cache))
            :a-value a-value
            :b-value b-value))))

(defun nskk-cache-test--assert-transaction-fixture (strategy fixture)
  "Assert that FIXTURE retains the exact STRATEGY cache object graph."
  (let* ((cache (plist-get fixture :cache))
         (a-record (plist-get fixture :a-record))
         (b-record (plist-get fixture :b-record))
         (a-value (plist-get fixture :a-value))
         (b-value (plist-get fixture :b-value)))
    (if (eq strategy 'lru)
        (let ((table (nskk-cache-lru-hash cache))
              (head (nskk-cache-lru-head cache))
              (tail (nskk-cache-lru-tail cache)))
          (should (= 2 (nskk-cache-lru-size cache)))
          (should (= 2 (hash-table-count table)))
          (should (eq (gethash "a" table) a-record))
          (should (eq (gethash "b" table) b-record))
          (should (eq (nskk-cache-lru-node-value a-record) a-value))
          (should (eq (nskk-cache-lru-node-value b-record) b-value))
          (should (eq (nskk-cache-lru-node-next head) b-record))
          (should (eq (nskk-cache-lru-node-prev b-record) head))
          (should (eq (nskk-cache-lru-node-next b-record) a-record))
          (should (eq (nskk-cache-lru-node-prev a-record) b-record))
          (should (eq (nskk-cache-lru-node-next a-record) tail))
          (should (eq (nskk-cache-lru-node-prev tail) a-record))
          (should (= 0 (nskk-cache-lru-hits cache)))
          (should (= 0 (nskk-cache-lru-misses cache))))
      (let ((table (nskk-cache-lfu-hash cache))
            (freq-table (nskk-cache-lfu-freq cache))
            (bucket (plist-get fixture :bucket)))
        (should (= 2 (nskk-cache-lfu-size cache)))
        (should (= 2 (hash-table-count table)))
        (should (= 1 (hash-table-count freq-table)))
        (should (eq (gethash "a" table) a-record))
        (should (eq (gethash "b" table) b-record))
        (should (eq (gethash 1 freq-table) bucket))
        (should (= 2 (hash-table-count bucket)))
        (should (gethash "a" bucket))
        (should (gethash "b" bucket))
        (should (eq (nskk-cache-lfu-entry-value a-record) a-value))
        (should (eq (nskk-cache-lfu-entry-value b-record) b-value))
        (should (= 1 (nskk-cache-lfu-entry-frequency a-record)))
        (should (= 1 (nskk-cache-lfu-entry-frequency b-record)))
        (should (= 1 (nskk-cache-lfu-min-freq cache)))
        (should (= 0 (nskk-cache-lfu-hits cache)))
        (should (= 0 (nskk-cache-lfu-misses cache)))))))

(ert-deftest nskk-cache-adversarial-owned-arbitrary-keys-and-values ()
  (dolist (strategy '(lru lfu))
    (let ((cache (nskk-cache-create :type strategy :capacity 1)))
      (dotimes (index 20)
        (let* ((key (format "key-%02d" index))
               (lookup (copy-sequence key))
               (value (list strategy index)))
          (nskk-cache-put cache key value)
          (should-not (eq (nskk-cache-test--stored-key cache) key))
          (aset key 0 ?X)
          (should (eq (nskk-cache-get cache lookup) value))
          (should (= 1 (nskk-cache-size cache)))
          (should (= 1 (hash-table-count
                        (nskk-cache-test--primary-table cache))))))
      (let* ((leaf (copy-sequence "shared"))
             (key (list leaf leaf))
             (lookup (list "shared" "shared"))
             (value (list :shared strategy)))
        (nskk-cache-put cache key value)
        (let ((stored-key (nskk-cache-test--stored-key cache)))
          (should-not (eq stored-key key))
          (should-not (eq (car stored-key) leaf))
          (should (eq (car stored-key) (cadr stored-key))))
        (aset leaf 0 ?X)
        (should (eq (nskk-cache-get cache lookup) value)))
      (let* ((key (list "cycle"))
             (lookup (list "cycle"))
             (value (list :cycle strategy)))
        (setcdr key key)
        (setcdr lookup lookup)
        (nskk-cache-put cache key value)
        (let ((stored-key (nskk-cache-test--stored-key cache)))
          (should-not (eq stored-key key))
          (should (eq (cdr stored-key) stored-key)))
        (setcar key "changed")
        (should (eq (nskk-cache-get cache lookup) value)))
      (let* ((key (list "update"))
             (lookup (list "update"))
             (old-value (list :old strategy))
             (new-value (vector :new strategy)))
        (nskk-cache-put cache key old-value)
        (nskk-cache-put cache lookup new-value)
        (should-not (eq (nskk-cache-test--stored-key cache) lookup))
        (should (eq (nskk-cache-get cache key) new-value))
        (should (= 1 (nskk-cache-size cache)))))))

(ert-deftest nskk-cache-adversarial-key-copy-failure-is-atomic ()
  (dolist (strategy '(lru lfu))
    (dolist (condition '(error quit))
      (let ((fixture
             (nskk-cache-test--make-transaction-fixture strategy)))
        (cl-letf (((symbol-function 'nskk-prolog-copy-term)
                   (lambda (_term) (signal condition nil))))
          (nskk-cache-test--expect-signal
           condition
           (lambda ()
             (nskk-cache-put (plist-get fixture :cache)
                             (list "c")
                             (list :new strategy)))))
        (nskk-cache-test--assert-transaction-fixture
         strategy fixture)))))

(ert-deftest nskk-cache-adversarial-lru-helper-fault-rolls-back ()
  (dolist (condition '(error quit))
    (dolist (case '((nskk-cache-lru--move-to-head get)
                    (nskk-cache-lru--remove-node invalidate)
                    (nskk-cache-lru--add-to-head put)
                    (nskk-cache-lru--remove-tail put)))
      (dotimes (_iteration 3)
        (let* ((fixture
                (nskk-cache-test--make-transaction-fixture 'lru))
               (cache (plist-get fixture :cache))
               (symbol (car case))
               (operation (cadr case))
               (recovery-value (list :recovery condition operation)))
          (nskk-cache-test--fault-after-call
           symbol condition
           (lambda ()
             (pcase operation
               ('get (nskk-cache-get cache "a"))
               ('invalidate (nskk-cache-invalidate cache "a"))
               ('put (nskk-cache-put cache "c" (list :new condition))))))
          (nskk-cache-test--assert-transaction-fixture
           'lru fixture)
          (nskk-cache-put cache "c" recovery-value)
          (should (eq (nskk-cache-get cache "c") recovery-value))
          (let* ((table (nskk-cache-lru-hash cache))
                 (head (nskk-cache-lru-head cache))
                 (tail (nskk-cache-lru-tail cache))
                 (a-record (plist-get fixture :a-record))
                 (b-record (plist-get fixture :b-record))
                 (b-value (plist-get fixture :b-value))
                 (c-record (gethash "c" table)))
            (should (= 2 (nskk-cache-lru-size cache)))
            (should (= 2 (hash-table-count table)))
            (should-not (gethash "a" table))
            (should-not (eq c-record a-record))
            (should (eq (gethash "b" table) b-record))
            (should (eq (nskk-cache-lru-node-value b-record) b-value))
            (should (eq (nskk-cache-lru-node-value c-record)
                        recovery-value))
            (should (eq (nskk-cache-lru-node-next head) c-record))
            (should (eq (nskk-cache-lru-node-prev c-record) head))
            (should (eq (nskk-cache-lru-node-next c-record) b-record))
            (should (eq (nskk-cache-lru-node-prev b-record) c-record))
            (should (eq (nskk-cache-lru-node-next b-record) tail))
            (should (eq (nskk-cache-lru-node-prev tail) b-record))
            (should (= 1 (nskk-cache-lru-hits cache)))
            (should (= 0 (nskk-cache-lru-misses cache)))))))))

(ert-deftest nskk-cache-adversarial-lfu-helper-fault-rolls-back ()
  (dolist (condition '(error quit))
    (dolist (case '((nskk-cache-lfu--remove-from-freq-bucket get)
                    (nskk-cache-lfu--update-freq get)
                    (nskk-cache-lfu--evict-min-freq put)
                    (nskk-cache-lfu--bucket-any-key/k put)
                    (remhash invalidate)))
      (let* ((fixture
              (nskk-cache-test--make-transaction-fixture 'lfu))
             (cache (plist-get fixture :cache))
             (symbol (car case))
             (operation (cadr case)))
        (nskk-cache-test--fault-after-call
         symbol condition
         (lambda ()
           (pcase operation
             ('get (nskk-cache-get cache "a"))
             ('invalidate (nskk-cache-invalidate cache "a"))
             ('put (nskk-cache-put cache "c" (list :new condition))))))
        (nskk-cache-test--assert-transaction-fixture
         'lfu fixture)))))

(ert-deftest nskk-cache-adversarial-lru-put-existing-fault-rolls-back ()
  (dolist (condition '(error quit))
    (dotimes (_iteration 3)
      (let* ((fixture (nskk-cache-test--make-transaction-fixture 'lru))
             (cache (plist-get fixture :cache)))
        (nskk-cache-test--fault-after-call
         'nskk-cache-lru--move-to-head condition
         (lambda ()
           (nskk-cache-put cache "a" (list :new condition))))
        (nskk-cache-test--assert-transaction-fixture 'lru fixture)))))

(ert-deftest nskk-cache-adversarial-lfu-put-existing-fault-rolls-back ()
  (dolist (condition '(error quit))
    (dotimes (_iteration 3)
      (let* ((fixture (nskk-cache-test--make-transaction-fixture 'lfu))
             (cache (plist-get fixture :cache)))
        (nskk-cache-test--fault-after-call
         'nskk-cache-lfu--update-freq condition
         (lambda ()
           (nskk-cache-put cache "a" (list :new condition))))
        (nskk-cache-test--assert-transaction-fixture 'lfu fixture)))))

(ert-deftest nskk-cache-adversarial-prepared-get-contract ()
(dolist (strategy (quote (lru lfu)))
  (let* ((cache (nskk-cache-create :type strategy :capacity 2))
         (stored (list :stored strategy))
         (prepared (list :prepared strategy))
         (preparer-input nil)
         (preparer-calls 0)
         (found-calls 0)
         (miss-calls 0)
         (received nil))
    (nskk-cache-put cache "a" stored)
    (nskk-cache-get-prepared/k
     cache "a"
     (lambda (value)
       (setq preparer-input value)
       (cl-incf preparer-calls)
       prepared)
     (lambda (value)
       (setq received value)
       (cl-incf found-calls))
     (lambda () (cl-incf miss-calls)))
    (should (eq preparer-input stored))
    (should (eq received prepared))
    (should (= preparer-calls 1))
    (should (= found-calls 1))
    (should (= miss-calls 0))
    (should (= (plist-get (nskk-cache-stats cache) :hits) 1)))
  (let ((cache (nskk-cache-create :type strategy :capacity 2))
        (preparer-calls 0)
        (found-calls 0)
        (miss-calls 0)
        (received :unset))
    (nskk-cache-put cache "nil" nil)
    (nskk-cache-get-prepared/k
     cache "nil"
     (lambda (value)
       (should (null value))
       (cl-incf preparer-calls)
       :prepared-nil)
     (lambda (value)
       (setq received value)
       (cl-incf found-calls))
     (lambda () (cl-incf miss-calls)))
    (should (eq received :prepared-nil))
    (should (= preparer-calls 1))
    (should (= found-calls 1))
    (should (= miss-calls 0)))
  (let ((cache (nskk-cache-create :type strategy :capacity 2))
        (preparer-calls 0)
        (found-calls 0)
        (miss-calls 0))
    (nskk-cache-get-prepared/k
     cache "missing"
     (lambda (value)
       (cl-incf preparer-calls)
       value)
     (lambda (_value) (cl-incf found-calls))
     (lambda () (cl-incf miss-calls)))
    (should (= preparer-calls 0))
    (should (= found-calls 0))
    (should (= miss-calls 1))
    (should (= (plist-get (nskk-cache-stats cache) :misses) 1)))
  (let* ((cache (nskk-cache-create :type strategy :capacity 2))
         (stored (list :legacy strategy))
         (nil-found 0)
         (miss-found 0))
    (nskk-cache-put cache "value" stored)
    (nskk-cache-put cache "nil" nil)
    (should (eq (nskk-cache-get cache "value") stored))
    (nskk-cache-get/k
     cache "nil"
     (lambda (value)
       (should (null value))
       (cl-incf nil-found))
     (lambda () (should nil)))
    (nskk-cache-get/k
     cache "missing"
     (lambda (_value) (should nil))
     (lambda () (cl-incf miss-found)))
    (should (= nil-found 1))
    (should (= miss-found 1)))))

(ert-deftest nskk-cache-adversarial-preparer-fault-is-atomic ()
(dolist (strategy (quote (lru lfu)))
  (dolist (condition (quote (error quit)))
    (let* ((fixture (nskk-cache-test--make-transaction-fixture strategy))
           (cache (plist-get fixture :cache))
           (stored (plist-get fixture :a-value))
           (prepared (list :retry strategy condition))
           (preparer-input nil)
           (preparer-calls 0)
           (found-calls 0)
           (miss-calls 0)
           (received nil))
      (dotimes (attempt 3)
        (nskk-cache-test--expect-signal
         condition
         (lambda ()
           (nskk-cache-get-prepared/k
            cache "a"
            (lambda (value)
              (setq preparer-input value)
              (cl-incf preparer-calls)
              (signal condition (list :prepared-hit-fault attempt)))
            (lambda (_value) (cl-incf found-calls))
            (lambda () (cl-incf miss-calls)))))
        (should (eq preparer-input stored))
        (should (= preparer-calls (1+ attempt)))
        (should (= found-calls 0))
        (should (= miss-calls 0))
        (nskk-cache-test--assert-transaction-fixture strategy fixture))
      (nskk-cache-get-prepared/k
       cache "a"
       (lambda (value)
         (setq preparer-input value)
         (cl-incf preparer-calls)
         prepared)
       (lambda (value)
         (setq received value)
         (cl-incf found-calls))
       (lambda () (cl-incf miss-calls)))
      (should (eq preparer-input stored))
      (should (eq received prepared))
      (should (= preparer-calls 4))
      (should (= found-calls 1))
      (should (= miss-calls 0))
      (if (eq strategy (quote lru))
          (let ((a-record (plist-get fixture :a-record))
                (b-record (plist-get fixture :b-record))
                (head (nskk-cache-lru-head cache))
                (tail (nskk-cache-lru-tail cache)))
            (should (eq (nskk-cache-lru-node-next head) a-record))
            (should (eq (nskk-cache-lru-node-prev a-record) head))
            (should (eq (nskk-cache-lru-node-next a-record) b-record))
            (should (eq (nskk-cache-lru-node-prev b-record) a-record))
            (should (eq (nskk-cache-lru-node-next b-record) tail))
            (should (eq (nskk-cache-lru-node-prev tail) b-record))
            (should (= (nskk-cache-lru-hits cache) 1))
            (should (= (nskk-cache-lru-misses cache) 0)))
        (let* ((a-record (plist-get fixture :a-record))
               (b-record (plist-get fixture :b-record))
               (freq-table (nskk-cache-lfu-freq cache))
               (bucket-one (gethash 1 freq-table))
               (bucket-two (gethash 2 freq-table)))
          (should (= (nskk-cache-lfu-entry-frequency a-record) 2))
          (should (= (nskk-cache-lfu-entry-frequency b-record) 1))
          (should-not (gethash "a" bucket-one))
          (should (gethash "b" bucket-one))
          (should (gethash "a" bucket-two))
          (should (= (nskk-cache-lfu-min-freq cache) 1))
          (should (= (nskk-cache-lfu-hits cache) 1))
          (should (= (nskk-cache-lfu-misses cache) 0))))))))

(provide (quote nskk-cache-test))

;;; nskk-cache-test.el ends here
