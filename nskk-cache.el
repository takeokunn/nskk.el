;;; nskk-cache.el --- Cache mechanism for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

;; Author: takeokunn <bararararatty@gmail.com>
;; Maintainer: takeokunn <bararararatty@gmail.com>
;; URL: https://github.com/takeokunn/nskk.el
;; Version: 0.1.0
;; Keywords: i18n

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

;; Caching layer for the NSKK dictionary search engine (Layer 1: Core Engine).
;;
;; Layer position: L1 (Core Engine) -- depends on nskk-prolog and nskk-cps-macros.
;;
;; Provides LRU and LFU cache implementations with O(1) get/put operations,
;; automatic eviction, and hit-rate statistics.  Cache type validation and
;; operation dispatch are driven by Prolog facts, keeping the dispatch table
;; declarative and consistent with the rest of the NSKK architecture.
;;
;; Supported algorithms (selectable at creation time):
;; - LRU (Least Recently Used): doubly-linked list + hash table
;; - LFU (Least Frequently Used): frequency buckets + hash table
;;
;; Both algorithms provide amortized O(1) get and put.  The LFU frequency
;; bucket uses a list per frequency level; promotion is O(k) where k is the
;; bucket occupancy, which is O(1) in the common case of spread frequencies.
;; Capacity is managed by entry count with automatic eviction.  Hit-rate
;; statistics are collected and accessible via `nskk-cache-stats'.
;;
;; Prolog predicates maintained by this module:
;; - `cache-type/1'            -- valid cache type membership (lru, lfu)
;; - `cache-eviction-policy/2' -- type -> policy name documentation
;; - `cache-dispatch-fn/3'     -- (type op fn) operation dispatch table
;; - `cache-field-fn/3'        -- (type field accessor-fn) field accessor table
;;
;; Key public API:
;; - `nskk-cache-create'             -- create LRU or LFU cache
;; - `nskk-cache-get'                -- retrieve a value by key
;; - `nskk-cache-put'                -- store a key/value pair
;; - `nskk-cache-invalidate'         -- remove a specific key
;; - `nskk-cache-invalidate-pattern' -- remove keys matching a regexp
;; - `nskk-cache-clear'              -- clear all entries
;; - `nskk-cache-stats'              -- return statistics plist
;; - `nskk-cache-hit-rate'           -- return hit rate as float
;; - `nskk-cache-size'               -- return current entry count
;; - `nskk-cache-p'                  -- test for valid cache object
;;
;; Performance targets:
;; - get: O(1), < 0.1ms (Prolog dispatch via hash index is ~20us)
;; - put: O(1), < 0.1ms
;; - cache-hit search: < 10ms
;;
;; Usage:
;;
;;   (setq cache (nskk-cache-create 'lru 1000))
;;   (nskk-cache-put cache "key" "value")
;;   (nskk-cache-get cache "key")  ; => "value"
;;   (nskk-cache-stats cache)      ; => (:hits N :misses N :hit-rate R ...)
;;
;;   ;; Query dispatch table via Prolog:
;;   (nskk-prolog-query-value '(cache-type \?t) '\?t)  ; => lru or lfu
;;   (nskk-prolog-query-value
;;     '(cache-eviction-policy lru \?p) '\?p)           ; => least-recently-used

;;; Code:

(require 'cl-lib)
(require 'nskk-prolog)
(require 'nskk-cps-macros)

(defgroup nskk-cache nil
  "Cache settings for NSKK."
  :prefix "nskk-cache-"
  :group 'nskk)

(defcustom nskk-cache-default-capacity 1000
  "Default cache capacity for LRU/LFU caches."
  :type 'integer
  :package-version '("nskk" . "0.1.0")
  :group 'nskk-cache)

(defcustom nskk-cache-strategy 'lru
  "Cache eviction strategy.
\\='lru means Least Recently Used.
\\='lfu means Least Frequently Used."
  :type '(choice (const :tag "LRU" lru)
                 (const :tag "LFU" lfu))
  :package-version '("nskk" . "0.1.0")
  :group 'nskk-cache)

;;; Prolog Facts

;; Cache type validation: (cache-type TYPE)
(nskk-prolog-define-fact-table cache-type (:arity 1 :index :hash)
  (lru)
  (lfu))

;; Cache eviction policy documentation: (cache-eviction-policy TYPE POLICY)
(nskk-prolog-define-fact-table cache-eviction-policy (:arity 2 :index :hash)
  (lru least-recently-used)
  (lfu least-frequently-used))

;; Cache operation dispatch table: (cache-dispatch-fn TYPE OP FN)
(nskk-prolog-define-fact-table cache-dispatch-fn (:arity 3 :index :hash)
  (lru get        nskk-cache-lru-get)
  (lru put        nskk-cache-lru-put)
  (lru invalidate nskk-cache-lru-invalidate)
  (lru clear      nskk-cache-lru-clear)
  (lru size       nskk-cache-lru-size)
  (lfu get        nskk-cache-lfu-get)
  (lfu put        nskk-cache-lfu-put)
  (lfu invalidate nskk-cache-lfu-invalidate)
  (lfu clear      nskk-cache-lfu-clear)
  (lfu size       nskk-cache-lfu-size))

;; Cache field accessor table: (cache-field-fn TYPE FIELD ACCESSOR-FN)
(nskk-prolog-define-fact-table cache-field-fn (:arity 3 :index :hash)
  (lru capacity nskk-cache-lru-capacity)
  (lru size     nskk-cache-lru-size)
  (lru hits     nskk-cache-lru-hits)
  (lru misses   nskk-cache-lru-misses)
  (lru hash     nskk-cache-lru-hash)
  (lfu capacity nskk-cache-lfu-capacity)
  (lfu size     nskk-cache-lfu-size)
  (lfu hits     nskk-cache-lfu-hits)
  (lfu misses   nskk-cache-lfu-misses)
  (lfu hash     nskk-cache-lfu-hash))

;;; Cache Type Dispatch

(defun nskk--cache-type-of (cache)
  "Return the cache type symbol for CACHE.
Returns the symbol `lru' or `lfu'.  Signals an error for invalid CACHE."
  (cond
   ((nskk-cache-lru-p cache) 'lru)
   ((nskk-cache-lfu-p cache) 'lfu)
   (t (error "Invalid cache type: %S" cache))))

(defmacro nskk-cache-dispatch (cache op &rest args)
  "Dispatch OP on CACHE via the Prolog cache-dispatch-fn/3 table.
OP is a literal symbol (unquoted) naming the operation (e.g., get, put).
ARGS are passed through to the dispatched implementation function.
The dispatch function is resolved at runtime via Prolog hash-indexed lookup.

Design note — why there is no per-instance cache-type/2 Prolog fact:
The LRU and LFU structs have no unique identity slot (no ID field).
Asserting a per-instance fact such as (cache-type ID lru) would require
either a globally unique integer counter (mutable global state) or a
gensym, and would demand a matching retract on cache destruction — but
Emacs Lisp has no finalizers, so the retract would never fire and the
Prolog DB would accumulate stale facts indefinitely.  The struct-field
approach via `nskk--cache-type-of' (which calls `nskk-cache-lru-p' /
`nskk-cache-lfu-p') is O(1), allocation-free, and leak-free, so it is
retained.  The global cache-dispatch-fn/3 table already provides the
declarative, Prolog-queryable dispatch that the architecture requires."
  (declare (indent 2) (debug t))
  `(nskk--cache-dispatch-prolog ,cache ',op ,@args))

(defun nskk--cache-dispatch-prolog (cache op &rest args)
  "Internal Prolog-backed dispatcher for OP on CACHE.
Queries cache-dispatch-fn/3 with (TYPE OP ?FN) and applies FN to CACHE
and ARGS."
  (let* ((type (nskk--cache-type-of cache))
         (fn   (nskk-prolog-query-value
                `(cache-dispatch-fn ,type ,op \?fn) '\?fn)))
    (unless fn
      (error "No Prolog dispatch for op=%s cache-type=%s" op type))
    (apply fn cache args)))

(defmacro nskk-cache-field (cache field &optional default)
  "Get FIELD from CACHE via the Prolog cache-field-fn/3 table.
FIELD is a literal symbol naming the struct slot (e.g., capacity, size).
DEFAULT is returned when no accessor is found (defaults to 0).
The accessor function is resolved at runtime via Prolog hash-indexed lookup."
  (declare (indent 0) (debug t))
  `(nskk--cache-field-prolog ,cache ',field ,(or default 0)))

(defun nskk--cache-field-prolog (cache field default)
  "Internal Prolog-backed field accessor for FIELD on CACHE.
Queries cache-field-fn/3 with (TYPE FIELD ?FN) and calls FN with CACHE."
  (let* ((type (nskk--cache-type-of cache))
         (fn   (nskk-prolog-query-value
                `(cache-field-fn ,type ,field \?fn) '\?fn)))
    (if fn (funcall fn cache) default)))

;;; LRU Cache Data Structures

;; LRU cache node (doubly-linked list element)
(cl-defstruct (nskk-cache-lru-node
               (:constructor nskk-cache-lru-node--create)
               (:copier nil))
  "LRU cache node for the doubly-linked list.
Slots:
  key   - the cache key
  value - the cached value
  prev  - previous node (nskk-cache-lru-node or nil)
  next  - next node (nskk-cache-lru-node or nil)"
  key value prev next)

;; LRU cache structure
(cl-defstruct (nskk-cache-lru
               (:constructor nskk-cache-lru--create)
               (:copier nil))
  "LRU cache structure.
Slots:
  capacity - maximum number of entries
  size     - current number of entries
  hash     - hash table mapping keys to nodes
  head     - dummy head node (most-recently-used side)
  tail     - dummy tail node (least-recently-used side)
  hits     - cumulative cache hit count
  misses   - cumulative cache miss count"
  (capacity 1000 :type integer)
  (size     0    :type integer)
  (hash     nil  :type hash-table)
  head tail
  (hits   0 :type integer)
  (misses 0 :type integer))

;;; LFU Cache Data Structures

;; LFU cache entry
(cl-defstruct (nskk-cache-lfu-entry
               (:constructor nskk-cache-lfu-entry--create)
               (:copier nil))
  "LFU cache entry.
Slots:
  key       - the cache key
  value     - the cached value
  frequency - access frequency count"
  key value
  (frequency 1 :type integer))

;; LFU cache structure
(cl-defstruct (nskk-cache-lfu
               (:constructor nskk-cache-lfu--create)
               (:copier nil))
  "LFU cache structure.
Slots:
  capacity  - maximum number of entries
  size      - current number of entries
  hash      - hash table mapping keys to entries
  freq      - hash table mapping frequency to list of keys
  min-freq  - current minimum frequency (used for eviction)
  hits      - cumulative cache hit count
  misses    - cumulative cache miss count"
  (capacity 1000 :type integer)
  (size     0    :type integer)
  (hash     nil  :type hash-table)
  (freq     nil  :type hash-table)
  (min-freq 0    :type integer)
  (hits     0    :type integer)
  (misses   0    :type integer))

;;; LRU Cache Implementation

(defun nskk-cache-lru-create (capacity)
  "Create an LRU cache with CAPACITY entries."
  (let ((head (nskk-cache-lru-node--create))
        (tail (nskk-cache-lru-node--create)))
    ;; Link dummy head and tail sentinels
    (setf (nskk-cache-lru-node-next head) tail)
    (setf (nskk-cache-lru-node-prev tail) head)
    (nskk-cache-lru--create
     :capacity capacity
     :size     0
     :hash     (make-hash-table :test 'equal :size capacity)
     :head     head
     :tail     tail
     :hits     0
     :misses   0)))

(defsubst nskk-cache-lru--remove-node (node)
  "Remove NODE from the doubly-linked list."
  (let ((prev-node (nskk-cache-lru-node-prev node))
        (next-node (nskk-cache-lru-node-next node)))
    (setf (nskk-cache-lru-node-next prev-node) next-node)
    (setf (nskk-cache-lru-node-prev next-node) prev-node)))

(defsubst nskk-cache-lru--add-to-head (cache node)
  "Insert NODE immediately after the dummy head of CACHE."
  (let* ((head      (nskk-cache-lru-head cache))
         (next-node (nskk-cache-lru-node-next head)))
    (setf (nskk-cache-lru-node-next node)      next-node)
    (setf (nskk-cache-lru-node-prev node)      head)
    (setf (nskk-cache-lru-node-next head)      node)
    (setf (nskk-cache-lru-node-prev next-node) node)))

(defsubst nskk-cache-lru--move-to-head (cache node)
  "Move NODE to the most-recently-used position in CACHE."
  (nskk-cache-lru--remove-node node)
  (nskk-cache-lru--add-to-head cache node))

(defsubst nskk-cache-lru--remove-tail (cache)
  "Remove and return the least-recently-used node from CACHE."
  (let* ((tail (nskk-cache-lru-tail cache))
         (node (nskk-cache-lru-node-prev tail)))
    (nskk-cache-lru--remove-node node)
    node))

(defun/k nskk-cache-lru-get (cache key)
  "Get the value for KEY from LRU CACHE.
Returns nil on a cache miss.  Updates access order on hit."
  (let ((node (gethash key (nskk-cache-lru-hash cache))))
    (if node
        (progn
          (nskk-cache-lru--move-to-head cache node)
          (cl-incf (nskk-cache-lru-hits cache))
          (succeed (nskk-cache-lru-node-value node)))
      (cl-incf (nskk-cache-lru-misses cache))
      (fail))))

(defun/done nskk-cache-lru-put (cache key value)
  "Store KEY with VALUE in LRU CACHE.
Updates VALUE and access order if KEY already exists.
Evicts the least-recently-used entry when CACHE is at capacity."
  (let ((node (gethash key (nskk-cache-lru-hash cache))))
    (if node
        ;; Existing entry: update value and promote to head
        (progn
          (setf (nskk-cache-lru-node-value node) value)
          (nskk-cache-lru--move-to-head cache node))
      ;; New entry: insert at head
      (let ((new-node (nskk-cache-lru-node--create :key key :value value)))
        (puthash key new-node (nskk-cache-lru-hash cache))
        (nskk-cache-lru--add-to-head cache new-node)
        (cl-incf (nskk-cache-lru-size cache))
        ;; Evict LRU entry if over capacity
        (when (> (nskk-cache-lru-size cache) (nskk-cache-lru-capacity cache))
          (let ((tail-node (nskk-cache-lru--remove-tail cache)))
            (remhash (nskk-cache-lru-node-key tail-node)
                     (nskk-cache-lru-hash cache))
            (cl-decf (nskk-cache-lru-size cache))))))))

(defun/k nskk-cache-lru-invalidate (cache key)
  "Remove KEY from LRU CACHE.
Returns t if KEY was found and removed, nil otherwise."
  (let ((node (gethash key (nskk-cache-lru-hash cache))))
    (if node
        (progn
          (nskk-cache-lru--remove-node node)
          (remhash key (nskk-cache-lru-hash cache))
          (cl-decf (nskk-cache-lru-size cache))
          (succeed t))
      (fail))))

(defun/done nskk-cache-lru-clear (cache)
  "Remove all entries from LRU CACHE and reset statistics."
  (clrhash (nskk-cache-lru-hash cache))
  (setf (nskk-cache-lru-size cache) 0)
  ;; Reconnect dummy head and tail sentinels
  (let ((head (nskk-cache-lru-head cache))
        (tail (nskk-cache-lru-tail cache)))
    (setf (nskk-cache-lru-node-next head) tail)
    (setf (nskk-cache-lru-node-prev tail) head))
  (setf (nskk-cache-lru-hits   cache) 0
        (nskk-cache-lru-misses cache) 0))

;;; LFU Cache Implementation

(defun/k nskk-cache-lfu--bucket-any-key (bucket)
  "Return any key from BUCKET (a hash-table mapping key -> t).
Calls on-found with the key when BUCKET is non-empty; on-not-found otherwise."
  (let ((k (catch 'found
              (maphash (lambda (key _) (throw 'found key)) bucket))))
    (if k (succeed k) (fail))))

(defun/done nskk-cache-lfu--evict-min-freq (cache)
  "Evict one entry at the minimum frequency from CACHE.
No-op when the min-freq bucket is absent or empty."
  (let* ((min-freq   (nskk-cache-lfu-min-freq cache))
         (freq-table (nskk-cache-lfu-freq cache))
         (bucket     (gethash min-freq freq-table)))
    (when bucket
      (nskk-cache-lfu--bucket-any-key/k bucket
        (lambda (evict-key)
          (remhash evict-key (nskk-cache-lfu-hash cache))
          (remhash evict-key bucket)
          (when (zerop (hash-table-count bucket))
            (remhash min-freq freq-table))
          (cl-decf (nskk-cache-lfu-size cache)))
        #'ignore))))

(defun nskk-cache-lfu-create (capacity)
  "Create an LFU cache with CAPACITY entries."
  (nskk-cache-lfu--create
   :capacity capacity
   :size     0
   :hash     (make-hash-table :test 'equal :size capacity)
   :freq     (make-hash-table :test 'equal :size capacity)
   :min-freq 0
   :hits     0
   :misses   0))

(defsubst nskk-cache-lfu--update-freq (cache entry old-freq)
  "Promote ENTRY in LFU CACHE from OLD-FREQ to its new frequency.
Removes ENTRY from the old frequency bucket and inserts into the new one.
Each bucket is a hash-table (key -> t) for O(1) add/remove.
Updates min-freq when the old minimum frequency bucket becomes empty."
  (let ((freq-table (nskk-cache-lfu-freq cache))
        (key        (nskk-cache-lfu-entry-key entry))
        (new-freq   (nskk-cache-lfu-entry-frequency entry)))
    ;; Remove from old frequency bucket (O(1))
    (when old-freq
      (let ((bucket (gethash old-freq freq-table)))
        (when bucket
          (remhash key bucket)
          (when (zerop (hash-table-count bucket))
            (remhash old-freq freq-table)
            ;; Advance min-freq when minimum bucket is emptied
            (when (= old-freq (nskk-cache-lfu-min-freq cache))
              (setf (nskk-cache-lfu-min-freq cache) new-freq))))))
    ;; Add to new frequency bucket (O(1))
    (let ((bucket (gethash new-freq freq-table)))
      (unless bucket
        (setq bucket (make-hash-table :test 'equal :size 4))
        (puthash new-freq bucket freq-table))
      (puthash key t bucket))))

(defun/k nskk-cache-lfu-get (cache key)
  "Get the value for KEY from LFU CACHE.
Returns nil on a cache miss.  Increments access frequency on hit."
  (let ((entry (gethash key (nskk-cache-lfu-hash cache))))
    (if entry
        (let ((old-freq (nskk-cache-lfu-entry-frequency entry)))
          (cl-incf (nskk-cache-lfu-entry-frequency entry))
          (nskk-cache-lfu--update-freq cache entry old-freq)
          (cl-incf (nskk-cache-lfu-hits cache))
          (succeed (nskk-cache-lfu-entry-value entry)))
      (cl-incf (nskk-cache-lfu-misses cache))
      (fail))))

(defun/done nskk-cache-lfu-put (cache key value)
  "Store KEY with VALUE in LFU CACHE.
Updates VALUE and increments frequency if KEY already exists.
Evicts the least-frequently-used entry when CACHE is at capacity."
  (let ((entry (gethash key (nskk-cache-lfu-hash cache))))
    (if entry
        ;; Existing entry: update value and increment frequency
        (let ((old-freq (nskk-cache-lfu-entry-frequency entry)))
          (setf (nskk-cache-lfu-entry-value entry) value)
          (cl-incf (nskk-cache-lfu-entry-frequency entry))
          (nskk-cache-lfu--update-freq cache entry old-freq))
      ;; New entry: evict if at capacity, then insert at frequency 1
      (when (>= (nskk-cache-lfu-size cache) (nskk-cache-lfu-capacity cache))
        (nskk-cache-lfu--evict-min-freq cache))
      (let ((new-entry (nskk-cache-lfu-entry--create
                        :key key :value value :frequency 1)))
        (puthash key new-entry (nskk-cache-lfu-hash cache))
        (nskk-cache-lfu--update-freq cache new-entry nil)
        (setf (nskk-cache-lfu-min-freq cache) 1)
        (cl-incf (nskk-cache-lfu-size cache))))))

(defun/k nskk-cache-lfu-invalidate (cache key)
  "Remove KEY from LFU CACHE.
Returns t if KEY was found and removed, nil otherwise."
  (let ((entry (gethash key (nskk-cache-lfu-hash cache))))
    (if entry
        (progn
          (let* ((freq-table (nskk-cache-lfu-freq cache))
                 (freq       (nskk-cache-lfu-entry-frequency entry))
                 (bucket     (gethash freq freq-table)))
            (when bucket
              (remhash key bucket)
              (when (zerop (hash-table-count bucket))
                (remhash freq freq-table))))
          (remhash key (nskk-cache-lfu-hash cache))
          (cl-decf (nskk-cache-lfu-size cache))
          (succeed t))
      (fail))))

(defun/done nskk-cache-lfu-clear (cache)
  "Remove all entries from LFU CACHE and reset statistics."
  (clrhash (nskk-cache-lfu-hash cache))
  (clrhash (nskk-cache-lfu-freq cache))
  (setf (nskk-cache-lfu-size     cache) 0
        (nskk-cache-lfu-min-freq cache) 0
        (nskk-cache-lfu-hits     cache) 0
        (nskk-cache-lfu-misses   cache) 0))

;;; Unified Interface

(defun nskk-cache-create--impl (&rest args)
  "Create a cache, returning an LRU or LFU cache structure.

ARGS supports three calling conventions:

  Positional:  (TYPE CAPACITY)
    TYPE     -- cache algorithm symbol, either \\='lru or \\='lfu
    CAPACITY -- maximum number of entries (positive integer)

  Keyword:     (:type TYPE :capacity CAP)
    :type     -- cache algorithm symbol, either \\='lru or \\='lfu
    :capacity -- maximum number of entries (positive integer)
    :size     -- alias for :capacity; overrides :capacity when both present

  No arguments: uses defaults from `nskk-cache-strategy' and
    `nskk-cache-default-capacity'.

When both :capacity and :size appear in a keyword call, :size takes
precedence because it is applied after :capacity.  In practice, pass
only one of the two.

TYPE defaults to `nskk-cache-strategy' (customizable, default \\='lru).
CAPACITY defaults to `nskk-cache-default-capacity' (customizable, default 1000).

Signals a `user-error' if TYPE is not a Prolog-registered cache type
(i.e., not a fact in cache-type/1).  Valid types are: lru, lfu."
  (let ((cache-type     nskk-cache-strategy)
        (cache-capacity nskk-cache-default-capacity))
    (cond
     ((null args))
     ((keywordp (car args))
      (let ((plist args))
        (when (plist-member plist :type)
          (setq cache-type (plist-get plist :type)))
        (when (plist-member plist :capacity)
          (setq cache-capacity (plist-get plist :capacity)))
        (when (plist-member plist :size)
          (setq cache-capacity (plist-get plist :size)))))
     (t
      (setq cache-type (car args))
      (when (cdr args)
        (setq cache-capacity (cadr args)))))
    ;; Validate via Prolog before constructing.  An unrecognized TYPE causes
    ;; nskk-prolog-holds-p to return nil, and the unless branch signals
    ;; user-error.  The pcase below is therefore exhaustive over valid types:
    ;; it will never reach an unmatched arm because the validation above
    ;; already rejects anything outside {lru, lfu}.
    (unless (nskk-prolog-holds-p `(cache-type ,cache-type))
      (user-error "Unknown cache type: %s; valid types: lru, lfu" cache-type))
    (pcase cache-type
      ('lru (nskk-cache-lru-create cache-capacity))
      ('lfu (nskk-cache-lfu-create cache-capacity)))))

;;;###autoload
(defun/k nskk-cache-create (&rest args)
  "Create a new NSKK cache of the specified type and capacity.
Calls on-found with the created cache object.  Always succeeds.

ARGS supports three calling conventions (see `nskk-cache-create--impl').

NOTE: Because `&rest args' cannot follow the continuations in the generated
`/k' signature, the generated `nskk-cache-create/k' has signature
  (on-found on-not-found &rest args)
where continuations come before the data arguments.  Use it directly;
it is not compatible with `<-' or `<-or'."
  (succeed (apply #'nskk-cache-create--impl args)))

;;;###autoload
(defun/k nskk-cache-get (cache key)
  "Get the value for KEY from CACHE in CPS style.
Calls on-found with the cached value on hit.
Calls on-not-found with no arguments on miss.

Unlike the sync `nskk-cache-get', this correctly distinguishes a stored
falsy value (nil, 0, \"\") from a cache miss, because it delegates to the
underlying type-specific /k implementation which tests for key presence
rather than value truthiness."
  (if (eq (nskk--cache-type-of cache) 'lru)
      (<-or val nskk-cache-lru-get cache key
            :found (succeed val)
            :fail (fail))
    (<-or val nskk-cache-lfu-get cache key
          :found (succeed val)
          :fail (fail))))

;;;###autoload
(defun/done nskk-cache-put (cache key value)
  "Store KEY with VALUE in CACHE."
  (nskk-cache-dispatch cache put key value))

;;;###autoload
(defun/k nskk-cache-invalidate (cache key)
  "Remove KEY from CACHE.
Returns t if KEY was found and removed, nil otherwise."
  (if (nskk-cache-dispatch cache invalidate key)
      (succeed t)
    (fail)))

;;;###autoload
(defun/done nskk-cache-clear (cache)
  "Remove all entries from CACHE and reset statistics."
  (nskk-cache-dispatch cache clear))

;;;###autoload
(defun/k nskk-cache-invalidate-pattern (cache pattern)
  "Remove all keys matching PATTERN from CACHE.
PATTERN is a regular expression matched against each key string.
Returns a list of the invalidated keys.

PATTERN must be a valid Emacs regexp; a malformed pattern will signal
`invalid-regexp'.  This is an internal API; PATTERN should always be a
literal regexp string from source code, never from user input."
  ;; Collect matching keys first to avoid mutating the hash table during
  ;; iteration, which is an anti-pattern even though Emacs technically
  ;; permits remhash inside maphash.
  (let ((keys-to-delete nil)
        (hash-table (nskk-cache-field cache hash)))
    (maphash (lambda (key _value)
               (when (string-match-p pattern key)
                 (push key keys-to-delete)))
             hash-table)
    (dolist (key keys-to-delete)
      (nskk-cache-invalidate cache key))
    (succeed keys-to-delete)))

;;;###autoload
(defun/k nskk-cache-p (cache)
  "Return non-nil if CACHE is a valid LRU or LFU cache structure."
  (if (or (nskk-cache-lru-p cache)
          (nskk-cache-lfu-p cache))
      (succeed t)
    (fail)))

;;;###autoload
(defun nskk-cache-stats (cache)
  "Return a statistics plist for CACHE.
The plist contains: :type, :capacity, :size, :hits, :misses, :hit-rate."
  (let* ((type     (nskk--cache-type-of cache))
         (capacity (nskk-cache-field cache capacity))
         (size     (nskk-cache-field cache size))
         (hits     (nskk-cache-field cache hits))
         (misses   (nskk-cache-field cache misses))
         (total    (+ hits misses))
         (hit-rate (if (> total 0) (/ (float hits) total) 0.0)))
    (list :type type
          :capacity capacity
          :size size
          :hits hits
          :misses misses
          :hit-rate hit-rate)))

;;;###autoload
(defun/k nskk-cache-hit-rate (cache)
  "Return the hit rate for CACHE as a float between 0.0 and 1.0."
  (succeed (plist-get (nskk-cache-stats cache) :hit-rate)))

;;;###autoload
(defun nskk-cache-size (cache)
  "Return the current number of entries in CACHE."
  (nskk-cache-dispatch cache size))

(provide 'nskk-cache)

;;; nskk-cache.el ends here
