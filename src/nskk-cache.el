;;; nskk-cache.el --- Cache mechanism for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

;; Author: takeokunn <bararararatty@gmail.com>
;; Maintainer: takeokunn <bararararatty@gmail.com>
;; URL: https://github.com/takeokunn/nskk.el
;; Keywords: i18n convenience

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

;; Cache mechanism for NSKK.
;;
;; Layer position: L1 (Core Engine) -- depends on nskk-prolog and nskk-cps-macros.

;;; Code:

(require 'cl-lib)
(require 'nskk-prolog)
(require 'nskk-cps-macros)

(defmacro nskk-cache--with-rollback (rollback &rest body)
  "Evaluate BODY and run ROLLBACK before re-signaling `error' or `quit'."
  (declare (indent 1) (debug (form body)))
  `(condition-case condition
       ,(macroexp-progn body)
     ((error quit)
      (let ((inhibit-quit t))
        ,rollback)
      (signal (car condition) (cdr condition)))))

(defun nskk--cache-key-parts (left right)
  "Return paired children for structurally comparable LEFT and RIGHT.
Return :different when their shapes or atomic values differ."
  (cond
   ((and (consp left) (consp right))
    (list (cons (car left) (car right))
          (cons (cdr left) (cdr right))))
   ((or (consp left) (consp right)) :different)
   ((and (vectorp left) (vectorp right)
         (= (length left) (length right)))
    (cl-loop for index below (length left)
             collect (cons (aref left index) (aref right index))))
   ((or (vectorp left) (vectorp right)) :different)
   ((equal left right) nil)
   (t :different)))
(defun nskk--cache-key-equal-p (left right)
  "Return non-nil when cache keys LEFT and RIGHT are structurally equal.
Unlike `equal', this comparison terminates for circular conses and vectors."
  (let ((pending (list (cons left right)))
        (seen (make-hash-table :test #'eq)))
    (catch 'different
      (while pending
        (let* ((pair (pop pending))
               (left-part (car pair))
               (right-part (cdr pair))
               (seen-rights (gethash left-part seen)))
          (unless (or (eq left-part right-part)
                      (memq right-part seen-rights))
            (puthash left-part (cons right-part seen-rights) seen)
            (let ((parts (nskk--cache-key-parts left-part right-part)))
              (if (eq parts :different)
                  (throw 'different nil)
                (setq pending (nconc parts pending)))))))
      t)))

(define-hash-table-test 'nskk-cache-key-equal
  #'nskk--cache-key-equal-p #'sxhash-equal)

(defgroup nskk-cache nil
  "Cache settings for NSKK."
  :prefix "nskk-cache-"
  :group 'nskk)

(defcustom nskk-cache-default-capacity 1000
  "Default cache capacity for LRU/LFU caches."
  :type 'natnum
  :safe #'natnump
  :package-version '(nskk . "0.1.0")
  :group 'nskk-cache)

(defcustom nskk-cache-strategy 'lru
  "Cache eviction strategy.
\\='lru means Least Recently Used.
\\='lfu means Least Frequently Used."
  :type '(choice (const :tag "LRU" lru)
                 (const :tag "LFU" lfu))
  :safe (lambda (v) (memq v '(lru lfu)))
  :package-version '(nskk . "0.1.0")
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

;; Cache constructor table: (cache-constructor TYPE CONSTRUCTOR-FN)
(nskk-prolog-define-fact-table cache-constructor (:arity 2 :index :hash)
  (lru nskk-cache-lru-create)
  (lfu nskk-cache-lfu-create))

;;; Cache Type Dispatch

(defun nskk--cache-type-of (cache)
  "Return the cache type symbol for CACHE.
Returns the symbol `lru' or `lfu'.  Signals an error for invalid CACHE."
  (pcase cache
    ((pred nskk-cache-lru-p) 'lru)
    ((pred nskk-cache-lfu-p) 'lfu)
    (_ (error "Invalid cache type: %S" cache))))

(defmacro nskk-cache-dispatch (cache op &rest args)
  "Dispatch OP on CACHE via the Prolog cache-dispatch-fn/3 table.
OP is a literal symbol (unquoted) naming the operation (e.g., get, put).
ARGS are passed through to the dispatched implementation function.
The dispatch function is resolved at runtime via Prolog hash-indexed lookup.

Design note: there is no per-instance cache-type/2 Prolog fact because the
structs carry no ID slot to key it on, and Emacs Lisp has no finalizers to
retract one on destruction, so it would leak.  `nskk--cache-type-of' gives
an O(1), allocation-free substitute instead."
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

(cl-defstruct (nskk-cache--base
               (:constructor nil)
               (:conc-name nskk-cache--base-)
               (:predicate nskk-cache-p)
               (:copier nil))
  "Shared cache state common to LRU and LFU caches.
Slots:
  capacity - maximum number of entries
  size     - current number of entries
  hash     - hash table mapping keys to cache-specific records
  hits     - cumulative cache hit count
  misses   - cumulative cache miss count"
  (capacity 1000 :type integer)
  (size     0    :type integer)
  (hash     nil  :type hash-table)
  (hits     0    :type integer)
  (misses   0    :type integer))

(cl-defstruct (nskk-cache-lru
               (:include nskk-cache--base)
               (:constructor nskk-cache-lru--create)
               (:copier nil))
  "LRU cache structure.
Slots:
  head - dummy head node (most-recently-used side)
  tail - dummy tail node (least-recently-used side)"
  head tail)

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

(cl-defstruct (nskk-cache-lfu
               (:include nskk-cache--base)
               (:constructor nskk-cache-lfu--create)
               (:copier nil))
  "LFU cache structure.
Slots:
  freq     - hash table mapping frequency to list of keys
  min-freq - current minimum frequency (used for eviction)"
  (freq     nil :type hash-table)
  (min-freq 0   :type integer))

;;; LRU Cache Implementation

(defun nskk-cache-lru-create (capacity)
  "Create an LRU cache with CAPACITY entries."
  (unless (and (integerp capacity) (> capacity 0))
    (user-error "Cache capacity must be a positive integer: %S" capacity))
  (let ((head (nskk-cache-lru-node--create))
        (tail (nskk-cache-lru-node--create)))
    (setf (nskk-cache-lru-node-next head) tail)
    (setf (nskk-cache-lru-node-prev tail) head)
    (nskk-cache-lru--create
     :capacity capacity
     :size     0
     :hash     (make-hash-table :test 'nskk-cache-key-equal :size capacity)
     :head     head
     :tail     tail
     :hits     0
     :misses   0)))

(defun nskk-cache-lru--remove-node (node)
  "Remove NODE from the doubly-linked list."
  (let ((prev-node (nskk-cache-lru-node-prev node))
        (next-node (nskk-cache-lru-node-next node)))
    (setf (nskk-cache-lru-node-next prev-node) next-node
          (nskk-cache-lru-node-prev next-node) prev-node)))

(defun nskk-cache-lru--add-to-head (cache node)
  "Insert NODE immediately after the dummy head of CACHE."
  (let* ((head (nskk-cache-lru-head cache))
         (next-node (nskk-cache-lru-node-next head)))
    (setf (nskk-cache-lru-node-next node) next-node
          (nskk-cache-lru-node-prev node) head
          (nskk-cache-lru-node-next head) node
          (nskk-cache-lru-node-prev next-node) node)))

(defun nskk-cache-lru--move-to-head (cache node)
  "Move NODE to the most-recently-used position in CACHE."
  (unless (eq node
              (nskk-cache-lru-node-next (nskk-cache-lru-head cache)))
    (nskk-cache-lru--remove-node node)
    (nskk-cache-lru--add-to-head cache node)))

(defun nskk-cache-lru--remove-tail (cache)
  "Remove and return the least-recently-used node from CACHE."
  (let* ((tail (nskk-cache-lru-tail cache))
         (node (nskk-cache-lru-node-prev tail)))
    (nskk-cache-lru--remove-node node)
    node))

(defun/k nskk-cache-lru--get-prepared (cache key preparer)
  "Get and prepare KEY from LRU CACHE before committing a hit."
  (let ((node (gethash key (nskk-cache-lru-hash cache)))
        found value)
    (if node
        (let ((prepared (funcall preparer (nskk-cache-lru-node-value node))))
          (let* ((head (nskk-cache-lru-head cache))
                 (old-prev (nskk-cache-lru-node-prev node))
                 (old-next (nskk-cache-lru-node-next node))
                 (old-head-next (nskk-cache-lru-node-next head))
                 (old-hits (nskk-cache-lru-hits cache)))
            (nskk-cache--with-rollback
                (setf (nskk-cache-lru-node-next old-prev) node
                      (nskk-cache-lru-node-prev old-next) node
                      (nskk-cache-lru-node-prev node) old-prev
                      (nskk-cache-lru-node-next node) old-next
                      (nskk-cache-lru-node-next head) old-head-next
                      (nskk-cache-lru-node-prev old-head-next) head
                      (nskk-cache-lru-hits cache) old-hits)
              (nskk-cache-lru--move-to-head cache node)
              (setf (nskk-cache-lru-hits cache) (1+ old-hits))
              (setq found t
                    value prepared))))
      (let ((old-misses (nskk-cache-lru-misses cache)))
        (nskk-cache--with-rollback
            (setf (nskk-cache-lru-misses cache) old-misses)
          (setf (nskk-cache-lru-misses cache) (1+ old-misses)))))
    (if found (succeed value) (fail))))

(defun/k nskk-cache-lru-get (cache key)
  "Get the value for KEY from LRU CACHE.
Returns nil on a cache miss.  Updates access order on hit."
  (<-or value nskk-cache-lru--get-prepared cache key #'identity
        :found (succeed value)
        :fail (fail)))

(defun nskk-cache-lru--put-existing-rollback
    (owned-key old-key node table old-value old-prev old-next head
               old-head-next)
  "Restore NODE's pre-update key, value, and list links after a faulted put."
  (remhash owned-key table)
  (puthash old-key node table)
  (setf (nskk-cache-lru-node-key node) old-key
        (nskk-cache-lru-node-value node) old-value
        (nskk-cache-lru-node-next old-prev) node
        (nskk-cache-lru-node-prev old-next) node
        (nskk-cache-lru-node-prev node) old-prev
        (nskk-cache-lru-node-next node) old-next
        (nskk-cache-lru-node-next head) old-head-next
        (nskk-cache-lru-node-prev old-head-next) head))

(defun nskk-cache-lru--put-existing-commit
    (cache table owned-key old-key node value)
  "Commit an update of NODE to OWNED-KEY/VALUE and move it to CACHE's head."
  (remhash old-key table)
  (puthash owned-key node table)
  (setf (nskk-cache-lru-node-key node) owned-key
        (nskk-cache-lru-node-value node) value)
  (nskk-cache-lru--move-to-head cache node))

(defun nskk-cache-lru--put-new-rollback
    (table owned-key evicted-node evicted-key evicted-prev head old-head-next
           tail old-tail-prev cache old-size)
  "Undo a faulted insertion of OWNED-KEY into CACHE's table and list."
  (remhash owned-key table)
  (when evicted-node
    (puthash evicted-key evicted-node table))
  (setf (nskk-cache-lru-node-next head) old-head-next
        (nskk-cache-lru-node-prev old-head-next) head
        (nskk-cache-lru-node-prev tail) old-tail-prev
        (nskk-cache-lru-node-next old-tail-prev) tail
        (nskk-cache-lru-size cache) old-size)
  (when evicted-node
    (setf (nskk-cache-lru-node-prev evicted-node) evicted-prev
          (nskk-cache-lru-node-next evicted-prev) evicted-node)))

(defun nskk-cache-lru--put-new-commit
    (cache table owned-key new-node will-evict evicted-key old-size)
  "Commit insertion of NEW-NODE into CACHE, evicting when WILL-EVICT."
  (puthash owned-key new-node table)
  (nskk-cache-lru--add-to-head cache new-node)
  (when will-evict
    (nskk-cache-lru--remove-tail cache)
    (remhash evicted-key table))
  (setf (nskk-cache-lru-size cache)
        (if will-evict old-size (1+ old-size))))

(defun/done nskk-cache-lru-put (cache key value)
  "Store KEY with VALUE in LRU CACHE.
Updates VALUE and access order if KEY already exists.
Evicts the least-recently-used entry when CACHE is at capacity."
  (let* ((owned-key (nskk-prolog-copy-term key))
         (table (nskk-cache-lru-hash cache))
         (node (gethash key table)))
    (if node
        (let* ((head (nskk-cache-lru-head cache))
               (old-key (nskk-cache-lru-node-key node))
               (old-value (nskk-cache-lru-node-value node))
               (old-prev (nskk-cache-lru-node-prev node))
               (old-next (nskk-cache-lru-node-next node))
               (old-head-next (nskk-cache-lru-node-next head)))
          (nskk-cache--with-rollback
              (nskk-cache-lru--put-existing-rollback
               owned-key old-key node table old-value old-prev old-next
               head old-head-next)
            (nskk-cache-lru--put-existing-commit
             cache table owned-key old-key node value)))
      (let* ((head (nskk-cache-lru-head cache))
             (tail (nskk-cache-lru-tail cache))
             (old-head-next (nskk-cache-lru-node-next head))
             (old-tail-prev (nskk-cache-lru-node-prev tail))
             (old-size (nskk-cache-lru-size cache))
             (will-evict (>= old-size (nskk-cache-lru-capacity cache)))
             (evicted-node (and will-evict old-tail-prev))
             (evicted-prev (and evicted-node
                                (nskk-cache-lru-node-prev evicted-node)))
             (evicted-key (and evicted-node
                               (nskk-cache-lru-node-key evicted-node)))
             (new-node (nskk-cache-lru-node--create
                        :key owned-key :value value)))
        (nskk-cache--with-rollback
            (nskk-cache-lru--put-new-rollback
             table owned-key evicted-node evicted-key evicted-prev head
             old-head-next tail old-tail-prev cache old-size)
          (nskk-cache-lru--put-new-commit
           cache table owned-key new-node will-evict evicted-key old-size))))))

(defun nskk-cache-lru--invalidate-rollback
    (table stored-key node old-prev old-next cache old-size)
  "Restore NODE and its list links in CACHE after a faulted invalidate."
  (puthash stored-key node table)
  (setf (nskk-cache-lru-node-next old-prev) node
        (nskk-cache-lru-node-prev old-next) node
        (nskk-cache-lru-node-prev node) old-prev
        (nskk-cache-lru-node-next node) old-next
        (nskk-cache-lru-size cache) old-size))

(defun/k nskk-cache-lru-invalidate (cache key)
  "Remove KEY from LRU CACHE.
Returns t if KEY was found and removed, nil otherwise."
  (let* ((table (nskk-cache-lru-hash cache))
         (node (gethash key table))
         removed)
    (when node
      (let ((stored-key (nskk-cache-lru-node-key node))
            (old-prev (nskk-cache-lru-node-prev node))
            (old-next (nskk-cache-lru-node-next node))
            (old-size (nskk-cache-lru-size cache)))
        (nskk-cache--with-rollback
            (nskk-cache-lru--invalidate-rollback
             table stored-key node old-prev old-next cache old-size)
          (nskk-cache-lru--remove-node node)
          (remhash stored-key table)
          (setf (nskk-cache-lru-size cache) (1- old-size))
          (setq removed t))))
    (if removed (succeed t) (fail))))

(defun/done nskk-cache-lru-clear (cache)
  "Remove all entries from LRU CACHE and reset statistics."
  (clrhash (nskk-cache-lru-hash cache))
  (setf (nskk-cache-lru-size cache) 0)
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
  (let (found key)
    (catch 'nskk-cache-lfu--bucket-entry-found
      (maphash
       (lambda (candidate _)
         (setq found t
               key candidate)
         (throw 'nskk-cache-lfu--bucket-entry-found nil))
       bucket))
    (if found (succeed key) (fail))))

(defun/done nskk-cache-lfu--evict-min-freq (cache)
  "Evict one entry at the minimum frequency from CACHE.
No-op when the min-freq bucket is absent or empty."
  (let* ((min-freq (nskk-cache-lfu-min-freq cache))
         (freq-table (nskk-cache-lfu-freq cache))
         (bucket (gethash min-freq freq-table)))
    (when bucket
      (nskk-cache-lfu--bucket-any-key/k bucket
        (lambda (evict-key)
          (remhash evict-key (nskk-cache-lfu-hash cache))
          (remhash evict-key bucket)
          (when (zerop (hash-table-count bucket))
            (remhash min-freq freq-table))
          (setf (nskk-cache-lfu-size cache)
                (1- (nskk-cache-lfu-size cache))))
        #'ignore))))

(defun nskk-cache-lfu-create (capacity)
  "Create an LFU cache with CAPACITY entries."
  (unless (and (integerp capacity) (> capacity 0))
    (user-error "Cache capacity must be a positive integer: %S" capacity))
  (nskk-cache-lfu--create
   :capacity capacity
   :size     0
   :hash     (make-hash-table :test 'nskk-cache-key-equal :size capacity)
   :freq     (make-hash-table :test 'equal :size capacity)
   :min-freq 0
   :hits     0
   :misses   0))

(defun nskk-cache-lfu--remove-from-freq-bucket
    (cache key old-freq new-freq)
  "Remove KEY from the OLD-FREQ bucket in LFU CACHE.
When the bucket becomes empty, remove it and advance min-freq to NEW-FREQ
if OLD-FREQ was the current minimum."
  (let* ((freq-table (nskk-cache-lfu-freq cache))
         (bucket (gethash old-freq freq-table)))
    (when bucket
      (remhash key bucket)
      (when (zerop (hash-table-count bucket))
        (remhash old-freq freq-table)
        (when (= old-freq (nskk-cache-lfu-min-freq cache))
          (setf (nskk-cache-lfu-min-freq cache) new-freq))))))

(defun nskk-cache-lfu--update-freq (cache entry old-freq)
  "Promote ENTRY in LFU CACHE from OLD-FREQ to its new frequency.
Removes ENTRY from the old frequency bucket and inserts into the new one.
Each bucket is a hash-table (key -> t) for O(1) add/remove.
Updates min-freq when the old minimum frequency bucket becomes empty."
  (let ((freq-table (nskk-cache-lfu-freq cache))
        (key (nskk-cache-lfu-entry-key entry))
        (new-freq (nskk-cache-lfu-entry-frequency entry)))
    (when old-freq
      (nskk-cache-lfu--remove-from-freq-bucket
       cache key old-freq new-freq))
    (let ((bucket (gethash new-freq freq-table)))
      (unless bucket
        (setq bucket (make-hash-table :test 'nskk-cache-key-equal :size 4))
        (puthash new-freq bucket freq-table))
      (puthash key t bucket))))

(defun nskk-cache-lfu--get-hit-rollback
    (freq-table new-freq stored-key old-new-bucket old-freq old-bucket entry
                old-min-freq cache old-hits)
  "Undo a faulted frequency promotion of ENTRY in CACHE."
  (let ((current-new-bucket (gethash new-freq freq-table)))
    (when current-new-bucket
      (remhash stored-key current-new-bucket)))
  (if old-new-bucket
      (puthash new-freq old-new-bucket freq-table)
    (remhash new-freq freq-table))
  (puthash old-freq old-bucket freq-table)
  (puthash stored-key t old-bucket)
  (setf (nskk-cache-lfu-entry-frequency entry) old-freq
        (nskk-cache-lfu-min-freq cache) old-min-freq
        (nskk-cache-lfu-hits cache) old-hits))

(defun nskk-cache-lfu--get-hit-commit (cache entry old-freq new-freq old-hits)
  "Promote ENTRY's frequency in CACHE and record the hit."
  (setf (nskk-cache-lfu-entry-frequency entry) new-freq)
  (nskk-cache-lfu--update-freq cache entry old-freq)
  (setf (nskk-cache-lfu-hits cache) (1+ old-hits)))

(defun/k nskk-cache-lfu--get-prepared (cache key preparer)
  "Get and prepare KEY from LFU CACHE before committing a hit."
  (let ((entry (gethash key (nskk-cache-lfu-hash cache)))
        found value)
    (if entry
        (let ((prepared (funcall preparer (nskk-cache-lfu-entry-value entry))))
          (let* ((stored-key (nskk-cache-lfu-entry-key entry))
                 (old-freq (nskk-cache-lfu-entry-frequency entry))
                 (new-freq (1+ old-freq))
                 (freq-table (nskk-cache-lfu-freq cache))
                 (old-bucket (gethash old-freq freq-table))
                 (old-new-bucket (gethash new-freq freq-table))
                 (old-min-freq (nskk-cache-lfu-min-freq cache))
                 (old-hits (nskk-cache-lfu-hits cache)))
            (nskk-cache--with-rollback
                (nskk-cache-lfu--get-hit-rollback
                 freq-table new-freq stored-key old-new-bucket old-freq
                 old-bucket entry old-min-freq cache old-hits)
              (nskk-cache-lfu--get-hit-commit
               cache entry old-freq new-freq old-hits)
              (setq found t
                    value prepared))))
      (let ((old-misses (nskk-cache-lfu-misses cache)))
        (nskk-cache--with-rollback
            (setf (nskk-cache-lfu-misses cache) old-misses)
          (setf (nskk-cache-lfu-misses cache) (1+ old-misses)))))
    (if found (succeed value) (fail))))

(defun/k nskk-cache-lfu-get (cache key)
  "Get the value for KEY from LFU CACHE.
Returns nil on a cache miss.  Increments access frequency on hit."
  (<-or value nskk-cache-lfu--get-prepared cache key #'identity
        :found (succeed value)
        :fail (fail)))

(defun nskk-cache-lfu--put-existing-rollback
    (table owned-key old-key entry freq-table new-freq old-new-bucket
           old-freq old-bucket old-value old-min-freq cache)
  "Undo a faulted update of ENTRY back to its pre-put state in CACHE."
  (remhash owned-key table)
  (puthash old-key entry table)
  (let ((current-new-bucket (gethash new-freq freq-table)))
    (when current-new-bucket
      (remhash old-key current-new-bucket)
      (remhash owned-key current-new-bucket)))
  (if old-new-bucket
      (puthash new-freq old-new-bucket freq-table)
    (remhash new-freq freq-table))
  (puthash old-freq old-bucket freq-table)
  (puthash old-key t old-bucket)
  (setf (nskk-cache-lfu-entry-key entry) old-key
        (nskk-cache-lfu-entry-value entry) old-value
        (nskk-cache-lfu-entry-frequency entry) old-freq
        (nskk-cache-lfu-min-freq cache) old-min-freq))

(defun nskk-cache-lfu--put-existing-commit
    (cache table owned-key old-key entry value new-freq old-freq freq-table)
  "Commit an update of ENTRY to OWNED-KEY/VALUE at NEW-FREQ in CACHE."
  (remhash old-key table)
  (puthash owned-key entry table)
  (setf (nskk-cache-lfu-entry-value entry) value
        (nskk-cache-lfu-entry-frequency entry) new-freq)
  (nskk-cache-lfu--update-freq cache entry old-freq)
  (let ((new-bucket (gethash new-freq freq-table)))
    (remhash old-key new-bucket)
    (puthash owned-key t new-bucket))
  (setf (nskk-cache-lfu-entry-key entry) owned-key))

(defun nskk-cache-lfu--put-new-rollback
    (table owned-key evicted-entry evicted-key evicted-bucket evicted-freq
           freq-table old-bucket-one cache old-size old-min-freq)
  "Undo a faulted insertion of OWNED-KEY into CACHE."
  (remhash owned-key table)
  (when evicted-entry
    (puthash evicted-key evicted-entry table))
  (let ((current-bucket-one (gethash 1 freq-table)))
    (when current-bucket-one
      (remhash owned-key current-bucket-one)))
  (if old-bucket-one
      (puthash 1 old-bucket-one freq-table)
    (remhash 1 freq-table))
  (when evicted-entry
    (puthash evicted-freq evicted-bucket freq-table)
    (puthash evicted-key t evicted-bucket))
  (setf (nskk-cache-lfu-size cache) old-size
        (nskk-cache-lfu-min-freq cache) old-min-freq))

(defun nskk-cache-lfu--put-new-commit
    (cache table owned-key new-entry will-evict old-size)
  "Commit insertion of NEW-ENTRY into CACHE, evicting when WILL-EVICT."
  (when will-evict
    (nskk-cache-lfu--evict-min-freq cache))
  (puthash owned-key new-entry table)
  (nskk-cache-lfu--update-freq cache new-entry nil)
  (setf (nskk-cache-lfu-min-freq cache) 1
        (nskk-cache-lfu-size cache)
        (if will-evict old-size (1+ old-size))))

(defun/done nskk-cache-lfu-put (cache key value)
  "Store KEY with VALUE in LFU CACHE.
Updates VALUE and increments frequency if KEY already exists.
Evicts the least-frequently-used entry when CACHE is at capacity."
  (let* ((owned-key (nskk-prolog-copy-term key))
         (table (nskk-cache-lfu-hash cache))
         (entry (gethash key table)))
    (if entry
        (let* ((old-key (nskk-cache-lfu-entry-key entry))
               (old-value (nskk-cache-lfu-entry-value entry))
               (old-freq (nskk-cache-lfu-entry-frequency entry))
               (new-freq (1+ old-freq))
               (freq-table (nskk-cache-lfu-freq cache))
               (old-bucket (gethash old-freq freq-table))
               (old-new-bucket (gethash new-freq freq-table))
               (old-min-freq (nskk-cache-lfu-min-freq cache)))
          (nskk-cache--with-rollback
              (nskk-cache-lfu--put-existing-rollback
               table owned-key old-key entry freq-table new-freq
               old-new-bucket old-freq old-bucket old-value old-min-freq
               cache)
            (nskk-cache-lfu--put-existing-commit
             cache table owned-key old-key entry value new-freq old-freq
             freq-table)))
      (let* ((freq-table (nskk-cache-lfu-freq cache))
             (old-size (nskk-cache-lfu-size cache))
             (old-min-freq (nskk-cache-lfu-min-freq cache))
             (old-bucket-one (gethash 1 freq-table))
             (will-evict (>= old-size (nskk-cache-lfu-capacity cache)))
             (evicted-freq (and will-evict old-min-freq))
             (evicted-bucket (and will-evict
                                  (gethash evicted-freq freq-table)))
             evicted-key evicted-entry)
        (when evicted-bucket
          (catch 'nskk-cache-lfu--rollback-key-found
            (maphash
             (lambda (candidate _)
               (setq evicted-key candidate
                     evicted-entry (gethash candidate table))
               (throw 'nskk-cache-lfu--rollback-key-found nil))
             evicted-bucket)))
        (let ((new-entry (nskk-cache-lfu-entry--create
                          :key owned-key :value value :frequency 1)))
          (nskk-cache--with-rollback
              (nskk-cache-lfu--put-new-rollback
               table owned-key evicted-entry evicted-key evicted-bucket
               evicted-freq freq-table old-bucket-one cache old-size
               old-min-freq)
            (nskk-cache-lfu--put-new-commit
             cache table owned-key new-entry will-evict old-size)))))))

(defun nskk-cache-lfu--invalidate-rollback
    (table stored-key entry bucket freq freq-table cache old-size)
  "Restore ENTRY and its bucket membership in CACHE after a faulted invalidate."
  (puthash stored-key entry table)
  (when bucket
    (puthash freq bucket freq-table)
    (puthash stored-key t bucket))
  (setf (nskk-cache-lfu-size cache) old-size))

(defun/k nskk-cache-lfu-invalidate (cache key)
  "Remove KEY from LFU CACHE.
Returns t if KEY was found and removed, nil otherwise."
  (let* ((table (nskk-cache-lfu-hash cache))
         (entry (gethash key table))
         removed)
    (when entry
      (let* ((stored-key (nskk-cache-lfu-entry-key entry))
             (freq-table (nskk-cache-lfu-freq cache))
             (freq (nskk-cache-lfu-entry-frequency entry))
             (bucket (gethash freq freq-table))
             (old-size (nskk-cache-lfu-size cache)))
        (nskk-cache--with-rollback
            (nskk-cache-lfu--invalidate-rollback
             table stored-key entry bucket freq freq-table cache old-size)
          (when bucket
            (remhash stored-key bucket)
            (when (zerop (hash-table-count bucket))
              (remhash freq freq-table)))
          (remhash stored-key table)
          (setf (nskk-cache-lfu-size cache) (1- old-size))
          (setq removed t))))
    (if removed (succeed t) (fail))))

(defun/done nskk-cache-lfu-clear (cache)
  "Remove all entries from LFU CACHE and reset statistics."
  (clrhash (nskk-cache-lfu-hash cache))
  (clrhash (nskk-cache-lfu-freq cache))
  (setf (nskk-cache-lfu-size     cache) 0
        (nskk-cache-lfu-min-freq cache) 0
        (nskk-cache-lfu-hits     cache) 0
        (nskk-cache-lfu-misses   cache) 0))

;;; Unified Interface

(defun nskk-cache-create (&rest args)
  "Create a new NSKK cache of the specified type and capacity.
Returns the created cache object.

ARGS accepts keyword arguments:
  :type     -- cache algorithm symbol, either \\='lru or \\='lfu
               defaults to `nskk-cache-strategy'
  :capacity -- maximum number of entries (positive integer)
               defaults to `nskk-cache-default-capacity'
  :size     -- alias for :capacity; takes precedence when both present

Calling with no arguments uses the defaults:
  (nskk-cache-create) ; => cache of `nskk-cache-strategy' type, default capacity

Signals a `user-error\\=' if :type is not a Prolog-registered cache type
\\(i.e., not a fact in cache-type/1), or if the requested capacity is not a
positive integer.  Valid types are: lru, lfu."
  (let* ((plist         args)
         (cache-type     (if (plist-member plist :type)
                             (plist-get plist :type)
                           nskk-cache-strategy))
         (cache-capacity (cond
                          ((plist-member plist :size)     (plist-get plist :size))
                          ((plist-member plist :capacity) (plist-get plist :capacity))
                          (t nskk-cache-default-capacity))))
    (unless (and (integerp cache-capacity) (> cache-capacity 0))
      (user-error "Cache capacity must be a positive integer: %S" cache-capacity))
    (unless (nskk-prolog-holds-p `(cache-type ,cache-type))
      (user-error "Unknown cache type: %s; valid types: lru, lfu" cache-type))
    (let ((ctor (nskk-prolog-query-value
                 `(cache-constructor ,cache-type \?fn) '\?fn)))
      (funcall ctor cache-capacity))))

(defun/k nskk-cache-get-prepared (cache key preparer)
  "Get and prepare the value for KEY from CACHE in CPS style.
On a hit, PREPARER is called exactly once with the stored value before any
cache hit mutation is committed.  Its return value is passed to ON-FOUND.
On a miss, PREPARER is not called and ON-NOT-FOUND receives no arguments.

If PREPARER signals an error or quit, CACHE remains unchanged.  PREPARER must
not access or mutate CACHE, directly or indirectly; cache reentry from it is
unsupported."
  (if (eq (nskk--cache-type-of cache) 'lru)
      (<-or val nskk-cache-lru--get-prepared cache key preparer
            :found (succeed val)
            :fail (fail))
    (<-or val nskk-cache-lfu--get-prepared cache key preparer
          :found (succeed val)
          :fail (fail))))

(defun/k nskk-cache-get (cache key)
  "Get the value for KEY from CACHE in CPS style.
Calls on-found with the cached value on hit.
Calls on-not-found with no arguments on miss.

Unlike its synchronous wrapper, this correctly distinguishes a stored
falsy value (nil, 0, \"\") from a cache miss, because it delegates to the
underlying type-specific /k implementation which tests for key presence
rather than value truthiness."
  (<-or val nskk-cache-get-prepared cache key #'identity
        :found (succeed val)
        :fail (fail)))

(defun/done nskk-cache-put (cache key value)
  "Store KEY with VALUE in CACHE."
  (nskk-cache-dispatch cache put key value))

(defun/k nskk-cache-invalidate (cache key)
  "Remove KEY from CACHE.
Returns t if KEY was found and removed, nil otherwise."
  (if (nskk-cache-dispatch cache invalidate key)
      (succeed t)
    (fail)))

(defun/done nskk-cache-clear (cache)
  "Remove all entries from CACHE and reset statistics."
  (nskk-cache-dispatch cache clear))

(defun nskk-cache-invalidate-pattern (cache pattern)
  "Remove all keys matching PATTERN from CACHE.
PATTERN is a regular expression matched against each string key.
Non-string keys are retained.  Returns a list of the invalidated keys.

PATTERN must be a valid Emacs regexp; a malformed pattern will signal
`invalid-regexp'.  This is an internal API; PATTERN should always be a
literal regexp string from source code, never from user input."
  ;; Collect matching keys first to avoid mutating the hash table during
  ;; iteration, which is an anti-pattern even though Emacs technically
  ;; permits remhash inside maphash.
  (let ((keys-to-delete nil)
        (hash-table (nskk-cache--base-hash cache)))
    (maphash (lambda (key _value)
               (when (and (stringp key)
                          (string-match-p pattern key))
                 (push key keys-to-delete)))
             hash-table)
    (dolist (key keys-to-delete)
      (nskk-cache-invalidate/k cache key #'ignore #'ignore))
    keys-to-delete))

(defun nskk-cache-stats (cache)
  "Return a statistics plist for CACHE.
The plist contains: :type, :capacity, :size, :hits, :misses, :hit-rate."
  (let* ((type     (nskk--cache-type-of cache))
         (capacity (nskk-cache--base-capacity cache))
         (size     (nskk-cache--base-size cache))
         (hits     (nskk-cache--base-hits cache))
         (misses   (nskk-cache--base-misses cache))
         (total    (+ hits misses))
         (hit-rate (if (> total 0) (/ (float hits) total) 0.0)))
    (list :type type
          :capacity capacity
          :size size
          :hits hits
          :misses misses
          :hit-rate hit-rate)))

(defun nskk-cache-hit-rate (cache)
  "Return the hit rate for CACHE as a float between 0.0 and 1.0."
  (plist-get (nskk-cache-stats cache) :hit-rate))

;;;###autoload
(defun nskk-cache-size (cache)
  "Return the current number of entries in CACHE."
  (nskk-cache-dispatch cache size))

(cl-defstruct (nskk-cache--snapshot
               (:constructor nskk-cache--snapshot--create)
               (:copier nil))
  "Full rollback snapshot of a cache's contents.
Captures hash-table entries verbatim.  For an LRU cache, only the head/tail
sentinel nodes and head's `next' / tail's `prev' links are captured;
interior node `prev'/`next' links are never captured or repaired.  For an
LFU cache, only the top level of the frequency table \(frequency -> bucket\)
is captured, not each bucket's own contents.
Slots:
  kind         - `lru' or `lfu'
  cache        - the cache object this snapshot was captured from
  capacity     - captured capacity
  size         - captured size
  hash-table   - the cache's hash-table object
  hash-entries - alist of the hash-table's key/value entries at capture time
  head         - captured LRU head node (nil for lfu)
  tail         - captured LRU tail node (nil for lfu)
  head-next    - captured value of head's next node (nil for lfu)
  tail-prev    - captured value of tail's prev node (nil for lfu)
  freq-table   - the LFU freq hash-table object (nil for lru)
  freq-entries - alist of the freq-table's top-level entries (nil for lru)
  min-freq     - captured min-freq (nil for lru)
  hits         - captured hits
  misses       - captured misses"
  kind cache capacity size hash-table hash-entries
  head tail head-next tail-prev
  freq-table freq-entries min-freq
  hits misses)

(cl-defstruct (nskk-cache--metadata-snapshot
               (:constructor nskk-cache--metadata-snapshot--create)
               (:copier nil))
  "Shallow rollback snapshot of a cache's metadata.
Captures object identities and scalar counters only; hash-table and node
CONTENTS are not captured.
Slots:
  kind     - `lru' or `lfu'
  cache    - the cache object this snapshot was captured from
  capacity - captured capacity
  size     - captured size
  hash     - captured hash-table object
  head     - captured LRU head node object (nil for lfu)
  tail     - captured LRU tail node object (nil for lfu)
  freq     - captured LFU freq-table object (nil for lru)
  min-freq - captured min-freq (nil for lru)
  hits     - captured hits
  misses   - captured misses"
  kind cache capacity size hash head tail freq min-freq hits misses)

(defun nskk-cache--snapshot-hash-table-entries (table)
  "Return an alist of TABLE's key/value entries."
  (let (entries)
    (maphash (lambda (key value) (push (cons key value) entries)) table)
    entries))

(defun nskk-cache--snapshot-restore-hash-table (table entries)
  "Reset TABLE in place to hold exactly ENTRIES."
  (clrhash table)
  (dolist (entry entries)
    (puthash (car entry) (cdr entry) table)))

(defun nskk-cache-capture-snapshot (cache)
  "Capture a full rollback snapshot of CACHE's contents.
Returns an `nskk-cache--snapshot'.  Restore with `nskk-cache-restore-snapshot'.

For an LRU CACHE, only the head/tail sentinel nodes and head's `next' /
tail's `prev' links are captured; interior node `prev'/`next' links are NOT
captured.  For an LFU CACHE, only the top level of the frequency table
\(frequency -> bucket\) is captured; the buckets are hash tables whose
contents are NOT captured."
  (let ((hash-table (nskk-cache--base-hash cache))
        (capacity (nskk-cache--base-capacity cache))
        (size (nskk-cache--base-size cache))
        (hits (nskk-cache--base-hits cache))
        (misses (nskk-cache--base-misses cache)))
    (cond
     ((nskk-cache-lru-p cache)
      (let ((head (nskk-cache-lru-head cache))
            (tail (nskk-cache-lru-tail cache)))
        (nskk-cache--snapshot--create
         :kind 'lru :cache cache
         :capacity capacity :size size
         :hash-table hash-table
         :hash-entries (nskk-cache--snapshot-hash-table-entries hash-table)
         :head head :tail tail
         :head-next (nskk-cache-lru-node-next head)
         :tail-prev (nskk-cache-lru-node-prev tail)
         :hits hits :misses misses)))
     ((nskk-cache-lfu-p cache)
      (let ((freq-table (nskk-cache-lfu-freq cache)))
        (nskk-cache--snapshot--create
         :kind 'lfu :cache cache
         :capacity capacity :size size
         :hash-table hash-table
         :hash-entries (nskk-cache--snapshot-hash-table-entries hash-table)
         :freq-table freq-table
         :freq-entries (nskk-cache--snapshot-hash-table-entries freq-table)
         :min-freq (nskk-cache-lfu-min-freq cache)
         :hits hits :misses misses))))))

(defun nskk-cache-restore-snapshot (snapshot)
  "Restore the cache state recorded in SNAPSHOT in place.
See `nskk-cache-capture-snapshot' for what is captured, including its LRU
interior-link and LFU bucket-content limitations."
  (let ((cache (nskk-cache--snapshot-cache snapshot)))
    (nskk-cache--snapshot-restore-hash-table
     (nskk-cache--snapshot-hash-table snapshot)
     (nskk-cache--snapshot-hash-entries snapshot))
    (setf (nskk-cache--base-capacity cache) (nskk-cache--snapshot-capacity snapshot)
          (nskk-cache--base-size cache) (nskk-cache--snapshot-size snapshot)
          (nskk-cache--base-hash cache) (nskk-cache--snapshot-hash-table snapshot)
          (nskk-cache--base-hits cache) (nskk-cache--snapshot-hits snapshot)
          (nskk-cache--base-misses cache) (nskk-cache--snapshot-misses snapshot))
    (pcase (nskk-cache--snapshot-kind snapshot)
      ('lru
       (let ((head (nskk-cache--snapshot-head snapshot))
             (tail (nskk-cache--snapshot-tail snapshot)))
         (setf (nskk-cache-lru-head cache) head
               (nskk-cache-lru-tail cache) tail
               (nskk-cache-lru-node-next head) (nskk-cache--snapshot-head-next snapshot)
               (nskk-cache-lru-node-prev tail) (nskk-cache--snapshot-tail-prev snapshot))))
      ('lfu
       (nskk-cache--snapshot-restore-hash-table
        (nskk-cache--snapshot-freq-table snapshot)
        (nskk-cache--snapshot-freq-entries snapshot))
       (setf (nskk-cache-lfu-freq cache) (nskk-cache--snapshot-freq-table snapshot)
             (nskk-cache-lfu-min-freq cache) (nskk-cache--snapshot-min-freq snapshot))))))

(defun nskk-cache-capture-metadata-snapshot (cache)
  "Capture CACHE metadata needed to undo a failed miss observation.
Returns an `nskk-cache--metadata-snapshot'.  Restore with
`nskk-cache-restore-metadata-snapshot'.

Captures capacity, size, hits, misses, and the hash-table object identity,
plus for an LRU CACHE the head/tail node objects or for an LFU CACHE the
freq-table object and min-freq.  Hash-table and node CONTENTS are not
captured; only object identities are."
  (let ((capacity (nskk-cache--base-capacity cache))
        (size (nskk-cache--base-size cache))
        (hash (nskk-cache--base-hash cache))
        (hits (nskk-cache--base-hits cache))
        (misses (nskk-cache--base-misses cache)))
    (cond
     ((nskk-cache-lru-p cache)
      (nskk-cache--metadata-snapshot--create
       :kind 'lru :cache cache
       :capacity capacity :size size :hash hash
       :head (nskk-cache-lru-head cache)
       :tail (nskk-cache-lru-tail cache)
       :hits hits :misses misses))
     ((nskk-cache-lfu-p cache)
      (nskk-cache--metadata-snapshot--create
       :kind 'lfu :cache cache
       :capacity capacity :size size :hash hash
       :freq (nskk-cache-lfu-freq cache)
       :min-freq (nskk-cache-lfu-min-freq cache)
       :hits hits :misses misses)))))

(defun nskk-cache-restore-metadata-snapshot (snapshot)
  "Restore CACHE metadata recorded in SNAPSHOT in place.
Only object identities and scalar counters are restored; hash-table and
node CONTENTS are unaffected.  See `nskk-cache-capture-metadata-snapshot'."
  (let ((cache (nskk-cache--metadata-snapshot-cache snapshot)))
    (setf (nskk-cache--base-capacity cache) (nskk-cache--metadata-snapshot-capacity snapshot)
          (nskk-cache--base-size cache) (nskk-cache--metadata-snapshot-size snapshot)
          (nskk-cache--base-hash cache) (nskk-cache--metadata-snapshot-hash snapshot)
          (nskk-cache--base-hits cache) (nskk-cache--metadata-snapshot-hits snapshot)
          (nskk-cache--base-misses cache) (nskk-cache--metadata-snapshot-misses snapshot))
    (pcase (nskk-cache--metadata-snapshot-kind snapshot)
      ('lru
       (setf (nskk-cache-lru-head cache) (nskk-cache--metadata-snapshot-head snapshot)
             (nskk-cache-lru-tail cache) (nskk-cache--metadata-snapshot-tail snapshot)))
      ('lfu
       (setf (nskk-cache-lfu-freq cache) (nskk-cache--metadata-snapshot-freq snapshot)
             (nskk-cache-lfu-min-freq cache) (nskk-cache--metadata-snapshot-min-freq snapshot))))))

(provide 'nskk-cache)

;;; nskk-cache.el ends here
