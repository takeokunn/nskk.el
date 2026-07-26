;;; nskk-prolog.el --- Embedded Prolog engine for NSKK -*- lexical-binding: t; -*-

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

;; Embedded Prolog engine for NSKK (Layer 0: Foundation).
;;
;; Layer position: L0 (Foundation) -- no dependencies on other NSKK modules.
;;
;; Self-contained Prolog engine providing unification, backtracking, and
;; declarative rule matching for NSKK's conversion and dispatch rules.
;; Used by all other NSKK modules to express logic as facts and rules
;; rather than imperative conditionals.
;;
;; Features:
;; - First-order unification without occurs check
;; - Depth-first search with backtracking (continuation-passing style engine)
;; - Cut (!) and negation-as-failure (not)
;; - Three index strategies: hash (O(1)), trie (prefix), list (scan)
;; - Assert/retract for dynamic clause management
;; - Arithmetic built-in goals: is/2 (supports +, -, *, / expressions), </2, >/2, <=/2, >=/2, =:=/2
;; - DSL macros for natural Prolog-like syntax
;;
;; Performance target: single query < 20us with hash indexing.
;;
;; Key public API:
;;
;; Assert / retract:
;; - `nskk-prolog-<-'               -- assert a fact or rule (DSL macro)
;; - `nskk-prolog-deffacts'         -- assert multiple facts in one declaration
;; - `nskk-prolog-define-fact-table' -- set-index + deffacts in one declaration
;; - `nskk-prolog-bulk-facts'       -- assert facts from a runtime list (defconst-friendly)
;; - `nskk-prolog-assert'           -- assert a clause (low-level)
;; - `nskk-prolog-retract'          -- retract first matching clause
;; - `nskk-prolog-retract-all'      -- retract all clauses for a predicate
;; - `nskk-prolog-clear-database'   -- reset the entire database
;; - `nskk-prolog-trie-bulk-assert' -- bulk-load a large fact table into a trie
;;
;; Query:
;; - `nskk-prolog-?-'               -- query for first solution (DSL macro)
;; - `nskk-prolog-query'            -- query, return all solution substitutions
;; - `nskk-prolog-query-one'        -- query, return first solution substitution
;; - `nskk-prolog-query-value'      -- query and extract one variable binding
;; - `nskk-prolog-query-values'     -- query and extract multiple bindings
;; - `nskk-prolog-query-all-values' -- query and extract all bindings for var
;; - `nskk-prolog-holds-p'          -- test if a goal has any solution (boolean)
;; - `nskk-when-prolog-holds'       -- guard macro: run body when query holds
;;
;; Prove (lower-level query):
;; - `nskk-prolog-prove'            -- prove goals list, return all substitutions
;; - `nskk-prolog-prove-one'        -- prove goals list, return first substitution
;;
;; Indexing:
;; - `nskk-prolog-set-index'        -- configure index strategy (:hash/:trie/:list)
;; - `nskk-prolog-trie-prefix-search' -- prefix search via trie index
;;
;; Term inspection:
;; - `nskk-prolog-variable-p'       -- test for Prolog variable symbol
;; - `nskk-prolog-ground-p'         -- test for ground (variable-free) term
;; - `nskk-prolog-walk'             -- dereference a variable in a substitution
;; - `nskk-prolog-substitute'       -- apply substitution to any term
;; - `nskk-prolog-unify'            -- unify two terms under a substitution
;;
;; Usage:
;;
;;   (nskk-prolog-clear-database)
;;   (nskk-prolog-<- (parent tom bob))
;;   (nskk-prolog-<- (parent bob ann))
;;   (nskk-prolog-<- (grandparent \?x \?z)
;;     (parent \?x \?y) (parent \?y \?z))
;;   (nskk-prolog-query-value '(grandparent tom \?who) '\?who)
;;   ;; => ann
;;
;; Note: Prolog variables use `?' prefix.  In Emacs Lisp source,
;; escape the `?' with backslash: `\?x', `\?who', `\?_'.
;;
;; Known limitations and design decisions:
;;
;; 1. No occurs check: Unification does not detect circular bindings
;;    (e.g., unifying ?x with (f ?x)).  Safe for NSKK's ground conversion
;;    rules, which never produce cyclic terms.
;;
;; 2. Non-standard cut semantics: Cut (!) uses per-clause catch/throw.
;;    Alternative clauses for the same predicate are still tried after a
;;    cut -- cut prevents only goals *after* the cut in the current clause
;;    body from being retried.  Standard Prolog cut prunes all remaining
;;    alternatives; this engine does not.
;;
;; 3. Global database: All Prolog facts are stored in a single global
;;    hash table shared across all Emacs buffers.  There is no
;;    per-buffer isolation.
;;
;; 4. Ground query return value: `nskk-prolog-query-one' returns t for
;;    ground success (empty substitution) and nil for no-solution, so
;;    callers can distinguish the two cases.  Use `nskk-prolog-query'
;;    when you need the actual substitution alist for ground queries.

;;; Code:

(require 'cl-lib)
(eval-when-compile (require 'nskk-cps-macros))
(require 'nskk-trie)

;;;; Variable Representation

(defsubst nskk-prolog-variable-p (x)
  "Return non-nil if X is a Prolog variable.
Prolog variables are symbols whose name starts with `?'
\(e.g., \\='\\?x, \\='\\?char, \\='\\?_).
In Emacs Lisp source code, write them as \\?x, \\?char, \\?_."
  (and (symbolp x)
       (string-prefix-p "?" (symbol-name x))))

(defsubst nskk--prolog-anonymous-p (x)
  "Return non-nil if X is the anonymous variable wildcard.
X is compared against the character literal `?_' (integer 95, the
underscore character), not the Prolog-variable symbol `\\='\\?_'.
This is intentional: the anonymous wildcard in clause bodies is
written as `?_' (bare character literal) to avoid creating a named
binding, whereas `\\='\\?_' would be a regular named variable."
  (eq x ?_))

;;;; Substitution / Walk

(defun nskk-prolog-walk (term subst)
  "Follow binding chains in SUBST until TERM is ground or unbound.
TERM is a Prolog term (atom, variable, or list).
SUBST is an alist of (variable . value) bindings.
Returns the fully dereferenced term.

Example: if SUBST is ((\\?x . \\?y) (\\?y . \"ka\")),
then (nskk-prolog-walk \\='\\?x subst) returns \"ka\"."
  (if (nskk-prolog-variable-p term)
      (let ((binding (assq term subst)))
        (if binding
            (nskk-prolog-walk (cdr binding) subst)
          term))
    term))

;;;; Unification

(defconst nskk--prolog-fail :fail
  "Sentinel value representing unification failure.
Distinguished from nil, which is a valid empty substitution.")

(defsubst nskk--prolog-fail-p (x)
  "Return non-nil if X represents unification failure."
  (eq x :fail))

;; Explicit pair: sync wrapper preserves :fail sentinel for callers that
;; check (nskk--prolog-fail-p result).  Standard defun/k would return nil.
(defun nskk-prolog-unify/k (term1 term2 subst on-found on-not-found)
  "Unify TERM1 and TERM2 under substitution SUBST in CPS style.
ON-FOUND receives the extended substitution on success.
ON-NOT-FOUND is called with no arguments on failure."
  (let ((t1 (nskk-prolog-walk term1 subst))
        (t2 (nskk-prolog-walk term2 subst)))
    (cond
     ((equal t1 t2)                (funcall on-found subst))
     ((nskk--prolog-anonymous-p t1) (funcall on-found subst))
     ((nskk--prolog-anonymous-p t2) (funcall on-found subst))
     ((nskk-prolog-variable-p t1)
      (funcall on-found (cons (cons t1 t2) subst)))
     ((nskk-prolog-variable-p t2)
      (funcall on-found (cons (cons t2 t1) subst)))
     ((and (consp t1) (consp t2))
      (nskk-prolog-unify/k
       (car t1) (car t2) subst
       (lambda (s)
         (nskk-prolog-unify/k (cdr t1) (cdr t2) s on-found on-not-found))
       on-not-found))
     (t (funcall on-not-found)))))
(put 'nskk-prolog-unify/k 'nskk--cps-continuation-pattern :found-not-found)

(defun nskk-prolog-unify (term1 term2 subst)
  "Unify TERM1 and TERM2 under substitution SUBST.
Returns the extended substitution on success, or `:fail' on failure.
An empty substitution nil is success, not failure.
Does not perform occurs check (not needed for nskk terms)."
  (nskk-prolog-unify/k term1 term2 subst #'identity (lambda () :fail)))

;;;; Clause Database

(defvar nskk--prolog-database (make-hash-table :test 'equal)
  "Clause database keyed by \"predicate/arity\" string.
Each value is a list of clauses in insertion order.
A clause is (head . body) where head is (predicate arg1 ...)
and body is a list of goals (nil for facts).")

(defvar nskk--prolog-database-tails (make-hash-table :test 'equal)
  "Tail cons-cell of each predicate's clause list in \`nskk--prolog-database'.
Enables O(1) append in \`nskk-prolog-assert' without walking the full list.")

(defvar nskk--prolog-index-bucket-tail-cache
  (make-hash-table :test 'equal)
  "Canonical O(1) tail cache for indexed Prolog clause buckets.
Each predicate entry is [INDEX-TYPE INDEX-OBJECT BUCKETS-TABLE].
Each BUCKETS-TABLE value is a fresh [BUCKET-HEAD BUCKET-TAIL] vector.")

(defconst nskk--prolog-cache-missing
  (make-symbol "nskk--prolog-cache-missing")
  "Sentinel used to distinguish absent Prolog cache entries.")

(defvar nskk--prolog-active-mutation-keys nil
  "Keys protected from public mutation during transaction callbacks.")
(defvar nskk--prolog-index-config)
(defvar nskk--prolog-hash-indices)
(defvar nskk--prolog-trie-indices)

(cl-defstruct (nskk--prolog-key-state
               (:constructor nskk--prolog-make-key-state))
  key
  mappings
  database-tail
  database-tail-cdr
  index-type
  index
  first-arg
  index-bucket
  index-bucket-tail
  index-bucket-tail-cdr
  cache-buckets
  cache-bucket-present-p
  cache-bucket)

(defun nskk--prolog-ensure-mutation-allowed (key)
  "Reject a public mutation of protected KEY before it has any effect."
  (when (member key nskk--prolog-active-mutation-keys)
    (error "Prolog transaction callback cannot mutate active key %s" key)))

(defun nskk--prolog-ensure-clear-allowed ()
  "Reject a global clear while any transaction callback is active."
  (when nskk--prolog-active-mutation-keys
    (error "Prolog transaction callback cannot clear the database")))

(defun nskk--prolog-capture-key-state (key &optional first-arg capture-index-p)
  "Capture exact rollback state for KEY.
FIRST-ARG identifies the indexed bucket when CAPTURE-INDEX-P is non-nil."
  (let* ((missing nskk--prolog-cache-missing)
         (tables (list nskk--prolog-database
                       nskk--prolog-database-tails
                       nskk--prolog-index-config
                       nskk--prolog-hash-indices
                       nskk--prolog-trie-indices
                       nskk--prolog-index-bucket-tail-cache))
         (mappings
          (mapcar
           (lambda (table)
             (let ((value (gethash key table missing)))
               (list table (not (eq value missing)) value)))
           tables))
         (database-tail (gethash key nskk--prolog-database-tails))
         (type (gethash key nskk--prolog-index-config))
         (index (nskk--prolog-current-index-object key type))
         (indexed-p
          (and capture-index-p
               index
               (or (eq type :hash)
                   (and (eq type :trie) (stringp first-arg)))))
         (index-bucket
          (and indexed-p
               (nskk--prolog-transaction-index-bucket
                type index first-arg)))
         (cache-entry
          (gethash key nskk--prolog-index-bucket-tail-cache missing))
         (cache-buckets
          (and (not (eq cache-entry missing))
               (vectorp cache-entry)
               (= (length cache-entry) 3)
               (hash-table-p (aref cache-entry 2))
               (aref cache-entry 2)))
         (cache-bucket
          (if cache-buckets
              (gethash first-arg cache-buckets missing)
            missing)))
    (nskk--prolog-make-key-state
     :key key
     :mappings mappings
     :database-tail database-tail
     :database-tail-cdr (and database-tail (cdr database-tail))
     :index-type (and indexed-p type)
     :index (and indexed-p index)
     :first-arg first-arg
     :index-bucket index-bucket
     :cache-buckets cache-buckets
     :cache-bucket-present-p (not (eq cache-bucket missing))
     :cache-bucket cache-bucket)))

(defun nskk--prolog-prepare-key-state-index-tail (state)
  "Record STATE's current index tail without walking a valid hot cache."
  (let ((type (nskk--prolog-key-state-index-type state))
        (index (nskk--prolog-key-state-index state))
        (bucket (nskk--prolog-key-state-index-bucket state)))
    (when type
      (let ((tail
             (nskk--prolog-index-bucket-tail
              (nskk--prolog-key-state-key state)
              type index
              (nskk--prolog-key-state-first-arg state)
              bucket)))
        (setf (nskk--prolog-key-state-index-bucket-tail state) tail
              (nskk--prolog-key-state-index-bucket-tail-cdr state)
              (and tail (cdr tail))))))
  state)

(defun nskk--prolog-restore-key-state (state)
  "Restore exact mapping and mutable cons identity captured in STATE."
  (let ((inhibit-quit t)
        (database-tail (nskk--prolog-key-state-database-tail state))
        (index-tail (nskk--prolog-key-state-index-bucket-tail state))
        (type (nskk--prolog-key-state-index-type state))
        (index (nskk--prolog-key-state-index state))
        (first-arg (nskk--prolog-key-state-first-arg state))
        (cache-buckets (nskk--prolog-key-state-cache-buckets state)))
    (when database-tail
      (setcdr database-tail
              (nskk--prolog-key-state-database-tail-cdr state)))
    (when index-tail
      (setcdr index-tail
              (nskk--prolog-key-state-index-bucket-tail-cdr state)))
    (when type
      (nskk--prolog-transaction-set-index-bucket
       type index first-arg
       (nskk--prolog-key-state-index-bucket state)))
    (when cache-buckets
      (if (nskk--prolog-key-state-cache-bucket-present-p state)
          (puthash first-arg
                   (nskk--prolog-key-state-cache-bucket state)
                   cache-buckets)
        (remhash first-arg cache-buckets)))
    (dolist (mapping (nskk--prolog-key-state-mappings state))
      (if (cadr mapping)
          (puthash (nskk--prolog-key-state-key state)
                   (caddr mapping) (car mapping))
        (remhash (nskk--prolog-key-state-key state) (car mapping)))))
  nil)

;;;; Indexing

(defvar nskk--prolog-index-config (make-hash-table :test 'equal)
  "Per-predicate index configuration.
Key: \"pred/arity\", Value: index type (:hash, :trie, or :list).")

(defvar nskk--prolog-hash-indices (make-hash-table :test 'equal)
  "Hash indices for predicates configured with :hash.
Key: \"pred/arity\", Value: hash-table (first-arg -> clause list).")

(defvar nskk--prolog-trie-indices (make-hash-table :test 'equal)
  "Trie indices for predicates configured with :trie.
Key: \"pred/arity\", Value: nskk-trie storing clause lists.")

(defun nskk--prolog-current-index-object (key type)
  "Return the current index object for KEY and TYPE."
  (pcase type
    (:hash (gethash key nskk--prolog-hash-indices))
    (:trie (gethash key nskk--prolog-trie-indices))
    (:list nil)))

(defun nskk--prolog-index-cache-entry (key type index)
  "Return a valid canonical cache entry for KEY, TYPE, and INDEX.
TYPE and INDEX must still be the current predicate configuration and object."
  (unless (and (eq type (gethash key nskk--prolog-index-config))
               (eq index (nskk--prolog-current-index-object key type)))
    (error "Stale Prolog index for %s" key))
  (if (eq type :list)
      (progn
        (remhash key nskk--prolog-index-bucket-tail-cache)
        nil)
    (let ((entry (gethash key nskk--prolog-index-bucket-tail-cache)))
      (unless (and (vectorp entry)
                   (= (length entry) 3)
                   (eq (aref entry 0) type)
                   (eq (aref entry 1) index)
                   (hash-table-p (aref entry 2)))
        (setq entry (vector type index (make-hash-table :test 'equal)))
        (puthash key entry nskk--prolog-index-bucket-tail-cache))
      entry)))

(defun nskk--prolog-index-bucket-tail (key type index first-arg bucket)
  "Return BUCKET's tail after validating KEY, TYPE, INDEX, and FIRST-ARG.
A cold or stale bucket performs exactly one `last'; a valid hit performs none.
This O(1) contract requires bucket mutation through the Prolog mutation APIs,
which publish matching cache metadata.  Arbitrary external `setcdr' mutation
is unsupported and is not detected by the hot-path cache validation."
  (let* ((entry (nskk--prolog-index-cache-entry key type index))
         (buckets (aref entry 2))
         (info (gethash first-arg buckets))
         (valid
          (and (vectorp info)
               (= (length info) 2)
               (eq (aref info 0) bucket)
               (if bucket
                   (let ((tail (aref info 1)))
                     (and (consp tail) (null (cdr tail))))
                 (null (aref info 1))))))
    (if valid
        (aref info 1)
      (let ((tail (and bucket (last bucket))))
        (puthash first-arg (vector bucket tail) buckets)
        tail))))

(defun nskk--prolog-index-cache-set-bucket
    (key type index first-arg bucket tail)
  "Store metadata for KEY, TYPE, INDEX, FIRST-ARG, BUCKET, and TAIL."
  (let* ((entry (nskk--prolog-index-cache-entry key type index))
         (buckets (aref entry 2)))
    (puthash first-arg (vector bucket tail) buckets)))

(defsubst nskk--prolog-clause-key (predicate arity)
  "Return the database key string for PREDICATE with ARITY."
  (format "%s/%d" predicate arity))

(defsubst nskk--prolog-head-key (head)
  "Return the database key string for clause HEAD."
  (nskk--prolog-clause-key (car head) (1- (length head))))

(defun nskk-prolog-set-index (predicate arity type)
  "Configure index strategy for PREDICATE with ARITY.
TYPE must be one of :hash, :trie, or :list.

Switching strategy rebuilds the index from existing clauses in insertion order."
  (unless (memq type '(:hash :trie :list))
    (error "Unsupported Prolog index type: %S" type))
  (let* ((key (nskk--prolog-clause-key predicate arity))
         (current-type (progn
  (nskk--prolog-ensure-mutation-allowed key)
  (gethash key nskk--prolog-index-config)))
         (current-index (nskk--prolog-current-index-object key current-type))
         (valid-current
          (and (eq type current-type)
               (pcase type
                 (:hash (hash-table-p current-index))
                 (:trie (nskk-trie-p current-index))
                 (:list t)))))
    (unless valid-current
      (let* ((clauses (gethash key nskk--prolog-database))
             (staged-config (make-hash-table :test 'equal))
             (staged-hash-indices (make-hash-table :test 'equal))
             (staged-trie-indices (make-hash-table :test 'equal))
             (staged-cache (make-hash-table :test 'equal))
             (missing nskk--prolog-cache-missing)
             staged-cache-entry)
        (puthash key type staged-config)
        (pcase type
          (:hash
           (puthash key (make-hash-table :test 'equal)
                    staged-hash-indices))
          (:trie
           (puthash key (nskk-trie-create)
                    staged-trie-indices)))
        (let ((nskk--prolog-index-config staged-config)
              (nskk--prolog-hash-indices staged-hash-indices)
              (nskk--prolog-trie-indices staged-trie-indices)
              (nskk--prolog-index-bucket-tail-cache staged-cache))
          (dolist (clause clauses)
            (nskk--prolog-index-add key clause))
          (setq staged-cache-entry
                (gethash key staged-cache missing)))
        (let ((old-config
               (gethash key nskk--prolog-index-config missing))
              (old-hash
               (gethash key nskk--prolog-hash-indices missing))
              (old-trie
               (gethash key nskk--prolog-trie-indices missing))
              (old-cache
               (gethash key nskk--prolog-index-bucket-tail-cache missing)))
          (condition-case condition
              (let ((inhibit-quit t))
                (puthash key type nskk--prolog-index-config)
                (pcase type
                  (:hash
                   (puthash key (gethash key staged-hash-indices)
                            nskk--prolog-hash-indices)
                   (remhash key nskk--prolog-trie-indices))
                  (:trie
                   (puthash key (gethash key staged-trie-indices)
                            nskk--prolog-trie-indices)
                   (remhash key nskk--prolog-hash-indices))
                  (:list
                   (remhash key nskk--prolog-hash-indices)
                   (remhash key nskk--prolog-trie-indices)))
                (if (eq staged-cache-entry missing)
                    (remhash key nskk--prolog-index-bucket-tail-cache)
                  (puthash key staged-cache-entry
                           nskk--prolog-index-bucket-tail-cache))
                (when quit-flag
                  (signal 'quit nil)))
            ((error quit)
             (let ((inhibit-quit t))
               (dolist
                   (entry
                    (list
                     (cons nskk--prolog-index-config old-config)
                     (cons nskk--prolog-hash-indices old-hash)
                     (cons nskk--prolog-trie-indices old-trie)
                     (cons nskk--prolog-index-bucket-tail-cache old-cache)))
                 (if (eq (cdr entry) missing)
                     (remhash key (car entry))
                   (puthash key (cdr entry) (car entry)))))
             (signal (car condition) (cdr condition)))))))))

(defun nskk--prolog-index-add (key clause)
  "Add CLAUSE to the configured index for KEY in O(1) on a cache hit."
  (let ((type (gethash key nskk--prolog-index-config))
        (first-arg (cadr (car clause))))
    (pcase type
      (:hash
       (let* ((index (gethash key nskk--prolog-hash-indices))
              (bucket (gethash first-arg index))
              (tail (nskk--prolog-index-bucket-tail
                     key type index first-arg bucket))
              (new-cell (list clause))
              (head (or bucket new-cell)))
         (if tail
             (setcdr tail new-cell)
           (puthash first-arg head index))
         (nskk--prolog-index-cache-set-bucket
          key type index first-arg head new-cell)))
      (:trie
       (when (stringp first-arg)
         (let* ((index (gethash key nskk--prolog-trie-indices))
                (bucket (nskk-trie-lookup index first-arg))
                (tail (nskk--prolog-index-bucket-tail
                       key type index first-arg bucket))
                (new-cell (list clause))
                (head (or bucket new-cell)))
           (if tail
               (setcdr tail new-cell)
             (nskk-trie-insert index first-arg head))
           (nskk--prolog-index-cache-set-bucket
            key type index first-arg head new-cell)))))))

(defun nskk--prolog-index-remove (key clause)
  "Remove one equal CLAUSE from KEY's configured index and refresh its cache."
  (let ((type (gethash key nskk--prolog-index-config))
        (first-arg (cadr (car clause))))
    (pcase type
      (:hash
       (let* ((index (gethash key nskk--prolog-hash-indices))
              (bucket (gethash first-arg index))
              (filtered (cl-remove clause bucket
                                   :test #'equal :count 1))
              (tail (and filtered (last filtered))))
         (if filtered
             (puthash first-arg filtered index)
           (remhash first-arg index))
         (nskk--prolog-index-cache-set-bucket
          key type index first-arg filtered tail)))
      (:trie
       (when (stringp first-arg)
         (let* ((index (gethash key nskk--prolog-trie-indices))
                (bucket (nskk-trie-lookup index first-arg))
                (filtered (cl-remove clause bucket
                                     :test #'equal :count 1))
                (tail (and filtered (last filtered))))
           (if filtered
               (nskk-trie-insert index first-arg filtered)
             (nskk-trie-delete index first-arg))
           (nskk--prolog-index-cache-set-bucket
            key type index first-arg filtered tail)))))))

(defun nskk--prolog-get-clauses (predicate args subst)
  "Retrieve candidate clauses for PREDICATE given ARGS and SUBST.
Uses the configured index strategy for dispatch:
- :hash with a ground first arg -> hash lookup
- :trie with a ground string first arg -> trie lookup
- Otherwise -> full clause list scan"
  (let* ((arity (length args))
         (key (nskk--prolog-clause-key predicate arity))
         (type (gethash key nskk--prolog-index-config))
         (first-arg (and args (nskk-prolog-walk (car args) subst))))
    (pcase type
      (:hash
       (if (and first-arg
                (not (nskk-prolog-variable-p first-arg)))
           (gethash first-arg
                    (gethash key nskk--prolog-hash-indices))
         (gethash key nskk--prolog-database)))
      (:trie
       (if (and (stringp first-arg)
                (not (nskk-prolog-variable-p first-arg)))
           (nskk-trie-lookup
            (gethash key nskk--prolog-trie-indices)
            first-arg)
         (gethash key nskk--prolog-database)))
      (_
       (gethash key nskk--prolog-database)))))

;;;; Variable Renaming

(defvar nskk--prolog-var-counter 0
  "Counter for generating fresh variable names.")

(defun nskk--prolog-rename-term (term counter mapping)
  "Rename Prolog variables in TERM using COUNTER suffix and MAPPING hash.
TERM is a Prolog term (atom, variable symbol, or cons cell).
COUNTER is an integer appended as `_N' suffix to named variable names.
MAPPING is a hash-table (eq-test) mapping original variable symbols to
fresh symbols; it is mutated in place to ensure consistent renaming of
the same variable across multiple occurrences in a clause.
Anonymous variables (?_, the integer 95) receive unique names of the
form `?_anon_N' using a fresh increment of `nskk--prolog-var-counter',
independent of COUNTER.  Non-variable atoms and numbers are returned
unchanged.
Returns the renamed term."
  (pcase term
    ((pred nskk--prolog-anonymous-p)
     (intern (format "?_anon_%d" (cl-incf nskk--prolog-var-counter))))
    ((pred nskk-prolog-variable-p)
     (or (gethash term mapping)
         (let ((fresh (intern (format "%s_%d" (symbol-name term) counter))))
           (puthash term fresh mapping)
           fresh)))
    (`(,h . ,tl)
     (cons (nskk--prolog-rename-term h counter mapping)
           (nskk--prolog-rename-term tl counter mapping)))
    (_ term)))

(defun nskk--prolog-rename-variables (clause counter)
  "Rename all variables in CLAUSE using COUNTER suffix.
Returns a new clause with fresh variable names to prevent
variable capture between different clause attempts."
  (nskk--prolog-rename-term clause counter (make-hash-table :test 'eq)))

;;;; Arithmetic Evaluator

(defconst nskk--prolog-arith-operators
  `((+ . ,#'+) (- . ,#'-) (* . ,#'*) (/ . ,#'/))
  "Alist mapping arithmetic operator symbols to their Elisp binary functions.
Used by `nskk--prolog-eval-arith' for operator dispatch.
This set is intentionally closed; all arithmetic needed by NSKK is covered.")

(defun nskk--prolog-eval-arith (expr subst)
  "Evaluate arithmetic EXPR under SUBST, returning a number.
EXPR may be a number, a bound Prolog variable, a numeric Emacs Lisp
constant, or a list (OP A B), where OP is one of +, -, *, / and A, B
are arithmetic expressions.  Bound Lisp symbols whose values are not
numbers are rejected rather than exposed through public Prolog queries."
  (pcase expr
    ((pred numberp) expr)
    ((pred nskk-prolog-variable-p)
     (let ((val (nskk-prolog-walk expr subst)))
       (if (eq val expr)
           (error "Unbound variable in arithmetic: %S" expr)
         (nskk--prolog-eval-arith val subst))))
    ((and (pred symbolp) (guard (boundp expr)))
     (let ((value (symbol-value expr)))
       (if (numberp value)
           value
         (error "Non-numeric arithmetic constant %S: %S" expr value))))
    ((pred consp)
     (let* ((op (car expr))
            (fn (cdr (assq op nskk--prolog-arith-operators)))
            (a (nskk--prolog-eval-arith (cadr expr) subst))
            (b (nskk--prolog-eval-arith (caddr expr) subst)))
       (if fn
           (funcall fn a b)
         (error "Unknown arithmetic operator: %S" op))))
    (_ (error "Cannot evaluate arithmetic expression: %S" expr))))

;;;; Built-in Goal Handlers

(defun nskk--prolog-goal-kind (goal)
  "Classify GOAL into a dispatch key for `nskk--prolog-builtin-table'."
  (pcase goal
    ('!                                    :cut)
    ((pred (not consp))                    :normal)
    (`(not      . ,_)                      :not)
    (`(assertz  . ,_)                      :assertz)
    (`(retract  . ,_)                      :retract)
    (`(,(or 'is '=:= '> '< '>= '<=) . ,_) :arith)
    (_                                     :normal)))

(defun nskk--prolog-handle-cut (_goal rest subst k)
  "Handle cut (!): commit to current clause, abort remaining alternatives.
REST is the remaining goals after cut.
SUBST is the current substitution.
K is called for each successful result."
  (let ((found nil))
    (catch 'nskk-prolog-cut
      (nskk--prolog-prove-internal rest subst
        (lambda (s) (setq found t) (funcall k s))))
    (when found
      (throw 'nskk-prolog-cut nil))))

(defun nskk--prolog-handle-not (goal rest subst k)
  "Handle negation-as-failure: succeed iff the negated GOAL has no solution.
REST is proved when GOAL fails.
SUBST is used while proving GOAL and REST.
K is called for each successful result."
  (unless (catch 'nskk-prolog-naf
            (nskk--prolog-prove-internal
             (list (cadr goal)) subst
             (lambda (_) (throw 'nskk-prolog-naf t)))
            nil)
    (nskk--prolog-prove-internal rest subst k)))

(defun nskk--prolog-handle-assertz (goal rest subst k)
  "Handle assertz GOAL: dynamically add a new fact/rule to the database.
REST is then proved with the updated database.
SUBST is applied to GOAL before assertion.
K is called for each successful result."
  (nskk-prolog-assert
   (list (nskk-prolog-substitute (cadr goal) subst)))
  (nskk--prolog-prove-internal rest subst k))

(defun nskk--prolog-handle-retract (goal rest subst k)
  "Handle retract GOAL: remove the first matching fact/rule from the database.
REST is proved only when a matching clause is removed.
SUBST is applied to GOAL before retraction.
K is called for each successful result."
  (when (nskk-prolog-retract
         (nskk-prolog-substitute (cadr goal) subst))
    (nskk--prolog-prove-internal rest subst k)))

(defun nskk--prolog-handle-arith (goal rest subst k)
  "Handle arithmetic GOAL: is/2, =:=/2, and comparison operators.
REST is proved when GOAL succeeds.
SUBST is used for expression evaluation and unification.
K is called for each successful result."
  (pcase (car goal)
    ('is
     (nskk-prolog-unify/k
      (cadr goal)
      (nskk--prolog-eval-arith (caddr goal) subst)
      subst
      (lambda (new-subst) (nskk--prolog-prove-internal rest new-subst k))
      #'ignore))
    ('=:=
     (when (= (nskk--prolog-eval-arith (cadr goal) subst)
              (nskk--prolog-eval-arith (caddr goal) subst))
       (nskk--prolog-prove-internal rest subst k)))
    (_
     (when (funcall (car goal)
                    (nskk--prolog-eval-arith (cadr goal) subst)
                    (nskk--prolog-eval-arith (caddr goal) subst))
       (nskk--prolog-prove-internal rest subst k)))))

(defun nskk--prolog-try-clause (clause goal rest subst on-solution)
  "Try to unify GOAL with CLAUSE head and prove the resulting goals.
Renames variables in CLAUSE to fresh names, unifies with GOAL, and on
success proves the concatenation of CLAUSE body and REST, calling
ON-SOLUTION for each solution.
Side effect: increments `nskk--prolog-var-counter' unconditionally
\(even when unification fails) to guarantee globally unique fresh names.
Cut semantics: per-clause catch/throw; does NOT prevent other clauses
from being tried by the caller."
  (let* ((counter (cl-incf nskk--prolog-var-counter))
         (renamed (nskk--prolog-rename-variables clause counter)))
    (nskk-prolog-unify/k
     goal (car renamed) subst
     (lambda (new-subst)
       (catch 'nskk-prolog-cut
         (nskk--prolog-prove-internal
          (append (cdr renamed) rest)
          new-subst on-solution)))
     #'ignore)))

(defun nskk--prolog-handle-normal (goal rest subst k)
  "Handle normal clause resolution for GOAL.
Performs variable rename, unify head, and prove body.
REST is appended after each selected clause body.
SUBST is the incoming substitution for clause resolution.
K is called for each successful result."
  (let* ((predicate (car goal))
         (args (cdr goal))
         (clauses (nskk--prolog-get-clauses predicate args subst)))
    (dolist (clause clauses)
      (nskk--prolog-try-clause clause goal rest subst k))))

(defconst nskk--prolog-builtin-table
  (let ((ht (make-hash-table :test 'eq)))
    (puthash :cut     #'nskk--prolog-handle-cut     ht)
    (puthash :not     #'nskk--prolog-handle-not     ht)
    (puthash :assertz #'nskk--prolog-handle-assertz ht)
    (puthash :retract #'nskk--prolog-handle-retract ht)
    (puthash :arith   #'nskk--prolog-handle-arith   ht)
    (puthash :normal  #'nskk--prolog-handle-normal  ht)
    ht)
  "Static hash-table mapping goal-kind keyword to handler function.
Built once at load time; O(1) dispatch via `nskk--prolog-goal-kind'.")

(defun nskk--prolog-dispatch-goal (goal rest-goals subst on-solution)
  "Dispatch GOAL and REST-GOALS via O(1) built-in handler lookup.
SUBST is forwarded to the selected handler.
ON-SOLUTION is passed to the selected handler as the success callback."
  (funcall (gethash (nskk--prolog-goal-kind goal) nskk--prolog-builtin-table)
           goal rest-goals subst on-solution))

;;;; Prove Engine

(defun nskk-prolog-copy-term (object)
  "Return a detached copy of OBJECT while preserving graph topology.
Conses, vectors, records, strings, char tables,
and hash tables are copied with an
iterative worklist and an eq memo table, so cycles and shared references
remain cycles and shared references in the result.  String text properties
and hash-table keys and values are copied as part of the same graph.  Hash
tables retain their test, size, rehash parameters, and weakness; their entries
are populated only after copied keys have reached their final non-hash shape.
Char tables retain their subtype, default, parent, extra slots, and raw ranges.
Functions, including closures and byte-code objects, are treated as atoms
before cons/vector dispatch and retain identity.  Symbols, numbers, and
unsupported object types likewise retain identity."
  (let ((copies (make-hash-table :test #'eq))
        (missing (make-symbol "nskk-prolog-copy-missing"))
        (pending (list object))
        composites
        char-table-snapshots
        hash-snapshots)
    (while pending
      (let ((current (pop pending)))
        (when (eq (gethash current copies missing) missing)
          (cond
           ((functionp current)
            (puthash current current copies))
           ((consp current)
            (puthash current (cons nil nil) copies)
            (push current composites)
            (push (car current) pending)
            (push (cdr current) pending))
           ((stringp current)
            (puthash current (substring-no-properties current) copies)
            (push current composites)
            (let ((position 0)
                  (limit (length current)))
              (while (< position limit)
                (let ((properties (text-properties-at position current)))
                  (while properties
                    (push (cadr properties) pending)
                    (setq properties (cddr properties))))
                (setq position
                      (or (next-property-change position current limit)
                          limit)))))
           ((hash-table-p current)
            (let (entries)
              (maphash
               (lambda (key value)
                 (push (cons key value) entries))
               current)
              (puthash
               current
               (make-hash-table
                :test (hash-table-test current)
                :size (hash-table-size current)
                :rehash-size (hash-table-rehash-size current)
                :rehash-threshold (hash-table-rehash-threshold current)
                :weakness (hash-table-weakness current))
               copies)
              (push (cons current entries) hash-snapshots)
              (dolist (entry entries)
                (push (car entry) pending)
                (push (cdr entry) pending))))
           ((bool-vector-p current)
            (puthash current (copy-sequence current) copies))
           ((char-table-p current)
            (let* ((copy (copy-sequence current))
                   (parent (char-table-parent current))
                   (default (char-table-range current nil))
                   (extra-count
                    (let ((index 0))
                      (condition-case nil
                          (while t
                            (char-table-extra-slot current index)
                            (setq index (1+ index)))
                        (args-out-of-range index))))
                   (extras (make-vector extra-count nil))
                   entries)
              (set-char-table-parent copy nil)
              (set-char-table-range copy nil missing)
              (puthash current copy copies)
              (map-char-table
               (lambda (range value)
                 (unless (eq value missing)
                   (push (cons (if (consp range)
                                   (cons (car range) (cdr range))
                                 range)
                               value)
                         entries)))
               copy)
              (dotimes (index extra-count)
                (let ((value (char-table-extra-slot current index)))
                  (aset extras index value)
                  (push value pending)))
              (push (list current parent default extras entries)
                    char-table-snapshots)
              (push parent pending)
              (push default pending)
              (dolist (entry entries)
                (push (cdr entry) pending))))
           ((recordp current)
            (puthash current (copy-sequence current) copies)
            (push current composites)
            (let ((index 1))
              (while (< index (length current))
                (push (aref current index) pending)
                (setq index (1+ index)))))
           ((vectorp current)
            (puthash current (make-vector (length current) nil) copies)
            (push current composites)
            (let ((index 0))
              (while (< index (length current))
                (push (aref current index) pending)
                (setq index (1+ index)))))
           (t
            (puthash current current copies))))))
    (cl-labels ((copy-of (value)
                  (gethash value copies value)))
      (dolist (current composites)
        (let ((copy (copy-of current)))
          (cond
           ((consp current)
            (setcar copy (copy-of (car current)))
            (setcdr copy (copy-of (cdr current))))
           ((stringp current)
            (let ((position 0)
                  (limit (length current)))
              (while (< position limit)
                (let ((next (or (next-property-change position current limit)
                                limit))
                      (properties (text-properties-at position current))
                      copied-properties)
                  (while properties
                    (push (car properties) copied-properties)
                    (push (copy-of (cadr properties)) copied-properties)
                    (setq properties (cddr properties)))
                  (when copied-properties
                    (add-text-properties
                     position next (nreverse copied-properties) copy))
                  (setq position next)))))
           ((recordp current)
            (let ((index 1))
              (while (< index (length current))
                (aset copy index (copy-of (aref current index)))
                (setq index (1+ index)))))
           ((vectorp current)
            (let ((index 0))
              (while (< index (length current))
                (aset copy index (copy-of (aref current index)))
                (setq index (1+ index))))))))
      (dolist (table-snapshot char-table-snapshots)
        (let* ((current (nth 0 table-snapshot))
               (parent (nth 1 table-snapshot))
               (default (nth 2 table-snapshot))
               (extras (nth 3 table-snapshot))
               (entries (nth 4 table-snapshot))
               (copy (copy-of current)))
          (set-char-table-range copy nil (copy-of default))
          (dotimes (index (length extras))
            (set-char-table-extra-slot
             copy index (copy-of (aref extras index))))
          (dolist (entry entries)
            (set-char-table-range
             copy (car entry) (copy-of (cdr entry))))
          (set-char-table-parent copy (copy-of parent))))
      (dolist (snapshot hash-snapshots)
        (let ((copy (copy-of (car snapshot))))
          (dolist (entry (cdr snapshot))
            (puthash
             (copy-of (car entry))
             (copy-of (cdr entry))
             copy))))
      (copy-of object))))

(defun nskk--prolog-prove-all-raw (goals subst)
  "Return all GOALS solutions under SUBST without detaching their value graph."
  (let (results)
    (nskk--prolog-prove-internal goals subst
      (lambda (solution)
        (push solution results)))
    (nreverse results)))

(defun nskk--prolog-prove-internal (goals subst on-solution)
  "Core Prolog solver; call ON-SOLUTION for each successful substitution.
GOALS is the list of goals remaining to prove.
SUBST is the current variable-binding alist.
ON-SOLUTION is called with each solution substitution.

This raw engine is shared by the public all-solution wrapper and the
internal first-solution helper."
  (if (null goals)
      (funcall on-solution subst)
    (nskk--prolog-dispatch-goal
     (car goals) (cdr goals) subst on-solution)))

(defun nskk-prolog-prove (goals subst)
  "Prove GOALS under SUBST and return detached solution substitutions.
An empty list means no solution; a list containing nil represents one
successful ground solution."
  (nskk-prolog-copy-term (nskk--prolog-prove-all-raw goals subst)))

(defun nskk--prolog-prove-first (goals subst)
  "Like `nskk-prolog-prove' but throw on the first matching solution.
GOALS is a list of Prolog goals to satisfy.
SUBST is the current variable substitution alist.
Used internally by `nskk-prolog-prove-one' for early termination.

Throws the first successful substitution via the tag
`nskk-prolog-first-solution', allowing the caller to return immediately
without exploring further branches.

Delegates to `nskk--prolog-prove-internal' with an on-solution callback
that throws instead of accumulating, so backtracking stops at the first match."
  (nskk--prolog-prove-internal goals subst
    (lambda (s) (throw 'nskk-prolog-first-solution s))))

(defun nskk--prolog-prove-one-raw (goals subst)
  "Return the first GOALS solution under SUBST without detaching its value graph."
  (let* ((missing (make-symbol "nskk-prolog-no-solution"))
         (result
          (catch 'nskk-prolog-first-solution
            (nskk--prolog-prove-first goals subst)
            missing)))
    (unless (eq result missing)
      (or result t))))

(defun nskk-prolog-prove-one (goals subst)
  "Prove GOALS under SUBST and return the first detached solution.
Return t for a successful ground query and nil when no solution exists."
  (nskk-prolog-copy-term (nskk--prolog-prove-one-raw goals subst)))

;;;; Assert / Retract

(defun nskk-prolog-assert (clause)
  "Copy CLAUSE, then publish the canonical copy atomically.
The database and any index receive the same canonical clause object.
Errors and quits before or during publication leave the prior state intact."
  (let* ((canonical-clause (nskk-prolog-copy-term clause))
         (head (car canonical-clause))
         (key (nskk--prolog-head-key head))
         (first-arg (cadr head)))
    (nskk--prolog-ensure-mutation-allowed key)
    (let* ((state (nskk--prolog-capture-key-state key first-arg t))
           (new-cell (list canonical-clause))
           (tail (nskk--prolog-key-state-database-tail state)))
      (condition-case condition
          (progn
            (nskk--prolog-prepare-key-state-index-tail state)
            (if tail
                (progn
                  (setcdr tail new-cell)
                  (puthash key new-cell nskk--prolog-database-tails))
              (puthash key new-cell nskk--prolog-database)
              (puthash key new-cell nskk--prolog-database-tails))
            (nskk--prolog-index-add key canonical-clause))
        ((error quit)
         (nskk--prolog-restore-key-state state)
         (signal (car condition) (cdr condition)))))))

(defun nskk-prolog-retract (head-pattern)
  "Remove atomically the first clause whose head unifies with HEAD-PATTERN.
Returns non-nil if a clause was removed.  Publication faults restore the exact
per-key database, index, and cache mappings."
  (let* ((key (nskk--prolog-head-key head-pattern)))
    (nskk--prolog-ensure-mutation-allowed key)
    (let* ((clauses (gethash key nskk--prolog-database))
           (found
            (cl-find-if
             (lambda (clause)
               (catch 'nskk--unify-ok
                 (nskk-prolog-unify/k
                  head-pattern (car clause) nil
                  (lambda (_)
                    (throw 'nskk--unify-ok t))
                  #'ignore)
                 nil))
             clauses)))
      (when found
        (let* ((first-arg (cadr (car found)))
               (state (nskk--prolog-capture-key-state key first-arg t))
               (new-list
                (cl-remove found clauses :test #'equal :count 1))
               (new-tail (and new-list (last new-list))))
          (condition-case condition
              (progn
                (if new-list
                    (progn
                      (puthash key new-list nskk--prolog-database)
                      (puthash key new-tail nskk--prolog-database-tails))
                  (remhash key nskk--prolog-database)
                  (remhash key nskk--prolog-database-tails))
                (nskk--prolog-index-remove key found)
                t)
            ((error quit)
             (nskk--prolog-restore-key-state state)
             (signal (car condition) (cdr condition)))))))))

  (cl-defstruct (nskk--prolog-transaction-journal
               (:constructor nskk--prolog-make-transaction-journal))
  key
  database-head
  database-tail
  database-predecessor
  database-predecessor-cdr
  database-append-tail
  index-type
  index
  first-arg
  index-bucket
  index-predecessor
  index-predecessor-cdr
  index-append-tail
  cache-entry-present-p
  cache-entry
  cache-buckets
  cache-bucket-present-p
  cache-bucket
  active)

(defun nskk--prolog-find-matching-cell (head-pattern clauses)
  "Return (PREDECESSOR CELL) in CLAUSES that matches HEAD-PATTERN."
  (let (predecessor)
    (catch 'found
      (while clauses
        (unless (eq (nskk-prolog-unify head-pattern
                                       (car (car clauses))
                                       nil)
                    :fail)
          (throw 'found (list predecessor clauses)))
        (setq predecessor clauses
              clauses (cdr clauses)))
      nil)))

(defun nskk--prolog-find-eq-cell (clause clauses)
  "Return (PREDECESSOR CELL) in CLAUSES for CLAUSE by object identity."
  (let (predecessor)
    (catch 'found
      (while clauses
        (when (eq clause (car clauses))
          (throw 'found (list predecessor clauses)))
        (setq predecessor clauses
              clauses (cdr clauses)))
      nil)))

(defun nskk--prolog-transaction-index (key type)
  "Return the index object for KEY and TYPE."
  (pcase type
    (:hash (gethash key nskk--prolog-hash-indices))
    (:trie (gethash key nskk--prolog-trie-indices))))

(defun nskk--prolog-transaction-index-bucket
    (type index first-arg)
  "Return the FIRST-ARG bucket for TYPE and INDEX."
  (pcase type
    (:hash (gethash first-arg index))
    (:trie (and (stringp first-arg)
                (nskk-trie-lookup index first-arg)))))

(defun nskk--prolog-transaction-set-index-bucket
    (type index first-arg bucket)
  "Set the FIRST-ARG BUCKET for TYPE and INDEX."
  (pcase type
    (:hash
     (if bucket
         (puthash first-arg bucket index)
       (remhash first-arg index)))
    (:trie
     (when (stringp first-arg)
       (if bucket
           (nskk-trie-insert index first-arg bucket)
         (nskk-trie-delete index first-arg))))))

(defun nskk--prolog-rollback-clause-transaction (journal)
  "Rollback JOURNAL and restore the original cons-cell and cache graph."
  (when (nskk--prolog-transaction-journal-active journal)
    (let ((inhibit-quit t))
      (let ((database-append-tail
             (nskk--prolog-transaction-journal-database-append-tail journal))
            (database-predecessor
             (nskk--prolog-transaction-journal-database-predecessor journal))
            (index-append-tail
             (nskk--prolog-transaction-journal-index-append-tail journal))
            (index-predecessor
             (nskk--prolog-transaction-journal-index-predecessor journal))
            (key (nskk--prolog-transaction-journal-key journal))
            (database-head
             (nskk--prolog-transaction-journal-database-head journal))
            (database-tail
             (nskk--prolog-transaction-journal-database-tail journal))
            (type (nskk--prolog-transaction-journal-index-type journal))
            (index (nskk--prolog-transaction-journal-index journal))
            (first-arg
             (nskk--prolog-transaction-journal-first-arg journal))
            (index-bucket
             (nskk--prolog-transaction-journal-index-bucket journal))
            (cache-buckets
             (nskk--prolog-transaction-journal-cache-buckets journal)))
        (when database-append-tail
          (setcdr database-append-tail nil))
        (when database-predecessor
          (setcdr
           database-predecessor
           (nskk--prolog-transaction-journal-database-predecessor-cdr
            journal)))
        (if database-head
            (puthash key database-head nskk--prolog-database)
          (remhash key nskk--prolog-database))
        (if database-tail
            (puthash key database-tail nskk--prolog-database-tails)
          (remhash key nskk--prolog-database-tails))
        (when index-append-tail
          (setcdr index-append-tail nil))
        (when index-predecessor
          (setcdr
           index-predecessor
           (nskk--prolog-transaction-journal-index-predecessor-cdr
            journal)))
        (when type
          (nskk--prolog-transaction-set-index-bucket
           type index first-arg index-bucket))
        (when cache-buckets
          (if (nskk--prolog-transaction-journal-cache-bucket-present-p
               journal)
              (puthash
               first-arg
               (nskk--prolog-transaction-journal-cache-bucket journal)
               cache-buckets)
            (remhash first-arg cache-buckets)))
        (if (nskk--prolog-transaction-journal-cache-entry-present-p
             journal)
            (puthash
             key
             (nskk--prolog-transaction-journal-cache-entry journal)
             nskk--prolog-index-bucket-tail-cache)
          (remhash key nskk--prolog-index-bucket-tail-cache)))
      (setf (nskk--prolog-transaction-journal-active journal) nil)))
  nil)

(defun nskk--prolog-commit-clause-transaction (journal)
  "Commit JOURNAL so it can no longer be rolled back."
  (setf (nskk--prolog-transaction-journal-active journal) nil)
  nil)

(defun nskk--prolog-replace-clause-transaction
    (old-head-pattern new-clause &optional callback)
  "Replace or delete one matching clause and commit atomically.
When OLD-HEAD-PATTERN has no match, append NEW-CLAUSE when non-nil.
A nil NEW-CLAUSE performs deletion only.  Any error or quit during
publication or CALLBACK restores the original object graph."
  (let* ((new-head (and new-clause (car new-clause)))
         (target-head (or new-head old-head-pattern))
         (key (if target-head
                  (nskk--prolog-head-key target-head)
                (error "Replacement requires a clause or old pattern")))
         (database-head (gethash key nskk--prolog-database))
         (database-tail (gethash key nskk--prolog-database-tails))
         (database-match
          (and old-head-pattern
               (nskk--prolog-find-matching-cell
                old-head-pattern database-head)))
         (database-predecessor (car database-match))
         (database-cell (cadr database-match))
         (old-clause (and database-cell (car database-cell)))
         (first-arg
          (if new-clause
              (cadr new-head)
            (if old-clause
                (cadr (car old-clause))
              (cadr old-head-pattern))))
         (type (gethash key nskk--prolog-index-config))
         (indexed-p (or (eq type :hash)
                        (and (eq type :trie) (stringp first-arg))))
         (mutation-p (or database-cell new-clause))
         (index (and indexed-p
                     (nskk--prolog-transaction-index key type)))
         (index-bucket
          (and indexed-p index
               (nskk--prolog-transaction-index-bucket
                type index first-arg)))
         (index-match
          (and old-clause indexed-p
               (nskk--prolog-find-eq-cell old-clause index-bucket)))
         (index-predecessor (car index-match))
         (index-cell (cadr index-match)))
    (when (and old-head-pattern
               (not (equal key (nskk--prolog-head-key old-head-pattern))))
      (error "Replacement predicate differs from old predicate"))
    (when (and new-clause old-clause
               (not (equal first-arg (cadr (car old-clause)))))
      (error "Replacement first argument differs from old clause"))
    (when (and mutation-p indexed-p (not index))
      (error "Configured Prolog index is missing for %s" key))
    (when (and old-clause indexed-p (not index-cell))
      (error "Indexed clause is missing from its first-argument bucket"))
    (let* ((cache-entry
            (gethash key nskk--prolog-index-bucket-tail-cache
                     nskk--prolog-cache-missing))
           (cache-entry-present-p
            (not (eq cache-entry nskk--prolog-cache-missing)))
           (cache-buckets
            (and cache-entry-present-p
                 (vectorp cache-entry)
                 (= (length cache-entry) 3)
                 (hash-table-p (aref cache-entry 2))
                 (aref cache-entry 2)))
           (cache-bucket
            (if cache-buckets
                (gethash first-arg cache-buckets
                         nskk--prolog-cache-missing)
              nskk--prolog-cache-missing))
           (cache-bucket-present-p
            (not (eq cache-bucket nskk--prolog-cache-missing)))
           (database-successor (and database-cell (cdr database-cell)))
           (database-remaining-head
            (if (eq database-cell database-head)
                database-successor
              database-head))
           (database-remaining-tail
            (if (eq database-cell database-tail)
                database-predecessor
              database-tail))
           (index-successor (and index-cell (cdr index-cell)))
           (index-remaining-head
            (if (eq index-cell index-bucket)
                index-successor
              index-bucket))
           (journal
            (nskk--prolog-make-transaction-journal
             :key key
             :database-head database-head
             :database-tail database-tail
             :database-predecessor database-predecessor
             :database-predecessor-cdr
             (and database-predecessor (cdr database-predecessor))
             :database-append-tail (and new-clause database-remaining-tail)
             :index-type (and mutation-p indexed-p type)
             :index index
             :first-arg first-arg
             :index-bucket index-bucket
             :index-predecessor index-predecessor
             :index-predecessor-cdr
             (and index-predecessor (cdr index-predecessor))
             :index-append-tail nil
             :cache-entry-present-p cache-entry-present-p
             :cache-entry cache-entry
             :cache-buckets cache-buckets
             :cache-bucket-present-p cache-bucket-present-p
             :cache-bucket cache-bucket
             :active t)))
      (condition-case condition
          (let* ((index-tail
                  (and mutation-p indexed-p
                       (nskk--prolog-index-bucket-tail
                        key type index first-arg index-bucket)))
                 (index-remaining-tail
                  (if (and index-cell (eq index-cell index-tail))
                      index-predecessor
                    index-tail))
                 (new-database-cell (and new-clause (list new-clause)))
                 (new-index-cell
                  (and new-clause indexed-p (list new-clause))))
            (setf (nskk--prolog-transaction-journal-index-append-tail journal)
                  (and new-clause index-remaining-tail))
            (when database-cell
              (if database-predecessor
                  (setcdr database-predecessor database-successor)
                (setq database-remaining-head database-successor)))
            (when new-database-cell
              (if database-remaining-tail
                  (setcdr database-remaining-tail new-database-cell)
                (setq database-remaining-head new-database-cell)))
            (when mutation-p
              (if database-remaining-head
                  (puthash key database-remaining-head nskk--prolog-database)
                (remhash key nskk--prolog-database))
              (if (or new-database-cell database-remaining-tail)
                  (puthash key (or new-database-cell database-remaining-tail)
                           nskk--prolog-database-tails)
                (remhash key nskk--prolog-database-tails)))
            (when (and mutation-p indexed-p)
              (when index-cell
                (if index-predecessor
                    (setcdr index-predecessor index-successor)
                  (setq index-remaining-head index-successor)))
              (when new-index-cell
                (if index-remaining-tail
                    (setcdr index-remaining-tail new-index-cell)
                  (setq index-remaining-head new-index-cell)))
              (nskk--prolog-transaction-set-index-bucket
               type index first-arg index-remaining-head)
              (nskk--prolog-index-cache-set-bucket
               key type index first-arg index-remaining-head
               (or new-index-cell index-remaining-tail)))
            (prog1
                (when callback
                  (let ((nskk--prolog-active-mutation-keys
                         (cons key nskk--prolog-active-mutation-keys)))
                    (funcall callback)))
              (nskk--prolog-commit-clause-transaction journal)))
        ((error quit)
         (nskk--prolog-rollback-clause-transaction journal)
         (signal (car condition) (cdr condition)))))))

(defun nskk-prolog-retract-all (predicate arity)
  "Remove every PREDICATE/ARITY clause as one publication transaction.
The configured index strategy is preserved.  On error or quit, exact per-key
mappings are restored; the replacement index is staged before publication."
  (let ((key (nskk--prolog-clause-key predicate arity)))
    (nskk--prolog-ensure-mutation-allowed key)
    (let* ((type (gethash key nskk--prolog-index-config))
           (staged-index
            (pcase type
              (:hash (make-hash-table :test 'equal))
              (:trie (nskk-trie-create))))
           (state (nskk--prolog-capture-key-state key)))
      (condition-case condition
          (progn
            (remhash key nskk--prolog-database)
            (remhash key nskk--prolog-database-tails)
            (remhash key nskk--prolog-index-bucket-tail-cache)
            (pcase type
              (:hash (puthash key staged-index nskk--prolog-hash-indices))
              (:trie (puthash key staged-index nskk--prolog-trie-indices)))
            nil)
        ((error quit)
         (nskk--prolog-restore-key-state state)
         (signal (car condition) (cdr condition)))))))

(defun nskk-prolog-clear-database ()
  "Reset the entire Prolog database, clearing indices and variable counter."
  (nskk--prolog-ensure-clear-allowed)
  (clrhash nskk--prolog-database)
  (clrhash nskk--prolog-database-tails)
  (clrhash nskk--prolog-index-config)
  (clrhash nskk--prolog-hash-indices)
  (clrhash nskk--prolog-trie-indices)
  (clrhash nskk--prolog-index-bucket-tail-cache)
  (setq nskk--prolog-var-counter 0))

;;;; Query API

(defun nskk-prolog-query (goal)
  "Query GOAL and return all detached solution substitutions."
  (nskk-prolog-copy-term
   (nskk--prolog-prove-all-raw (list goal) nil)))

(defun nskk-prolog-query-one (goal)
  "Query GOAL and return the first detached solution.
Return t for ground success and nil when no solution exists."
  (nskk-prolog-copy-term
   (nskk--prolog-prove-one-raw (list goal) nil)))

(defun nskk-prolog-query-value (goal var)
  "Query GOAL and return a detached first binding of VAR."
  (let ((solution (nskk--prolog-prove-one-raw (list goal) nil)))
    (nskk-prolog-copy-term
     (when (and solution (listp solution))
       (nskk-prolog-walk var solution)))))

(defun nskk-prolog-query-all-values (goal var)
  "Query GOAL and return detached bindings of VAR across all solutions."
  (nskk-prolog-copy-term
   (mapcar
    (lambda (solution)
      (nskk-prolog-walk var solution))
    (nskk--prolog-prove-all-raw (list goal) nil))))

(defun nskk-prolog-query-values (goal vars)
  "Query GOAL and return detached first-solution bindings for VARS."
  (let ((solution (nskk--prolog-prove-one-raw (list goal) nil)))
    (nskk-prolog-copy-term
     (when (and solution (listp solution))
       (mapcar
        (lambda (var)
          (nskk-prolog-walk var solution))
        vars)))))

(defun nskk-prolog-query-bindings (goal variables)
  "Query GOAL and return detached VARIABLES bindings for all solutions."
  (nskk-prolog-copy-term
   (mapcar
    (lambda (solution)
      (mapcar
       (lambda (variable)
         (nskk-prolog-walk variable solution))
       variables))
    (nskk--prolog-prove-all-raw (list goal) nil))))

;;;; Utility Functions

(defun nskk-prolog-ground-p (term)
  "Return non-nil if TERM has no unbound Prolog variables.
A ground term is fully instantiated with no unbound variables."
  (pcase term
    ((pred nskk-prolog-variable-p) nil)
    (`(,h . ,tl) (and (nskk-prolog-ground-p h) (nskk-prolog-ground-p tl)))
    (_ t)))

(defun nskk-prolog-substitute (term subst)
  "Apply substitution SUBST to TERM, replacing all bound variables.
Walks each variable to its final binding and reconstructs the term.
Unbound variables remain as-is in the result."
  (pcase term
    ((pred nskk-prolog-variable-p)
     (let ((walked (nskk-prolog-walk term subst)))
       (if (nskk-prolog-variable-p walked)
           walked
         (nskk-prolog-substitute walked subst))))
    (`(,h . ,tl)
     (cons (nskk-prolog-substitute h subst)
           (nskk-prolog-substitute tl subst)))
    (_ term)))

(defun nskk-prolog-trie-prefix-search (predicate arity prefix)
  "Search PREDICATE/ARITY trie for keys starting with PREFIX.
Return detached (key . value) pairs matching PREFIX.
PREDICATE is a symbol, ARITY is integer, PREFIX is a string.
For arity-2 predicates, value is the second argument of each fact.
Uses the trie index for O(k+n) performance instead of O(N).
Returns nil if no trie index exists or PREFIX matches nothing."
  (let* ((key (nskk--prolog-clause-key predicate arity))
         (trie (gethash key nskk--prolog-trie-indices)))
    (nskk-prolog-copy-term
     (when trie
       (cl-loop for (index-key . clauses) in (nskk-trie-prefix-search trie prefix)
                for head = (caar clauses)
                when (and head (>= (length head) 3))
                collect (cons index-key (nth 2 head)))))))

(defun nskk-prolog-trie-has-prefix-p (predicate arity prefix)
  "Return non-nil if PREFIX leads to a node in PREDICATE/ARITY trie.
This means PREFIX is either a complete key or a proper prefix of some key."
  (let* ((key (nskk--prolog-clause-key predicate arity))
         (trie (gethash key nskk--prolog-trie-indices)))
    (when trie
      (nskk-trie-has-prefix-p trie prefix))))

(defun nskk-prolog-trie-bulk-assert (predicate arity kana-candidates-pairs)
  "Bulk-assert dictionary entries into the trie index and flat clause database.
PREDICATE and ARITY identify the predicate (e.g., \\='system-dict-entry and 2).
KANA-CANDIDATES-PAIRS is a list of (KANA . CANDIDATES-LIST) pairs where
KANA is a string (the trie key / first argument) and CANDIDATES-LIST is
the second argument value.

Mutation permission is checked once at key entry.  Each pair inserts one fact
\\=(PREDICATE KANA CANDIDATES-LIST) and commits independently through
`nskk-prolog-assert'; the batch is intentionally not atomic.  Each pair is
written to both the trie index (for O(k+n) prefix lookup) and the flat clause
database (so that variable-first-arg queries fall back correctly).  Use
`nskk-prolog-retract-all' to remove all bulk-asserted entries.

Requires the predicate to have a :trie index configured via
`nskk-prolog-set-index' before calling this function."

  (let* ((dbkey (nskk--prolog-clause-key predicate arity))
         (trie
          (progn
            (nskk--prolog-ensure-mutation-allowed dbkey)
            (gethash dbkey nskk--prolog-trie-indices))))
    (unless trie
      (error "No trie index for %s/%d; call (nskk-prolog-set-index '%s %d :trie) first"
             predicate arity predicate arity))
    (dolist (pair kana-candidates-pairs)
      (let* ((kana (car pair))
             (candidates (cdr pair))
             (clause (list (list predicate kana candidates))))
        (when (stringp kana)
          ;; nskk-prolog-assert writes to both the flat clause database (for
          ;; variable-first-arg fallback) and the trie index (via
          ;; nskk--prolog-index-add), so no separate trie-insert is needed.
          (nskk-prolog-assert clause))))))

(defun nskk-prolog-holds-p (goal)
  "Return non-nil if GOAL succeeds in the Prolog database.
GOAL is a Prolog term (predicate arg1 arg2 ...).
Returns t if GOAL has at least one solution, nil otherwise.

Uses the raw first-solution path because no solution graph escapes.

Example:
  (nskk-prolog-holds-p (quote (dict-initialized)))
  ;; => t   (when the fact is asserted)
  ;; => nil (when the fact is absent)"
  (and (nskk--prolog-prove-one-raw (list goal) nil) t))

;;;; DSL Macros

(defmacro nskk-prolog-<- (head &rest body)
  "Assert a Prolog fact or rule into the database.
HEAD is (predicate arg1 ...) -- not quoted.
BODY is zero or more goals.  If empty, asserts a fact.
If the first element of BODY is :-, it is stripped.

Examples:
  (nskk-prolog-<- (romaji-to-kana \"ka\" \"ka\"))
  (nskk-prolog-<- (grandparent \\?x \\?z)
    (parent \\?x \\?y) (parent \\?y \\?z))
  (nskk-prolog-<- (can-convert \\?x)
    :- (valid-input \\?x))"
  (declare (indent 1) (debug t))
  (let ((real-body (if (eq (car body) :-)
                       (cdr body)
                     body)))
    `(nskk-prolog-assert
      (list ',head ,@(mapcar (lambda (g) `',g) real-body)))))

(defmacro nskk-prolog-deffacts (predicate &rest fact-rows)
  "Assert multiple Prolog facts for PREDICATE in a single declaration.
PREDICATE is the predicate name symbol (not quoted).
FACT-ROWS is a list of argument lists; each row becomes one fact.

Each row (ARG...) expands to (nskk-prolog-<- (PREDICATE ARG...)).
Facts are asserted in listing order, which determines first-match
priority for hash- and list-indexed predicates.

The caller must call `nskk-prolog-set-index' BEFORE this macro.
Without it, the predicate falls back to O(N) list scan, which
violates the <20μs query performance target.

Example:
  (nskk-prolog-set-index \\='key-action 3 :hash)
  (nskk-prolog-deffacts key-action
    (space converting next-candidate)
    (space preedit   start-conversion)
    (space normal    self-insert))"
  (declare (indent 1) (debug t))
  `(progn
     ,@(mapcar (lambda (row)
                 `(nskk-prolog-<- (,predicate ,@row)))
               fact-rows)))

(defmacro nskk-prolog-define-fact-table (name options &rest fact-tuples)
  "Define a Prolog fact table for NAME with OPTIONS and FACT-TUPLES.
NAME is the predicate symbol (not quoted).
OPTIONS is a plist with :arity (integer) and :index (keyword, e.g. :hash).
FACT-TUPLES is a list of argument lists WITHOUT the predicate name prefix;
each tuple becomes one fact row.

This macro expands to a call to `nskk-prolog-set-index' followed by
`nskk-prolog-deffacts', combining both into a single declaration.

Example:
  (nskk-prolog-define-fact-table valid-mode (:arity 1 :index :hash)
    (hiragana) (katakana) (latin))

  expands to:
  (nskk-prolog-set-index \\='valid-mode 1 :hash)
  (nskk-prolog-deffacts valid-mode
    (hiragana)
    (katakana)
    (latin))

Note: tuples are passed WITHOUT the predicate name prefix.
`nskk-prolog-deffacts' prepends NAME internally to each row."
  (declare (indent 2) (debug t))
  (let ((arity (plist-get options :arity))
        (index (plist-get options :index)))
    `(progn
       (nskk-prolog-set-index ',name ,arity ,index)
       (nskk-prolog-deffacts ,name
         ,@fact-tuples))))

(defmacro nskk-prolog-bulk-facts (predicate rules)
  "Assert all entries in RULES as Prolog facts for PREDICATE.
PREDICATE is an unquoted symbol naming the Prolog predicate (e.g.,
`romaji-to-kana').  RULES is a variable or expression evaluated at load
time, producing a list of argument lists (one per fact).  Unlike
`nskk-prolog-deffacts', RULES is not expanded at compile time, allowing
data stored in a `defconst' to be used as the fact source.
For trie-indexed predicates, prefer `nskk-prolog-trie-bulk-assert' which
inserts directly into the trie index without rebuilding it per fact.

Example:
  (defconst my-rules \\='((\"a\" \"あ\") (\"i\" \"い\")))
  (nskk-prolog-bulk-facts romaji-to-kana my-rules)"
  (declare (indent 1) (debug t))
  `(dolist (rule ,rules)
     (nskk-prolog-assert (list (cons ',predicate rule)))))

(defmacro nskk-prolog-?- (goal)
  "Query the Prolog database and return the first solution.
GOAL is (predicate arg1 ...) -- not quoted.

Example:
  (nskk-prolog-?- (romaji-to-kana \"ka\" \\?kana))
  ;; => substitution alist for first solution"
  (declare (indent 0) (debug t))
  `(nskk-prolog-query-one ',goal))

(defmacro nskk-when-prolog-holds (query &rest body)
  "Execute BODY when Prolog QUERY has at least one solution.
QUERY is a runtime-evaluated list such as \\=`(valid-mode ,mode) -- it is
NOT auto-quoted by this macro (unlike `nskk-prolog-?-').  Use a quoted
literal \\='(pred arg) or a backquoted form \\=`(pred ,var) as appropriate.
BODY is one or more forms evaluated when QUERY succeeds.
Uses `nskk-prolog-query' rather than `nskk-prolog-query-one' to avoid nil
ambiguity: ground queries return (nil) on success vs nil on failure."
  (declare (indent 1) (debug t))
  `(when (nskk-prolog-query ,query)
     ,@body))

(provide 'nskk-prolog)

;;; nskk-prolog.el ends here
