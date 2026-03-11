;;; nskk-prolog.el --- Embedded Prolog engine for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

;; Author: takeokunn <bararararatty@gmail.com>
;; Maintainer: takeokunn <bararararatty@gmail.com>
;; URL: https://github.com/takeokunn/nskk.el
;; Version: 0.1.0
;; Keywords: i18n, japanese

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
;; - Arithmetic built-in goals: is/2, </2, >/2, <=/2, >=/2, =:=/2
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

;;;; Variable Representation

(defun nskk-prolog-variable-p (x)
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

(defun nskk-prolog-unify (term1 term2 subst)
  "Unify TERM1 and TERM2 under substitution SUBST.
Returns an extended substitution on success, or the keyword
`:fail' on failure.  An empty substitution is nil (not failure).
Does not perform occurs check (not needed for nskk terms).

Handles:
- Two identical atoms: return SUBST unchanged
- Variable vs anything: extend SUBST
- Two lists: unify element-by-element (car then cdr)
- Mismatch: return `:fail'"
  (let ((t1 (nskk-prolog-walk term1 subst))
        (t2 (nskk-prolog-walk term2 subst)))
    (cond
     ;; Identical terms
     ((equal t1 t2) subst)
     ;; Anonymous variable matches anything without binding
     ((nskk--prolog-anonymous-p t1) subst)
     ((nskk--prolog-anonymous-p t2) subst)
     ;; Variable on left: bind it
     ((nskk-prolog-variable-p t1)
      (cons (cons t1 t2) subst))
     ;; Variable on right: bind it
     ((nskk-prolog-variable-p t2)
      (cons (cons t2 t1) subst))
     ;; Two conses: unify recursively
     ((and (consp t1) (consp t2))
      (let ((s (nskk-prolog-unify (car t1) (car t2) subst)))
        (if (nskk--prolog-fail-p s)
            :fail
          (nskk-prolog-unify (cdr t1) (cdr t2) s))))
     ;; Mismatch
     (t :fail))))

;;;; Clause Database

(defvar nskk--prolog-database (make-hash-table :test 'equal)
  "Clause database keyed by \"predicate/arity\" string.
Each value is a list of clauses in insertion order.
A clause is (head . body) where head is (predicate arg1 ...)
and body is a list of goals (nil for facts).")

(defvar nskk--prolog-database-tails (make-hash-table :test 'equal)
  "Tail cons-cell of each predicate's clause list in `nskk--prolog-database'.
Enables O(1) append in `nskk-prolog-assert' without walking the full list.")

(defsubst nskk--prolog-clause-key (predicate arity)
  "Return the database key string for PREDICATE with ARITY."
  (format "%s/%d" predicate arity))

(defsubst nskk--prolog-head-key (head)
  "Return the database key string for clause HEAD."
  (nskk--prolog-clause-key (car head) (1- (length head))))

;;;; Private Trie Implementation

(cl-defstruct (nskk--prolog-trie-node
               (:constructor nskk--prolog-trie-node--create)
               (:copier nil))
  "Private trie node for nskk-prolog internal use.
Slots:
  char     - character this node represents (nil for root)
  children - hash-table of child nodes (char -> node)
  value    - value stored at this node (terminal nodes only)
  is-end   - non-nil if this node terminates a key
  count    - number of keys passing through this node"
  (char nil :type (or null character))
  (children nil :type (or null hash-table))
  (value nil)
  (is-end nil :type boolean)
  (count 0 :type integer))

(cl-defstruct (nskk--prolog-trie
               (:constructor nskk--prolog-trie--create-internal)
               (:copier nil))
  "Private trie structure for nskk-prolog internal use.
Slots:
  root  - root node
  size  - total number of stored keys"
  (root nil :type nskk--prolog-trie-node)
  (size 0 :type integer))

(defun nskk--prolog-trie-create ()
  "Create and return a new empty private trie."
  (nskk--prolog-trie--create-internal
   :root (nskk--prolog-trie-node--create)
   :size 0))

(defun nskk--prolog-trie--get-or-create-child (node char)
  "Return the child of NODE for CHAR, creating it if absent.
If NODE has no children table, one is created (hash-table with `eq' test,
initial size 50 — a conservative over-estimate for typical trie branching
factors on kana/romaji strings).  Looks up CHAR in the children table;
creates a new child node for CHAR if missing.  Unconditionally increments
the child node's `count' field on every call, tracking path-traversal
frequency for bookkeeping.
Mutates NODE (may set its `children' slot) and the returned child node.
Returns the child node for CHAR."
  (unless (nskk--prolog-trie-node-children node)
    (setf (nskk--prolog-trie-node-children node)
          (make-hash-table :test 'eq :size 50)))
  (let ((child (gethash char (nskk--prolog-trie-node-children node))))
    (unless child
      (setq child (nskk--prolog-trie-node--create :char char))
      (puthash char child (nskk--prolog-trie-node-children node)))
    (cl-incf (nskk--prolog-trie-node-count child))
    child))

(defun nskk--prolog-trie-insert (trie key value)
  "Insert KEY with VALUE into private TRIE.
KEY must be a non-empty string.  Returns TRIE."
  (unless (stringp key)
    (error "Key must be a string: %s" key))
  (when (zerop (length key))
    (error "Key cannot be empty"))
  (let ((node (nskk--prolog-trie-root trie))
        (was-new nil))
    (dotimes (i (length key))
      (setq node (nskk--prolog-trie--get-or-create-child node (aref key i))))
    ;; Record whether this key was previously absent so we increment
    ;; `size' exactly once, even if the same key is inserted again.
    (setq was-new (not (nskk--prolog-trie-node-is-end node)))
    (setf (nskk--prolog-trie-node-is-end node) t)
    (setf (nskk--prolog-trie-node-value node) value)
    (when was-new
      (cl-incf (nskk--prolog-trie-size trie)))
    trie))

(defun nskk--prolog-trie-lookup (trie key)
  "Look up KEY in private TRIE.
Returns (value . t) if found, (nil . nil) otherwise."
  (unless (stringp key)
    (error "Key must be a string: %s" key))
  (let ((node (nskk--prolog-trie--find-node trie key)))
    (if (and node (nskk--prolog-trie-node-is-end node))
        (cons (nskk--prolog-trie-node-value node) t)
      (cons nil nil))))

(defun nskk--prolog-trie-delete (trie key)
  "Delete KEY from private TRIE.
Returns t if deleted, nil if KEY was not present."
  (unless (stringp key)
    (error "Key must be a string: %s" key))
  (let ((node (nskk--prolog-trie--find-node trie key)))
    (when (and node (nskk--prolog-trie-node-is-end node))
      (setf (nskk--prolog-trie-node-is-end node) nil)
      (setf (nskk--prolog-trie-node-value node) nil)
      (cl-decf (nskk--prolog-trie-size trie))
      (nskk--prolog-trie--cleanup-path trie key)
      t)))

(defun nskk--prolog-trie--find-node (trie key)
  "Return the trie node for KEY in private TRIE, or nil if not found."
  (let ((node (nskk--prolog-trie-root trie))
        (key-len (length key)))
    (catch 'not-found
      (dotimes (i key-len)
        (let ((char (aref key i)))
          (unless (and (nskk--prolog-trie-node-children node)
                       (setq node (gethash char (nskk--prolog-trie-node-children node))))
            (throw 'not-found nil))))
      node)))

(defun nskk--prolog-trie--cleanup-path (trie key)
  "Remove leaf nodes that are no longer needed after deleting KEY from TRIE.
Uses a two-pass algorithm:
  Pass 1: walk forward collecting all parent nodes onto a stack.
  Pass 2: walk backward from the deleted node, unlinking leaf nodes
          from their parents until a shared-prefix node (has other children)
          or a terminal node (is-end) is encountered.
This ensures that shared prefix paths are preserved for other keys."
  (let ((node (nskk--prolog-trie-root trie))
        (parent-stack nil)
        (key-len (length key)))
    (dotimes (i key-len)
      (let ((char (aref key i)))
        (push node parent-stack)
        (setq node (gethash char (nskk--prolog-trie-node-children node)))))
    ;; Walk backward through the path, removing leaf nodes that are
    ;; no longer needed.  Stop as soon as a node with remaining children
    ;; or a terminal (is-end) node is encountered.
    (while (and node
                parent-stack
                (not (nskk--prolog-trie-node-is-end node))
                (or (null (nskk--prolog-trie-node-children node))
                    (zerop (hash-table-count
                            (nskk--prolog-trie-node-children node)))))
      (let ((parent (pop parent-stack)))
        (when (nskk--prolog-trie-node-children parent)
          (remhash (nskk--prolog-trie-node-char node)
                   (nskk--prolog-trie-node-children parent)))
        (setq node parent)))))

(defun nskk--prolog-trie-prefix-search (trie prefix &optional limit)
  "Search private TRIE for all keys starting with PREFIX.
Returns a list of (key . value) pairs.
Optional LIMIT caps the number of results."
  (unless (stringp prefix)
    (error "Prefix must be a string: %s" prefix))
  (let ((node (if (zerop (length prefix))
                  (nskk--prolog-trie-root trie)
                (nskk--prolog-trie--find-node trie prefix))))
    (when node
      (nskk--prolog-trie--collect-all node prefix limit 0))))

(defun nskk--prolog-trie--collect-all (node prefix limit collected-count)
  "Collect all (key . value) pairs reachable from NODE with PREFIX.
NODE is the trie node to start DFS traversal from.
PREFIX is the string prefix accumulated so far.
LIMIT, when non-nil, caps the total number of results.
COLLECTED-COUNT is the number already gathered.
Returns a list of (key . value) pairs in DFS order."
  (let ((results nil)
        (count collected-count))
    (cl-labels
        ((dfs (n pfx)
           (when (nskk--prolog-trie-node-is-end n)
             (push (cons pfx (nskk--prolog-trie-node-value n)) results)
             (cl-incf count)
             (when (and limit (>= count limit))
               (throw 'limit-reached nil)))
           (when-let* ((children (nskk--prolog-trie-node-children n)))
             (maphash (lambda (char child)
                        (dfs child (concat pfx (char-to-string char))))
                      children))))
      (catch 'limit-reached
        (dfs node prefix)))
    (nreverse results)))

;;;; Indexing

(defvar nskk--prolog-index-config (make-hash-table :test 'equal)
  "Per-predicate index configuration.
Key: \"pred/arity\", Value: index type (:hash, :trie, or :list).")

(defvar nskk--prolog-hash-indices (make-hash-table :test 'equal)
  "Hash indices for predicates configured with :hash.
Key: \"pred/arity\", Value: hash-table (first-arg -> clause list).")

(defvar nskk--prolog-trie-indices (make-hash-table :test 'equal)
  "Trie indices for predicates configured with :trie.
Key: \"pred/arity\", Value: private nskk--prolog-trie storing clause lists.")

(defun nskk-prolog-set-index (predicate arity type)
  "Configure index strategy for PREDICATE with ARITY.
TYPE must be one of :hash, :trie, or :list.

:hash provides O(1) dispatch on the first argument.
:trie provides prefix matching on the first argument (strings).
:list is a plain scan, the default for small clause sets."
  (let ((key (nskk--prolog-clause-key predicate arity)))
    (puthash key type nskk--prolog-index-config)
    (pcase type
      (:hash
       (unless (gethash key nskk--prolog-hash-indices)
         (puthash key (make-hash-table :test 'equal)
                  nskk--prolog-hash-indices)))
      (:trie
       (unless (gethash key nskk--prolog-trie-indices)
         (puthash key (nskk--prolog-trie-create)
                  nskk--prolog-trie-indices))))))

(defun nskk--prolog-index-add (key clause)
  "Add CLAUSE to the index for KEY if indexing is configured.
This is a no-op when no index strategy is configured for KEY."
  (let ((type (gethash key nskk--prolog-index-config))
        (first-arg (cadr (car clause))))
    (pcase type
      (:hash
       (let* ((ht (gethash key nskk--prolog-hash-indices))
              (existing (gethash first-arg ht)))
         (puthash first-arg (nconc existing (list clause)) ht)))
      (:trie
       (when (stringp first-arg)
         (let* ((trie (gethash key nskk--prolog-trie-indices))
                (existing (car (nskk--prolog-trie-lookup trie first-arg))))
           (nskk--prolog-trie-insert
            trie first-arg
            (nconc existing (list clause)))))))))

(defun nskk--prolog-index-remove (key clause)
  "Remove CLAUSE from the index for KEY if indexing is configured.
This is a no-op when no index strategy is configured for KEY."
  (let ((type (gethash key nskk--prolog-index-config))
        (first-arg (cadr (car clause))))
    (pcase type
      (:hash
       (let* ((ht (gethash key nskk--prolog-hash-indices))
              (existing (gethash first-arg ht))
              (filtered (cl-remove clause existing
                                  :test #'equal :count 1)))
         (if filtered
             (puthash first-arg filtered ht)
           (remhash first-arg ht))))
      (:trie
       (when (stringp first-arg)
         (let* ((trie (gethash key nskk--prolog-trie-indices))
                (existing (car (nskk--prolog-trie-lookup trie first-arg)))
                (filtered (cl-remove clause existing
                                    :test #'equal :count 1)))
           (if filtered
               (nskk--prolog-trie-insert trie first-arg filtered)
             (nskk--prolog-trie-delete trie first-arg))))))))

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
           (car (nskk--prolog-trie-lookup
                 (gethash key nskk--prolog-trie-indices)
                 first-arg))
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
  (cond
   ((nskk--prolog-anonymous-p term)
    (intern (format "?_anon_%d" (cl-incf nskk--prolog-var-counter))))
   ((nskk-prolog-variable-p term)
    (or (gethash term mapping)
        (let ((fresh (intern (format "%s_%d" (symbol-name term) counter))))
          (puthash term fresh mapping)
          fresh)))
   ((consp term)
    (cons (nskk--prolog-rename-term (car term) counter mapping)
          (nskk--prolog-rename-term (cdr term) counter mapping)))
   (t term)))

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
EXPR may be a number, a bound Prolog variable, or a list (OP A B)
where OP is one of +, -, *, / and A, B are arithmetic expressions."
  (cond
   ((numberp expr) expr)
   ((nskk-prolog-variable-p expr)
    (let ((val (nskk-prolog-walk expr subst)))
      (if (eq val expr)
          (error "Unbound variable in arithmetic: %S" expr)
        (nskk--prolog-eval-arith val subst))))
   ;; Emacs Lisp bound symbol (e.g., defconst values used in rule bodies)
   ((and (symbolp expr) (not (nskk-prolog-variable-p expr)) (boundp expr))
    (nskk--prolog-eval-arith (symbol-value expr) subst))
   ((consp expr)
    (let* ((op (car expr))
           (fn (cdr (assq op nskk--prolog-arith-operators)))
           (a (nskk--prolog-eval-arith (cadr expr) subst))
           (b (nskk--prolog-eval-arith (caddr expr) subst)))
      (if fn
          (funcall fn a b)
        (error "Unknown arithmetic operator: %S" op))))
   (t (error "Cannot evaluate arithmetic expression: %S" expr))))

;;;; Built-in Goal Handler Registry

(defvar nskk--prolog-goal-handlers nil
  "Ordered alist of registered built-in Prolog goal handlers.
Each entry is a list (NAME MATCH-FN BODY-FN) where:
  NAME     -- handler identifier symbol (used for deduplication on reload)
  MATCH-FN -- (lambda (goal)) returning non-nil when this handler applies
  BODY-FN  -- (lambda (goal rest-goals subst on-solution)) executing the handler

Handlers are tried in registration order; the first matching handler runs.
Re-registering a handler with the same NAME updates the existing entry in place,
preserving registration order (idempotent on file reload).")

(defmacro nskk-define-goal-handler (name args &rest plist)
  "Define and register a built-in Prolog goal handler.
NAME is a symbol identifying this handler (used for deduplication).
ARGS is a parameter list: (GOAL REST-GOALS SUBST ON-SOLUTION).
ARGS must contain exactly 4 symbols in positional order: GOAL, REST-GOALS,
SUBST, ON-SOLUTION.  :match takes exactly ONE form; :body takes ONE OR MORE
forms (spliced into the handler lambda body).
PLIST must contain:
  :match FORM     -- evaluated with GOAL bound; non-nil means this handler
                     applies
  :body  FORM...  -- handler body forms evaluated with ARGS bound

Handlers are tried in registration order; first match wins.
Re-defining a handler with the same NAME replaces it in place (idempotent).

The BODY may call `nskk--prolog-prove-internal' directly for recursive goals.
`catch'/`throw' tags (`nskk-prolog-cut', `nskk-prolog-naf') propagate correctly
through handler lambdas because Emacs Lisp catch/throw is dynamic-scoped."
  (declare (indent 2) (debug t))
  (let* ((g-new-entry (make-symbol "new-entry"))
         (g-existing  (make-symbol "existing"))
         (match-form (cadr (memq :match plist)))
         (body-forms (cdr (memq :body plist)))
         (goal-arg   (nth 0 args))
         (rest-arg   (nth 1 args))
         (subst-arg  (nth 2 args))
         (k-arg      (nth 3 args)))
    `(let ((,g-new-entry (list ',name
                               (lambda (,goal-arg) ,match-form)
                               (lambda (,goal-arg ,rest-arg ,subst-arg ,k-arg)
                                 ,@body-forms))))
       (let ((,g-existing (assq ',name nskk--prolog-goal-handlers)))
         (if ,g-existing
             (progn (setcar (cdr ,g-existing)   (nth 1 ,g-new-entry))
                    (setcar (cddr ,g-existing)  (nth 2 ,g-new-entry)))
           (setq nskk--prolog-goal-handlers
                 (append nskk--prolog-goal-handlers (list ,g-new-entry))))))))

(defun nskk--prolog-dispatch-goal (goal rest-goals subst on-solution)
  "Dispatch GOAL to the first matching built-in handler.
Tries each handler in `nskk--prolog-goal-handlers' in registration order.
Calls the matching handler's body with GOAL, REST-GOALS, SUBST, ON-SOLUTION.
Exits immediately after the first matching handler via throw, making dispatch
O(number-of-handlers-before-match).
Signals an error if no handler matches (should never happen with the `normal'
catch-all handler registered last)."
  ;; Safety assertion: the `normal' handler (registered last) always
  ;; matches (and goal t) for any non-nil GOAL.  Goals are guaranteed
  ;; non-nil because `nskk--prolog-prove-internal' short-circuits on
  ;; (null goals) before dispatching.  The error below fires only if
  ;; a future handler registration bug removes the `normal' catch-all.
  (unless (catch 'nskk--prolog-handler-matched
            (dolist (entry nskk--prolog-goal-handlers)
              (when (funcall (nth 1 entry) goal)
                (funcall (nth 2 entry) goal rest-goals subst on-solution)
                (throw 'nskk--prolog-handler-matched t)))
            nil)
    (error "No handler matched goal: %S" goal)))

;; Register built-in goal handlers in dispatch priority order.
;; The `normal' handler is last (catch-all).

;; cut (!): prunes remaining alternatives in the CURRENT clause body only.
(nskk-define-goal-handler cut (goal rest subst k)
  :match (eq goal '!)
  :body
  (ignore goal)
  (let ((found nil))
    (catch 'nskk-prolog-cut
      (nskk--prolog-prove-internal rest subst
        (lambda (s) (setq found t) (funcall k s))))
    (when found
      (throw 'nskk-prolog-cut nil))))

;; negation-as-failure (NAF): succeeds iff the negated goal has no solution.
(nskk-define-goal-handler negation (goal rest subst k)
  :match (and (consp goal) (eq (car goal) 'not))
  :body
  (unless (catch 'nskk-prolog-naf
            (nskk--prolog-prove-internal
             (list (cadr goal)) subst
             (lambda (_) (throw 'nskk-prolog-naf t)))
            nil)
    (nskk--prolog-prove-internal rest subst k)))

;; assertz: dynamically adds a new fact/rule to the Prolog database at runtime.
(nskk-define-goal-handler assertz (goal rest subst k)
  :match (and (consp goal) (eq (car goal) 'assertz))
  :body
  (nskk-prolog-assert
   (list (nskk-prolog-substitute (cadr goal) subst)))
  (nskk--prolog-prove-internal rest subst k))

;; retract: removes the first matching fact/rule from the Prolog database.
(nskk-define-goal-handler retract (goal rest subst k)
  :match (and (consp goal) (eq (car goal) 'retract))
  :body
  (when (nskk-prolog-retract
         (nskk-prolog-substitute (cadr goal) subst))
    (nskk--prolog-prove-internal rest subst k)))

;; arith-compare: arithmetic ordering (<, >, =<, >=); both args must be ground numbers.
(nskk-define-goal-handler arith-compare (goal rest subst k)
  :match (and (consp goal) (memq (car goal) '(>= <= > <)))
  :body
  (let ((a (nskk--prolog-eval-arith (cadr goal) subst))
        (b (nskk--prolog-eval-arith (caddr goal) subst)))
    (when (funcall (car goal) a b)
      (nskk--prolog-prove-internal rest subst k))))

;; arith-eq (=:=, =\=): arithmetic equality/inequality; both args must evaluate to numbers.
(nskk-define-goal-handler arith-eq (goal rest subst k)
  :match (and (consp goal) (eq (car goal) '=:=))
  :body
  (let ((a (nskk--prolog-eval-arith (cadr goal) subst))
        (b (nskk--prolog-eval-arith (caddr goal) subst)))
    (when (= a b)
      (nskk--prolog-prove-internal rest subst k))))

;; arith-is (is/2): evaluates right-hand expression and unifies result with left-hand variable.
(nskk-define-goal-handler arith-is (goal rest subst k)
  :match (and (consp goal) (eq (car goal) 'is))
  :body
  (let* ((new-subst (nskk-prolog-unify
                     (cadr goal)
                     (nskk--prolog-eval-arith (caddr goal) subst)
                     subst)))
    (unless (nskk--prolog-fail-p new-subst)
      (nskk--prolog-prove-internal rest new-subst k))))

(defun nskk--prolog-try-clause (clause goal rest subst on-solution)
  "Try to unify GOAL with CLAUSE head and prove the resulting goals.
Renames variables in CLAUSE to fresh names (incrementing
`nskk--prolog-var-counter' even on fast-fail paths), unifies with GOAL,
and on success proves the concatenation of CLAUSE body and REST under the
extended substitution, calling ON-SOLUTION for each solution.
If cut (!) is thrown inside the body, it is caught here, implementing
per-clause cut semantics: cut prevents retry of goals after ! in this
clause body, but does NOT prevent other clauses from being tried by the
caller.  See `nskk--prolog-prove-internal' commentary for the full
non-standard cut semantics.
Returns nil in all cases; results are communicated via ON-SOLUTION."
  (let* ((counter (cl-incf nskk--prolog-var-counter))
         (renamed (nskk--prolog-rename-variables clause counter))
         (new-subst (nskk-prolog-unify goal (car renamed) subst)))
    (unless (nskk--prolog-fail-p new-subst)
      (catch 'nskk-prolog-cut
        (nskk--prolog-prove-internal
         (append (cdr renamed) rest)
         new-subst on-solution)))))

;; normal (catch-all): standard clause resolution — variable rename, unify head, prove body.
(nskk-define-goal-handler normal (goal rest subst k)
  :match (and goal t)
  :body
  (let* ((predicate (car goal))
         (args (cdr goal))
         (clauses (nskk--prolog-get-clauses predicate args subst)))
    (dolist (clause clauses)
      (nskk--prolog-try-clause clause goal rest subst k))))

;;;; Prove Engine

(defun nskk--prolog-prove-internal (goals subst on-solution)
  "Core Prolog solver; call ON-SOLUTION for each successful substitution.
GOALS is the list of goals remaining to prove.
SUBST is the current variable-binding alist.
ON-SOLUTION is a unary function called with each solution substitution.

This is the single implementation shared by `nskk-prolog-prove' (which
collects all solutions) and `nskk--prolog-prove-first' (which stops at the
first solution).  Do not call this function directly; use the public API.

Built-in goals handled in addition to user-defined predicates:
  `!'           Cut: commit to current clause, abort alternatives.
  `(not GOAL)'  Negation-as-failure: succeed iff GOAL has no solution.
  `(assertz H)' Side-effect: assert ground H as a new fact, then continue.
  `(retract H)' Side-effect: remove first matching clause H, then continue.
  `(is V E)'    Arithmetic: unify variable V with the value of expression E.
  `(=:= A B)'   Arithmetic equality: succeed iff A and B evaluate equal.
  `(>= A B)', `(<= A B)', `(> A B)', `(< A B)' — comparison built-ins.

Cut semantics: this engine uses per-clause catch/throw for cut.
Alternative clauses for the same predicate are still tried after a cut --
cut prevents only the goals *after* cut in the current clause body from
being retried.  This differs from standard Prolog cut."
  (if (null goals)
      (funcall on-solution subst)
    (nskk--prolog-dispatch-goal
     (car goals) (cdr goals) subst on-solution)))

(defun nskk-prolog-prove (goals subst)
  "Prove GOALS under substitution SUBST using depth-first backtracking.
GOALS is a list of Prolog goals to satisfy.
SUBST is the current variable substitution alist.
Returns a list of all successful substitutions (possibly empty).
An empty list nil means no solution was found; a list containing
nil means one solution with an empty substitution.

Delegates to `nskk--prolog-prove-internal' with an accumulator callback.
For single-solution efficiency, prefer `nskk-prolog-prove-one'."
  (let (results)
    (nskk--prolog-prove-internal goals subst
      (lambda (s) (push s results)))
    (nreverse results)))

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

(defun nskk-prolog-prove-one (goals subst)
  "Like `nskk-prolog-prove' but return only the first solution.
GOALS is a list of Prolog goals to satisfy.
SUBST is the current variable substitution alist.
Returns the first successful substitution, or nil if none.

Uses `nskk--prolog-prove-first' internally for early termination:
stops backtracking as soon as one solution is found.

For ground queries (no Prolog variables), returns t on success
and nil when no solution exists, so callers can distinguish the two
cases.  Use `nskk-prolog-prove' when you need the actual substitution
alist for a ground query."
  (let ((result (catch 'nskk-prolog-first-solution
                  (nskk--prolog-prove-first goals subst)
                  :nskk-no-solution)))
    (if (eq result :nskk-no-solution)
        nil
      (or result t))))

;;;; Assert / Retract

(defun nskk-prolog-assert (clause)
  "Add CLAUSE to the Prolog database and update indices.
CLAUSE format: ((pred arg1 arg2 ...)) for facts,
               ((pred arg1 ...) goal1 goal2 ...) for rules.

Example fact:  ((romaji-to-kana \"ka\" \"ka\"))
Example rule:  ((grandparent \\?x \\?z)
                (parent \\?x \\?y) (parent \\?y \\?z))

Uses O(1) append via `nskk--prolog-database-tails' to avoid the O(N²)
cost of repeated `nconc' calls on large clause lists."
  (let* ((head (car clause))
         (key (nskk--prolog-head-key head))
         (new-cell (list clause))
         (tail (gethash key nskk--prolog-database-tails)))
    (if tail
        ;; O(1): set the cdr of the stored tail cell, advance tail pointer
        (progn
          (setcdr tail new-cell)
          (puthash key new-cell nskk--prolog-database-tails))
      ;; First clause for this key: initialize both database and tail
      (puthash key new-cell nskk--prolog-database)
      (puthash key new-cell nskk--prolog-database-tails))
    (nskk--prolog-index-add key clause)))

(defun nskk-prolog-retract (head-pattern)
  "Remove the first clause matching HEAD-PATTERN from the database.
HEAD-PATTERN is matched against clause heads using unification.
Returns t if a clause was removed, nil otherwise."
  (let* ((key (nskk--prolog-head-key head-pattern))
         (clauses (gethash key nskk--prolog-database))
         (found (cl-find-if
                 (lambda (clause)
                   (not (nskk--prolog-fail-p
                         (nskk-prolog-unify head-pattern (car clause) nil))))
                 clauses)))
    (when found
      (let ((new-list (cl-remove found clauses :test #'equal :count 1)))
        (if new-list
            (progn
              (puthash key new-list nskk--prolog-database)
              (puthash key (last new-list) nskk--prolog-database-tails))
          (remhash key nskk--prolog-database)
          (remhash key nskk--prolog-database-tails)))
      (nskk--prolog-index-remove key found)
      t)))

(defun nskk-prolog-retract-all (predicate arity)
  "Remove all clauses for PREDICATE with ARITY from the database.
Clears index data (trie or hash-table contents) while preserving the
configured index strategy, so subsequent `nskk-prolog-assert' calls
still use the same index."
  ;; Index configuration is preserved intentionally: after retract-all,
  ;; subsequent assert calls should still use the configured index strategy.
  (let ((key (nskk--prolog-clause-key predicate arity)))
    (remhash key nskk--prolog-database)
    (remhash key nskk--prolog-database-tails)
    (let ((type (gethash key nskk--prolog-index-config)))
      (pcase type
        (:hash (puthash key (make-hash-table :test 'equal)
                        nskk--prolog-hash-indices))
        (:trie (puthash key (nskk--prolog-trie-create)
                        nskk--prolog-trie-indices))))))

(defun nskk-prolog-clear-database ()
  "Reset the entire Prolog database, clearing indices and variable counter."
  (clrhash nskk--prolog-database)
  (clrhash nskk--prolog-database-tails)
  (clrhash nskk--prolog-index-config)
  (clrhash nskk--prolog-hash-indices)
  (clrhash nskk--prolog-trie-indices)
  (setq nskk--prolog-var-counter 0))

;;;; Query API

(defun nskk-prolog-query (goal)
  "Query the Prolog database with GOAL, returning all solutions.
GOAL is a list (predicate arg1 arg2 ...).
Returns a list of substitution alists, one per solution.

Example:
  (nskk-prolog-query \\='(parent \\?x bob))
  ;; => ((\\?x_1 . tom) ...)"
  (nskk-prolog-prove (list goal) nil))

(defun nskk-prolog-query-one (goal)
  "Query the Prolog database with GOAL, returning the first solution.
GOAL is a list (predicate arg1 arg2 ...).
Returns a substitution alist, t for ground query success, or nil
if no solution.

More efficient than `nskk-prolog-query' for deterministic lookups."
  (nskk-prolog-prove-one (list goal) nil))

(defun nskk-prolog-query-value (goal var)
  "Query GOAL and return the binding of VAR in the first solution.
GOAL is a list (predicate arg1 arg2 ...).
VAR is a Prolog variable symbol (e.g., \\='\\?kana).

Uses `nskk-prolog-query-one' internally for efficiency (stops after
the first solution instead of collecting all solutions).

Example:
  (nskk-prolog-query-value
    \\='(romaji-to-kana \"ka\" \\?kana) \\='\\?kana)
  ;; => \"ka\""
  (let ((solution (nskk-prolog-query-one goal)))
    (when (and solution (listp solution))
      (nskk-prolog-walk var solution))))

(defun nskk-prolog-query-all-values (goal var)
  "Query GOAL and return all bindings of VAR across solutions.
GOAL is a list (predicate arg1 arg2 ...).
VAR is a Prolog variable symbol (e.g., \\='\\?romaji).

Example:
  (nskk-prolog-query-all-values \\='(parent \\?x bob) \\='\\?x)
  ;; => (tom)"
  (let ((solutions (nskk-prolog-query goal)))
    (mapcar (lambda (subst) (nskk-prolog-walk var subst))
            solutions)))

(defun nskk-prolog-query-values (goal vars)
  "Query GOAL and return bindings of VARS as a list.
GOAL is a list (predicate arg1 arg2 ...).
VARS is a list of Prolog variable symbols.
Returns a list of values in the same order as VARS, or nil if no solution.

This is a convenience function for extracting multiple variable
bindings from a single query without calling `nskk-prolog-query-value'
repeatedly.  Uses `nskk-prolog-query-one' internally so only the
first solution is considered.

Returns nil both when no solution exists and when the query is fully ground
\(no variable bindings, so the solution substitution is the atom t rather than
a list).  Use `nskk-prolog-query-one' directly for pure existence checks on
ground queries.

Example:
  (nskk-prolog-query-values
    \\='(mode-info hiragana \\?s \\?f \\?h)
    \\='(\\?s \\?f \\?h))
  ;; => (\"かな\" nskk-modeline-hiragana-face \"Hiragana input mode\")"
  (let ((solution (nskk-prolog-query-one goal)))
    (when (and solution (listp solution))
      (mapcar (lambda (var) (nskk-prolog-walk var solution)) vars))))

;;;; Utility Functions

(defun nskk-prolog-ground-p (term)
  "Return non-nil if TERM has no unbound Prolog variables.
A ground term is fully instantiated with no unbound variables."
  (cond
   ((nskk-prolog-variable-p term) nil)
   ((consp term)
    (and (nskk-prolog-ground-p (car term))
         (nskk-prolog-ground-p (cdr term))))
   (t t)))

(defun nskk-prolog-substitute (term subst)
  "Apply substitution SUBST to TERM, replacing all bound variables.
Walks each variable to its final binding and reconstructs the term.
Unbound variables remain as-is in the result."
  (cond
   ((nskk-prolog-variable-p term)
    (let ((walked (nskk-prolog-walk term subst)))
      (if (nskk-prolog-variable-p walked)
          walked
        (nskk-prolog-substitute walked subst))))
   ((consp term)
    (cons (nskk-prolog-substitute (car term) subst)
          (nskk-prolog-substitute (cdr term) subst)))
   (t term)))

(defun nskk-prolog-trie-prefix-search (predicate arity prefix)
  "Search PREDICATE/ARITY trie for keys starting with PREFIX.
Return list of (key . value) pairs matching PREFIX.
PREDICATE is a symbol, ARITY is integer, PREFIX is a string.
For arity-2 predicates, value is the second argument of each fact.
Uses the trie index for O(k+n) performance instead of O(N).
Returns nil if no trie index exists or PREFIX matches nothing."
  (let* ((key (nskk--prolog-clause-key predicate arity))
         (trie (gethash key nskk--prolog-trie-indices)))
    (when trie
      (cl-loop for (index-key . clauses) in (nskk--prolog-trie-prefix-search trie prefix)
               for head = (caar clauses)
               when (and head (>= (length head) 3))
               collect (cons index-key (nth 2 head))))))

(defun nskk-prolog-trie-bulk-assert (predicate arity kana-candidates-pairs)
  "Bulk-assert dictionary entries into the trie index and flat clause database.
PREDICATE and ARITY identify the predicate (e.g., \\='system-dict-entry and 2).
KANA-CANDIDATES-PAIRS is a list of (KANA . CANDIDATES-LIST) pairs where
KANA is a string (the trie key / first argument) and CANDIDATES-LIST is
the second argument value.

Each pair inserts one fact (PREDICATE KANA CANDIDATES-LIST).  Each pair is
written to both the trie index (for O(k+n) prefix lookup) and the flat clause
database (so that variable-first-arg queries fall back correctly).  Use
`nskk-prolog-retract-all' to remove all bulk-asserted entries.

Requires the predicate to have a :trie index configured via
`nskk-prolog-set-index' before calling this function."
  (let* ((dbkey (nskk--prolog-clause-key predicate arity))
         (trie (gethash dbkey nskk--prolog-trie-indices)))
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

Convenience wrapper around `nskk-prolog-query-one' for boolean
holds-checking.  Particularly useful for testing zero-arity facts
like \\='(dict-initialized).

Example:
  (nskk-prolog-holds-p \\='(dict-initialized))
  ;; => t   (when the fact is asserted)
  ;; => nil (when the fact is absent)"
  (not (null (nskk-prolog-query-one goal))))

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
