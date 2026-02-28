;;; nskk-prolog.el --- Embedded Prolog engine for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

;; Author: takeokunn <bararararatty@gmail.com>
;; Maintainer: takeokunn <bararararatty@gmail.com>
;; URL: https://github.com/takeokunn/nskk.el
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

;; Self-built Prolog engine providing unification, backtracking,
;; and declarative rule matching for NSKK's conversion rules.
;;
;; Features:
;; - First-order unification without occurs check
;; - Depth-first search with backtracking
;; - Cut (!) and negation-as-failure (not)
;; - Three index strategies: hash (O(1)), trie (prefix), list (scan)
;; - Assert/retract for dynamic clause management
;; - Arithmetic built-in goals: is/2, </2, >/2, <=/2, >=/2, =:=/2
;; - DSL macros for natural Prolog-like syntax
;;
;; Performance target: single query < 20us with hash indexing
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

(defsubst nskk-prolog--anonymous-p (x)
  "Return non-nil if X is the anonymous variable `?_'."
  (eq x '?_))

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

(defconst nskk-prolog--fail :fail
  "Sentinel value representing unification failure.
Distinguished from nil, which is a valid empty substitution.")

(defsubst nskk-prolog--fail-p (x)
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
     ((nskk-prolog--anonymous-p t1) subst)
     ((nskk-prolog--anonymous-p t2) subst)
     ;; Variable on left: bind it
     ((nskk-prolog-variable-p t1)
      (cons (cons t1 t2) subst))
     ;; Variable on right: bind it
     ((nskk-prolog-variable-p t2)
      (cons (cons t2 t1) subst))
     ;; Two conses: unify recursively
     ((and (consp t1) (consp t2))
      (let ((s (nskk-prolog-unify (car t1) (car t2) subst)))
        (if (nskk-prolog--fail-p s)
            :fail
          (nskk-prolog-unify (cdr t1) (cdr t2) s))))
     ;; Mismatch
     (t :fail))))

;;;; Clause Database

(defvar nskk-prolog--database (make-hash-table :test 'equal)
  "Clause database keyed by \"predicate/arity\" string.
Each value is a list of clauses in insertion order.
A clause is (head . body) where head is (predicate arg1 ...)
and body is a list of goals (nil for facts).")

(defsubst nskk-prolog--clause-key (predicate arity)
  "Return the database key string for PREDICATE with ARITY."
  (format "%s/%d" predicate arity))

(defsubst nskk-prolog--head-key (head)
  "Return the database key string for clause HEAD."
  (nskk-prolog--clause-key (car head) (1- (length head))))

;;;; Private Trie Implementation

(cl-defstruct (nskk-prolog--trie-node
               (:constructor nskk-prolog--trie-node--create)
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

(cl-defstruct (nskk-prolog--trie
               (:constructor nskk-prolog--trie--create-internal)
               (:copier nil))
  "Private trie structure for nskk-prolog internal use.
Slots:
  root     - root node
  size     - total number of stored keys
  metadata - metadata plist"
  (root nil :type nskk-prolog--trie-node)
  (size 0 :type integer)
  (metadata nil :type list))

(defun nskk-prolog--trie-create ()
  "Create and return a new empty private trie."
  (nskk-prolog--trie--create-internal
   :root (nskk-prolog--trie-node--create)
   :size 0
   :metadata nil))

(defun nskk-prolog--trie-insert (trie key value)
  "Insert KEY with VALUE into private TRIE.
KEY must be a non-empty string.  Returns TRIE."
  (unless (stringp key)
    (error "Key must be a string: %s" key))
  (when (zerop (length key))
    (error "Key cannot be empty"))
  (let ((node (nskk-prolog--trie-root trie))
        (key-len (length key))
        (was-new nil))
    (dotimes (i key-len)
      (let ((char (aref key i)))
        (unless (nskk-prolog--trie-node-children node)
          (setf (nskk-prolog--trie-node-children node)
                (make-hash-table :test 'eq :size 50)))
        (let ((next-node (gethash char (nskk-prolog--trie-node-children node))))
          (unless next-node
            (setq next-node (nskk-prolog--trie-node--create :char char))
            (puthash char next-node (nskk-prolog--trie-node-children node)))
          (cl-incf (nskk-prolog--trie-node-count next-node))
          (setq node next-node))))
    (setq was-new (not (nskk-prolog--trie-node-is-end node)))
    (setf (nskk-prolog--trie-node-is-end node) t)
    (setf (nskk-prolog--trie-node-value node) value)
    (when was-new
      (cl-incf (nskk-prolog--trie-size trie)))
    trie))

(defun nskk-prolog--trie-lookup (trie key)
  "Look up KEY in private TRIE.
Returns (value . t) if found, (nil . nil) otherwise."
  (unless (stringp key)
    (error "Key must be a string: %s" key))
  (let ((node (nskk-prolog--trie--find-node trie key)))
    (if (and node (nskk-prolog--trie-node-is-end node))
        (cons (nskk-prolog--trie-node-value node) t)
      (cons nil nil))))

(defun nskk-prolog--trie-delete (trie key)
  "Delete KEY from private TRIE.
Returns t if deleted, nil if KEY was not present."
  (unless (stringp key)
    (error "Key must be a string: %s" key))
  (let ((node (nskk-prolog--trie--find-node trie key)))
    (when (and node (nskk-prolog--trie-node-is-end node))
      (setf (nskk-prolog--trie-node-is-end node) nil)
      (setf (nskk-prolog--trie-node-value node) nil)
      (cl-decf (nskk-prolog--trie-size trie))
      (nskk-prolog--trie--cleanup-path trie key)
      t)))

(defun nskk-prolog--trie--find-node (trie key)
  "Return the trie node for KEY in private TRIE, or nil if not found."
  (let ((node (nskk-prolog--trie-root trie))
        (key-len (length key)))
    (catch 'not-found
      (dotimes (i key-len)
        (let ((char (aref key i)))
          (unless (and (nskk-prolog--trie-node-children node)
                       (setq node (gethash char (nskk-prolog--trie-node-children node))))
            (throw 'not-found nil))))
      node)))

(defun nskk-prolog--trie--cleanup-path (trie key)
  "Remove leaf nodes that are no longer needed after deleting KEY from TRIE."
  (let ((node (nskk-prolog--trie-root trie))
        (parent-stack nil)
        (key-len (length key)))
    (dotimes (i key-len)
      (let ((char (aref key i)))
        (push node parent-stack)
        (setq node (gethash char (nskk-prolog--trie-node-children node)))))
    (dotimes (_i key-len)
      (when (and node
                 (not (nskk-prolog--trie-node-is-end node))
                 (or (null (nskk-prolog--trie-node-children node))
                     (zerop (hash-table-count (nskk-prolog--trie-node-children node)))))
        (let ((parent (pop parent-stack)))
          (when (nskk-prolog--trie-node-children parent)
            (remhash (nskk-prolog--trie-node-char node)
                     (nskk-prolog--trie-node-children parent)))
          (setq node parent))))))

(defun nskk-prolog--trie-prefix-search (trie prefix &optional limit)
  "Search private TRIE for all keys starting with PREFIX.
Returns a list of (key . value) pairs.
Optional LIMIT caps the number of results."
  (unless (stringp prefix)
    (error "Prefix must be a string: %s" prefix))
  (let ((node (if (zerop (length prefix))
                  (nskk-prolog--trie-root trie)
                (nskk-prolog--trie--find-node trie prefix))))
    (when node
      (nskk-prolog--trie--collect-all node prefix limit 0))))

(defun nskk-prolog--trie--collect-all (node prefix limit collected-count)
  "Collect all (key . value) pairs reachable from NODE with PREFIX.
LIMIT caps results; COLLECTED-COUNT tracks how many have been gathered."
  (let ((results nil)
        (count collected-count))
    (catch 'limit-reached
      (cl-labels ((dfs-collect (node prefix)
                    (when (nskk-prolog--trie-node-is-end node)
                      (push (cons prefix (nskk-prolog--trie-node-value node)) results)
                      (cl-incf count)
                      (when (and limit (>= count limit))
                        (throw 'limit-reached nil)))
                    (when (nskk-prolog--trie-node-children node)
                      (maphash (lambda (char child-node)
                                 (let ((new-prefix (concat prefix (char-to-string char))))
                                   (dfs-collect child-node new-prefix)))
                               (nskk-prolog--trie-node-children node)))))
        (dfs-collect node prefix)))
    (nreverse results)))

;;;; Indexing

(defvar nskk-prolog--index-config (make-hash-table :test 'equal)
  "Per-predicate index configuration.
Key: \"pred/arity\", Value: index type (:hash, :trie, or :list).")

(defvar nskk-prolog--hash-indices (make-hash-table :test 'equal)
  "Hash indices for predicates configured with :hash.
Key: \"pred/arity\", Value: hash-table (first-arg -> clause list).")

(defvar nskk-prolog--trie-indices (make-hash-table :test 'equal)
  "Trie indices for predicates configured with :trie.
Key: \"pred/arity\", Value: private nskk-prolog--trie storing clause lists.")

(defun nskk-prolog-set-index (predicate arity type)
  "Configure index strategy for PREDICATE with ARITY.
TYPE must be one of :hash, :trie, or :list.

:hash provides O(1) dispatch on the first argument.
:trie provides prefix matching on the first argument (strings).
:list is a plain scan, the default for small clause sets."
  (let ((key (nskk-prolog--clause-key predicate arity)))
    (puthash key type nskk-prolog--index-config)
    (pcase type
      (:hash
       (unless (gethash key nskk-prolog--hash-indices)
         (puthash key (make-hash-table :test 'equal)
                  nskk-prolog--hash-indices)))
      (:trie
       (unless (gethash key nskk-prolog--trie-indices)
         (puthash key (nskk-prolog--trie-create)
                  nskk-prolog--trie-indices))))))

(defun nskk-prolog--index-add (key clause)
  "Add CLAUSE to the index for KEY if indexing is configured."
  (let ((type (gethash key nskk-prolog--index-config))
        (first-arg (cadr (car clause))))
    (pcase type
      (:hash
       (let* ((ht (gethash key nskk-prolog--hash-indices))
              (existing (gethash first-arg ht)))
         (puthash first-arg (nconc existing (list clause)) ht)))
      (:trie
       (when (stringp first-arg)
         (let* ((trie (gethash key nskk-prolog--trie-indices))
                (existing (car (nskk-prolog--trie-lookup trie first-arg))))
           (nskk-prolog--trie-insert
            trie first-arg
            (nconc existing (list clause)))))))))

(defun nskk-prolog--index-remove (key clause)
  "Remove CLAUSE from the index for KEY if indexing is configured."
  (let ((type (gethash key nskk-prolog--index-config))
        (first-arg (cadr (car clause))))
    (pcase type
      (:hash
       (let* ((ht (gethash key nskk-prolog--hash-indices))
              (existing (gethash first-arg ht))
              (filtered (cl-remove clause existing
                                  :test #'equal :count 1)))
         (if filtered
             (puthash first-arg filtered ht)
           (remhash first-arg ht))))
      (:trie
       (when (stringp first-arg)
         (let* ((trie (gethash key nskk-prolog--trie-indices))
                (existing (car (nskk-prolog--trie-lookup trie first-arg)))
                (filtered (cl-remove clause existing
                                    :test #'equal :count 1)))
           (if filtered
               (nskk-prolog--trie-insert trie first-arg filtered)
             (nskk-prolog--trie-delete trie first-arg))))))))

(defun nskk-prolog--get-clauses (predicate args subst)
  "Retrieve candidate clauses for PREDICATE given ARGS and SUBST.
Uses the configured index strategy for dispatch:
- :hash with a ground first arg -> hash lookup
- :trie with a ground string first arg -> trie lookup
- Otherwise -> full clause list scan"
  (let* ((arity (length args))
         (key (nskk-prolog--clause-key predicate arity))
         (type (gethash key nskk-prolog--index-config))
         (first-arg (and args (nskk-prolog-walk (car args) subst))))
    (pcase type
      (:hash
       (if (and first-arg
                (not (nskk-prolog-variable-p first-arg)))
           (gethash first-arg
                    (gethash key nskk-prolog--hash-indices))
         (gethash key nskk-prolog--database)))
      (:trie
       (if (and (stringp first-arg)
                (not (nskk-prolog-variable-p first-arg)))
           (car (nskk-prolog--trie-lookup
                 (gethash key nskk-prolog--trie-indices)
                 first-arg))
         (gethash key nskk-prolog--database)))
      (_
       (gethash key nskk-prolog--database)))))

;;;; Variable Renaming

(defvar nskk-prolog--var-counter 0
  "Counter for generating fresh variable names.")

(defun nskk-prolog--rename-variables (clause counter)
  "Rename all variables in CLAUSE using COUNTER suffix.
Returns a new clause with fresh variable names to prevent
variable capture between different clause attempts."
  (let ((mapping (make-hash-table :test 'eq)))
    (cl-labels
        ((rename (term)
           (cond
            ((nskk-prolog--anonymous-p term)
             (intern (format "?_anon_%d"
                             (cl-incf nskk-prolog--var-counter))))
            ((nskk-prolog-variable-p term)
             (or (gethash term mapping)
                 (let ((fresh (intern (format "%s_%d"
                                             (symbol-name term)
                                             counter))))
                   (puthash term fresh mapping)
                   fresh)))
            ((consp term)
             (cons (rename (car term)) (rename (cdr term))))
            (t term))))
      (rename clause))))

;;;; Arithmetic Evaluator

(defun nskk-prolog--eval-arith (expr subst)
  "Evaluate arithmetic EXPR under SUBST, returning a number.
EXPR may be a number, a bound Prolog variable, or a list (OP A B)
where OP is one of +, -, *, / and A, B are arithmetic expressions."
  (cond
   ((numberp expr) expr)
   ((nskk-prolog-variable-p expr)
    (let ((val (nskk-prolog-walk expr subst)))
      (if (eq val expr)
          (error "nskk-prolog: unbound variable in arithmetic: %S" expr)
        (nskk-prolog--eval-arith val subst))))
   ;; Emacs Lisp bound symbol (e.g., defconst values used in rule bodies)
   ((and (symbolp expr) (not (nskk-prolog-variable-p expr)) (boundp expr))
    (nskk-prolog--eval-arith (symbol-value expr) subst))
   ((consp expr)
    (let ((op (car expr))
          (a (nskk-prolog--eval-arith (cadr expr) subst))
          (b (nskk-prolog--eval-arith (caddr expr) subst)))
      (cond
       ((eq op '+) (+ a b))
       ((eq op '-) (- a b))
       ((eq op '*) (* a b))
       ((eq op '/) (/ a b))
       (t (error "nskk-prolog: unknown arithmetic operator: %S" op)))))
   (t (error "nskk-prolog: cannot evaluate arithmetic: %S" expr))))

;;;; Prove Engine (Backtracking)

(defun nskk-prolog-prove (goals subst)
  "Prove GOALS under substitution SUBST using depth-first backtracking.
GOALS is a list of Prolog goals to satisfy.
SUBST is the current variable substitution alist.
Returns a list of all successful substitutions (possibly empty).
An empty list nil means no solution was found; a list containing
nil means one solution with an empty substitution.

For each goal, candidate clauses are retrieved (via indexing),
variables are renamed fresh, and the goal is unified with each
clause head.  On success, the clause body and remaining goals
are proved recursively."
  (if (null goals)
      (list subst)
    (let* ((goal (car goals))
           (rest-goals (cdr goals)))
      (cond
       ;; Cut: commit to current choice
       ((eq goal '!)
        (let ((results (nskk-prolog-prove rest-goals subst)))
          (when results
            (throw 'nskk-prolog-cut results))
          nil))
       ;; Negation-as-failure: succeed iff goal has no solutions
       ((and (consp goal) (eq (car goal) 'not))
        (if (null (nskk-prolog-prove (list (cadr goal)) subst))
            (nskk-prolog-prove rest-goals subst)
          nil))
       ;; assertz: side-effect goal - assert a new fact into database
       ((and (consp goal) (eq (car goal) 'assertz))
        (let* ((head-term (cadr goal))
               (ground-head (nskk-prolog-substitute head-term subst)))
          (nskk-prolog-assert (list ground-head))
          (nskk-prolog-prove rest-goals subst)))
       ;; retract: side-effect goal - remove first matching clause
       ((and (consp goal) (eq (car goal) 'retract))
        (let* ((head-term (cadr goal))
               (ground-head (nskk-prolog-substitute head-term subst)))
          (when (nskk-prolog-retract ground-head)
            (nskk-prolog-prove rest-goals subst))))
       ;; Arithmetic comparisons: (>= a b), (<= a b), (> a b), (< a b)
       ((and (consp goal)
             (memq (car goal) '(>= <= > <)))
        (let* ((op (car goal))
               (a (nskk-prolog--eval-arith (cadr goal) subst))
               (b (nskk-prolog--eval-arith (caddr goal) subst)))
          (when (funcall op a b)
            (nskk-prolog-prove rest-goals subst))))
       ;; Arithmetic equality: (=:= a b)
       ((and (consp goal) (eq (car goal) (intern "=:=")))
        (let ((a (nskk-prolog--eval-arith (cadr goal) subst))
              (b (nskk-prolog--eval-arith (caddr goal) subst)))
          (when (= a b)
            (nskk-prolog-prove rest-goals subst))))
       ;; Arithmetic assignment: (is ?var expr) — unify ?var with evaluated result
       ((and (consp goal) (eq (car goal) 'is))
        (let* ((var (cadr goal))
               (val (nskk-prolog--eval-arith (caddr goal) subst))
               (new-subst (nskk-prolog-unify var val subst)))
          (unless (nskk-prolog--fail-p new-subst)
            (nskk-prolog-prove rest-goals new-subst))))
       ;; Normal goal resolution
       (t
        (let* ((predicate (car goal))
               (args (cdr goal))
               (clauses (nskk-prolog--get-clauses
                         predicate args subst))
               (results nil))
          (dolist (clause clauses)
            (let* ((counter (cl-incf nskk-prolog--var-counter))
                   (renamed (nskk-prolog--rename-variables
                             clause counter))
                   (renamed-head (car renamed))
                   (renamed-body (cdr renamed))
                   (new-subst (nskk-prolog-unify
                               goal renamed-head subst)))
              (unless (nskk-prolog--fail-p new-subst)
                (let ((cut-results
                       (catch 'nskk-prolog-cut
                         (nskk-prolog-prove
                          (append renamed-body rest-goals)
                          new-subst))))
                  (when cut-results
                    (setq results
                          (nconc results cut-results)))))))
          results))))))

(defun nskk-prolog-prove-first (goals subst)
  "Like `nskk-prolog-prove' but throw the first solution found.
Used internally by `nskk-prolog-prove-one' for early termination.
GOALS is a list of Prolog goals to satisfy.
SUBST is the current variable substitution alist.

Instead of collecting all solutions, this function throws the
first successful substitution via the `nskk-prolog-first-solution'
tag, allowing `nskk-prolog-prove-one' to return immediately
without exploring further branches.

Cut (!) is handled correctly: the `nskk-prolog-cut' tag is caught
within this function so that cut semantics are preserved while
still allowing early termination of the overall search."
  (if (null goals)
      (throw 'nskk-prolog-first-solution subst)
    (let* ((goal (car goals))
           (rest-goals (cdr goals)))
      (cond
       ;; Cut: commit to current choice
       ((eq goal '!)
        (let ((result (catch 'nskk-prolog-cut
                        (nskk-prolog-prove-first rest-goals subst))))
          (when result
            (throw 'nskk-prolog-first-solution result))))
       ;; Negation-as-failure: succeed iff goal has no solutions
       ((and (consp goal) (eq (car goal) 'not))
        (let ((naf-result (catch 'nskk-prolog-first-solution
                            (nskk-prolog-prove-first (list (cadr goal)) subst)
                            :not-found)))
          (when (eq naf-result :not-found)
            (nskk-prolog-prove-first rest-goals subst))))
       ;; assertz: side-effect goal - assert a new fact into database
       ((and (consp goal) (eq (car goal) 'assertz))
        (let* ((head-term (cadr goal))
               (ground-head (nskk-prolog-substitute head-term subst)))
          (nskk-prolog-assert (list ground-head))
          (nskk-prolog-prove-first rest-goals subst)))
       ;; retract: side-effect goal - remove first matching clause
       ((and (consp goal) (eq (car goal) 'retract))
        (let* ((head-term (cadr goal))
               (ground-head (nskk-prolog-substitute head-term subst)))
          (when (nskk-prolog-retract ground-head)
            (nskk-prolog-prove-first rest-goals subst))))
       ;; Arithmetic comparisons: (>= a b), (<= a b), (> a b), (< a b)
       ((and (consp goal)
             (memq (car goal) '(>= <= > <)))
        (let* ((op (car goal))
               (a (nskk-prolog--eval-arith (cadr goal) subst))
               (b (nskk-prolog--eval-arith (caddr goal) subst)))
          (when (funcall op a b)
            (nskk-prolog-prove-first rest-goals subst))))
       ;; Arithmetic equality: (=:= a b)
       ((and (consp goal) (eq (car goal) (intern "=:=")))
        (let ((a (nskk-prolog--eval-arith (cadr goal) subst))
              (b (nskk-prolog--eval-arith (caddr goal) subst)))
          (when (= a b)
            (nskk-prolog-prove-first rest-goals subst))))
       ;; Arithmetic assignment: (is ?var expr) — unify ?var with evaluated result
       ((and (consp goal) (eq (car goal) 'is))
        (let* ((var (cadr goal))
               (val (nskk-prolog--eval-arith (caddr goal) subst))
               (new-subst (nskk-prolog-unify var val subst)))
          (unless (nskk-prolog--fail-p new-subst)
            (nskk-prolog-prove-first rest-goals new-subst))))
       ;; Normal goal resolution
       (t
        (let* ((predicate (car goal))
               (args (cdr goal))
               (clauses (nskk-prolog--get-clauses predicate args subst)))
          (dolist (clause clauses)
            (let* ((counter (cl-incf nskk-prolog--var-counter))
                   (renamed (nskk-prolog--rename-variables clause counter))
                   (renamed-head (car renamed))
                   (renamed-body (cdr renamed))
                   (new-subst (nskk-prolog-unify goal renamed-head subst)))
              (unless (nskk-prolog--fail-p new-subst)
                (catch 'nskk-prolog-cut
                  (nskk-prolog-prove-first
                   (append renamed-body rest-goals)
                   new-subst)))))))))))

(defun nskk-prolog-prove-one (goals subst)
  "Like `nskk-prolog-prove' but return only the first solution.
Uses early termination for efficiency - stops searching after the
first successful substitution is found.
GOALS is a list of Prolog goals to satisfy.
SUBST is the current variable substitution alist.
Returns the first successful substitution, or nil if none.

Unlike `nskk-prolog-prove' which collects all solutions via
depth-first backtracking, this function uses catch/throw with
the `nskk-prolog-first-solution' tag to abort the search as soon
as one solution is found.  This is significantly more efficient
for deterministic queries or when only the first answer is needed.

For ground queries (no variables), success returns t rather
than nil, so callers can distinguish success from no-solution.
Use `nskk-prolog-prove' when you need the actual substitution
alist for ground queries."
  (let ((result (catch 'nskk-prolog-first-solution
                  (nskk-prolog-prove-first goals subst)
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
                (parent \\?x \\?y) (parent \\?y \\?z))"
  (let* ((head (car clause))
         (key (nskk-prolog--head-key head))
         (existing (gethash key nskk-prolog--database)))
    (puthash key (nconc existing (list clause))
             nskk-prolog--database)
    (nskk-prolog--index-add key clause)))

(defun nskk-prolog-retract (head-pattern)
  "Remove the first clause matching HEAD-PATTERN from the database.
HEAD-PATTERN is matched against clause heads using unification.
Returns t if a clause was removed, nil otherwise."
  (let* ((key (nskk-prolog--head-key head-pattern))
         (clauses (gethash key nskk-prolog--database))
         (found nil))
    (when clauses
      (catch 'done
        (dolist (clause clauses)
          (unless (nskk-prolog--fail-p
                   (nskk-prolog-unify
                    head-pattern (car clause) nil))
            (setq found clause)
            (throw 'done nil))))
      (when found
        (puthash key
                 (cl-remove found clauses :test #'equal :count 1)
                 nskk-prolog--database)
        (nskk-prolog--index-remove key found)
        t))))

(defun nskk-prolog-retract-all (predicate arity)
  "Remove all clauses for PREDICATE with ARITY.
Also clears any associated indices."
  (let ((key (nskk-prolog--clause-key predicate arity)))
    (remhash key nskk-prolog--database)
    (let ((type (gethash key nskk-prolog--index-config)))
      (pcase type
        (:hash (puthash key (make-hash-table :test 'equal)
                        nskk-prolog--hash-indices))
        (:trie (puthash key (nskk-prolog--trie-create)
                        nskk-prolog--trie-indices))))))

(defun nskk-prolog-clear-database ()
  "Reset the entire Prolog database, indices, and variable counter."
  (clrhash nskk-prolog--database)
  (clrhash nskk-prolog--index-config)
  (clrhash nskk-prolog--hash-indices)
  (clrhash nskk-prolog--trie-indices)
  (setq nskk-prolog--var-counter 0))

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
  "Return non-nil if TERM contains no Prolog variables.
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
  (let* ((key (nskk-prolog--clause-key predicate arity))
         (trie (gethash key nskk-prolog--trie-indices)))
    (when trie
      (let ((raw-results (nskk-prolog--trie-prefix-search trie prefix)))
        (delq nil
              (mapcar (lambda (entry)
                        (let* ((index-key (car entry))
                               (clauses (cdr entry))
                               (first-clause (car clauses))
                               (head (when first-clause (car first-clause)))
                               (value (when (and head (>= (length head) 3))
                                        (nth 2 head))))
                          (when value
                            (cons index-key value))))
                      raw-results))))))

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

(defmacro nskk-prolog-?- (goal)
  "Query the Prolog database and return the first solution.
GOAL is (predicate arg1 ...) -- not quoted.

Example:
  (nskk-prolog-?- (romaji-to-kana \"ka\" \\?kana))
  ;; => substitution alist for first solution"
  (declare (debug t))
  `(nskk-prolog-query-one ',goal))

(provide 'nskk-prolog)

;;; nskk-prolog.el ends here
