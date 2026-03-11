;;; nskk-cps-macros.el --- CPS transformation macros for nskk.el  -*- lexical-binding: t -*-

;; Copyright (C) 2026 NSKK Contributors

;; Author: takeokunn <bararararatty@gmail.com>
;; Maintainer: takeokunn <bararararatty@gmail.com>
;; URL: https://github.com/takeokunn/nskk.el
;; Version: 0.1.0
;; Keywords: i18n convenience
;; Package-Requires: ((emacs "29.1") (cl-lib "1.0"))

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

;; This module provides compile-time CPS (Continuation-Passing Style) transformation
;; macros for nskk.el.  It implements:
;;
;;   `defun/k'    — Pattern B (2-continuation: on-found / on-not-found)
;;   `defun/done' — Pattern A (1-continuation: on-done, no return value)
;;   `defun/3k'   — Pattern D (3-continuation: user-named, no CPS transform)
;;   `nskk-<-'    — Standalone pipeline-bind (on-found only; for defun/3k contexts)
;;   `nskk-<-or'  — Standalone pipeline-bind (both continuations; for defun/3k contexts)
;;
;; L0 Foundation layer.  No dependencies on other nskk modules.
;;
;; The macros allow writing functions once in a natural direct style using the
;; special forms `succeed', `fail', and `<-' (CPS bind), which are then
;; mechanically transformed into proper continuation-passing form at
;; macro-expansion time.
;;
;; Special forms recognized inside `defun/k' bodies:
;;
;;   (succeed VALUE)
;;     Calls the on-found continuation with VALUE.
;;
;;   (fail)
;;     Calls the on-not-found continuation with no arguments.
;;
;;   (<- VAR FN-NAME ARG...)
;;     Binds the CPS result of FN-NAME/k to VAR and continues.
;;     FN-NAME must NOT end in /k — the macro appends /k automatically.
;;     The rest of the enclosing body forms become the continuation body.
;;
;;   (<-or VAR FN-NAME ARG... :found FOUND-FORM :fail FAIL-FORM)
;;     Two-arm CPS bind.  VAR is bound in FOUND-FORM; FAIL-FORM runs when
;;     FN-NAME/k calls its on-not-found continuation.
;;
;;   (call/cc (lambda (K) BODY...))
;;     Binds K to the current on-found continuation (first-class, multi-shot).
;;     Calling (funcall K v) is equivalent to (succeed v) but K can be stored
;;     or called multiple times.
;;
;;   (escape K BODY...)
;;     Binds K to a single-shot escape continuation.  Calling (funcall K v)
;;     aborts BODY and calls on-found with v.  BODY continues normally if K
;;     is never called.
;;
;;   :interactive t | "SPEC"
;;     (Place before any body forms.) Adds `(interactive)' or
;;     `(interactive SPEC)' to the sync wrapper only; the /k function
;;     is never interactive.  Supported by both `defun/k' and `defun/done'.

;;; Code:

(require 'cl-lib)


;;; -----------------------------------------------------------------------
;;; Internal AST walker helpers
;;; -----------------------------------------------------------------------

(defun nskk--cps-transform-body-list (forms on-found-sym on-not-found-sym)
  "Transform a list of FORMS for CPS in tail position.
ON-FOUND-SYM and ON-NOT-FOUND-SYM are the continuation parameter symbols
available in scope.  Returns a transformed list of forms.

The last form in FORMS is the tail-position form and is fully transformed.
Preceding forms are left unchanged (they are evaluated for side effects).

A `(<- VAR FN ...)' form anywhere in FORMS captures all subsequent forms
as its continuation body and transforms the whole remainder."
  (when forms
    (let ((head (car forms))
          (tail (cdr forms)))
      (cond
       ;; (<- var fn args...) — CPS bind: captures tail as continuation
       ((and (consp head) (eq (car head) '<-))
        (list (nskk--cps-transform-<- head tail on-found-sym on-not-found-sym)))

       ;; (<-or var fn args... :found f :fail g) — two-arm CPS bind
       ((and (consp head) (eq (car head) '<-or))
        (list (nskk--cps-transform-<-or head on-found-sym on-not-found-sym)))

       ;; More forms follow: head is NOT in tail position, recurse on tail
       (tail
        (cons head (nskk--cps-transform-body-list tail on-found-sym on-not-found-sym)))

       ;; head IS the last form (tail position): transform it
       (t
        (list (nskk--cps-transform-form head on-found-sym on-not-found-sym)))))))

;;; -----------------------------------------------------------------------
;;; Internal form dispatch table
;;; -----------------------------------------------------------------------

(defvar nskk--cps-form-dispatch nil
  "Dispatch table for `nskk--cps-transform-form'.
An alist mapping form-head symbol to a handler function.
Each handler has the signature (FORM ON-FOUND-SYM ON-NOT-FOUND-SYM) and
returns the CPS-transformed form.
Set unconditionally at load time so `eval-buffer' picks up any changes.
New forms can be supported by pushing (SYMBOL . HANDLER-FN) entries onto
this variable.")

(defun nskk--cps-transform-if (form on-found-sym on-not-found-sym)
  "CPS-transform an (if TEST THEN [ELSE...]) FORM.
ON-FOUND-SYM and ON-NOT-FOUND-SYM are the continuation parameter symbols.
Multiple else forms (e.g., (if c t e1 e2)) are wrapped in an implicit progn."
  (let* ((test       (nth 1 form))
         (then       (nth 2 form))
         (else-forms (nthcdr 3 form))
         (else (cond
                ((null else-forms)  nil)
                ((cdr else-forms)   `(progn ,@else-forms))
                (t                  (car else-forms)))))
    (if else
        `(if ,test
             ,(nskk--cps-transform-form then on-found-sym on-not-found-sym)
           ,(nskk--cps-transform-form else on-found-sym on-not-found-sym))
      `(if ,test
           ,(nskk--cps-transform-form then on-found-sym on-not-found-sym)))))

(defun nskk--cps-transform-cond (form on-found-sym on-not-found-sym)
  "CPS-transform a (cond CLAUSES...) FORM.
Each clause body is transformed; test-only clauses pass through unchanged."
  `(cond
    ,@(mapcar (lambda (clause)
                (let ((test (car clause))
                      (body (cdr clause)))
                  (if body
                      `(,test ,@(nskk--cps-transform-body-list
                                 body on-found-sym on-not-found-sym))
                    `(,test))))
              (cdr form))))

(defun nskk--cps-transform-when (form on-found-sym on-not-found-sym)
  "CPS-transform a (when TEST BODY...) FORM.
The last body form is transformed; preceding forms pass through."
  (let ((test (cadr form))
        (body (cddr form)))
    `(when ,test
       ,@(nskk--cps-transform-body-list body on-found-sym on-not-found-sym))))

(defun nskk--cps-transform-unless (form on-found-sym on-not-found-sym)
  "CPS-transform an (unless TEST BODY...) FORM.
The last body form is transformed; preceding forms pass through."
  (let ((test (cadr form))
        (body (cddr form)))
    `(unless ,test
       ,@(nskk--cps-transform-body-list body on-found-sym on-not-found-sym))))

(defun nskk--cps-transform-progn (form on-found-sym on-not-found-sym)
  "CPS-transform a (progn FORMS...) FORM.
The last form is transformed; preceding forms pass through unchanged."
  `(progn ,@(nskk--cps-transform-body-list
             (cdr form) on-found-sym on-not-found-sym)))

(defun nskk--cps-transform-let (form on-found-sym on-not-found-sym)
  "CPS-transform a (let BINDINGS BODY...) FORM.
Binding expressions are not transformed; only the body tail is."
  (let ((bindings (cadr form))
        (body     (cddr form)))
    `(let ,bindings
       ,@(nskk--cps-transform-body-list body on-found-sym on-not-found-sym))))

(defun nskk--cps-transform-let* (form on-found-sym on-not-found-sym)
  "CPS-transform a (let* BINDINGS BODY...) FORM.
Binding expressions are not transformed; only the body tail is."
  (let ((bindings (cadr form))
        (body     (cddr form)))
    `(let* ,bindings
       ,@(nskk--cps-transform-body-list body on-found-sym on-not-found-sym))))

(defun nskk--cps-transform-pcase-let* (form on-found-sym on-not-found-sym)
  "CPS-transform a (pcase-let* BINDINGS BODY...) FORM.
Binding patterns and expressions are not transformed; only the body tail is."
  (let ((bindings (cadr form))
        (body     (cddr form)))
    `(pcase-let* ,bindings
       ,@(nskk--cps-transform-body-list body on-found-sym on-not-found-sym))))

(defun nskk--cps-transform-pcase (form on-found-sym on-not-found-sym)
  "CPS-transform a (pcase EXPR CLAUSES...) FORM.
Each clause body is transformed; patterns and EXPR pass through unchanged."
  (let ((expr    (cadr form))
        (clauses (cddr form)))
    `(pcase ,expr
       ,@(mapcar (lambda (clause)
                   (let ((pattern (car clause))
                         (body    (cdr clause)))
                     `(,pattern
                       ,@(nskk--cps-transform-body-list
                          body on-found-sym on-not-found-sym))))
                 clauses))))

(defun nskk--cps-transform-call/cc (form on-found-sym on-not-found-sym)
  "CPS-transform a (call/cc (lambda (K) BODY...)) FORM.
K is bound to ON-FOUND-SYM (the current found continuation).
Calling (funcall K v) is equivalent to (succeed v) but K may be stored
or called multiple times (multi-shot).  Only valid in tail position."
  (let* ((lambda-form (cadr form)))
    (unless (and (consp lambda-form) (eq (car lambda-form) 'lambda))
      (error "nskk-cps: call/cc requires a lambda argument, got: %S" form))
    (let ((lambda-params (cadr lambda-form))
          (lambda-body   (cddr lambda-form)))
      (unless (and (= (length lambda-params) 1) (symbolp (car lambda-params)))
        (error "nskk-cps: call/cc lambda must take exactly one parameter, got: %S"
               lambda-form))
      (let ((k-sym (car lambda-params)))
        `(let ((,k-sym ,on-found-sym))
           ,@(nskk--cps-transform-body-list
              lambda-body on-found-sym on-not-found-sym))))))

(defun nskk--cps-transform-escape (form on-found-sym on-not-found-sym)
  "CPS-transform an (escape K BODY...) FORM.
K is bound to a single-shot escape continuation.  Calling (funcall K v)
from inside BODY immediately calls on-found with v and aborts BODY.
If BODY completes normally without calling K, the last-form CPS
transformation applies.  Only valid in tail position."
  (let* ((k-name (cadr form))
         (body   (cddr form))
         (tag    (make-symbol "nskk-cps-escape")))
    (unless (symbolp k-name)
      (error "nskk-cps: escape requires a symbol as first argument, got: %S"
             k-name))
    `(catch ',tag
       (let ((,k-name (lambda (v) (throw ',tag (funcall ,on-found-sym v)))))
         ,@(nskk--cps-transform-body-list body on-found-sym on-not-found-sym)))))

(defun nskk--cps-transform-and (form on-found-sym on-not-found-sym)
  "CPS-transform an (and FORMS...) FORM.
Only the last form is CPS-transformed; preceding forms act as guards.
An empty (and) passes through unchanged."
  (let ((guards (butlast (cdr form)))
        (last   (car (last (cdr form)))))
    (if last
        `(and ,@guards
              ,(nskk--cps-transform-form last on-found-sym on-not-found-sym))
      form)))

(defun nskk--cps-transform-or (form on-found-sym on-not-found-sym)
  "CPS-transform an (or FORMS...) FORM.
Only the last form is CPS-transformed; preceding forms act as fallbacks.
An empty (or) passes through unchanged."
  (let ((fallbacks (butlast (cdr form)))
        (last      (car (last (cdr form)))))
    (if last
        `(or ,@fallbacks
             ,(nskk--cps-transform-form last on-found-sym on-not-found-sym))
      form)))

(setq nskk--cps-form-dispatch
      (list (cons 'if      #'nskk--cps-transform-if)
            (cons 'cond    #'nskk--cps-transform-cond)
            (cons 'when    #'nskk--cps-transform-when)
            (cons 'unless  #'nskk--cps-transform-unless)
            (cons 'progn   #'nskk--cps-transform-progn)
            (cons 'let     #'nskk--cps-transform-let)
            (cons 'let*    #'nskk--cps-transform-let*)
            (cons 'pcase        #'nskk--cps-transform-pcase)
            (cons 'pcase-let*  #'nskk--cps-transform-pcase-let*)
            (cons 'call/cc #'nskk--cps-transform-call/cc)
            (cons 'escape  #'nskk--cps-transform-escape)
            (cons 'and     #'nskk--cps-transform-and)
            (cons 'or      #'nskk--cps-transform-or)))

(defun nskk--cps-transform-form (form on-found-sym on-not-found-sym)
  "Transform a single FORM in CPS tail position.
ON-FOUND-SYM and ON-NOT-FOUND-SYM are the continuation parameter symbols.
Returns the transformed form.

Atoms and unrecognized forms pass through unchanged.  Recognized special
forms are dispatched via `nskk--cps-form-dispatch'.  The `fail', `succeed',
`<-', and `<-or' special forms are handled inline due to their arity
requirements or dedicated helper functions."
  (cond
   ;; Atom — pass through unchanged (no CPS transformation possible)
   ((not (consp form)) form)

   ;; (fail) — zero-arity not-found continuation
   ((eq (car form) 'fail)
    (if (null (cdr form))
        `(funcall ,on-not-found-sym)
      (error "nskk-cps: (fail) takes no arguments, got: %S" form)))

   ;; (succeed VALUE) — one-arity found continuation
   ((eq (car form) 'succeed)
    (cond
     ((null (cdr form))
      (error "nskk-cps: (succeed) requires exactly one argument"))
     ((cddr form)
      (error "nskk-cps: (succeed) takes exactly one argument, got: %S" form))
     (t
      `(funcall ,on-found-sym ,(cadr form)))))

   ;; (<- var fn args...) — CPS bind in tail position with no following forms
   ((eq (car form) '<-)
    (nskk--cps-transform-<- form nil on-found-sym on-not-found-sym))

   ;; (<-or ...) — two-arm CPS bind
   ((eq (car form) '<-or)
    (nskk--cps-transform-<-or form on-found-sym on-not-found-sym))

   ;; Dispatch via table, or pass through if the form head is not registered
   (t
    (let ((handler (assq (car form) nskk--cps-form-dispatch)))
      (if handler
          (funcall (cdr handler) form on-found-sym on-not-found-sym)
        form)))))

(defun nskk--cps-transform-<- (form rest-forms on-found-sym on-not-found-sym)
  "Transform a (<- VAR FN-NAME ARG...) CPS bind form.
FORM is the full `<-' form.  REST-FORMS are the body forms that follow it
in the enclosing body list (they become the continuation).
ON-FOUND-SYM and ON-NOT-FOUND-SYM are the outer continuation symbols.

FN-NAME must NOT already end in /k; the macro appends /k automatically.
Signals an error at macro-expansion time if FN-NAME ends in /k."
  (let* ((var     (nth 1 form))
         (fn-name (nth 2 form))
         (args    (nthcdr 3 form))
         (fn-k    (intern (concat (symbol-name fn-name) "/k"))))
    ;; Compile-time guard
    (when (string-suffix-p "/k" (symbol-name fn-name))
      (error "nskk-cps: `<-' fn-name must be a non-/k name, got: %s \
\(would generate %s/k — double /k suffix)" fn-name fn-name))
    ;; Best-effort compile-time guard: if fn-k was generated by defun/done,
    ;; its /k takes only one continuation (on-done), not two.  Signal an
    ;; error early rather than silently passing on-not-found as a value.
    (when (eq (get fn-k 'nskk--cps-continuation-pattern) :done)
      (error "nskk-cps: `<-' cannot bind a defun/done function `%s'.
`defun/done' /k functions take one continuation (on-done), but `<-' emits
two (on-found, on-not-found).  Call %s directly or use its sync wrapper."
             fn-k fn-k))
    ;; Generate a gensym param to prevent variable capture in the lambda
    (let ((param (make-symbol (concat "--" (symbol-name var) "--"))))
      ;; The continuation body is REST-FORMS (recursively transformed)
      (let ((cont-body (if rest-forms
                           (nskk--cps-transform-body-list
                            rest-forms on-found-sym on-not-found-sym)
                         ;; No rest forms: forward found value to on-found
                         `((funcall ,on-found-sym ,var)))))
        `(,fn-k ,@args
                (lambda (,param)
                  (let ((,var ,param))
                    ,@cont-body))
                ,on-not-found-sym)))))

(defun nskk--cps-transform-<-or (form on-found-sym on-not-found-sym)
  "Transform a (<-or VAR FN-NAME ARG... :found FOUND-FORM :fail FAIL-FORM) form.
FORM is the full `<-or' form.
ON-FOUND-SYM and ON-NOT-FOUND-SYM are the outer continuation symbols.

The :found and :fail keywords split the argument list.  Each takes exactly
one form; use `progn' for multiple forms."
  (let* ((var     (nth 1 form))
         (fn-name (nth 2 form))
         (rest    (nthcdr 3 form))
         (fn-k    (intern (concat (symbol-name fn-name) "/k"))))
    (when (string-suffix-p "/k" (symbol-name fn-name))
      (error "nskk-cps: `<-or' fn-name must be a non-/k name, got: %s \
\(would generate %s/k — double /k suffix)" fn-name fn-name))
    ;; Split on :found and :fail keywords
    (let* ((found-pos (cl-position :found rest))
           (fail-pos  (cl-position :fail  rest)))
      (unless found-pos
        (error "nskk-cps: `<-or' missing :found keyword in form %S" form))
      (unless fail-pos
        (error "nskk-cps: `<-or' missing :fail keyword in form %S" form))
      (when (>= (1+ found-pos) (length rest))
        (error "nskk-cps: `<-or' :found keyword has no following form in %S" form))
      (when (>= (1+ fail-pos) (length rest))
        (error "nskk-cps: `<-or' :fail keyword has no following form in %S" form))
      (let* ((args       (cl-subseq rest 0 (min found-pos fail-pos)))
             (found-form (nth (1+ found-pos) rest))
             (fail-form  (nth (1+ fail-pos)  rest))
             (param      (make-symbol (concat "--" (symbol-name var) "--"))))
        `(,fn-k ,@args
                (lambda (,param)
                  (let ((,var ,param))
                    ,(nskk--cps-transform-form
                      found-form on-found-sym on-not-found-sym)))
                (lambda ()
                  ,(nskk--cps-transform-form
                    fail-form on-found-sym on-not-found-sym)))))))


;;; -----------------------------------------------------------------------
;;; Arglist helpers
;;; -----------------------------------------------------------------------

(defun nskk--cps-args-info (args)
  "Parse lambda ARGS into (PLAIN-ARGS . REST-SYM-OR-NIL).
PLAIN-ARGS is a list of all arg names with lambda-list keywords stripped.
`&optional' and `&rest' are handled correctly.  `&key' and
`&allow-other-keys' are stripped but their parameters are treated as
positional in the generated sync-call, which is INCORRECT for proper keyword
dispatch.  Do not use `&key' in `defun/k' or `defun/done' argument lists.
REST-SYM-OR-NIL is the symbol following &rest, or nil if none.

This is used by `defun/k' and `defun/done' to build correct call forms
inside sync wrappers: lambda keywords are declaration syntax, not values,
so they cannot appear in a function call position."
  (let ((plain nil)
        (rest-sym nil)
        (reading-rest nil))
    (dolist (arg args)
      (cond
       (reading-rest
        (setq rest-sym arg reading-rest nil))
       ((eq arg '&rest)
        (setq reading-rest t))
       ((memq arg '(&optional &key &allow-other-keys))
        nil)                            ; skip keyword, keep following arg names
       (t
        (push arg plain))))
    (cons (nreverse plain) rest-sym)))


;;; -----------------------------------------------------------------------
;;; Public macros
;;; -----------------------------------------------------------------------

;;;###autoload
(defmacro defun/k (name args docstring &rest body)
  "Define a CPS function pair from a single body written with CPS special forms.

Generates two definitions:

  NAME/k (ARG... ON-FOUND ON-NOT-FOUND)
    CPS version.  `succeed' and `fail' calls in BODY are transformed to
    `(funcall ON-FOUND value)' and `(funcall ON-NOT-FOUND)' respectively.
    `<-' and `<-or' CPS bind forms are also transformed.

  NAME (ARG...)
    Synchronous convenience wrapper.  Calls NAME/k with `#\\='identity' as
    ON-FOUND and `#\\='ignore' as ON-NOT-FOUND.  Returns the found value
    or nil.

ARGS is the plain argument list (without continuation parameters).
DOCSTRING is required.

CPS special forms recognized in BODY:

  (succeed VALUE)       — call on-found continuation with VALUE
  (fail)                — call on-not-found continuation with no args
  (<- VAR FN ARGS...)   — bind CPS result of FN/k, continue with rest of body
  (<-or VAR FN ARGS... :found FOUND-FORM :fail FAIL-FORM)
                        — two-arm CPS bind
  (call/cc (lambda (K) BODY...))
                        — bind K to on-found (first-class continuation);
                          K can be stored, passed around, or called many times
  (escape K BODY...)    — bind K to a single-shot escape continuation;
                          (funcall K v) aborts BODY and calls on-found
  :interactive t          — sync wrapper includes (interactive); /k does not.
  :interactive \"SPEC\"     — sync wrapper includes (interactive SPEC).
  (Place :interactive before any body forms.)

Example:

  (defun/k nskk-dict-lookup (key)
    \"Look up KEY.\"
    (let* ((result (do-lookup key)))
      (if result (succeed result) (fail))))"
  (declare (doc-string 3) (indent defun) (debug (symbolp listp stringp body)))
  (let* ((args-info        (nskk--cps-args-info args))
         (plain-args       (car args-info))
         (rest-sym         (cdr args-info))
         (name/k           (intern (concat (symbol-name name) "/k")))
         (on-found-sym     (make-symbol "on-found"))
         (on-not-found-sym (make-symbol "on-not-found"))
         (cps-docstring    (concat docstring " [CPS]"))
         ;; Optional :interactive keyword (sync wrapper only; /k is never interactive).
         ;; :interactive t   → (interactive)
         ;; :interactive "p" → (interactive "p")
         (interactivep (and (consp body) (eq (car body) :interactive)))
         (interactive-spec (when interactivep (cadr body)))
         (interactive-form (when interactivep
                             (if (eq interactive-spec t)
                                 '(interactive)
                               `(interactive ,interactive-spec))))
         (real-body        (if interactivep (cddr body) body))
         (transformed      (nskk--cps-transform-body-list
                            real-body on-found-sym on-not-found-sym))
         (sync-call        (if rest-sym
                               `(apply #',name/k ,@plain-args #'identity #'ignore ,rest-sym)
                             `(,name/k ,@plain-args #'identity #'ignore))))
    `(progn
       ;; For &rest args, continuations must precede &rest in the /k signature.
       ;; (Emacs silently ignores named params after &rest, binding them nil.)
       ,(if rest-sym
            `(defun ,name/k (,@plain-args ,on-found-sym ,on-not-found-sym
                             &rest ,rest-sym)
               ,cps-docstring
               ,@transformed)
          `(defun ,name/k (,@args ,on-found-sym ,on-not-found-sym)
             ,cps-docstring
             ,@transformed))
       (defun ,name (,@args)
         ,docstring
         ;; :interactive applies to the sync wrapper only.
         ,@(when interactive-form (list interactive-form))
         ;; plain-args strips &optional/&rest keywords for a valid call form.
         ,sync-call)
       (put ',name/k 'nskk--cps-continuation-pattern :found-not-found))))

;;;###autoload
(defmacro defun/done (name args docstring &rest body)
  "Define a Pattern-A CPS function pair with a single `on-done' continuation.

Generates two definitions:

  NAME/k (ARG... ON-DONE)
    CPS version.  BODY is executed for side effects, then `(funcall ON-DONE)'
    is appended at the end.  No AST transformation of `succeed'/`fail' is
    performed; this pattern is for side-effecting functions.

  NAME (ARG...)
    Synchronous wrapper.  Calls NAME/k with `#\\='ignore' as ON-DONE.

If the keyword `:interactive' appears as the first element of BODY (before any
other forms), the sync wrapper will include `(interactive)' (for
`:interactive t') or `(interactive SPEC)' (for `:interactive \"SPEC\"').
The /k function is
never interactive.
Note: place `;;;###autoload' before the `defun/done' call site in the source
file to autoload the sync wrapper — autoload cookies cannot be generated
inside macro expansions.

ARGS is the plain argument list (without continuation parameters).
DOCSTRING is required.

Note: CPS special forms (`succeed', `fail', `<-', `<-or', `call/cc',
`escape') are NOT recognized in BODY and pass through untransformed.
Use `defun/k' if CPS transformation of the body is needed.

Note: `call/cc' forms inside a `defun/done' body are NOT transformed — they
produce a runtime symbol-as-function error rather than a compile-time
diagnostic.  Use `defun/k' if continuation capture is needed.

Example:

  (defun/done nskk-cancel-conversion ()
    \"Cancel the current conversion.\"
    :interactive t
    (when (nskk-converting-p)
      (nskk-rollback-conversion)))"
  (declare (doc-string 3) (indent defun) (debug (symbolp listp stringp body)))
  (let* ((args-info    (nskk--cps-args-info args))
         (plain-args   (car args-info))
         (rest-sym     (cdr args-info))
         (name/k       (intern (concat (symbol-name name) "/k")))
         (on-done-sym  (make-symbol "on-done"))
         (cps-docstring (concat docstring " [CPS]"))
         ;; Check for leading :interactive keyword followed by its spec value.
         ;; :interactive t   → (interactive)
         ;; :interactive "p" → (interactive "p")
         ;; :interactive FORM → (interactive FORM)
         (interactivep (and (consp body) (eq (car body) :interactive)))
         (interactive-spec (when interactivep (cadr body)))
         (interactive-form (when interactivep
                             (if (eq interactive-spec t)
                                 '(interactive)
                               `(interactive ,interactive-spec))))
         (real-body    (if interactivep (cddr body) body))
         ;; Build the sync call form, handling &rest via apply.
         (sync-call    (if rest-sym
                           `(apply #',name/k ,@plain-args #'ignore ,rest-sym)
                         `(,name/k ,@plain-args #'ignore))))
    `(progn
       ,(if rest-sym
            `(defun ,name/k (,@plain-args ,on-done-sym &rest ,rest-sym)
               ,cps-docstring
               ,@real-body
               (funcall ,on-done-sym))
          `(defun ,name/k (,@args ,on-done-sym)
             ,cps-docstring
             ,@real-body
             (funcall ,on-done-sym)))
       (defun ,name (,@args)
         ,docstring
         ,@(when interactive-form (list interactive-form))
         ,sync-call)
       (put ',name/k 'nskk--cps-continuation-pattern :done))))

;;;###autoload
(defmacro defun/3k (name args cont-names docstring &rest body)
  "Define a three-continuation CPS function.

Generates one definition:

  NAME/k (ARG... CONT1 CONT2 CONT3)
    CPS version with three named continuations.  BODY is NOT CPS-transformed;
    write (funcall CONT1 ...) / (funcall CONT2 ...) / (funcall CONT3 ...)
    directly.

ARGS is the plain argument list (without continuation parameters).
CONT-NAMES is a list of exactly three symbols naming the continuation
parameters (e.g., (on-match on-incomplete on-fail)).
DOCSTRING is required (fourth positional argument).

Unlike `defun/k' and `defun/done', this macro does NOT generate a sync
wrapper, because three-continuation patterns have no universal default
mapping to a single return value.  Write the sync wrapper manually if needed.

The :interactive keyword is NOT supported.

Annotates NAME/k with property `nskk--cps-continuation-pattern' = :3k.
The function is NOT compatible with the `<-' or `<-or' CPS bind macros.
Call it directly.

Example:

  (defun/3k nskk-converter-convert (romaji)
      (on-match on-incomplete on-fail)
    \"Convert ROMAJI to kana, dispatching to one of three continuations.
ON-MATCH is called as (funcall ON-MATCH kana remaining) on a full match.
ON-INCOMPLETE is called as (funcall ON-INCOMPLETE romaji) on a prefix match.
ON-FAIL is called as (funcall ON-FAIL) on no match.\"
    (if (not (and (stringp romaji) (> (length romaji) 0)))
        (funcall on-fail)
      ...))"
  (declare (doc-string 4) (indent defun)
           (debug (symbolp listp listp stringp body)))
  (unless (and (listp cont-names) (= (length cont-names) 3)
               (cl-every #'symbolp cont-names))
    (error "defun/3k: CONT-NAMES must be a list of exactly 3 symbols, got %S"
           cont-names))
  (let* ((name/k        (intern (concat (symbol-name name) "/k")))
         (cont1         (nth 0 cont-names))
         (cont2         (nth 1 cont-names))
         (cont3         (nth 2 cont-names))
         (cps-docstring (concat docstring " [CPS]")))
    `(progn
       (defun ,name/k (,@args ,cont1 ,cont2 ,cont3)
         ,cps-docstring
         ,@body)
       (put ',name/k 'nskk--cps-continuation-pattern :3k))))


;;;; Standalone CPS Pipeline Bind Macros

;; These macros are for use OUTSIDE `defun/k' bodies (e.g. in `defun/3k',
;; plain `defun', or `defun/done' bodies).  They generate explicit lambda
;; wrappers without requiring the CPS AST transformer.
;;
;; Do NOT use these inside `defun/k' bodies — use the CPS transformer special
;; forms (<- VAR FN ARGS...) and (<-or VAR FN ARGS... :found F :fail G)
;; instead.  Those are recognized by the AST transformer and generate
;; optimized code; these macros would bypass the transformer.
;;
;; Syntax differences from the CPS transformer special forms:
;;   Transformer: (<- VAR FN-NAME ARGS...)     FN-NAME auto-gets /k appended
;;   Standalone:  (<- (BINDINGS) (FN/K ARGS...) BODY...)  FN already ends in /k

;;;###autoload
(defmacro nskk-<- (bindings fn-call &rest body)
  "Thread the on-found continuation of FN-CALL, ignoring on-not-found.
BINDINGS is the argument list for the on-found lambda.
FN-CALL is a fully-qualified CPS call form including the /k suffix and all
arguments (without continuations), e.g. (nskk-core-search/k key nil nil).
BODY is the on-found handler body.  The on-not-found continuation is #\\='ignore.
Returns the return value of the expanded FN-CALL (typically unspecified).

For use in `defun/3k' bodies and plain functions.
NOT for use inside `defun/k' bodies (use the CPS transformer `<-' form there).

Example:
  (nskk-<- (result) (nskk-core-search/k key nil nil)
    (process result))
expands to:
  (nskk-core-search/k key nil nil
    (lambda (result) (process result))
    #\\='ignore)"
  (declare (indent 2) (debug (listp form body)))
  `(,(car fn-call) ,@(cdr fn-call)
    (lambda ,bindings ,@body)
    #'ignore))

;;;###autoload
(defmacro nskk-<-or (bindings fn-call else-form &rest body)
  "Thread both continuations of FN-CALL.
BINDINGS is the argument list for the on-found lambda.
FN-CALL is a fully-qualified CPS call form including the /k suffix and all
arguments (without continuations), e.g. (nskk-core-search/k key nil nil).
ELSE-FORM is the on-not-found handler form (wrapped in a zero-arg lambda).
ELSE-FORM must be a single form; use `progn' for multiple statements.
BODY is the on-found handler body.
Returns the return value of the expanded FN-CALL (typically unspecified).

For use in `defun/3k' bodies and plain functions.
NOT for use inside `defun/k' bodies (use the `<-or' CPS transformer form there).

Example:
  (nskk-<-or (result) (nskk-core-search/k key nil nil)
             (handle-not-found)
    (process result))
expands to:
  (nskk-core-search/k key nil nil
    (lambda (result) (process result))
    (lambda () (handle-not-found)))"
  (declare (indent 3) (debug (listp form form body)))
  `(,(car fn-call) ,@(cdr fn-call)
    (lambda ,bindings ,@body)
    (lambda () ,else-form)))

(provide 'nskk-cps-macros)

;;; nskk-cps-macros.el ends here
