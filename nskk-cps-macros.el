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
;;   `defun/done' — Pattern A (1-continuation: on-done)
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

;;; Code:

(require 'cl-lib)


;;; -----------------------------------------------------------------------
;;; Internal AST walker helpers
;;; -----------------------------------------------------------------------

(defun nskk-cps--transform-body-list (forms on-found-sym on-not-found-sym)
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
        (list (nskk-cps--transform-<- head tail on-found-sym on-not-found-sym)))

       ;; (<-or var fn args... :found f :fail g) — two-arm CPS bind
       ((and (consp head) (eq (car head) '<-or))
        (list (nskk-cps--transform-<-or head on-found-sym on-not-found-sym)))

       ;; More forms follow: head is NOT in tail position, recurse on tail
       (tail
        (cons head (nskk-cps--transform-body-list tail on-found-sym on-not-found-sym)))

       ;; head IS the last form (tail position): transform it
       (t
        (list (nskk-cps--transform-form head on-found-sym on-not-found-sym)))))))

(defun nskk-cps--transform-form (form on-found-sym on-not-found-sym)
  "Transform a single FORM in CPS tail position.
ON-FOUND-SYM and ON-NOT-FOUND-SYM are the continuation parameter symbols.
Returns the transformed form."
  (cond
   ;; Not a list: atom — return as-is (no CPS transformation possible)
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
    ;; In tail position with no following body: continuation is just (fail)
    (nskk-cps--transform-<- form nil on-found-sym on-not-found-sym))

   ;; (<-or ...) — two-arm CPS bind
   ((eq (car form) '<-or)
    (nskk-cps--transform-<-or form on-found-sym on-not-found-sym))

   ;; (if TEST THEN) or (if TEST THEN ELSE...) — Emacs allows multiple else forms
   ;; e.g. (if cond then else1 else2) behaves like (if cond then (progn else1 else2))
   ((eq (car form) 'if)
    (let* ((test       (nth 1 form))
           (then       (nth 2 form))
           (else-forms (nthcdr 3 form))
           ;; Normalise: 0 else → nil, 1 → the form, 2+ → wrap in progn
           (else (cond
                  ((null else-forms)      nil)
                  ((cdr else-forms)       `(progn ,@else-forms))
                  (t                      (car else-forms)))))
      (if else
          `(if ,test
               ,(nskk-cps--transform-form then on-found-sym on-not-found-sym)
             ,(nskk-cps--transform-form else on-found-sym on-not-found-sym))
        `(if ,test
             ,(nskk-cps--transform-form then on-found-sym on-not-found-sym)))))

   ;; (cond CLAUSES...) — transform last form in each clause body
   ((eq (car form) 'cond)
    `(cond
      ,@(mapcar (lambda (clause)
                  (let* ((test  (car clause))
                         (body  (cdr clause)))
                    (if body
                        `(,test ,@(nskk-cps--transform-body-list
                                   body on-found-sym on-not-found-sym))
                      ;; Clause with test-only: pass through unchanged
                      `(,test))))
                (cdr form))))

   ;; (when TEST BODY...) — transform last body form
   ((eq (car form) 'when)
    (let ((test (cadr form))
          (body (cddr form)))
      `(when ,test
         ,@(nskk-cps--transform-body-list body on-found-sym on-not-found-sym))))

   ;; (unless TEST BODY...) — transform last body form
   ((eq (car form) 'unless)
    (let ((test (cadr form))
          (body (cddr form)))
      `(unless ,test
         ,@(nskk-cps--transform-body-list body on-found-sym on-not-found-sym))))

   ;; (progn FORMS...) — transform last form
   ((eq (car form) 'progn)
    (let ((body (cdr form)))
      `(progn ,@(nskk-cps--transform-body-list body on-found-sym on-not-found-sym))))

   ;; (let BINDINGS BODY...) — pass through bindings, transform body
   ((eq (car form) 'let)
    (let ((bindings (cadr form))
          (body     (cddr form)))
      `(let ,bindings
         ,@(nskk-cps--transform-body-list body on-found-sym on-not-found-sym))))

   ;; (let* BINDINGS BODY...) — pass through bindings, transform body
   ((eq (car form) 'let*)
    (let ((bindings (cadr form))
          (body     (cddr form)))
      `(let* ,bindings
         ,@(nskk-cps--transform-body-list body on-found-sym on-not-found-sym))))

   ;; (pcase EXPR CLAUSES...) — transform each clause body
   ((eq (car form) 'pcase)
    (let ((expr    (cadr form))
          (clauses (cddr form)))
      `(pcase ,expr
         ,@(mapcar (lambda (clause)
                     (let ((pattern (car clause))
                           (body    (cdr clause)))
                       `(,pattern
                         ,@(nskk-cps--transform-body-list
                            body on-found-sym on-not-found-sym))))
                   clauses))))

   ;; (call/cc (lambda (K) BODY...)) — capture on-found as first-class K
   ;; K is bound to the current on-found continuation.
   ;; Calling (funcall K v) from anywhere in BODY is equivalent to (succeed v)
   ;; but K can be stored, passed to other functions, or called multiple times
   ;; (multi-shot).  Only valid in tail position.
   ((and (eq (car form) 'call/cc)
         (consp (cadr form))
         (eq (caadr form) 'lambda))
    (let* ((lambda-form   (cadr form))
           (lambda-params (cadr lambda-form))
           (lambda-body   (cddr lambda-form)))
      (unless (and (= (length lambda-params) 1) (symbolp (car lambda-params)))
        (error "nskk-cps: call/cc lambda must take exactly one parameter, got: %S"
               lambda-form))
      (let ((k-sym (car lambda-params)))
        `(let ((,k-sym ,on-found-sym))
           ,@(nskk-cps--transform-body-list
              lambda-body on-found-sym on-not-found-sym)))))

   ;; (escape K BODY...) — escape (single-shot) continuation
   ;; K is bound to an escape function.  Calling (funcall K v) from anywhere
   ;; inside BODY immediately throws to the escape boundary, which then calls
   ;; on-found with v.  After the throw, no further BODY forms are executed.
   ;; If BODY completes normally the last-form CPS transformation applies.
   ;; Only valid in tail position.
   ((eq (car form) 'escape)
    (let* ((k-name (cadr form))
           (body   (cddr form))
           (tag    (make-symbol "nskk-cps-escape")))
      (unless (symbolp k-name)
        (error "nskk-cps: escape requires a symbol as first argument, got: %S" k-name))
      `(catch ',tag
         (let ((,k-name (lambda (v) (throw ',tag (funcall ,on-found-sym v)))))
           ,@(nskk-cps--transform-body-list body on-found-sym on-not-found-sym)))))

   ;; (and FORMS...) — transform last form only (others are guards)
   ((eq (car form) 'and)
    (let ((guards (butlast (cdr form)))
          (last   (car (last (cdr form)))))
      (if last
          `(and ,@guards
                ,(nskk-cps--transform-form last on-found-sym on-not-found-sym))
        form)))

   ;; (or FORMS...) — transform last form only (others are fallbacks)
   ((eq (car form) 'or)
    (let ((fallbacks (butlast (cdr form)))
          (last      (car (last (cdr form)))))
      (if last
          `(or ,@fallbacks
               ,(nskk-cps--transform-form last on-found-sym on-not-found-sym))
        form)))

   ;; Unknown form: attempt single-level macro expansion and retry.
   ;; This lets the transformer see through custom macros (e.g.
   ;; `nskk-with-conversion-context' → `when') without requiring an
   ;; explicit case for every project-specific macro.  If the form is
   ;; not a macro (expansion returns the same object), pass through as-is.
   (t
    (let ((expanded (macroexpand form)))
      (if (eq expanded form)
          form                         ; not a macro — pass through unchanged
        (nskk-cps--transform-form expanded on-found-sym on-not-found-sym))))))

(defun nskk-cps--transform-<- (form rest-forms on-found-sym on-not-found-sym)
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
    ;; Generate a gensym param to prevent variable capture in the lambda
    (let ((param (make-symbol (concat "--" (symbol-name var) "--"))))
      ;; The continuation body is REST-FORMS (recursively transformed)
      (let ((cont-body (if rest-forms
                           (nskk-cps--transform-body-list
                            rest-forms on-found-sym on-not-found-sym)
                         ;; No rest forms: forward found value to on-found
                         `((funcall ,on-found-sym ,var)))))
        `(,fn-k ,@args
                (lambda (,param)
                  (let ((,var ,param))
                    ,@cont-body))
                ,on-not-found-sym)))))

(defun nskk-cps--transform-<-or (form on-found-sym on-not-found-sym)
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
                    ,(nskk-cps--transform-form
                      found-form on-found-sym on-not-found-sym)))
                (lambda ()
                  ,(nskk-cps--transform-form
                    fail-form on-found-sym on-not-found-sym)))))))


;;; -----------------------------------------------------------------------
;;; Arglist helpers
;;; -----------------------------------------------------------------------

(defun nskk-cps--args-info (args)
  "Parse lambda ARGS into (PLAIN-ARGS . REST-SYM-OR-NIL).
PLAIN-ARGS is a list of all arg names with lambda-list keywords stripped
\(&optional, &key, &allow-other-keys are removed; their following args kept).
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

Example:

  (defun/k nskk-dict-lookup (key)
    \"Look up KEY.\"
    (let* ((result (do-lookup key)))
      (if result (succeed result) (fail))))"
  (declare (doc-string 3) (indent defun) (debug (symbolp listp stringp body)))
  (let* ((args-info        (nskk-cps--args-info args))
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
         (transformed      (nskk-cps--transform-body-list
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
         ,sync-call))))

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
other forms), the sync wrapper will include `(interactive)'.
Note: place `;;;###autoload' before the `defun/done' call site in the source
file to autoload the sync wrapper — autoload cookies cannot be generated
inside macro expansions.

ARGS is the plain argument list (without continuation parameters).
DOCSTRING is required.

Example:

  (defun/done nskk-cancel-conversion ()
    \"Cancel the current conversion.\"
    :interactive t
    (when (nskk-converting-p)
      (nskk-rollback-conversion)))"
  (declare (doc-string 3) (indent defun) (debug (symbolp listp stringp body)))
  (let* ((args-info    (nskk-cps--args-info args))
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
    (if interactivep
        `(progn
           (defun ,name/k (,@args ,on-done-sym)
             ,cps-docstring
             ,@real-body
             (funcall ,on-done-sym))
           (defun ,name (,@args)
             ,docstring
             ,interactive-form
             ,sync-call))
      `(progn
         (defun ,name/k (,@args ,on-done-sym)
           ,cps-docstring
           ,@real-body
           (funcall ,on-done-sym))
         (defun ,name (,@args)
           ,docstring
           ,sync-call)))))


(provide 'nskk-cps-macros)

;;; nskk-cps-macros.el ends here
