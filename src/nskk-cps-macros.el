;;; nskk-cps-macros.el --- CPS transformation macros for nskk.el  -*- lexical-binding: t -*-

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

;; CPS transformation macros for nskk.el.

;;; Code:

(require 'cl-lib)

(defun nskk--cps-target-fn (fn-name operator)
  "Return the /k function symbol FN-NAME resolves to under OPERATOR.
OPERATOR names the bind form being expanded, for error reporting.
Rejects a `defun/done' function: its /k takes one continuation while the
bind forms emit two, so the failure continuation would silently arrive as
a positional value instead.  That rejection is best-effort — the property
it reads is stamped when the target's defining module loads, so it reads
nil and the guard passes if that module has not been loaded by expansion
time.  Do not drop a manual arity check elsewhere on the strength of it."
  (let ((name (symbol-name fn-name)))
    (when (string-suffix-p "/k" name)
      (error "NSKK-CPS: `%s' fn must not end in /k, got: %s" operator fn-name))
    (let ((fn-k (intern (concat name "/k"))))
      (when (eq (get fn-k 'nskk--cps-continuation-pattern) :done)
        (error "NSKK-CPS: `%s' cannot bind `%s', which `defun/done' defined"
               operator fn-k))
      fn-k)))

(defun nskk--cps-bind-param (var)
  "Return a fresh uninterned symbol standing in for VAR inside a lambda."
  (make-symbol (concat "--" (symbol-name var) "--")))

(defun nskk--cps-parse-kw-args (rest required-kws)
  "Split REST into positional arguments and the REQUIRED-KWS values.
Returns (POSITIONAL . ((KW . FORM) ...)).  Every keyword in REQUIRED-KWS
must appear with a form after it.  A positional value that is itself one
of REQUIRED-KWS would be taken for the keyword marker."
  (let ((positions (mapcar (lambda (kw)
                             (let ((pos (cl-position kw rest)))
                               (unless pos
                                 (error "NSKK-CPS: missing %S keyword" kw))
                               (when (>= (1+ pos) (length rest))
                                 (error "NSKK-CPS: %S has no following form" kw))
                               (cons kw pos)))
                           required-kws)))
    (cons (cl-subseq rest 0 (apply #'min (mapcar #'cdr positions)))
          (mapcar (lambda (kw-pos)
                    (cons (car kw-pos) (nth (1+ (cdr kw-pos)) rest)))
                  positions))))

(defun nskk--cps-transform-body-list (forms on-found on-not-found)
  "Transform FORMS, a body list whose last form sits in CPS tail position.
Only that last form is transformed against ON-FOUND and ON-NOT-FOUND;
earlier forms are left as they are, except that a `<-' captures every form
after it as its continuation."
  (when forms
    (let ((head (car forms))
          (tail (cdr forms)))
      (pcase head
        (`(<- . ,_)
         (list (nskk--cps-transform-bind head tail on-found on-not-found)))
        (`(<-or . ,_)
         (list (nskk--cps-transform-bind-or head on-found on-not-found)))
        (_
         (if tail
             (cons head (nskk--cps-transform-body-list tail on-found on-not-found))
           (list (nskk--cps-transform-form head on-found on-not-found))))))))

(defun nskk--cps-transform-form (form on-found on-not-found)
  "Transform FORM, a single form in CPS tail position.
Throughout this file ON-FOUND and ON-NOT-FOUND are the symbols naming the
generated function's continuation parameters, not the continuations
themselves, so they are spliced into the output rather than called.
Atoms and forms with no CPS meaning pass through unchanged."
  (pcase form
    ((pred (not consp)) form)

    (`(fail) `(funcall ,on-not-found))
    (`(fail . ,_)
     (error "NSKK-CPS: (fail) takes no arguments, got: %S" form))
    (`(succeed ,val) `(funcall ,on-found ,val))
    (`(succeed . ,_)
     (error "NSKK-CPS: (succeed) takes exactly one argument, got: %S" form))

    (`(<- . ,_)    (nskk--cps-transform-bind form nil on-found on-not-found))
    (`(<-or . ,_)  (nskk--cps-transform-bind-or form on-found on-not-found))
    (`(<-seq . ,_) (nskk--cps-transform-bind-seq form on-found on-not-found))

    (`(,(and head (or 'let 'let* 'pcase-let*)) ,bindings . ,body)
     `(,head ,bindings
             ,@(nskk--cps-transform-body-list body on-found on-not-found)))

    (`(,(and head (or 'when 'unless)) ,test . ,body)
     `(,head ,test
             ,@(nskk--cps-transform-body-list body on-found on-not-found)))

    ;; Only the final operand of `and'/`or' is reached in tail position.
    (`(,(and head (or 'and 'or)) . ,operands)
     (if operands
         `(,head ,@(butlast operands)
                 ,(nskk--cps-transform-form (car (last operands))
                                            on-found on-not-found))
       form))

    (`(progn . ,body)
     `(progn ,@(nskk--cps-transform-body-list body on-found on-not-found)))

    ;; The else of an `if' is already an implicit progn, so the body-list
    ;; transform splices straight into it.
    (`(if ,test ,then . ,else)
     `(if ,test
          ,(nskk--cps-transform-form then on-found on-not-found)
        ,@(nskk--cps-transform-body-list else on-found on-not-found)))

    (`(cond . ,clauses)
     `(cond ,@(mapcar (lambda (clause)
                        (pcase clause
                          (`(,test) `(,test))
                          (`(,test . ,body)
                           `(,test ,@(nskk--cps-transform-body-list
                                      body on-found on-not-found)))
                          ;; A malformed clause is emitted as written, so
                          ;; `cond' itself reports it.  Falling off this
                          ;; `pcase' would yield nil and drop the clause.
                          (_ clause)))
                      clauses)))

    (`(pcase ,expr . ,clauses)
     `(pcase ,expr
        ,@(mapcar (lambda (clause)
                    `(,(car clause)
                      ,@(nskk--cps-transform-body-list
                         (cdr clause) on-found on-not-found)))
                  clauses)))

    (`(call/cc . ,_)  (nskk--cps-transform-call/cc form on-found on-not-found))
    (`(escape . ,_)   (nskk--cps-transform-escape form on-found on-not-found))

    (_ form)))

(defun nskk--cps-transform-bind (form rest on-found on-not-found)
  "Transform a (<- VAR FN ARG...) FORM, with REST as its continuation body.
With no REST the bound value is handed straight to ON-FOUND.  Failure
reaches ON-NOT-FOUND unchanged, with no arm of its own."
  (pcase-let* ((`(,_ ,var ,fn-name . ,args) form)
               (fn-k (nskk--cps-target-fn fn-name '<-))
               (param (nskk--cps-bind-param var)))
    `(,fn-k ,@args
            (lambda (,param)
              (let ((,var ,param))
                ,@(if rest
                      (nskk--cps-transform-body-list rest on-found on-not-found)
                    `((funcall ,on-found ,var)))))
            ,on-not-found)))

(defun nskk--cps-transform-bind-or (form on-found on-not-found)
  "Transform a (<-or VAR FN ARG... :found FOUND :fail FAIL) FORM.
Each of :found and :fail takes exactly one form, and both are transformed
against ON-FOUND and ON-NOT-FOUND rather than calling them directly."
  (pcase-let* ((`(,_ ,var ,fn-name . ,rest) form)
               (fn-k (nskk--cps-target-fn fn-name '<-or))
               (`(,args . ,kws) (nskk--cps-parse-kw-args rest '(:found :fail)))
               (param (nskk--cps-bind-param var)))
    `(,fn-k ,@args
            (lambda (,param)
              (let ((,var ,param))
                ,(nskk--cps-transform-form (cdr (assq :found kws))
                                           on-found on-not-found)))
            (lambda ()
              ,(nskk--cps-transform-form (cdr (assq :fail kws))
                                         on-found on-not-found)))))

(defun nskk--cps-transform-bind-seq (form on-found on-not-found)
  "Transform a (<-seq [VAR (FN ARG...)] BODY...) FORM.
BODY is transformed against ON-FOUND, and failure propagates to
ON-NOT-FOUND without an explicit arm; the binding and its continuation are
one syntactic unit, unlike `<-'."
  (let ((binding (nth 1 form))
        (body (nthcdr 2 form)))
    ;; The call form is checked before destructuring: a `pcase-let*' pattern
    ;; that fails binds nil instead of signalling, which would emit a call to
    ;; `nil/k' rather than reporting the malformed binding.
    (unless (and (vectorp binding) (= (length binding) 2)
                 (symbolp (aref binding 0)) (consp (aref binding 1)))
      (error "NSKK-CPS: <-seq binding must be [var (fn args...)], got %S"
             binding))
    (pcase-let* ((var (aref binding 0))
                 (`(,fn-name . ,args) (aref binding 1))
                 (fn-k (nskk--cps-target-fn fn-name '<-seq))
                 (param (nskk--cps-bind-param var)))
      `(,fn-k ,@args
              (lambda (,param)
                (let ((,var ,param))
                  ,@(nskk--cps-transform-body-list body on-found on-not-found)))
              ,on-not-found))))

(defun nskk--cps-transform-call/cc (form on-found on-not-found)
  "Transform a (call/cc (lambda (K) BODY...)) FORM.
K is bound to ON-FOUND, so it may be stored and called more than once.
ON-NOT-FOUND passes through as the failure continuation."
  (let ((lambda-form (cadr form)))
    (unless (and (consp lambda-form) (eq (car lambda-form) 'lambda))
      (error "NSKK-CPS: call/cc requires a lambda argument, got: %S" form))
    (let ((params (cadr lambda-form))
          (body (cddr lambda-form)))
      (unless (and (= (length params) 1) (symbolp (car params)))
        (error "NSKK-CPS: call/cc lambda takes exactly one parameter, got: %S"
               lambda-form))
      `(let ((,(car params) ,on-found))
         ,@(nskk--cps-transform-body-list body on-found on-not-found)))))

(defun nskk--cps-transform-escape (form on-found on-not-found)
  "Transform an (escape K BODY...) FORM.
K is bound to a single-shot escape continuation: calling it aborts BODY
and hands its argument to ON-FOUND.  ON-NOT-FOUND passes through.  BODY
completing without calling K falls back to the tail-position transform."
  (let ((k-name (cadr form))
        (body (cddr form))
        (tag (make-symbol "nskk-cps-escape")))
    (unless (symbolp k-name)
      (error "NSKK-CPS: escape requires a symbol as first argument, got: %S"
             k-name))
    `(catch ',tag
       (let ((,k-name (lambda (v) (throw ',tag (funcall ,on-found v)))))
         ,@(nskk--cps-transform-body-list body on-found on-not-found)))))

(defun nskk--cps-args-info (args)
  "Parse lambda list ARGS into (PLAIN-ARGS . REST-SYM-OR-NIL).
PLAIN-ARGS names every argument with lambda-list keywords stripped, so it
can be spliced into a call form.  `&key' and `&allow-other-keys' signal,
because the generated sync wrapper cannot reproduce keyword dispatch."
  (when (or (memq '&key args) (memq '&allow-other-keys args))
    (error "NSKK-CPS: &key and &allow-other-keys are not supported: %S" args))
  (let* ((rest-pos (cl-position '&rest args))
         (pre-rest (if rest-pos (cl-subseq args 0 rest-pos) args)))
    (cons (cl-remove-if (lambda (a)
                          (memq a '(&optional &key &allow-other-keys)))
                        pre-rest)
          (when rest-pos (nth (1+ rest-pos) args)))))

(defun nskk--cps-k-name (name)
  "Return the /k function symbol paired with definition NAME."
  (intern (concat (symbol-name name) "/k")))

(defun nskk--cps-k-arglist (args plain-args rest-sym conts)
  "Return the /k lambda list for ARGS with continuations CONTS appended.
PLAIN-ARGS and REST-SYM come from `nskk--cps-args-info'.  With a &rest
parameter the continuations must precede it: Emacs binds a named
parameter written after &rest to nil rather than signalling."
  (if rest-sym
      `(,@plain-args ,@conts &rest ,rest-sym)
    `(,@args ,@conts)))

(defun nskk--cps-sync-call (name/k plain-args rest-sym conts)
  "Return the sync wrapper's call form for NAME/K with CONTS.
PLAIN-ARGS and REST-SYM come from `nskk--cps-args-info'."
  (if rest-sym
      `(apply #',name/k ,@plain-args ,@conts ,rest-sym)
    `(,name/k ,@plain-args ,@conts)))

(defun nskk--cps-parse-interactive (body)
  "Split an optional leading :interactive spec off BODY.
Returns (INTERACTIVE-FORM . REAL-BODY), with a nil form when absent."
  (if (and (consp body) (eq (car body) :interactive))
      (cons (if (eq (cadr body) t)
                '(interactive)
              `(interactive ,(cadr body)))
            (cddr body))
    (cons nil body)))

(defun nskk--cps-parse-sync-fallback (body)
  "Split an optional leading :sync-fallback value off BODY.
Returns (PRESENT VALUE . REAL-BODY).  PRESENT separates an explicit nil
fallback from an absent option."
  (cond
   ((not (and (consp body) (eq (car body) :sync-fallback)))
    (cons nil (cons nil body)))
   ((null (cdr body))
    (error "NSKK-CPS: :sync-fallback requires a value"))
   (t (cons t (cons (cadr body) (cddr body))))))

(defun nskk--cps-reject-misplaced-options (body options)
  "Signal an error if any of OPTIONS survives among top-level BODY forms."
  (dolist (form body)
    (when (memq form options)
      (error "NSKK-CPS: option %S must precede all body forms" form))))

;;;###autoload
(defmacro defun/k (name args docstring &rest body)
  "Define a CPS function pair NAME and NAME/k from one BODY.

NAME/k takes ARGS followed by ON-FOUND and ON-NOT-FOUND.  NAME is a
synchronous wrapper calling NAME/k with `identity' as ON-FOUND, so
failure returns nil unless `:sync-fallback' supplies another value.

BODY may use these forms in tail position:

  (succeed VALUE)       call ON-FOUND with VALUE
  (fail)                call ON-NOT-FOUND
  (<- VAR FN ARG...)    bind FN/k's result, continue with the rest of BODY
  (<-or VAR FN ARG... :found FOUND :fail FAIL)   two-arm bind
  (<-seq [VAR (FN ARG...)] BODY...)              bind, failure propagates
  (call/cc (lambda (K) BODY...))                 K is a multi-shot ON-FOUND
  (escape K BODY...)                             K aborts BODY via ON-FOUND

Options come before any body form, in this order:

  :interactive t or SPEC   sync wrapper only; NAME/k is never interactive
  :sync-fallback VALUE     sync wrapper returns VALUE on `fail'

ARGS excludes the continuation parameters.  DOCSTRING is required."
  (declare (doc-string 3) (indent defun) (debug (symbolp listp stringp body)))
  (pcase-let* ((`(,plain-args . ,rest-sym) (nskk--cps-args-info args))
               (name/k (nskk--cps-k-name name))
               (on-found (make-symbol "on-found"))
               (on-not-found (make-symbol "on-not-found"))
               (`(,interactive-form . ,rest-body)
                (nskk--cps-parse-interactive body))
               (`(,fallback-p ,fallback . ,real-body)
                (nskk--cps-parse-sync-fallback rest-body)))
    (nskk--cps-reject-misplaced-options real-body
                                        '(:interactive :sync-fallback))
    `(progn
       (defun ,name/k ,(nskk--cps-k-arglist args plain-args rest-sym
                                            (list on-found on-not-found))
         ,(concat docstring "\n[CPS]")
         ,@(nskk--cps-transform-body-list real-body on-found on-not-found))
       (defun ,name ,args
         ,docstring
         ,@(when interactive-form (list interactive-form))
         ,(nskk--cps-sync-call name/k plain-args rest-sym
                               (list '#'identity
                                     (if fallback-p
                                         `(lambda () ,fallback)
                                       '#'ignore))))
       (put ',name/k 'nskk--cps-continuation-pattern :found-not-found))))

;;;###autoload
(defmacro defun/done (name args docstring &rest body)
  "Define a side-effecting CPS function pair NAME and NAME/k from BODY.

NAME/k takes ARGS followed by a single ON-DONE continuation, runs BODY
for effect, then calls ON-DONE.  NAME is a synchronous wrapper passing
`ignore'.  `:interactive t' or `:interactive SPEC' as the first BODY
element makes the wrapper interactive; NAME/k never is.

BODY is not CPS-transformed, so `succeed', `fail', `<-' and `call/cc'
pass through untouched and fail at runtime rather than expansion time.
Use `defun/k' when the body needs them.

An autoload cookie on a `defun/done' call records the macro call rather
than the definitions it expands to, so generated autoloads break.

ARGS excludes the continuation parameter.  DOCSTRING is required."
  (declare (doc-string 3) (indent defun) (debug (symbolp listp stringp body)))
  (pcase-let* ((`(,plain-args . ,rest-sym) (nskk--cps-args-info args))
               (name/k (nskk--cps-k-name name))
               (on-done (make-symbol "on-done"))
               (`(,interactive-form . ,real-body)
                (nskk--cps-parse-interactive body)))
    (nskk--cps-reject-misplaced-options real-body '(:interactive))
    `(progn
       (defun ,name/k ,(nskk--cps-k-arglist args plain-args rest-sym
                                            (list on-done))
         ,(concat docstring "\n[CPS]")
         ,@real-body
         (funcall ,on-done))
       (defun ,name ,args
         ,docstring
         ,@(when interactive-form (list interactive-form))
         ,(nskk--cps-sync-call name/k plain-args rest-sym (list '#'ignore)))
       (put ',name/k 'nskk--cps-continuation-pattern :done))))

;;;###autoload
(defmacro defun/3k (name args cont-names docstring &rest body)
  "Define NAME/k taking ARGS plus the three continuations CONT-NAMES.

BODY is not CPS-transformed; call the continuations directly.  No sync
wrapper is generated, because three continuations have no single natural
mapping onto one return value, and `:interactive' is unsupported.

NAME/k is annotated `nskk--cps-continuation-pattern' `:3k'.  The bind
forms emit two continuations, so binding a `:3k' function through them
fails with a runtime arity error — unlike a `defun/done' target, which
they reject while expanding.  Call NAME/k directly.

CONT-NAMES is a list of exactly three symbols.  DOCSTRING is required."
  (declare (doc-string 4) (indent defun)
           (debug (symbolp listp listp stringp body)))
  (unless (and (listp cont-names) (= (length cont-names) 3)
               (cl-every #'symbolp cont-names))
    (error "defun/3k: CONT-NAMES must be a list of exactly 3 symbols, got %S"
           cont-names))
  (let ((name/k (nskk--cps-k-name name)))
    `(progn
       (defun ,name/k (,@args ,@cont-names)
         ,(concat docstring "\n[CPS]")
         ,@body)
       (put ',name/k 'nskk--cps-continuation-pattern :3k))))

;;;###autoload
(defmacro nskk-<-or (bindings fn-call else-form &rest body)
  "Thread both continuations of FN-CALL, binding its result to BINDINGS.
FN-CALL is a complete CPS call including the /k suffix and its arguments
but not its continuations.  ELSE-FORM is one form run on failure; BODY is
the success handler.

For `defun/3k' bodies and plain functions.  Inside `defun/k' use the
`<-or' form, which the CPS transformer recognizes."
  (declare (indent 3) (debug (listp form form body)))
  `(,(car fn-call) ,@(cdr fn-call)
    (lambda ,bindings ,@body)
    (lambda () ,else-form)))

(provide 'nskk-cps-macros)

;;; nskk-cps-macros.el ends here
