;;; nskk-cps-macros-test.el --- Tests for nskk-cps-macros  -*- lexical-binding: t -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test
;; Homepage: https://github.com/takeokunn/nskk.el

;; This file is part of NSKK.

;;; Commentary:

;; Unit tests for the `defun/k' and `defun/done' CPS transformation macros
;; defined in nskk-cps-macros.el.
;;
;; Test categories:
;; 1. defun/k macro expansion (macroexpand-1)
;; 2. (succeed VALUE) AST transformation
;; 3. (fail) AST transformation
;; 4. (<- VAR FN ARGS...) CPS bind transformation
;; 5. (<-or VAR FN ARGS... :found F :fail G) transformation
;; 6. AST walker coverage (all special forms)
;; 7. defun/done macro expansion
;; 8. defun/done with :interactive keyword
;; 9. Functional behavior tests (actually call the generated functions)
;; 10. Pass-through tests (non-CPS forms are not modified)
;; 11. Module loading tests
;; 12. Arity error tests (fail/succeed arity checks)
;; 13. Walker edge case tests

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nskk-cps-macros)


;;; -------------------------------------------------------------------
;;; Test helpers
;;; -------------------------------------------------------------------

(defun nskk-cps-test--funcall-sym-named-p (form name)
  "Return t if FORM is (funcall SYM ...) where SYM has `symbol-name' NAME.
Does not check the arguments beyond verifying FORM starts with funcall and
that the second element is a symbol with the given name."
  (and (consp form)
       (eq (car form) 'funcall)
       (symbolp (cadr form))
       (equal (symbol-name (cadr form)) name)))

(defun nskk-cps-test--funcall-sym-named-1arg-p (form name arg)
  "Return t if FORM is (funcall SYM ARG) where SYM has `symbol-name' NAME
and the single argument equals ARG."
  (and (nskk-cps-test--funcall-sym-named-p form name)
       (= (length form) 3)
       (equal (nth 2 form) arg)))

(defun nskk-cps-test--args-contain-sym-named-p (args name)
  "Return t if ARGS list contains a symbol whose `symbol-name' is NAME."
  (cl-some (lambda (s) (and (symbolp s) (equal (symbol-name s) name)))
           args))


;;; ===================================================================
;;; Section 1: defun/k macro expansion tests
;;; ===================================================================

(ert-deftest nskk-cps-macros-defun/k-expands-to-progn-test ()
  "defun/k expands to a top-level progn containing two defuns."
  (let ((expansion (macroexpand-1
                    '(defun/k nskk-cps--test-expand-basic (x)
                       "Test doc."
                       (if x (succeed x) (fail))))))
    (should (consp expansion))
    (should (eq (car expansion) 'progn))
    (should (= (length expansion) 3))))

(ert-deftest nskk-cps-macros-defun/k-generates-/k-function-test ()
  "defun/k generates a NAME/k function with on-found and on-not-found args."
  (let* ((expansion (macroexpand-1
                     '(defun/k nskk-cps--test-expand-k (x)
                        "Doc."
                        (if x (succeed x) (fail)))))
         (k-def (nth 1 expansion)))
    (should (eq (car k-def) 'defun))
    (should (eq (cadr k-def) 'nskk-cps--test-expand-k/k))
    ;; Args must include x plus symbols named on-found and on-not-found.
    ;; The continuation symbols are gensym'd (uninterned), so check by name.
    (let ((args (nth 2 k-def)))
      (should (memq 'x args))
      (should (nskk-cps-test--args-contain-sym-named-p args "on-found"))
      (should (nskk-cps-test--args-contain-sym-named-p args "on-not-found")))))

(ert-deftest nskk-cps-macros-defun/k-generates-sync-function-test ()
  "defun/k generates a sync NAME function that calls NAME/k with identity/nil."
  (let* ((expansion (macroexpand-1
                     '(defun/k nskk-cps--test-expand-sync (x)
                        "Doc."
                        (succeed x))))
         (sync-def (nth 2 expansion)))
    (should (eq (car sync-def) 'defun))
    (should (eq (cadr sync-def) 'nskk-cps--test-expand-sync))
    ;; Sync function body is at index 4 (after defun, name, args, docstring)
    (let ((body (nth 4 sync-def)))
      (should (consp body))
      (should (eq (car body) 'nskk-cps--test-expand-sync/k))
      (should (member '#'identity body)))))

(ert-deftest nskk-cps-macros-defun/k-sync-args-match-plain-args-test ()
  "Sync wrapper receives only the plain args (no continuations)."
  (let* ((expansion (macroexpand-1
                     '(defun/k nskk-cps--test-expand-args (a b c)
                        "Doc."
                        (succeed a))))
         (sync-def (nth 2 expansion)))
    (let ((args (nth 2 sync-def)))
      (should (equal args '(a b c))))))

(ert-deftest nskk-cps-macros-defun/k-docstring-cps-suffix-test ()
  "defun/k appends \" [CPS]\" to the docstring in the /k function."
  (let* ((expansion (macroexpand-1
                     '(defun/k nskk-cps--test-doc (x)
                        "Original doc."
                        (succeed x))))
         (k-def (nth 1 expansion))
         (k-doc (nth 3 k-def)))
    (should (stringp k-doc))
    (should (string-suffix-p " [CPS]" k-doc))
    (should (string-prefix-p "Original doc." k-doc))))

(ert-deftest nskk-cps-macros-defun/k-sync-docstring-unchanged-test ()
  "defun/k preserves the original docstring in the sync wrapper."
  (let* ((expansion (macroexpand-1
                     '(defun/k nskk-cps--test-doc-sync (x)
                        "Preserved doc."
                        (succeed x))))
         (sync-def (nth 2 expansion))
         (sync-doc (nth 3 sync-def)))
    (should (equal sync-doc "Preserved doc."))))


;;; ===================================================================
;;; Section 2: (succeed VALUE) transformation tests
;;; ===================================================================

(ert-deftest nskk-cps-macros-succeed-simple-body-test ()
  "A (succeed v) in simple body becomes (funcall <on-found-sym> v)."
  (let* ((expansion (macroexpand-1
                     '(defun/k nskk-cps--test-succeed-simple ()
                        "Doc."
                        (succeed 42))))
         (k-def  (nth 1 expansion))
         (k-body (nthcdr 4 k-def))
         (form   (car k-body)))
    ;; The symbol in funcall is gensym'd, so check by name, not identity.
    (should (nskk-cps-test--funcall-sym-named-p form "on-found"))
    (should (equal (nth 2 form) 42))
    ;; Confirm the symbol is uninterned (gensym'd).
    (should-not (eq (cadr form) 'on-found))))

(ert-deftest nskk-cps-macros-succeed-in-if-then-branch-test ()
  "(succeed v) in the then-branch of an if is transformed."
  (let* ((expansion (macroexpand-1
                     '(defun/k nskk-cps--test-succeed-if-then (x)
                        "Doc."
                        (if x (succeed x) (fail)))))
         (k-def    (nth 1 expansion))
         (if-form  (car (nthcdr 4 k-def))))
    (should (eq (car if-form) 'if))
    ;; then branch: (funcall <on-found-sym> x)
    (let ((then (nth 2 if-form)))
      (should (nskk-cps-test--funcall-sym-named-p then "on-found"))
      (should (equal (nth 2 then) 'x)))))

(ert-deftest nskk-cps-macros-succeed-in-let-body-test ()
  "(succeed x) inside a let body is transformed."
  (let* ((expansion (macroexpand-1
                     '(defun/k nskk-cps--test-succeed-let ()
                        "Doc."
                        (let ((v 1)) (succeed v)))))
         (k-def   (nth 1 expansion))
         (let-form (car (nthcdr 4 k-def))))
    (should (eq (car let-form) 'let))
    (let ((body-form (car (nthcdr 2 let-form))))
      (should (nskk-cps-test--funcall-sym-named-p body-form "on-found"))
      (should (equal (nth 2 body-form) 'v)))))

(ert-deftest nskk-cps-macros-succeed-in-cond-test ()
  "(succeed v) in a cond clause body is transformed."
  (let* ((expansion (macroexpand-1
                     '(defun/k nskk-cps--test-succeed-cond ()
                        "Doc."
                        (cond (t (succeed 99))))))
         (k-def     (nth 1 expansion))
         (cond-form (car (nthcdr 4 k-def)))
         (clause    (cadr cond-form))
         (clause-body (cadr clause)))
    (should (eq (car cond-form) 'cond))
    (should (nskk-cps-test--funcall-sym-named-p clause-body "on-found"))
    (should (equal (nth 2 clause-body) 99))))


;;; ===================================================================
;;; Section 3: (fail) transformation tests
;;; ===================================================================

(ert-deftest nskk-cps-macros-fail-simple-body-test ()
  "A (fail) in simple body becomes (funcall <on-not-found-sym>)."
  (let* ((expansion (macroexpand-1
                     '(defun/k nskk-cps--test-fail-simple ()
                        "Doc."
                        (fail))))
         (k-def  (nth 1 expansion))
         (k-body (nthcdr 4 k-def))
         (form   (car k-body)))
    ;; The symbol is gensym'd, so check by name.
    (should (nskk-cps-test--funcall-sym-named-p form "on-not-found"))
    (should (= (length form) 2))          ; (funcall SYM) — no args
    (should-not (eq (cadr form) 'on-not-found))))

(ert-deftest nskk-cps-macros-fail-in-if-else-branch-test ()
  "(fail) in the else-branch of an if is transformed."
  (let* ((expansion (macroexpand-1
                     '(defun/k nskk-cps--test-fail-else (x)
                        "Doc."
                        (if x (succeed x) (fail)))))
         (k-def   (nth 1 expansion))
         (if-form (car (nthcdr 4 k-def))))
    (should (eq (car if-form) 'if))
    ;; else branch: (funcall <on-not-found-sym>)
    (let ((else (nth 3 if-form)))
      (should (nskk-cps-test--funcall-sym-named-p else "on-not-found"))
      (should (= (length else) 2)))))

(ert-deftest nskk-cps-macros-fail-in-when-body-test ()
  "(fail) in a when body is transformed."
  (let* ((expansion (macroexpand-1
                     '(defun/k nskk-cps--test-fail-when (x)
                        "Doc."
                        (when x (fail)))))
         (k-def      (nth 1 expansion))
         (when-form  (car (nthcdr 4 k-def))))
    (should (eq (car when-form) 'when))
    (let ((body-form (caddr when-form)))
      (should (nskk-cps-test--funcall-sym-named-p body-form "on-not-found"))
      (should (= (length body-form) 2)))))


;;; ===================================================================
;;; Section 4: (<- VAR FN ARGS...) transformation tests
;;; ===================================================================

(ert-deftest nskk-cps-macros-<--basic-expansion-test ()
  "(<- result fn arg)(succeed result) expands to fn/k call with continuation."
  (let* ((expansion (macroexpand-1
                     '(defun/k nskk-cps--test-<--basic (arg)
                        "Doc."
                        (<- result nskk-cps--test-helper arg)
                        (succeed result))))
         (k-def  (nth 1 expansion))
         (body   (car (nthcdr 4 k-def))))
    ;; The outer call should be nskk-cps--test-helper/k
    (should (eq (car body) 'nskk-cps--test-helper/k))
    ;; arg should appear in the call
    (should (member 'arg (cdr body)))
    ;; The continuation lambda should follow
    (let ((found-lambda (cl-find-if #'functionp
                                    (mapcar (lambda (f)
                                              (when (and (consp f)
                                                         (eq (car f) 'lambda))
                                                f))
                                            (cdr body)))))
      ;; There must be a lambda in the call
      (should (cl-some (lambda (f) (and (consp f) (eq (car f) 'lambda)))
                       (cdr body))))))

(ert-deftest nskk-cps-macros-<--generates-fn/k-name-test ()
  "<- automatically appends /k to the function name."
  (let* ((expansion (macroexpand-1
                     '(defun/k nskk-cps--test-<--name ()
                        "Doc."
                        (<- r my-lookup-fn)
                        (succeed r))))
         (k-def (nth 1 expansion))
         (body  (car (nthcdr 4 k-def))))
    (should (eq (car body) 'my-lookup-fn/k))))

(ert-deftest nskk-cps-macros-<--no-rest-forms-forwards-to-found-test ()
  "<- with no following forms: continuation body calls on-found with var."
  ;; C3 fix: (<- var fn) with no rest forms generates (funcall on-found var),
  ;; NOT (funcall on-not-found).
  (let* ((expansion (macroexpand-1
                     '(defun/k nskk-cps--test-<--no-rest ()
                        "Doc."
                        (<- r nskk-cps--test-helper))))
         (k-def (nth 1 expansion))
         (call  (car (nthcdr 4 k-def)))
         ;; Find the found-lambda (first lambda in call)
         (found-lambda (cl-find-if (lambda (f) (and (consp f) (eq (car f) 'lambda)))
                                   (cdr call)))
         ;; The lambda body: (let ((r --r--)) CONT-BODY)
         (let-form (caddr found-lambda))
         (cont-body (caddr let-form)))
    ;; Continuation body must call on-found (not on-not-found)
    (should (nskk-cps-test--funcall-sym-named-p cont-body "on-found"))
    ;; The argument to on-found must be the bound variable r
    (should (equal (nth 2 cont-body) 'r))))

(ert-deftest nskk-cps-macros-<--gensym-hygiene-test ()
  "The lambda parameter in <- is a gensym, NOT the same symbol as VAR."
  (let* ((expansion (macroexpand-1
                     '(defun/k nskk-cps--test-<--gensym ()
                        "Doc."
                        (<- myvar nskk-cps--test-helper)
                        (succeed myvar))))
         (k-def  (nth 1 expansion))
         (call   (car (nthcdr 4 k-def)))
         ;; Find the found-lambda (first lambda in call)
         (found-lambda (cl-find-if (lambda (f) (and (consp f) (eq (car f) 'lambda)))
                                   (cdr call)))
         (lambda-param (caadr found-lambda)))
    ;; The lambda parameter must not be the interned symbol 'myvar
    (should-not (eq lambda-param 'myvar))
    ;; It should be an uninterned symbol (gensym): interning its name gives a different object
    (should-not (eq (intern (symbol-name lambda-param)) lambda-param))))

(ert-deftest nskk-cps-macros-<--error-on-/k-suffix-test ()
  "<- signals an error at macroexpand time if fn-name ends in /k."
  (should-error
   (macroexpand '(defun/k nskk-cps--test-<--bad-name ()
                   "Doc."
                   (<- r already/k)
                   (succeed r)))
   :type 'error))

(ert-deftest nskk-cps-macros-<--passes-on-not-found-through-test ()
  "<- passes the outer on-not-found symbol as the not-found continuation."
  (let* ((expansion (macroexpand-1
                     '(defun/k nskk-cps--test-<--passthrough (arg)
                        "Doc."
                        (<- result nskk-cps--test-helper arg)
                        (succeed result))))
         (k-def (nth 1 expansion))
         (call  (car (nthcdr 4 k-def)))
         ;; The last argument to fn/k must be the on-not-found gensym.
         (last-arg (car (last call))))
    ;; The symbol is gensym'd, so check by name, not identity.
    (should (symbolp last-arg))
    (should (equal (symbol-name last-arg) "on-not-found"))
    ;; Confirm it is uninterned.
    (should-not (eq last-arg 'on-not-found))))


;;; ===================================================================
;;; Section 5: (<-or VAR FN ARGS... :found F :fail G) transformation tests
;;; ===================================================================

(ert-deftest nskk-cps-macros-<-or-basic-expansion-test ()
  "(<-or r fn :found (succeed r) :fail (fail)) expands to two-lambda fn/k call."
  (let* ((expansion (macroexpand-1
                     '(defun/k nskk-cps--test-<-or-basic ()
                        "Doc."
                        (<-or result nskk-cps--test-helper
                              :found (succeed result)
                              :fail  (fail)))))
         (k-def (nth 1 expansion))
         (call  (car (nthcdr 4 k-def))))
    ;; Head is fn/k
    (should (eq (car call) 'nskk-cps--test-helper/k))
    ;; There must be exactly two lambdas in the call
    (let ((lambdas (cl-remove-if-not (lambda (f) (and (consp f) (eq (car f) 'lambda)))
                                     (cdr call))))
      (should (= (length lambdas) 2)))))

(ert-deftest nskk-cps-macros-<-or-found-branch-transforms-succeed-test ()
  "The :found branch of <-or transforms (succeed r) to (funcall <on-found-sym> r)."
  (let* ((expansion (macroexpand-1
                     '(defun/k nskk-cps--test-<-or-found ()
                        "Doc."
                        (<-or r nskk-cps--test-helper
                              :found (succeed r)
                              :fail  (fail)))))
         (k-def  (nth 1 expansion))
         (call   (car (nthcdr 4 k-def)))
         (lambdas (cl-remove-if-not (lambda (f) (and (consp f) (eq (car f) 'lambda)))
                                    (cdr call)))
         ;; First lambda is the found-lambda (takes one arg)
         (found-lambda (car lambdas))
         (found-body   (caddr found-lambda)))
    ;; found-body should be (let ((r --r--)) (funcall <on-found-sym> r))
    (should (eq (car found-body) 'let))
    (let ((inner (caddr found-body)))
      (should (nskk-cps-test--funcall-sym-named-p inner "on-found"))
      (should (equal (nth 2 inner) 'r)))))

(ert-deftest nskk-cps-macros-<-or-fail-branch-transforms-fail-test ()
  "The :fail branch of <-or transforms (fail) to (funcall <on-not-found-sym>)."
  (let* ((expansion (macroexpand-1
                     '(defun/k nskk-cps--test-<-or-fail ()
                        "Doc."
                        (<-or r nskk-cps--test-helper
                              :found (succeed r)
                              :fail  (fail)))))
         (k-def  (nth 1 expansion))
         (call   (car (nthcdr 4 k-def)))
         (lambdas (cl-remove-if-not (lambda (f) (and (consp f) (eq (car f) 'lambda)))
                                    (cdr call)))
         ;; Second lambda is the fail-lambda (takes no args)
         (fail-lambda (cadr lambdas))
         (fail-body   (caddr fail-lambda)))
    (should (nskk-cps-test--funcall-sym-named-p fail-body "on-not-found"))
    (should (= (length fail-body) 2))))

(ert-deftest nskk-cps-macros-<-or-error-on-/k-suffix-test ()
  "<-or signals an error at macroexpand time if fn-name ends in /k."
  (should-error
   (macroexpand '(defun/k nskk-cps--test-<-or-bad ()
                   "Doc."
                   (<-or r bad-fn/k :found (succeed r) :fail (fail))))
   :type 'error))

(ert-deftest nskk-cps-macros-<-or-missing-found-keyword-test ()
  "<-or signals an error if :found keyword is missing."
  (should-error
   (macroexpand '(defun/k nskk-cps--test-<-or-no-found ()
                   "Doc."
                   (<-or r nskk-cps--test-helper :fail (fail))))
   :type 'error))

(ert-deftest nskk-cps-macros-<-or-missing-fail-keyword-test ()
  "<-or signals an error if :fail keyword is missing."
  (should-error
   (macroexpand '(defun/k nskk-cps--test-<-or-no-fail ()
                   "Doc."
                   (<-or r nskk-cps--test-helper :found (succeed r))))
   :type 'error))


;;; ===================================================================
;;; Section 6: AST walker coverage — all supported special forms
;;; ===================================================================

(ert-deftest nskk-cps-macros-walker-if-two-branch-test ()
  "Walker transforms both branches of a two-branch if."
  (let* ((expansion (macroexpand-1
                     '(defun/k nskk-cps--test-walker-if2 (x)
                        "Doc."
                        (if x (succeed 1) (fail)))))
         (k-def   (nth 1 expansion))
         (if-form (car (nthcdr 4 k-def))))
    (should (eq (car if-form) 'if))
    (let ((then (nth 2 if-form))
          (else (nth 3 if-form)))
      (should (nskk-cps-test--funcall-sym-named-p then "on-found"))
      (should (equal (nth 2 then) 1))
      (should (nskk-cps-test--funcall-sym-named-p else "on-not-found"))
      (should (= (length else) 2)))))

(ert-deftest nskk-cps-macros-walker-if-one-branch-test ()
  "Walker transforms the then-branch of a one-branch if, leaving no else."
  (let* ((expansion (macroexpand-1
                     '(defun/k nskk-cps--test-walker-if1 (x)
                        "Doc."
                        (if x (succeed x)))))
         (k-def   (nth 1 expansion))
         (if-form (car (nthcdr 4 k-def))))
    (should (eq (car if-form) 'if))
    (let ((then (nth 2 if-form)))
      (should (nskk-cps-test--funcall-sym-named-p then "on-found"))
      (should (equal (nth 2 then) 'x)))
    ;; No else clause: only 3 elements in the if form
    (should (null (nth 3 if-form)))))

(ert-deftest nskk-cps-macros-walker-cond-test ()
  "Walker transforms each cond clause body."
  (let* ((expansion (macroexpand-1
                     '(defun/k nskk-cps--test-walker-cond (x)
                        "Doc."
                        (cond (x (succeed x))
                              (t (fail))))))
         (k-def     (nth 1 expansion))
         (cond-form (car (nthcdr 4 k-def)))
         (clause1   (nth 1 cond-form))
         (clause2   (nth 2 cond-form)))
    (should (eq (car cond-form) 'cond))
    (let ((body1 (cadr clause1))
          (body2 (cadr clause2)))
      (should (nskk-cps-test--funcall-sym-named-p body1 "on-found"))
      (should (equal (nth 2 body1) 'x))
      (should (nskk-cps-test--funcall-sym-named-p body2 "on-not-found"))
      (should (= (length body2) 2)))))

(ert-deftest nskk-cps-macros-walker-when-test ()
  "Walker transforms the last body form of a when."
  (let* ((expansion (macroexpand-1
                     '(defun/k nskk-cps--test-walker-when (x)
                        "Doc."
                        (when x (succeed x)))))
         (k-def     (nth 1 expansion))
         (when-form (car (nthcdr 4 k-def))))
    (should (eq (car when-form) 'when))
    (let ((body-form (caddr when-form)))
      (should (nskk-cps-test--funcall-sym-named-p body-form "on-found"))
      (should (equal (nth 2 body-form) 'x)))))

(ert-deftest nskk-cps-macros-walker-unless-test ()
  "Walker transforms the last body form of an unless."
  (let* ((expansion (macroexpand-1
                     '(defun/k nskk-cps--test-walker-unless (x)
                        "Doc."
                        (unless x (fail)))))
         (k-def       (nth 1 expansion))
         (unless-form (car (nthcdr 4 k-def))))
    (should (eq (car unless-form) 'unless))
    (let ((body-form (caddr unless-form)))
      (should (nskk-cps-test--funcall-sym-named-p body-form "on-not-found"))
      (should (= (length body-form) 2)))))

(ert-deftest nskk-cps-macros-walker-progn-test ()
  "Walker transforms the last form of a progn; preceding forms pass through."
  (let* ((expansion (macroexpand-1
                     '(defun/k nskk-cps--test-walker-progn ()
                        "Doc."
                        (progn (setq x 1) (succeed x)))))
         (k-def      (nth 1 expansion))
         (progn-form (car (nthcdr 4 k-def))))
    (should (eq (car progn-form) 'progn))
    ;; First form passes through unchanged
    (should (equal (nth 1 progn-form) '(setq x 1)))
    ;; Last form is transformed
    (let ((last-form (nth 2 progn-form)))
      (should (nskk-cps-test--funcall-sym-named-p last-form "on-found"))
      (should (equal (nth 2 last-form) 'x)))))

(ert-deftest nskk-cps-macros-walker-let-test ()
  "Walker transforms the body of a let, passing bindings through unchanged."
  (let* ((expansion (macroexpand-1
                     '(defun/k nskk-cps--test-walker-let ()
                        "Doc."
                        (let ((v 1)) (succeed v)))))
         (k-def   (nth 1 expansion))
         (let-form (car (nthcdr 4 k-def))))
    (should (eq (car let-form) 'let))
    ;; Bindings unchanged
    (should (equal (cadr let-form) '((v 1))))
    ;; Body transformed
    (let ((body-form (caddr let-form)))
      (should (nskk-cps-test--funcall-sym-named-p body-form "on-found"))
      (should (equal (nth 2 body-form) 'v)))))

(ert-deftest nskk-cps-macros-walker-let*-test ()
  "Walker transforms the body of a let*, passing bindings through unchanged."
  (let* ((expansion (macroexpand-1
                     '(defun/k nskk-cps--test-walker-let* ()
                        "Doc."
                        (let* ((x 1) (y x)) (succeed y)))))
         (k-def    (nth 1 expansion))
         (let*-form (car (nthcdr 4 k-def))))
    (should (eq (car let*-form) 'let*))
    (should (equal (cadr let*-form) '((x 1) (y x))))
    (let ((body-form (caddr let*-form)))
      (should (nskk-cps-test--funcall-sym-named-p body-form "on-found"))
      (should (equal (nth 2 body-form) 'y)))))

(ert-deftest nskk-cps-macros-walker-pcase-test ()
  "Walker transforms each pcase clause body."
  (let* ((expansion (macroexpand-1
                     '(defun/k nskk-cps--test-walker-pcase (v)
                        "Doc."
                        (pcase v
                          ('match (succeed v))
                          (_      (fail))))))
         (k-def      (nth 1 expansion))
         (pcase-form (car (nthcdr 4 k-def)))
         (clause1    (nth 2 pcase-form))
         (clause2    (nth 3 pcase-form)))
    (should (eq (car pcase-form) 'pcase))
    (let ((body1 (nth 1 clause1))
          (body2 (nth 1 clause2)))
      (should (nskk-cps-test--funcall-sym-named-p body1 "on-found"))
      (should (equal (nth 2 body1) 'v))
      (should (nskk-cps-test--funcall-sym-named-p body2 "on-not-found"))
      (should (= (length body2) 2)))))

(ert-deftest nskk-cps-macros-walker-and-transforms-last-test ()
  "Walker transforms only the last form of an and; guards pass through."
  (let* ((expansion (macroexpand-1
                     '(defun/k nskk-cps--test-walker-and (x)
                        "Doc."
                        (and x (succeed x)))))
         (k-def   (nth 1 expansion))
         (and-form (car (nthcdr 4 k-def))))
    (should (eq (car and-form) 'and))
    ;; Guard passes through
    (should (equal (nth 1 and-form) 'x))
    ;; Last form transformed
    (let ((last-form (nth 2 and-form)))
      (should (nskk-cps-test--funcall-sym-named-p last-form "on-found"))
      (should (equal (nth 2 last-form) 'x)))))

(ert-deftest nskk-cps-macros-walker-or-transforms-last-test ()
  "Walker transforms only the last form of an or; fallbacks pass through."
  (let* ((expansion (macroexpand-1
                     '(defun/k nskk-cps--test-walker-or (x fallback)
                        "Doc."
                        (or fallback (succeed x)))))
         (k-def  (nth 1 expansion))
         (or-form (car (nthcdr 4 k-def))))
    (should (eq (car or-form) 'or))
    ;; Fallback passes through
    (should (equal (nth 1 or-form) 'fallback))
    ;; Last form transformed
    (let ((last-form (nth 2 or-form)))
      (should (nskk-cps-test--funcall-sym-named-p last-form "on-found"))
      (should (equal (nth 2 last-form) 'x)))))

(ert-deftest nskk-cps-macros-walker-multi-body-non-tail-passthrough-test ()
  "Non-tail forms in a body are left unchanged; only the last is transformed."
  (let* ((expansion (macroexpand-1
                     '(defun/k nskk-cps--test-walker-multi ()
                        "Doc."
                        (setq side 1)
                        (setq side 2)
                        (succeed side))))
         (k-def (nth 1 expansion))
         (body  (nthcdr 4 k-def)))
    ;; First two forms pass through untransformed
    (should (equal (nth 0 body) '(setq side 1)))
    (should (equal (nth 1 body) '(setq side 2)))
    ;; Last form is transformed
    (let ((last-form (nth 2 body)))
      (should (nskk-cps-test--funcall-sym-named-p last-form "on-found"))
      (should (equal (nth 2 last-form) 'side)))))


;;; ===================================================================
;;; Section 7: defun/done macro expansion tests
;;; ===================================================================

(ert-deftest nskk-cps-macros-defun/done-expands-to-progn-test ()
  "defun/done expands to a progn containing two defuns."
  (let ((expansion (macroexpand-1
                    '(defun/done nskk-cps--test-done-expand (x)
                       "Doc."
                       (setq nskk-cps--done-result x)))))
    (should (consp expansion))
    (should (eq (car expansion) 'progn))
    (should (= (length expansion) 3))))

(ert-deftest nskk-cps-macros-defun/done-generates-/k-with-on-done-test ()
  "defun/done generates NAME/k with on-done arg and appends (funcall on-done)."
  (let* ((expansion (macroexpand-1
                     '(defun/done nskk-cps--test-done-k (x)
                        "Doc."
                        (setq nskk-cps--done-result x))))
         (k-def  (nth 1 expansion))
         (k-args (nth 2 k-def))
         (k-body (nthcdr 4 k-def)))
    (should (eq (car k-def) 'defun))
    (should (eq (cadr k-def) 'nskk-cps--test-done-k/k))
    ;; on-done may be gensym'd — check by name.
    (should (nskk-cps-test--args-contain-sym-named-p k-args "on-done"))
    ;; Last form in body must be (funcall <on-done-sym>)
    (let ((last-form (car (last k-body))))
      (should (nskk-cps-test--funcall-sym-named-p last-form "on-done"))
      (should (= (length last-form) 2)))))

(ert-deftest nskk-cps-macros-defun/done-sync-calls-/k-with-ignore-test ()
  "defun/done sync wrapper calls NAME/k with #'ignore."
  (let* ((expansion (macroexpand-1
                     '(defun/done nskk-cps--test-done-sync (x)
                        "Doc."
                        (setq nskk-cps--done-result x))))
         (sync-def  (nth 2 expansion))
         ;; Sync def layout: (defun NAME ARGS DOCSTRING BODY...)
         ;; body call is at index 4
         (sync-body (nth 4 sync-def)))
    (should (eq (car sync-def) 'defun))
    (should (eq (cadr sync-def) 'nskk-cps--test-done-sync))
    (should (eq (car sync-body) 'nskk-cps--test-done-sync/k))
    (should (member '#'ignore sync-body))))

(ert-deftest nskk-cps-macros-defun/done-docstring-cps-suffix-test ()
  "defun/done appends \" [CPS]\" to the docstring of the /k function."
  (let* ((expansion (macroexpand-1
                     '(defun/done nskk-cps--test-done-doc ()
                        "Side-effect doc."
                        nil)))
         (k-def (nth 1 expansion))
         (k-doc (nth 3 k-def)))
    (should (string-suffix-p " [CPS]" k-doc))
    (should (string-prefix-p "Side-effect doc." k-doc))))

(ert-deftest nskk-cps-macros-defun/done-body-not-ast-transformed-test ()
  "defun/done does NOT transform (succeed)/(fail) forms in BODY."
  (let* ((expansion (macroexpand-1
                     '(defun/done nskk-cps--test-done-no-ast ()
                        "Doc."
                        (succeed 42))))
         (k-def  (nth 1 expansion))
         (k-body (nthcdr 4 k-def)))
    ;; First body form should remain (succeed 42) — NOT (funcall on-found 42)
    (should (equal (car k-body) '(succeed 42)))))


;;; ===================================================================
;;; Section 8: defun/done with :interactive keyword
;;; ===================================================================

(ert-deftest nskk-cps-macros-defun/done-interactive-sync-has-interactive-test ()
  "defun/done with :interactive t produces a sync wrapper with (interactive)."
  (let* ((expansion (macroexpand-1
                     '(defun/done nskk-cps--test-done-interactive ()
                        "Interactive doc."
                        :interactive t
                        (message "hello"))))
         (sync-def (nth 2 expansion)))
    (should (eq (car sync-def) 'defun))
    ;; The body of the sync function must include (interactive)
    (let ((sync-body (nthcdr 3 sync-def)))
      (should (member '(interactive) sync-body)))))

(ert-deftest nskk-cps-macros-defun/done-interactive-/k-excludes-interactive-test ()
  "The /k function generated by :interactive defun/done does NOT include (interactive)."
  (let* ((expansion (macroexpand-1
                     '(defun/done nskk-cps--test-done-interactive-k ()
                        "Doc."
                        :interactive t
                        (message "hello"))))
         (k-def  (nth 1 expansion))
         (k-body (nthcdr 4 k-def)))
    (should-not (member '(interactive) k-body))))

(ert-deftest nskk-cps-macros-defun/done-interactive-body-excludes-keyword-test ()
  "The :interactive t keyword and its value are excluded from the real body."
  (let* ((expansion (macroexpand-1
                     '(defun/done nskk-cps--test-done-interactive-body ()
                        "Doc."
                        :interactive t
                        (message "hello"))))
         (k-def  (nth 1 expansion))
         (k-body (nthcdr 4 k-def)))
    ;; Body should contain (message "hello") and (funcall <on-done-sym>), not :interactive
    (should-not (member :interactive k-body))
    (should (member '(message "hello") k-body))
    ;; The last form must be (funcall <on-done-sym>)
    (let ((last-form (car (last k-body))))
      (should (nskk-cps-test--funcall-sym-named-p last-form "on-done"))
      (should (= (length last-form) 2)))))

(ert-deftest nskk-cps-macros-defun/done-non-interactive-no-interactive-form-test ()
  "defun/done without :interactive does NOT include (interactive) in sync wrapper."
  (let* ((expansion (macroexpand-1
                     '(defun/done nskk-cps--test-done-no-interactive ()
                        "Doc."
                        (message "hello"))))
         (sync-def  (nth 2 expansion))
         (sync-body (nthcdr 3 sync-def)))
    (should-not (member '(interactive) sync-body))))


;;; ===================================================================
;;; Section 9: Functional behavior tests
;;; ===================================================================

;; Define test functions at top level so they are available in all tests.

(defun/k nskk-cps--test-lookup (key)
  "Look up KEY in a small fixed alist. [test-only]"
  (let ((result (assoc key '(("a" . 1) ("b" . 2) ("c" . 3)))))
    (if result (succeed (cdr result)) (fail))))

(defun/k nskk-cps--test-always-succeed ()
  "Always succeeds with the value 42. [test-only]"
  (succeed 42))

(defun/k nskk-cps--test-always-fail ()
  "Always fails. [test-only]"
  (fail))

(defun/k nskk-cps--test-chained (key)
  "Chain two CPS lookups using <-. [test-only]"
  (<- value nskk-cps--test-lookup key)
  (succeed (1+ value)))

(defvar nskk-cps--test-action-result nil
  "Holds the side-effect result for defun/done functional tests.")

(defun/done nskk-cps--test-action (x)
  "Side-effecting action that stores X. [test-only]"
  (setq nskk-cps--test-action-result x))

;; --- /k CPS version tests ---

(ert-deftest nskk-cps-macros-functional-/k-found-test ()
  "Calling the /k function invokes on-found with the looked-up value."
  (let (found-val)
    (nskk-cps--test-lookup/k "a"
                              (lambda (v) (setq found-val v))
                              (lambda () (should nil)))
    (should (equal found-val 1))))

(ert-deftest nskk-cps-macros-functional-/k-not-found-test ()
  "Calling the /k function invokes on-not-found when key is absent."
  (let (not-found-called)
    (nskk-cps--test-lookup/k "z"
                              (lambda (_v) (should nil))
                              (lambda () (setq not-found-called t)))
    (should not-found-called)))

(ert-deftest nskk-cps-macros-functional-/k-multiple-keys-test ()
  "CPS lookup returns correct values for all known keys."
  (dolist (pair '(("a" . 1) ("b" . 2) ("c" . 3)))
    (let (found-val)
      (nskk-cps--test-lookup/k (car pair)
                                (lambda (v) (setq found-val v))
                                (lambda () (should nil)))
      (should (equal found-val (cdr pair))))))

;; --- Sync wrapper tests ---

(ert-deftest nskk-cps-macros-functional-sync-found-test ()
  "Sync wrapper returns the value when the key is found."
  (should (equal (nskk-cps--test-lookup "a") 1))
  (should (equal (nskk-cps--test-lookup "b") 2))
  (should (equal (nskk-cps--test-lookup "c") 3)))

(ert-deftest nskk-cps-macros-functional-sync-not-found-returns-nil-test ()
  "Sync wrapper returns nil when the key is not found."
  (should (null (nskk-cps--test-lookup "z")))
  (should (null (nskk-cps--test-lookup ""))))

(ert-deftest nskk-cps-macros-functional-sync-always-succeed-test ()
  "Sync wrapper for always-succeed returns 42."
  (should (equal (nskk-cps--test-always-succeed) 42)))

(ert-deftest nskk-cps-macros-functional-sync-always-fail-test ()
  "Sync wrapper for always-fail returns nil."
  (should (null (nskk-cps--test-always-fail))))

;; --- CPS consistency: /k result == sync result ---

(ert-deftest nskk-cps-macros-functional-cps-sync-consistency-test ()
  "The /k version and sync wrapper produce equivalent results."
  (dolist (key '("a" "b" "c" "z" ""))
    (let ((sync-result (nskk-cps--test-lookup key))
          (cps-result  (nskk-cps--test-lookup/k key #'identity (lambda () nil))))
      (should (equal sync-result cps-result)))))

;; --- Chaining with <- ---

(ert-deftest nskk-cps-macros-functional-chain-found-test ()
  "Chained CPS functions via <- compose correctly when inner succeeds."
  (should (equal (nskk-cps--test-chained "a") 2))  ; 1 + 1
  (should (equal (nskk-cps--test-chained "b") 3))  ; 2 + 1
  (should (equal (nskk-cps--test-chained "c") 4))) ; 3 + 1

(ert-deftest nskk-cps-macros-functional-chain-not-found-test ()
  "Chained CPS function propagates not-found when inner fails."
  (should (null (nskk-cps--test-chained "z"))))

;; --- defun/done functional tests ---

(ert-deftest nskk-cps-macros-functional-done-sync-test ()
  "defun/done sync wrapper runs body as a side effect."
  (let ((nskk-cps--test-action-result nil))
    (nskk-cps--test-action :sentinel)
    (should (eq nskk-cps--test-action-result :sentinel))))

(ert-deftest nskk-cps-macros-functional-done-/k-calls-on-done-test ()
  "defun/done /k function runs body then calls on-done."
  (let ((nskk-cps--test-action-result nil)
        (done-called nil))
    (nskk-cps--test-action/k :kvalue (lambda () (setq done-called t)))
    (should (eq nskk-cps--test-action-result :kvalue))
    (should done-called)))

(ert-deftest nskk-cps-macros-functional-done-/k-on-done-called-after-body-test ()
  "defun/done /k function calls on-done strictly after body executes."
  ;; Strategy: record whether body has already run when on-done fires.
  ;; nskk-cps--test-action sets nskk-cps--test-action-result, so if on-done
  ;; fires after the body, nskk-cps--test-action-result will already be :sentinel.
  (let ((nskk-cps--test-action-result nil)
        (result-at-on-done-time nil))
    (nskk-cps--test-action/k :sentinel
                              (lambda ()
                                ;; Capture the result value at the moment on-done fires
                                (setq result-at-on-done-time nskk-cps--test-action-result)))
    ;; Body ran first (set result to :sentinel), then on-done captured it
    (should (eq result-at-on-done-time :sentinel))))


;;; ===================================================================
;;; Section 10: Pass-through tests (non-CPS forms are not modified)
;;; ===================================================================

(ert-deftest nskk-cps-macros-passthrough-setq-non-tail-test ()
  "A setq in non-tail position passes through unchanged."
  (let* ((expansion (macroexpand-1
                     '(defun/k nskk-cps--test-passthrough-setq ()
                        "Doc."
                        (setq x 1)
                        (succeed x))))
         (k-def (nth 1 expansion))
         (body  (nthcdr 4 k-def)))
    (should (equal (car body) '(setq x 1)))))

(ert-deftest nskk-cps-macros-passthrough-function-call-non-tail-test ()
  "A regular function call in non-tail position passes through unchanged."
  (let* ((expansion (macroexpand-1
                     '(defun/k nskk-cps--test-passthrough-call ()
                        "Doc."
                        (message "side effect")
                        (succeed 1))))
         (k-def (nth 1 expansion))
         (body  (nthcdr 4 k-def)))
    (should (equal (car body) '(message "side effect")))))

(ert-deftest nskk-cps-macros-passthrough-variable-atom-test ()
  "An atom (symbol) in tail position passes through unchanged (not transformed)."
  (let* ((expansion (macroexpand-1
                     '(defun/k nskk-cps--test-passthrough-atom ()
                        "Doc."
                        my-variable)))
         (k-def (nth 1 expansion))
         (body  (nthcdr 4 k-def)))
    (should (equal (car body) 'my-variable))))

(ert-deftest nskk-cps-macros-passthrough-number-atom-test ()
  "A number literal in tail position passes through unchanged."
  (let* ((expansion (macroexpand-1
                     '(defun/k nskk-cps--test-passthrough-number ()
                        "Doc."
                        42)))
         (k-def (nth 1 expansion))
         (body  (nthcdr 4 k-def)))
    (should (equal (car body) 42))))

(ert-deftest nskk-cps-macros-passthrough-unknown-special-form-test ()
  "An unknown special form in tail position passes through unchanged."
  (let* ((expansion (macroexpand-1
                     '(defun/k nskk-cps--test-passthrough-unknown ()
                        "Doc."
                        (some-unknown-form arg1 arg2))))
         (k-def (nth 1 expansion))
         (body  (nthcdr 4 k-def)))
    (should (equal (car body) '(some-unknown-form arg1 arg2)))))

(ert-deftest nskk-cps-macros-passthrough-let-bindings-unchanged-test ()
  "Binding expressions inside let are not CPS-transformed."
  (let* ((expansion (macroexpand-1
                     '(defun/k nskk-cps--test-passthrough-let-bindings ()
                        "Doc."
                        (let ((x (regular-call 1))
                              (y (another-call 2)))
                          (succeed x)))))
         (k-def    (nth 1 expansion))
         (let-form (car (nthcdr 4 k-def)))
         (bindings (cadr let-form)))
    ;; Bindings themselves must not be transformed
    (should (equal (nth 0 bindings) '(x (regular-call 1))))
    (should (equal (nth 1 bindings) '(y (another-call 2))))))

(ert-deftest nskk-cps-macros-passthrough-cond-test-not-transformed-test ()
  "The test expression of a cond clause is not CPS-transformed."
  (let* ((expansion (macroexpand-1
                     '(defun/k nskk-cps--test-passthrough-cond-test (x)
                        "Doc."
                        (cond ((> x 0) (succeed x))
                              (t       (fail))))))
         (k-def     (nth 1 expansion))
         (cond-form (car (nthcdr 4 k-def)))
         (clause1   (nth 1 cond-form)))
    ;; The cond test (> x 0) must be unchanged
    (should (equal (car clause1) '(> x 0)))))


;;; ===================================================================
;;; Section 11: Module loading tests
;;; ===================================================================

(ert-deftest nskk-cps-macros-feature-provided-test ()
  "nskk-cps-macros provides the nskk-cps-macros feature."
  (should (featurep 'nskk-cps-macros)))

(ert-deftest nskk-cps-macros-defun/k-macro-defined-test ()
  "defun/k is defined as a macro."
  (should (macrop (symbol-function 'defun/k))))

(ert-deftest nskk-cps-macros-defun/done-macro-defined-test ()
  "defun/done is defined as a macro."
  (should (macrop (symbol-function 'defun/done))))

(ert-deftest nskk-cps-macros-internal-transform-body-list-defined-test ()
  "Internal walker function nskk-cps--transform-body-list is defined."
  (should (fboundp 'nskk-cps--transform-body-list)))

(ert-deftest nskk-cps-macros-internal-transform-form-defined-test ()
  "Internal walker function nskk-cps--transform-form is defined."
  (should (fboundp 'nskk-cps--transform-form)))


;;; ===================================================================
;;; Section 12: Arity error tests (W5/W6 — fail/succeed arity checks)
;;; ===================================================================

(ert-deftest nskk-cps-macros-fail-with-args-error-test ()
  "Using (fail arg) inside defun/k signals error at expansion time."
  (should-error
   (macroexpand-1 '(defun/k nskk-cps--test-fail-bad-arity ()
                     "Doc."
                     (fail "bad")))
   :type 'error))

(ert-deftest nskk-cps-macros-succeed-no-args-error-test ()
  "Using (succeed) with no args inside defun/k signals error at expansion time."
  (should-error
   (macroexpand-1 '(defun/k nskk-cps--test-succeed-no-args ()
                     "Doc."
                     (succeed)))
   :type 'error))

(ert-deftest nskk-cps-macros-succeed-too-many-args-error-test ()
  "Using (succeed a b) with too many args inside defun/k signals error at expansion time."
  (should-error
   (macroexpand-1 '(defun/k nskk-cps--test-succeed-too-many ()
                     "Doc."
                     (succeed 1 2)))
   :type 'error))


;;; ===================================================================
;;; Section 13: Walker edge case tests (C4 and uncovered paths)
;;; ===================================================================

(ert-deftest nskk-cps-macros-walker-and-empty-test ()
  "(and) with no sub-forms passes through unchanged."
  (let* ((expansion (macroexpand-1
                     '(defun/k nskk-cps--test-and-empty ()
                        "Doc."
                        (and))))
         (k-def (nth 1 expansion))
         (body  (car (nthcdr 4 k-def))))
    (should (equal body '(and)))))

(ert-deftest nskk-cps-macros-walker-or-empty-test ()
  "(or) with no sub-forms passes through unchanged."
  (let* ((expansion (macroexpand-1
                     '(defun/k nskk-cps--test-or-empty ()
                        "Doc."
                        (or))))
         (k-def (nth 1 expansion))
         (body  (car (nthcdr 4 k-def))))
    (should (equal body '(or)))))

(ert-deftest nskk-cps-macros-walker-cond-test-only-clause-test ()
  "A cond clause with no body passes the test expression through unchanged (C4 fix)."
  ;; With the C4 fix, (cond (some-expr)) — test-only — does NOT transform
  ;; the test expression via the CPS walker.  It passes through as-is.
  (let* ((expansion (macroexpand-1
                     '(defun/k nskk-cps--test-cond-test-only ()
                        "Doc."
                        (cond (some-expr)))))
         (k-def     (nth 1 expansion))
         (cond-form (car (nthcdr 4 k-def))))
    ;; The whole cond form should be (cond (some-expr)) — test NOT transformed.
    (should (equal cond-form '(cond (some-expr))))))

(ert-deftest nskk-cps-macros-walker-cond-test-only-symbol-not-funcall-test ()
  "A test-only cond clause does not produce a (funcall ...) form for the test."
  ;; Additional check: the test atom is not wrapped in funcall.
  (let* ((expansion (macroexpand-1
                     '(defun/k nskk-cps--test-cond-to-no-funcall ()
                        "Doc."
                        (cond (my-flag)))))
         (k-def       (nth 1 expansion))
         (cond-form   (car (nthcdr 4 k-def)))
         (clause      (cadr cond-form))
         (clause-test (car clause)))
    ;; Test must be the bare symbol, not (funcall ...)
    (should (eq clause-test 'my-flag))
    (should-not (and (consp clause-test) (eq (car clause-test) 'funcall)))))


;;; -------------------------------------------------------------------
;;; Section 14: call/cc special form
;;; -------------------------------------------------------------------

(ert-deftest nskk-cps-macros-call/cc-binds-k-to-on-found-test ()
  "call/cc binds K to on-found; (funcall k v) is equivalent to (succeed v)."
  (defun/k nskk-cps-test--callcc-basic (x)
    "Return double of X via call/cc."
    (call/cc (lambda (k)
               (funcall k (* x 2)))))
  (should (equal (nskk-cps-test--callcc-basic 5) 10))
  (should (equal (nskk-cps-test--callcc-basic/k 5 #'identity #'ignore) 10)))

(ert-deftest nskk-cps-macros-call/cc-k-can-be-stored-test ()
  "call/cc: K captured in call/cc can be saved and called later."
  (let (saved-k)
    (defun/k nskk-cps-test--callcc-store (x)
      "Capture K and return double of X."
      (call/cc (lambda (k)
                 (setq saved-k k)
                 (succeed (* x 2)))))
    ;; Call the function normally
    (should (equal (nskk-cps-test--callcc-store 3) 6))
    ;; saved-k is on-found from the last call; call it again with new value
    (should (equal (funcall saved-k 99) 99))))

(ert-deftest nskk-cps-macros-call/cc-succeed-inside-still-works-test ()
  "succeed inside call/cc body still calls on-found via CPS transform."
  (defun/k nskk-cps-test--callcc-with-succeed (flag)
    "Return :yes or :no depending on FLAG."
    (call/cc (lambda (_k)
               (if flag (succeed :yes) (fail)))))
  (should (eq (nskk-cps-test--callcc-with-succeed t)   :yes))
  (should (eq (nskk-cps-test--callcc-with-succeed nil)  nil)))

(ert-deftest nskk-cps-macros-call/cc-macro-expansion-test ()
  "call/cc expands to (let ((k on-found)) ...) form."
  (let* ((expansion (macroexpand-1 '(defun/k nskk-cps-test--callcc-expand (x)
                                      "Doc."
                                      (call/cc (lambda (k) (funcall k x))))))
         (/k-def (cadr expansion))
         (body   (cddr (cddr /k-def))))  ; skip name, args, docstring
    ;; The body should contain a let binding k to on-found
    (should (consp body))
    (let ((let-form (car body)))
      (should (eq (car let-form) 'let))
      (let* ((bindings (cadr let-form))
             (binding  (car bindings)))
        (should (eq (car binding) 'k))))))

;;; -------------------------------------------------------------------
;;; Section 15: escape special form
;;; -------------------------------------------------------------------

(ert-deftest nskk-cps-macros-escape-early-exit-test ()
  "escape: (funcall k v) aborts remaining body and calls on-found with v."
  (defun/k nskk-cps-test--escape-basic (items)
    "Return first even number in ITEMS, or fail."
    (escape k
      (dolist (n items)
        (when (zerop (% n 2))
          (funcall k n)))
      (fail)))
  (should (equal (nskk-cps-test--escape-basic '(1 3 4 7 8)) 4))
  (should (equal (nskk-cps-test--escape-basic '(1 3 5))     nil))
  (should (equal (nskk-cps-test--escape-basic '())          nil)))

(ert-deftest nskk-cps-macros-escape-does-not-continue-after-throw-test ()
  "escape: body forms after (funcall k v) are not evaluated."
  (let ((side-effect nil))
    (defun/k nskk-cps-test--escape-no-continue (x)
      "Escape on first call; subsequent body must not run."
      (escape k
        (funcall k x)
        (setq side-effect :ran)))  ; must NOT run
    (nskk-cps-test--escape-no-continue/k 42 #'identity #'ignore)
    (should (null side-effect))))

(ert-deftest nskk-cps-macros-escape-normal-completion-test ()
  "escape: when escape continuation is never called, body completes normally."
  (defun/k nskk-cps-test--escape-no-escape (flag)
    "Succeed with :done when FLAG is non-nil, without using escape."
    (escape _k
      (if flag (succeed :done) (fail))))
  (should (eq (nskk-cps-test--escape-no-escape t)   :done))
  (should (eq (nskk-cps-test--escape-no-escape nil)  nil)))

(ert-deftest nskk-cps-macros-escape-macro-expansion-uses-catch-test ()
  "escape expands to a (catch ...) form wrapping the body."
  (let* ((expansion (macroexpand-1 '(defun/k nskk-cps-test--escape-expand (xs)
                                      "Doc."
                                      (escape k (dolist (x xs) (funcall k x)) (fail)))))
         (/k-def (cadr expansion))
         (body   (cddr (cddr /k-def))))
    (should (consp body))
    (let ((catch-form (car body)))
      (should (eq (car catch-form) 'catch)))))

(ert-deftest nskk-cps-macros-escape-invalid-k-name-error-test ()
  "escape signals an error when its first argument is not a symbol."
  (should-error
   (macroexpand-1 '(defun/k nskk-cps-test--escape-bad ()
                     "Doc."
                     (escape 42 (succeed :x))))))

(ert-deftest nskk-cps-macros-call/cc-invalid-lambda-error-test ()
  "call/cc signals an error when its argument is not a single-param lambda."
  (should-error
   (macroexpand-1 '(defun/k nskk-cps-test--callcc-bad ()
                     "Doc."
                     (call/cc (lambda (a b) (succeed a)))))))


(provide 'nskk-cps-macros-test)

;;; nskk-cps-macros-test.el ends here
