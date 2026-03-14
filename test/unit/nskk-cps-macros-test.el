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
(require 'nskk-test-macros)


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

(defun nskk-cps-test--args-contain-sym-named-p (args name)
  "Return t if ARGS list contains a symbol whose `symbol-name' is NAME."
  (cl-some (lambda (s) (and (symbolp s) (equal (symbol-name s) name)))
           args))


;;;
;;; defun/k macro
;;;

(nskk-describe "defun/k macro"

  (nskk-context "macro expansion structure"
    (nskk-it "expands to a progn containing two defuns and a put annotation"
      (let ((expansion (macroexpand-1
                        '(defun/k nskk--cps-test-expand-basic (x)
                           "Test doc."
                           (if x (succeed x) (fail))))))
        (should (consp expansion))
        (should (eq (car expansion) 'progn))
        ;; (progn /k-defun sync-defun put-annotation)
        (should (= (length expansion) 4))))

    (nskk-it "generates a NAME/k function with on-found and on-not-found args"
      (let* ((expansion (macroexpand-1
                         '(defun/k nskk--cps-test-expand-k (x)
                            "Doc."
                            (if x (succeed x) (fail)))))
             (k-def (nth 1 expansion)))
        (should (eq (car k-def) 'defun))
        (should (eq (cadr k-def) 'nskk--cps-test-expand-k/k))
        ;; Args must include x plus symbols named on-found and on-not-found.
        ;; The continuation symbols are gensym'd (uninterned), so check by name.
        (let ((args (nth 2 k-def)))
          (should (memq 'x args))
          (should (nskk-cps-test--args-contain-sym-named-p args "on-found"))
          (should (nskk-cps-test--args-contain-sym-named-p args "on-not-found")))))

    (nskk-it "generates a sync NAME function that calls NAME/k with identity/nil"
      (let* ((expansion (macroexpand-1
                         '(defun/k nskk--cps-test-expand-sync (x)
                            "Doc."
                            (succeed x))))
             (sync-def (nth 2 expansion)))
        (should (eq (car sync-def) 'defun))
        (should (eq (cadr sync-def) 'nskk--cps-test-expand-sync))
        ;; Sync function body is at index 4 (after defun, name, args, docstring)
        (let ((body (nth 4 sync-def)))
          (should (consp body))
          (should (eq (car body) 'nskk--cps-test-expand-sync/k))
          (should (member '#'identity body)))))

    (nskk-it "sync wrapper receives only the plain args without continuation parameters"
      (let* ((expansion (macroexpand-1
                         '(defun/k nskk--cps-test-expand-args (a b c)
                            "Doc."
                            (succeed a))))
             (sync-def (nth 2 expansion)))
        (let ((args (nth 2 sync-def)))
          (should (equal args '(a b c)))))))

  (nskk-context "docstring handling"
    (nskk-it "appends [CPS] suffix to the docstring in the /k function"
      (let* ((expansion (macroexpand-1
                         '(defun/k nskk--cps-test-doc (x)
                            "Original doc."
                            (succeed x))))
             (k-def (nth 1 expansion))
             (k-doc (nth 3 k-def)))
        (should (stringp k-doc))
        (should (string-suffix-p "\n[CPS]" k-doc))
        (should (string-prefix-p "Original doc." k-doc))))

    (nskk-it "preserves the original docstring unchanged in the sync wrapper"
      (let* ((expansion (macroexpand-1
                         '(defun/k nskk--cps-test-doc-sync (x)
                            "Preserved doc."
                            (succeed x))))
             (sync-def (nth 2 expansion))
             (sync-doc (nth 3 sync-def)))
        (should (equal sync-doc "Preserved doc.")))))

  (nskk-context ":interactive keyword support"
    (nskk-it "produces a sync wrapper with (interactive) when :interactive t is specified"
      (let* ((expansion (macroexpand-1
                         '(defun/k nskk--cps-test-k-interactive ()
                            "Interactive doc."
                            :interactive t
                            (succeed :done))))
             (sync-def (nth 2 expansion)))
        (should (eq (car sync-def) 'defun))
        (let ((sync-body (nthcdr 3 sync-def)))
          (should (member '(interactive) sync-body)))))

    (nskk-it "does not include (interactive) in the /k function when :interactive t is specified"
      (let* ((expansion (macroexpand-1
                         '(defun/k nskk--cps-test-k-interactive-k ()
                            "Doc."
                            :interactive t
                            (succeed :done))))
             (k-def  (nth 1 expansion))
             (k-body (nthcdr 4 k-def)))
        (should-not (member '(interactive) k-body))))

    (nskk-it "excludes the :interactive keyword and its value from the /k body"
      (let* ((expansion (macroexpand-1
                         '(defun/k nskk--cps-test-k-interactive-body ()
                            "Doc."
                            :interactive t
                            (succeed :done))))
             (k-def  (nth 1 expansion))
             (k-body (nthcdr 4 k-def)))
        (should-not (member :interactive k-body))
        (let ((last-form (car (last k-body))))
          (should (nskk-cps-test--funcall-sym-named-p last-form "on-found")))))

    (nskk-it "does not include (interactive) in the sync wrapper when :interactive is absent"
      (let* ((expansion (macroexpand-1
                         '(defun/k nskk--cps-test-k-no-interactive ()
                            "Doc."
                            (succeed :done))))
             (sync-def  (nth 2 expansion))
             (sync-body (nthcdr 3 sync-def)))
        (should-not (member '(interactive) sync-body)))))

  (nskk-context "&rest and &optional argument handling"
    (nskk-it "sync wrapper calls NAME/k via apply when the arglist contains &rest"
      (let* ((expansion (macroexpand-1
                         '(defun/k nskk--cps-test-k-rest (a &rest rest)
                            "Doc."
                            (succeed a))))
             (sync-def  (nth 2 expansion))
             ;; Sync body is at index 4 (defun name args docstring BODY)
             (sync-body (nth 4 sync-def)))
        (should (eq (car sync-body) 'apply))))

    (nskk-it "sync wrapper call does not include the &optional keyword when &optional is used"
      (let* ((expansion (macroexpand-1
                         '(defun/k nskk--cps-test-k-optional (a &optional b)
                            "Doc."
                            (if a (succeed a) (succeed b)))))
             (sync-def  (nth 2 expansion))
             (sync-body (nth 4 sync-def)))
        ;; The call form must NOT contain the &optional keyword
        (should-not (memq '&optional sync-body)))))

  (nskk-context "symbol property annotation"
    (nskk-it "expansion includes a put form annotating NAME/k with :found-not-found"
      (let* ((expansion (macroexpand-1
                         '(defun/k nskk--cps-test-prop-k (x)
                            "Doc."
                            (succeed x))))
             ;; The progn has: (progn /k-defun sync-defun put-form)
             (put-form (nth 3 expansion)))
        (should (consp put-form))
        (should (eq (car put-form) 'put))
        ;; (nth 2 put-form) is the quoted form (quote nskk--cps-continuation-pattern);
        ;; use cadr to extract the symbol and compare with eq.
        (should (eq (cadr (nth 2 put-form)) 'nskk--cps-continuation-pattern))
        (should (eq (nth 3 put-form) ':found-not-found))))

    (nskk-it "the /k symbol has the :found-not-found property after evaluation at runtime"
      (defun/k nskk-cps-test--prop-runtime (x)
        "Test-only runtime property check."
        (succeed x))
      (should (eq (get 'nskk-cps-test--prop-runtime/k
                       'nskk--cps-continuation-pattern)
                  :found-not-found)))))


;;;
;;; succeed anaphoric macro
;;;

(nskk-describe "succeed anaphoric macro"

  (nskk-context "simple body transformation"
    (nskk-it "transforms (succeed v) in a simple body to (funcall <on-found-sym> v)"
      (let* ((expansion (macroexpand-1
                         '(defun/k nskk--cps-test-succeed-simple ()
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

    (nskk-it "transforms (succeed v) in the then-branch of an if"
      (let* ((expansion (macroexpand-1
                         '(defun/k nskk--cps-test-succeed-if-then (x)
                            "Doc."
                            (if x (succeed x) (fail)))))
             (k-def    (nth 1 expansion))
             (if-form  (car (nthcdr 4 k-def))))
        (should (eq (car if-form) 'if))
        ;; then branch: (funcall <on-found-sym> x)
        (let ((then (nth 2 if-form)))
          (should (nskk-cps-test--funcall-sym-named-p then "on-found"))
          (should (equal (nth 2 then) 'x)))))

    (nskk-it "transforms (succeed x) inside a let body"
      (let* ((expansion (macroexpand-1
                         '(defun/k nskk--cps-test-succeed-let ()
                            "Doc."
                            (let ((v 1)) (succeed v)))))
             (k-def   (nth 1 expansion))
             (let-form (car (nthcdr 4 k-def))))
        (should (eq (car let-form) 'let))
        (let ((body-form (car (nthcdr 2 let-form))))
          (should (nskk-cps-test--funcall-sym-named-p body-form "on-found"))
          (should (equal (nth 2 body-form) 'v)))))

    (nskk-it "transforms (succeed v) in a cond clause body"
      (let* ((expansion (macroexpand-1
                         '(defun/k nskk--cps-test-succeed-cond ()
                            "Doc."
                            (cond (t (succeed 99))))))
             (k-def     (nth 1 expansion))
             (cond-form (car (nthcdr 4 k-def)))
             (clause    (cadr cond-form))
             (clause-body (cadr clause)))
        (should (eq (car cond-form) 'cond))
        (should (nskk-cps-test--funcall-sym-named-p clause-body "on-found"))
        (should (equal (nth 2 clause-body) 99)))))

  (nskk-context "arity error detection"
    (nskk-it "signals an error at expansion time when (succeed) has no arguments"
      (should-error
       (macroexpand-1 '(defun/k nskk--cps-test-succeed-no-args ()
                         "Doc."
                         (succeed)))
       :type 'error))

    (nskk-it "signals an error at expansion time when (succeed a b) has too many arguments"
      (should-error
       (macroexpand-1 '(defun/k nskk--cps-test-succeed-too-many ()
                         "Doc."
                         (succeed 1 2)))
       :type 'error))))


;;;
;;; fail anaphoric macro
;;;

(nskk-describe "fail anaphoric macro"

  (nskk-context "simple body transformation"
    (nskk-it "transforms (fail) in a simple body to (funcall <on-not-found-sym>)"
      (let* ((expansion (macroexpand-1
                         '(defun/k nskk--cps-test-fail-simple ()
                            "Doc."
                            (fail))))
             (k-def  (nth 1 expansion))
             (k-body (nthcdr 4 k-def))
             (form   (car k-body)))
        ;; The symbol is gensym'd, so check by name.
        (should (nskk-cps-test--funcall-sym-named-p form "on-not-found"))
        (should (= (length form) 2))          ; (funcall SYM) -- no args
        (should-not (eq (cadr form) 'on-not-found))))

    (nskk-it "transforms (fail) in the else-branch of an if"
      (let* ((expansion (macroexpand-1
                         '(defun/k nskk--cps-test-fail-else (x)
                            "Doc."
                            (if x (succeed x) (fail)))))
             (k-def   (nth 1 expansion))
             (if-form (car (nthcdr 4 k-def))))
        (should (eq (car if-form) 'if))
        ;; else branch: (funcall <on-not-found-sym>)
        (let ((else (nth 3 if-form)))
          (should (nskk-cps-test--funcall-sym-named-p else "on-not-found"))
          (should (= (length else) 2)))))

    (nskk-it "transforms (fail) in a when body"
      (let* ((expansion (macroexpand-1
                         '(defun/k nskk--cps-test-fail-when (x)
                            "Doc."
                            (when x (fail)))))
             (k-def      (nth 1 expansion))
             (when-form  (car (nthcdr 4 k-def))))
        (should (eq (car when-form) 'when))
        (let ((body-form (caddr when-form)))
          (should (nskk-cps-test--funcall-sym-named-p body-form "on-not-found"))
          (should (= (length body-form) 2))))))

  (nskk-context "arity error detection"
    (nskk-it "signals an error at expansion time when (fail arg) is given an argument"
      (should-error
       (macroexpand-1 '(defun/k nskk--cps-test-fail-bad-arity ()
                         "Doc."
                         (fail "bad")))
       :type 'error))))


;;;
;;; <- macro
;;;

(nskk-describe "<- macro"

  (nskk-context "basic expansion"
    (nskk-it "expands (<- result fn arg)(succeed result) to fn/k call with continuation"
      (let* ((expansion (macroexpand-1
                         '(defun/k nskk--cps-test-<--basic (arg)
                            "Doc."
                            (<- result nskk--cps-test-helper arg)
                            (succeed result))))
             (k-def  (nth 1 expansion))
             (body   (car (nthcdr 4 k-def))))
        ;; The outer call should be nskk--cps-test-helper/k
        (should (eq (car body) 'nskk--cps-test-helper/k))
        ;; arg should appear in the call
        (should (member 'arg (cdr body)))
        ;; The continuation lambda should follow
        (should (cl-some (lambda (f) (and (consp f) (eq (car f) 'lambda)))
                         (cdr body)))))

    (nskk-it "automatically appends /k to the function name"
      (let* ((expansion (macroexpand-1
                         '(defun/k nskk--cps-test-<--name ()
                            "Doc."
                            (<- r my-lookup-fn)
                            (succeed r))))
             (k-def (nth 1 expansion))
             (body  (car (nthcdr 4 k-def))))
        (should (eq (car body) 'my-lookup-fn/k))))

    (nskk-it "continuation body calls on-found with var when no following forms exist"
      ;; C3 fix: (<- var fn) with no rest forms generates (funcall on-found var),
      ;; NOT (funcall on-not-found).
      (let* ((expansion (macroexpand-1
                         '(defun/k nskk--cps-test-<--no-rest ()
                            "Doc."
                            (<- r nskk--cps-test-helper))))
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

    (nskk-it "passes the outer on-not-found symbol as the not-found continuation"
      (let* ((expansion (macroexpand-1
                         '(defun/k nskk--cps-test-<--passthrough (arg)
                            "Doc."
                            (<- result nskk--cps-test-helper arg)
                            (succeed result))))
             (k-def (nth 1 expansion))
             (call  (car (nthcdr 4 k-def)))
             ;; The last argument to fn/k must be the on-not-found gensym.
             (last-arg (car (last call))))
        ;; The symbol is gensym'd, so check by name, not identity.
        (should (symbolp last-arg))
        (should (equal (symbol-name last-arg) "on-not-found"))
        ;; Confirm it is uninterned.
        (should-not (eq last-arg 'on-not-found)))))

  (nskk-context "hygienic lambda parameter"
    (nskk-it "uses a gensym lambda parameter that is not the same symbol as VAR"
      (let* ((expansion (macroexpand-1
                         '(defun/k nskk--cps-test-<--gensym ()
                            "Doc."
                            (<- myvar nskk--cps-test-helper)
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
        (should-not (eq (intern (symbol-name lambda-param)) lambda-param)))))

  (nskk-context "continuation chaining"
    (nskk-it "two sequential <- forms produce correctly nested CPS continuations"
      ;; (<- a fn1 x) (<- b fn2 a) (succeed (+ a b)) should expand to
      ;; nested calls: fn1/k calls fn2/k in its continuation body.
      (let* ((expansion (macroexpand-1
                         '(defun/k nskk--cps-test-<--chain (x)
                            "Doc."
                            (<- a nskk--cps-test-helper x)
                            (<- b nskk--cps-test-helper a)
                            (succeed (+ a b)))))
             (k-def (nth 1 expansion))
             (outer-call (car (nthcdr 4 k-def))))
        ;; Outer call is fn1/k (nskk--cps-test-helper/k)
        (should (eq (car outer-call) 'nskk--cps-test-helper/k))
        ;; The found-lambda body should itself contain another fn/k call
        (let* ((found-lambda (cl-find-if (lambda (f) (and (consp f) (eq (car f) 'lambda)))
                                         (cdr outer-call)))
               ;; Lambda: (lambda (--a--) (let ((a --a--)) INNER-CALL))
               (let-form     (caddr found-lambda))
               (inner-call   (caddr let-form)))
          (should (eq (car inner-call) 'nskk--cps-test-helper/k))))))

  (nskk-context "error conditions"
    (nskk-it "signals an error at macroexpand time if fn-name ends in /k"
      (should-error
       (macroexpand '(defun/k nskk--cps-test-<--bad-name ()
                       "Doc."
                       (<- r already/k)
                       (succeed r)))
       :type 'error))

    (nskk-it "signals an error at macroexpand time when fn was generated by defun/done"
      ;; First define a defun/done function so its /k gets the :done property.
      (defun/done nskk--cps-test-done-fn-guard ()
        "Test-only side-effect function for guard test."
        nil)
      ;; Now attempt to use it with <-; it should error.
      (should-error
       (macroexpand '(defun/k nskk--cps-test-guard-trigger ()
                       "Doc."
                       (<- r nskk--cps-test-done-fn-guard)
                       (succeed r)))
       :type 'error))))


;;;
;;; <-or macro
;;;

(nskk-describe "<-or macro"

  (nskk-context "basic expansion"
    (nskk-it "expands (<-or r fn :found (succeed r) :fail (fail)) to a two-lambda fn/k call"
      (let* ((expansion (macroexpand-1
                         '(defun/k nskk--cps-test-<-or-basic ()
                            "Doc."
                            (<-or result nskk--cps-test-helper
                                  :found (succeed result)
                                  :fail  (fail)))))
             (k-def (nth 1 expansion))
             (call  (car (nthcdr 4 k-def))))
        ;; Head is fn/k
        (should (eq (car call) 'nskk--cps-test-helper/k))
        ;; There must be exactly two lambdas in the call
        (let ((lambdas (cl-remove-if-not (lambda (f) (and (consp f) (eq (car f) 'lambda)))
                                         (cdr call))))
          (should (= (length lambdas) 2)))))

    (nskk-it "transforms (succeed r) in the :found branch to (funcall <on-found-sym> r)"
      (let* ((expansion (macroexpand-1
                         '(defun/k nskk--cps-test-<-or-found ()
                            "Doc."
                            (<-or r nskk--cps-test-helper
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

    (nskk-it "transforms (fail) in the :fail branch to (funcall <on-not-found-sym>)"
      (let* ((expansion (macroexpand-1
                         '(defun/k nskk--cps-test-<-or-fail ()
                            "Doc."
                            (<-or r nskk--cps-test-helper
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

    (nskk-it "passes positional args to fn/k before the found and fail lambdas"
      (let* ((expansion (macroexpand-1
                         '(defun/k nskk--cps-test-<-or-with-args (x y)
                            "Doc."
                            (<-or r nskk--cps-test-helper x y
                                  :found (succeed r)
                                  :fail  (fail)))))
             (k-def (nth 1 expansion))
             (call  (car (nthcdr 4 k-def))))
        ;; Head is fn/k
        (should (eq (car call) 'nskk--cps-test-helper/k))
        ;; x and y must appear in the call before the lambdas
        (should (memq 'x (cdr call)))
        (should (memq 'y (cdr call))))))

  (nskk-context "error conditions"
    (nskk-it "signals an error at macroexpand time if fn-name ends in /k"
      (should-error
       (macroexpand '(defun/k nskk--cps-test-<-or-bad ()
                       "Doc."
                       (<-or r bad-fn/k :found (succeed r) :fail (fail))))
       :type 'error))

    (nskk-it "signals an error when the :found keyword is missing"
      (should-error
       (macroexpand '(defun/k nskk--cps-test-<-or-no-found ()
                       "Doc."
                       (<-or r nskk--cps-test-helper :fail (fail))))
       :type 'error))

    (nskk-it "signals an error when the :fail keyword is missing"
      (should-error
       (macroexpand '(defun/k nskk--cps-test-<-or-no-fail ()
                       "Doc."
                       (<-or r nskk--cps-test-helper :found (succeed r))))
       :type 'error))))


;;;
;;; CPS walker
;;;

(nskk-describe "CPS walker"

  (nskk-context "if form"
    (nskk-it "transforms both branches of a two-branch if"
      (let* ((expansion (macroexpand-1
                         '(defun/k nskk--cps-test-walker-if2 (x)
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

    (nskk-it "transforms the then-branch of a one-branch if leaving no else clause"
      (let* ((expansion (macroexpand-1
                         '(defun/k nskk--cps-test-walker-if1 (x)
                            "Doc."
                            (if x (succeed x)))))
             (k-def   (nth 1 expansion))
             (if-form (car (nthcdr 4 k-def))))
        (should (eq (car if-form) 'if))
        (let ((then (nth 2 if-form)))
          (should (nskk-cps-test--funcall-sym-named-p then "on-found"))
          (should (equal (nth 2 then) 'x)))
        ;; No else clause: only 3 elements in the if form
        (should (null (nth 3 if-form))))))

  (nskk-context "cond form"
    (nskk-it "transforms each cond clause body independently"
      (let* ((expansion (macroexpand-1
                         '(defun/k nskk--cps-test-walker-cond (x)
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
          (should (= (length body2) 2))))))

  (nskk-context "when and unless forms"
    (nskk-it "transforms the last body form of a when"
      (let* ((expansion (macroexpand-1
                         '(defun/k nskk--cps-test-walker-when (x)
                            "Doc."
                            (when x (succeed x)))))
             (k-def     (nth 1 expansion))
             (when-form (car (nthcdr 4 k-def))))
        (should (eq (car when-form) 'when))
        (let ((body-form (caddr when-form)))
          (should (nskk-cps-test--funcall-sym-named-p body-form "on-found"))
          (should (equal (nth 2 body-form) 'x)))))

    (nskk-it "transforms the last body form of an unless"
      (let* ((expansion (macroexpand-1
                         '(defun/k nskk--cps-test-walker-unless (x)
                            "Doc."
                            (unless x (fail)))))
             (k-def       (nth 1 expansion))
             (unless-form (car (nthcdr 4 k-def))))
        (should (eq (car unless-form) 'unless))
        (let ((body-form (caddr unless-form)))
          (should (nskk-cps-test--funcall-sym-named-p body-form "on-not-found"))
          (should (= (length body-form) 2))))))

  (nskk-context "progn form"
    (nskk-it "transforms the last form of a progn while preceding forms pass through unchanged"
      (let* ((expansion (macroexpand-1
                         '(defun/k nskk--cps-test-walker-progn ()
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
          (should (equal (nth 2 last-form) 'x))))))

  (nskk-context "let and let* forms"
    (nskk-it "transforms the body of a let passing bindings through unchanged"
      (let* ((expansion (macroexpand-1
                         '(defun/k nskk--cps-test-walker-let ()
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

    (nskk-it "transforms the body of a let-star form passing bindings through unchanged"
      (let* ((expansion (macroexpand-1
                         '(defun/k nskk--cps-test-walker-let* ()
                            "Doc."
                            (let* ((x 1) (y x)) (succeed y)))))
             (k-def    (nth 1 expansion))
             (let*-form (car (nthcdr 4 k-def))))
        (should (eq (car let*-form) 'let*))
        (should (equal (cadr let*-form) '((x 1) (y x))))
        (let ((body-form (caddr let*-form)))
          (should (nskk-cps-test--funcall-sym-named-p body-form "on-found"))
          (should (equal (nth 2 body-form) 'y)))))

    (nskk-it "transforms pcase-let* body in tail position"
      (defun/k nskk-cps-test--pcase-let*-fn (x)
        "Test pcase-let* CPS transform."
        (pcase-let* ((`(,a ,b) x))
          (succeed (+ a b))))
      (should (= (nskk-cps-test--pcase-let*-fn '(3 4)) 7))
      (let ((found nil) (not-found-called nil))
        (nskk-cps-test--pcase-let*-fn/k '(3 4)
          (lambda (v) (setq found v))
          (lambda () (setq not-found-called t)))
        (should (= found 7))
        (should (null not-found-called)))))

  (nskk-context "pcase form"
    (nskk-it "transforms each pcase clause body while leaving patterns and expr unchanged"
      (let* ((expansion (macroexpand-1
                         '(defun/k nskk--cps-test-walker-pcase (v)
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
          (should (= (length body2) 2))))))

  (nskk-context "and and or forms"
    (nskk-it "transforms only the last form of an and while guard forms pass through"
      (let* ((expansion (macroexpand-1
                         '(defun/k nskk--cps-test-walker-and (x)
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

    (nskk-it "transforms only the last form of an or while fallback forms pass through"
      (let* ((expansion (macroexpand-1
                         '(defun/k nskk--cps-test-walker-or (x fallback)
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

    (nskk-it "(and) with no sub-forms passes through unchanged"
      (let* ((expansion (macroexpand-1
                         '(defun/k nskk--cps-test-and-empty ()
                            "Doc."
                            (and))))
             (k-def (nth 1 expansion))
             (body  (car (nthcdr 4 k-def))))
        (should (equal body '(and)))))

    (nskk-it "(or) with no sub-forms passes through unchanged"
      (let* ((expansion (macroexpand-1
                         '(defun/k nskk--cps-test-or-empty ()
                            "Doc."
                            (or))))
             (k-def (nth 1 expansion))
             (body  (car (nthcdr 4 k-def))))
        (should (equal body '(or))))))

  (nskk-context "multi-form body"
    (nskk-it "leaves non-tail forms unchanged and only transforms the last form"
      (let* ((expansion (macroexpand-1
                         '(defun/k nskk--cps-test-walker-multi ()
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
          (should (equal (nth 2 last-form) 'side))))))

  (nskk-context "cond test-only clause edge cases"
    (nskk-it "a cond clause with no body passes the test expression through unchanged"
      ;; With the C4 fix, (cond (some-expr)) -- test-only -- does NOT transform
      ;; the test expression via the CPS walker.  It passes through as-is.
      (let* ((expansion (macroexpand-1
                         '(defun/k nskk--cps-test-cond-test-only ()
                            "Doc."
                            (cond (some-expr)))))
             (k-def     (nth 1 expansion))
             (cond-form (car (nthcdr 4 k-def))))
        ;; The whole cond form should be (cond (some-expr)) -- test NOT transformed.
        (should (equal cond-form '(cond (some-expr))))))

    (nskk-it "a test-only cond clause does not wrap the test in a funcall form"
      ;; Additional check: the test atom is not wrapped in funcall.
      (let* ((expansion (macroexpand-1
                         '(defun/k nskk--cps-test-cond-to-no-funcall ()
                            "Doc."
                            (cond (my-flag)))))
             (k-def       (nth 1 expansion))
             (cond-form   (car (nthcdr 4 k-def)))
             (clause      (cadr cond-form))
             (clause-test (car clause)))
        ;; Test must be the bare symbol, not (funcall ...)
        (should (eq clause-test 'my-flag))
        (should-not (and (consp clause-test) (eq (car clause-test) 'funcall)))))))


;;;
;;; defun/done macro
;;;

(nskk-describe "defun/done macro"

  (nskk-context "macro expansion structure"
    (nskk-it "expands to a progn containing two defuns and a put annotation"
      (let ((expansion (macroexpand-1
                        '(defun/done nskk--cps-test-done-expand (x)
                           "Doc."
                           (setq nskk--cps-done-result x)))))
        (should (consp expansion))
        (should (eq (car expansion) 'progn))
        ;; (progn /k-defun sync-defun put-annotation)
        (should (= (length expansion) 4))))

    (nskk-it "generates NAME/k with an on-done arg and appends (funcall on-done) at the end"
      (let* ((expansion (macroexpand-1
                         '(defun/done nskk--cps-test-done-k (x)
                            "Doc."
                            (setq nskk--cps-done-result x))))
             (k-def  (nth 1 expansion))
             (k-args (nth 2 k-def))
             (k-body (nthcdr 4 k-def)))
        (should (eq (car k-def) 'defun))
        (should (eq (cadr k-def) 'nskk--cps-test-done-k/k))
        ;; on-done may be gensym'd -- check by name.
        (should (nskk-cps-test--args-contain-sym-named-p k-args "on-done"))
        ;; Last form in body must be (funcall <on-done-sym>)
        (let ((last-form (car (last k-body))))
          (should (nskk-cps-test--funcall-sym-named-p last-form "on-done"))
          (should (= (length last-form) 2)))))

    (nskk-it "sync wrapper calls NAME/k with #'ignore as the on-done continuation"
      (let* ((expansion (macroexpand-1
                         '(defun/done nskk--cps-test-done-sync (x)
                            "Doc."
                            (setq nskk--cps-done-result x))))
             (sync-def  (nth 2 expansion))
             ;; Sync def layout: (defun NAME ARGS DOCSTRING BODY...)
             ;; body call is at index 4
             (sync-body (nth 4 sync-def)))
        (should (eq (car sync-def) 'defun))
        (should (eq (cadr sync-def) 'nskk--cps-test-done-sync))
        (should (eq (car sync-body) 'nskk--cps-test-done-sync/k))
        (should (member '#'ignore sync-body))))

    (nskk-it "sync wrapper calls NAME/k via apply when the arglist contains &rest"
      (let* ((expansion (macroexpand-1
                         '(defun/done nskk--cps-test-done-rest (a &rest rest)
                            "Doc."
                            (setq nskk--cps-test-action-result a))))
             (sync-def  (nth 2 expansion))
             (sync-body (nth 4 sync-def)))
        (should (eq (car sync-body) 'apply)))))

  (nskk-context "docstring handling"
    (nskk-it "appends [CPS] suffix to the docstring of the /k function"
      (let* ((expansion (macroexpand-1
                         '(defun/done nskk--cps-test-done-doc ()
                            "Side-effect doc."
                            nil)))
             (k-def (nth 1 expansion))
             (k-doc (nth 3 k-def)))
        (should (string-suffix-p "\n[CPS]" k-doc))
        (should (string-prefix-p "Side-effect doc." k-doc)))))

  (nskk-context "no AST transformation"
    (nskk-it "does not CPS-transform (succeed)/(fail) forms in the body"
      (let* ((expansion (macroexpand-1
                         '(defun/done nskk--cps-test-done-no-ast ()
                            "Doc."
                            (succeed 42))))
             (k-def  (nth 1 expansion))
             (k-body (nthcdr 4 k-def)))
        ;; First body form should remain (succeed 42) -- NOT (funcall on-found 42)
        (should (equal (car k-body) '(succeed 42))))))

  (nskk-context ":interactive keyword support"
    (nskk-it "produces a sync wrapper with (interactive) when :interactive t is specified"
      (let* ((expansion (macroexpand-1
                         '(defun/done nskk--cps-test-done-interactive ()
                            "Interactive doc."
                            :interactive t
                            (message "hello"))))
             (sync-def (nth 2 expansion)))
        (should (eq (car sync-def) 'defun))
        ;; The body of the sync function must include (interactive)
        (let ((sync-body (nthcdr 3 sync-def)))
          (should (member '(interactive) sync-body)))))

    (nskk-it "does not include (interactive) in the /k function when :interactive t is specified"
      (let* ((expansion (macroexpand-1
                         '(defun/done nskk--cps-test-done-interactive-k ()
                            "Doc."
                            :interactive t
                            (message "hello"))))
             (k-def  (nth 1 expansion))
             (k-body (nthcdr 4 k-def)))
        (should-not (member '(interactive) k-body))))

    (nskk-it "excludes the :interactive keyword and its value from the real body"
      (let* ((expansion (macroexpand-1
                         '(defun/done nskk--cps-test-done-interactive-body ()
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

    (nskk-it "does not include (interactive) in the sync wrapper when :interactive is absent"
      (let* ((expansion (macroexpand-1
                         '(defun/done nskk--cps-test-done-no-interactive ()
                            "Doc."
                            (message "hello"))))
             (sync-def  (nth 2 expansion))
             (sync-body (nthcdr 3 sync-def)))
        (should-not (member '(interactive) sync-body)))))

  (nskk-context "symbol property annotation"
    (nskk-it "expansion includes a put form annotating NAME/k with :done"
      (let* ((expansion (macroexpand-1
                         '(defun/done nskk--cps-test-prop-done (x)
                            "Doc."
                            (setq nskk--cps-test-action-result x))))
             (put-form (nth 3 expansion)))
        (should (consp put-form))
        (should (eq (car put-form) 'put))
        ;; (nth 2 put-form) is the quoted form (quote nskk--cps-continuation-pattern);
        ;; use cadr to extract the symbol and compare with eq.
        (should (eq (cadr (nth 2 put-form)) 'nskk--cps-continuation-pattern))
        (should (eq (nth 3 put-form) ':done))))

    (nskk-it "the /k symbol has the :done property after evaluation at runtime"
      (defun/done nskk-cps-test--prop-done-runtime (x)
        "Test-only runtime property check."
        (setq nskk--cps-test-action-result x))
      (should (eq (get 'nskk-cps-test--prop-done-runtime/k
                       'nskk--cps-continuation-pattern)
                  :done)))))


;;;
;;; defun/3k macro
;;;

(nskk-describe "defun/3k macro"

  (nskk-context "macro expansion structure"
    (nskk-it "expands to (progn defun put) with exactly 3 elements and no sync wrapper"
      (let ((expansion (macroexpand-1
                        '(defun/3k nskk--cps-3k-test-expand ()
                             (on-a on-b on-c)
                           "Test doc."
                           (funcall on-a 1)))))
        (should (consp expansion))
        (should (eq (car expansion) 'progn))
        ;; (progn /k-defun put-annotation) -- no sync wrapper
        (should (= (length expansion) 3))))

    (nskk-it "generates NAME/k only without generating a sync NAME function"
      (let* ((expansion (macroexpand-1
                         '(defun/3k nskk--cps-3k-test-k-only ()
                               (on-a on-b on-c)
                             "Test doc."
                             (funcall on-a 42))))
             (k-def  (nth 1 expansion))
             (put-form (nth 2 expansion)))
        ;; First form is the /k defun
        (should (eq (car k-def) 'defun))
        (should (equal (symbol-name (cadr k-def)) "nskk--cps-3k-test-k-only/k"))
        ;; Second form is the put annotation
        (should (eq (car put-form) 'put))))

    (nskk-it "appends the three named continuation symbols to the arglist"
      (let* ((expansion (macroexpand-1
                         '(defun/3k nskk--cps-3k-test-args (x y)
                               (on-match on-partial on-fail)
                             "Test doc."
                             (funcall on-match x))))
             (k-def  (nth 1 expansion))
             (arglist (caddr k-def)))
        ;; Plain args + 3 continuations
        (should (equal arglist '(x y on-match on-partial on-fail))))))

  (nskk-context "docstring handling"
    (nskk-it "appends [CPS] suffix to the docstring of the generated /k function"
      (let* ((expansion (macroexpand-1
                         '(defun/3k nskk--cps-3k-test-doc ()
                               (on-a on-b on-c)
                             "My doc."
                             (funcall on-a nil))))
             (k-def   (nth 1 expansion))
             (docstr  (nth 3 k-def)))
        (should (stringp docstr))
        (should (string-suffix-p "\n[CPS]" docstr)))))

  (nskk-context "no AST transformation"
    (nskk-it "does not CPS-transform the body and passes funcall forms through verbatim"
      (let* ((expansion (macroexpand-1
                         '(defun/3k nskk--cps-3k-test-no-transform (x)
                               (on-a on-b on-c)
                             "Test doc."
                             (funcall on-a x)
                             (funcall on-b)
                             (funcall on-c))))
             (k-def (nth 1 expansion))
             (body  (nthcdr 4 k-def)))
        ;; Body is verbatim -- not rewritten via succeed/fail
        (should (equal (nth 0 body) '(funcall on-a x)))
        (should (equal (nth 1 body) '(funcall on-b)))
        (should (equal (nth 2 body) '(funcall on-c))))))

  (nskk-context "symbol property annotation"
    (nskk-it "annotates NAME/k with nskk--cps-continuation-pattern = :3k in the expansion"
      (let* ((expansion (macroexpand-1
                         '(defun/3k nskk--cps-3k-test-put ()
                               (on-a on-b on-c)
                             "Test doc."
                             (funcall on-a nil))))
             (put-form (nth 2 expansion)))
        ;; (put 'NAME/k 'nskk--cps-continuation-pattern :3k)
        (should (eq (car put-form) 'put))
        (should (equal (cadr put-form) ''nskk--cps-3k-test-put/k))
        (should (equal (cadddr put-form) :3k))))

    (nskk-it "the /k symbol has the :3k property after evaluation at runtime"
      (defun/3k nskk--cps-3k-put-prop-test ()
          (on-a on-b on-c)
        "Test."
        (funcall on-a nil))
      (should (eq (get 'nskk--cps-3k-put-prop-test/k
                       'nskk--cps-continuation-pattern)
                  :3k))))

  (nskk-context "error conditions"
    (nskk-it "signals an error when CONT-NAMES does not have exactly 3 symbols"
      (should-error
       (macroexpand-1
        '(defun/3k nskk--cps-3k-test-err-count ()
             (on-a on-b)          ; only 2 -- must be 3
           "Test doc."
           (funcall on-a nil)))))

    (nskk-it "signals an error when CONT-NAMES contains a non-symbol"
      (should-error
       (macroexpand-1
        '(defun/3k nskk--cps-3k-test-err-sym ()
             (on-a "not-a-symbol" on-c)
           "Test doc."
           (funcall on-a nil))))))

  (nskk-context "runtime behavior"
    (nskk-it "all three continuation paths are reachable at runtime"
      (defun/3k nskk--cps-3k-runtime-test (x)
          (on-positive on-zero on-negative)
        "Return x dispatched to one of three continuations."
        (cond ((> x 0) (funcall on-positive x))
              ((= x 0) (funcall on-zero))
              (t       (funcall on-negative (- x)))))
      ;; on-positive path
      (should (equal (nskk--cps-3k-runtime-test/k 5
                                                   (lambda (v) (list 'pos v))
                                                   (lambda ()  'zero)
                                                   (lambda (v) (list 'neg v)))
                     '(pos 5)))
      ;; on-zero path
      (should (equal (nskk--cps-3k-runtime-test/k 0
                                                   (lambda (v) (list 'pos v))
                                                   (lambda ()  'zero)
                                                   (lambda (v) (list 'neg v)))
                     'zero))
      ;; on-negative path
      (should (equal (nskk--cps-3k-runtime-test/k -3
                                                   (lambda (v) (list 'pos v))
                                                   (lambda ()  'zero)
                                                   (lambda (v) (list 'neg v)))
                     '(neg 3))))))


;;;
;;; nskk--cps-args-info helper
;;;

(nskk-describe "nskk--cps-args-info"

  (nskk-context "lambda-list keyword stripping"
    (nskk-it "correctly strips &optional leaving plain arg names and no rest-sym"
      (let ((result (nskk--cps-args-info '(a &optional b c))))
        (should (equal (car result) '(a b c)))
        (should (null (cdr result)))))

    (nskk-it "correctly strips &rest and captures the rest symbol"
      (let ((result (nskk--cps-args-info '(a &rest rest))))
        (should (equal (car result) '(a)))
        (should (eq (cdr result) 'rest))))

    (nskk-it "correctly strips combined &optional and &rest keywords"
      (let ((result (nskk--cps-args-info '(a &optional b &rest rest))))
        (should (equal (car result) '(a b)))
        (should (eq (cdr result) 'rest))))))


;;;
;;; Functional behavior tests
;;;

;; Define test functions at top level so they are available in all tests.

(defun/k nskk--cps-test-lookup (key)
  "Look up KEY in a small fixed alist. [test-only]"
  (let ((result (assoc key '(("a" . 1) ("b" . 2) ("c" . 3)))))
    (if result (succeed (cdr result)) (fail))))

(defun/k nskk--cps-test-always-succeed ()
  "Always succeeds with the value 42. [test-only]"
  (succeed 42))

(defun/k nskk--cps-test-always-fail ()
  "Always fails. [test-only]"
  (fail))

(defun/k nskk--cps-test-chained (key)
  "Chain two CPS lookups using <-. [test-only]"
  (<- value nskk--cps-test-lookup key)
  (succeed (1+ value)))

(defvar nskk--cps-test-action-result nil
  "Holds the side-effect result for defun/done functional tests.")

(defun/done nskk--cps-test-action (x)
  "Side-effecting action that stores X. [test-only]"
  (setq nskk--cps-test-action-result x))

(nskk-describe "functional behavior of defun/k generated functions"

  (nskk-context "/k CPS version"
    (nskk-it "invokes on-found with the looked-up value when the key is present"
      (let (found-val)
        (nskk--cps-test-lookup/k "a"
                                  (lambda (v) (setq found-val v))
                                  (lambda () (should nil)))
        (should (equal found-val 1))))

    (nskk-it "invokes on-not-found when the key is absent"
      (let (not-found-called)
        (nskk--cps-test-lookup/k "z"
                                  (lambda (_v) (should nil))
                                  (lambda () (setq not-found-called t)))
        (should not-found-called)))

    (nskk-it "returns correct values for all known keys via CPS"
      (dolist (pair '(("a" . 1) ("b" . 2) ("c" . 3)))
        (let (found-val)
          (nskk--cps-test-lookup/k (car pair)
                                    (lambda (v) (setq found-val v))
                                    (lambda () (should nil)))
          (should (equal found-val (cdr pair)))))))

  (nskk-context "sync wrapper"
    (nskk-it "returns the value when the key is found"
      (should (equal (nskk--cps-test-lookup "a") 1))
      (should (equal (nskk--cps-test-lookup "b") 2))
      (should (equal (nskk--cps-test-lookup "c") 3)))

    (nskk-it "returns nil when the key is not found"
      (should (null (nskk--cps-test-lookup "z")))
      (should (null (nskk--cps-test-lookup ""))))

    (nskk-it "always-succeed sync wrapper returns 42"
      (should (equal (nskk--cps-test-always-succeed) 42)))

    (nskk-it "always-fail sync wrapper returns nil"
      (should (null (nskk--cps-test-always-fail)))))

  (nskk-context "CPS and sync consistency"
    (nskk-it "the /k version and sync wrapper produce equivalent results for all inputs"
      (dolist (key '("a" "b" "c" "z" ""))
        (let ((sync-result (nskk--cps-test-lookup key))
              (cps-result  (nskk--cps-test-lookup/k key #'identity (lambda () nil))))
          (should (equal sync-result cps-result))))))

  (nskk-context "<- chaining"
    (nskk-it "chained CPS functions via <- compose correctly when the inner function succeeds"
      (should (equal (nskk--cps-test-chained "a") 2))  ; 1 + 1
      (should (equal (nskk--cps-test-chained "b") 3))  ; 2 + 1
      (should (equal (nskk--cps-test-chained "c") 4))) ; 3 + 1

    (nskk-it "chained CPS function propagates not-found when the inner function fails"
      (should (null (nskk--cps-test-chained "z")))))

  (nskk-context "defun/done functional behavior"
    (nskk-it "sync wrapper runs the body as a side effect"
      (let ((nskk--cps-test-action-result nil))
        (nskk--cps-test-action :sentinel)
        (should (eq nskk--cps-test-action-result :sentinel))))

    (nskk-it "/k function runs the body then calls on-done"
      (let ((nskk--cps-test-action-result nil)
            (done-called nil))
        (nskk--cps-test-action/k :kvalue (lambda () (setq done-called t)))
        (should (eq nskk--cps-test-action-result :kvalue))
        (should done-called)))

    (nskk-it "/k function calls on-done strictly after the body executes"
      ;; Strategy: record whether body has already run when on-done fires.
      ;; nskk--cps-test-action sets nskk--cps-test-action-result, so if on-done
      ;; fires after the body, nskk--cps-test-action-result will already be :sentinel.
      (let ((nskk--cps-test-action-result nil)
            (result-at-on-done-time nil))
        (nskk--cps-test-action/k :sentinel
                                  (lambda ()
                                    ;; Capture the result value at the moment on-done fires
                                    (setq result-at-on-done-time nskk--cps-test-action-result)))
        ;; Body ran first (set result to :sentinel), then on-done captured it
        (should (eq result-at-on-done-time :sentinel))))))


;;;
;;; Pass-through tests
;;;

(nskk-describe "pass-through of non-CPS forms"

  (nskk-context "non-tail position forms"
    (nskk-it "a setq in non-tail position passes through unchanged"
      (let* ((expansion (macroexpand-1
                         '(defun/k nskk--cps-test-passthrough-setq ()
                            "Doc."
                            (setq x 1)
                            (succeed x))))
             (k-def (nth 1 expansion))
             (body  (nthcdr 4 k-def)))
        (should (equal (car body) '(setq x 1)))))

    (nskk-it "a regular function call in non-tail position passes through unchanged"
      (let* ((expansion (macroexpand-1
                         '(defun/k nskk--cps-test-passthrough-call ()
                            "Doc."
                            (message "side effect")
                            (succeed 1))))
             (k-def (nth 1 expansion))
             (body  (nthcdr 4 k-def)))
        (should (equal (car body) '(message "side effect"))))))

  (nskk-context "tail position atoms and unrecognized forms"
    (nskk-it "a symbol atom in tail position passes through unchanged"
      (let* ((expansion (macroexpand-1
                         '(defun/k nskk--cps-test-passthrough-atom ()
                            "Doc."
                            my-variable)))
             (k-def (nth 1 expansion))
             (body  (nthcdr 4 k-def)))
        (should (equal (car body) 'my-variable))))

    (nskk-it "a number literal in tail position passes through unchanged"
      (let* ((expansion (macroexpand-1
                         '(defun/k nskk--cps-test-passthrough-number ()
                            "Doc."
                            42)))
             (k-def (nth 1 expansion))
             (body  (nthcdr 4 k-def)))
        (should (equal (car body) 42))))

    (nskk-it "an unknown special form in tail position passes through unchanged"
      (let* ((expansion (macroexpand-1
                         '(defun/k nskk--cps-test-passthrough-unknown ()
                            "Doc."
                            (some-unknown-form arg1 arg2))))
             (k-def (nth 1 expansion))
             (body  (nthcdr 4 k-def)))
        (should (equal (car body) '(some-unknown-form arg1 arg2))))))

  (nskk-context "let binding expressions"
    (nskk-it "binding expressions inside let are not CPS-transformed"
      (let* ((expansion (macroexpand-1
                         '(defun/k nskk--cps-test-passthrough-let-bindings ()
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

    (nskk-it "the test expression of a cond clause is not CPS-transformed"
      (let* ((expansion (macroexpand-1
                         '(defun/k nskk--cps-test-passthrough-cond-test (x)
                            "Doc."
                            (cond ((> x 0) (succeed x))
                                  (t       (fail))))))
             (k-def     (nth 1 expansion))
             (cond-form (car (nthcdr 4 k-def)))
             (clause1   (nth 1 cond-form)))
        ;; The cond test (> x 0) must be unchanged
        (should (equal (car clause1) '(> x 0)))))))


;;;
;;; Module loading tests
;;;

(nskk-describe "module loading and symbol definitions"

  (nskk-context "feature registration"
    (nskk-it "nskk-cps-macros provides the nskk-cps-macros feature"
      (should (featurep 'nskk-cps-macros))))

  (nskk-context "macro definitions"
    (nskk-it "defun/k is defined as a macro"
      (should (macrop (symbol-function 'defun/k))))

    (nskk-it "defun/done is defined as a macro"
      (should (macrop (symbol-function 'defun/done)))))

  (nskk-context "internal function definitions"
    (nskk-it "internal walker function nskk--cps-transform-body-list is defined"
      (should (fboundp 'nskk--cps-transform-body-list)))

    (nskk-it "internal walker function nskk--cps-transform-form is defined"
      (should (fboundp 'nskk--cps-transform-form))))

  (nskk-context "dispatch table"
    (nskk-it "nskk--cps-form-dispatch is a list containing all 13 expected form heads"
      (should (listp nskk--cps-form-dispatch))
      (should (= (length nskk--cps-form-dispatch) 13))
      (dolist (expected-key '(if cond when unless progn let let* pcase pcase-let*
                                 call/cc escape and or))
        (let ((entry (assq expected-key nskk--cps-form-dispatch)))
          (should entry)
          (should (functionp (cdr entry))))))))


;;;
;;; call/cc special form
;;;

(nskk-describe "call/cc special form"

  (nskk-context "basic behavior"
    (nskk-it "binds K to on-found so (funcall k v) is equivalent to (succeed v)"
      (defun/k nskk-cps-test--callcc-basic (x)
        "Return double of X via call/cc."
        (call/cc (lambda (k)
                   (funcall k (* x 2)))))
      (should (equal (nskk-cps-test--callcc-basic 5) 10))
      (should (equal (nskk-cps-test--callcc-basic/k 5 #'identity #'ignore) 10)))

    (nskk-it "K captured in call/cc can be saved and called later as a multi-shot continuation"
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

    (nskk-it "succeed inside call/cc body still calls on-found via the CPS transform"
      (defun/k nskk-cps-test--callcc-with-succeed (flag)
        "Return :yes or :no depending on FLAG."
        (call/cc (lambda (_k)
                   (if flag (succeed :yes) (fail)))))
      (should (eq (nskk-cps-test--callcc-with-succeed t)   :yes))
      (should (eq (nskk-cps-test--callcc-with-succeed nil)  nil))))

  (nskk-context "macro expansion"
    (nskk-it "expands to a (let ((k on-found)) ...) form binding K to the found continuation"
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
            (should (eq (car binding) 'k)))))))

  (nskk-context "error conditions"
    (nskk-it "signals an error when its argument is not a single-parameter lambda"
      (should-error
       (macroexpand-1 '(defun/k nskk-cps-test--callcc-bad ()
                         "Doc."
                         (call/cc (lambda (a b) (succeed a)))))))))


;;;
;;; escape special form
;;;

(nskk-describe "escape special form"

  (nskk-context "basic behavior"
    (nskk-it "(funcall k v) aborts remaining body and calls on-found with v"
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

    (nskk-it "body forms after (funcall k v) are not evaluated"
      (let ((side-effect nil))
        (defun/k nskk-cps-test--escape-no-continue (x)
          "Escape on first call; subsequent body must not run."
          (escape k
            (funcall k x)
            (setq side-effect :ran)))  ; must NOT run
        (nskk-cps-test--escape-no-continue/k 42 #'identity #'ignore)
        (should (null side-effect))))

    (nskk-it "body completes normally when the escape continuation is never called"
      (defun/k nskk-cps-test--escape-no-escape (flag)
        "Succeed with :done when FLAG is non-nil, without using escape."
        (escape _k
          (if flag (succeed :done) (fail))))
      (should (eq (nskk-cps-test--escape-no-escape t)   :done))
      (should (eq (nskk-cps-test--escape-no-escape nil)  nil))))

  (nskk-context "macro expansion"
    (nskk-it "expands to a (catch ...) form wrapping the body"
      (let* ((expansion (macroexpand-1 '(defun/k nskk-cps-test--escape-expand (xs)
                                          "Doc."
                                          (escape k (dolist (x xs) (funcall k x)) (fail)))))
             (/k-def (cadr expansion))
             (body   (cddr (cddr /k-def))))
        (should (consp body))
        (let ((catch-form (car body)))
          (should (eq (car catch-form) 'catch))))))

  (nskk-context "error conditions"
    (nskk-it "signals an error when its first argument is not a symbol"
      (should-error
       (macroexpand-1 '(defun/k nskk-cps-test--escape-bad ()
                         "Doc."
                         (escape 42 (succeed :x))))))))


;;;
;;; Property-based tests
;;;

(nskk-describe "property-based contracts"

  (nskk-context "sync and CPS consistency"
    (nskk-it "sync wrapper and /k version produce identical results for all inputs"
      ;; Uses the nskk--cps-test-lookup fixture defined above.
      ;; For any input key, the sync result must equal the CPS result.
      (dolist (input '("a" "b" "c" "z" "" "missing"))
        (let ((sync-result (nskk--cps-test-lookup input))
              (cps-result  (nskk--cps-test-lookup/k input #'identity (lambda () nil))))
          (should (equal sync-result cps-result))))))

  (nskk-context "non-tail passthrough"
    (nskk-it "any form in non-tail position is never CPS-transformed regardless of its shape"
      ;; Property: regardless of what the non-tail form is, it passes through
      ;; unchanged in the /k expansion.
      (dolist (side-effect-form '((setq x 1)
                                   (message "test")
                                   (push 1 lst)))
        (let* ((expansion (macroexpand-1
                           `(defun/k nskk--cps-pbt-passthrough ()
                              "Doc."
                              ,side-effect-form
                              (succeed 42))))
               (k-def (nth 1 expansion))
               (body  (nthcdr 4 k-def)))
          (should (equal (car body) side-effect-form)))))))

(nskk-property-test-seeded cps-macros-pbt-sync-cps-consistency
  ((key ascii-string))
  (let ((sync-result (nskk--cps-test-lookup key))
        (cps-result  (nskk--cps-test-lookup/k key #'identity (lambda () nil))))
    (equal sync-result cps-result))
  50 42)

(nskk-deftest-table cps-macros-docstring-cps-suffix
  :columns (macro-form fn-name)
  :rows (((defun/k nskk--cps-table-doc-k (x) "Doc." (succeed x))
          nskk--cps-table-doc-k/k)
         ((defun/done nskk--cps-table-doc-done (x) "Doc." (setq nskk--cps-test-action-result x))
          nskk--cps-table-doc-done/k)
         ((defun/3k nskk--cps-table-doc-3k (x) (on-a on-b on-c) "Doc." (funcall on-a x))
          nskk--cps-table-doc-3k/k))
  :description "Each generated /k function has [CPS] appended to its docstring"
  :body (progn
          (eval macro-form)
          (should (string-suffix-p "\n[CPS]" (or (documentation fn-name) "")))))


;;; -------------------------------------------------------------------
;;; nskk-<- and nskk-<-or standalone pipeline-bind macros
;;; -------------------------------------------------------------------

(nskk-describe "nskk-<- standalone pipeline macro"
  (nskk-it "expands to CPS call with on-found lambda and #'ignore for on-not-found"
    (let ((expansion (macroexpand-1
                      '(nskk-<- (result) (my-func/k key)
                         (use result)))))
      ;; Head: the /k function name extracted from fn-call
      (should (eq (car expansion) 'my-func/k))
      ;; Args from fn-call (without continuations)
      (should (equal (nth 1 expansion) 'key))
      ;; on-found is a (lambda (result) (use result))
      (let ((on-found (nth 2 expansion)))
        (should (eq (car on-found) 'lambda))
        (should (equal (cadr on-found) '(result)))
        (should (equal (caddr on-found) '(use result))))
      ;; on-not-found is #'ignore
      (should (equal (nth 3 expansion) '#'ignore))))

  (nskk-it "works with multiple body forms"
    (let* ((expansion (macroexpand-1
                       '(nskk-<- (a b) (fn/k x y)
                          (setq r a)
                          (+ r b))))
           (on-found (nth 3 expansion)))
      (should (eq (car on-found) 'lambda))
      (should (equal (cadr on-found) '(a b)))
      (should (equal (caddr on-found) '(setq r a)))
      (should (equal (cadddr on-found) '(+ r b))))))

(nskk-describe "nskk-<-or standalone pipeline macro"
  (nskk-it "expands to CPS call with both on-found and on-not-found lambdas"
    (let ((expansion (macroexpand-1
                      '(nskk-<-or (result) (my-func/k key)
                                  (handle-miss)
                         (use result)))))
      ;; Head: the /k function name
      (should (eq (car expansion) 'my-func/k))
      ;; Args from fn-call
      (should (equal (nth 1 expansion) 'key))
      ;; on-found lambda
      (let ((on-found (nth 2 expansion)))
        (should (eq (car on-found) 'lambda))
        (should (equal (cadr on-found) '(result)))
        (should (equal (caddr on-found) '(use result))))
      ;; on-not-found is a zero-arg lambda wrapping else-form
      (let ((on-miss (nth 3 expansion)))
        (should (eq (car on-miss) 'lambda))
        (should (equal (cadr on-miss) '()))
        (should (equal (caddr on-miss) '(handle-miss))))))

  (nskk-it "wraps else-form in a zero-arg lambda not evaluated at expansion time"
    ;; The else-form should NOT be evaluated — it's wrapped in (lambda () ...)
    (let* ((expansion (macroexpand-1
                       '(nskk-<-or (v) (fn/k arg)
                                   (error "not-found!")
                          v)))
           (on-miss (nth 3 expansion)))
      (should (eq (car on-miss) 'lambda))
      (should (null (cadr on-miss)))      ; zero args
      (should (equal (caddr on-miss) '(error "not-found!"))))))


;;; -------------------------------------------------------------------
;;; <-seq CPS special form tests
;;; -------------------------------------------------------------------

(nskk-describe "defun/k <-seq special form"
  (nskk-it "propagates success: binds result and evaluates body"
    ;; <-seq [result (helper arg)] body  is equivalent to
    ;; (<- result helper arg) body
    (defun/k nskk-cps-test--always-found (x)
      "Always succeed with X doubled."
      (succeed (* x 2)))
    (defun/k nskk-cps-test--seq-caller (n)
      "Use <-seq to bind and double-double N."
      (<-seq [doubled (nskk-cps-test--always-found n)]
        (succeed (* doubled 2))))
    (should (= (nskk-cps-test--seq-caller 3) 12)))

  (nskk-it "propagates failure to the outer on-not-found"
    (defun/k nskk-cps-test--always-failed (_x)
      "Always fail."
      (fail))
    (defun/k nskk-cps-test--seq-fail-caller (n)
      "Use <-seq; inner failure should propagate."
      (<-seq [_result (nskk-cps-test--always-failed n)]
        (succeed "should-not-reach")))
    ;; sync wrapper returns nil on failure
    (should (null (nskk-cps-test--seq-fail-caller 99))))

  (nskk-it "body is not reached when CPS call fails"
    (defun/k nskk-cps-test--always-failed (_x)
      "Always fail."
      (fail))
    (let ((body-reached nil))
      (defun/k nskk-cps-test--seq-side-effect-caller (n)
        "Track whether body was executed on failure."
        (<-seq [_result (nskk-cps-test--always-failed n)]
          (progn (setq body-reached t) (succeed t))))
      (nskk-cps-test--seq-side-effect-caller 0)
      (should (null body-reached))))

  (nskk-it "macroexpand produces fn/k call with two lambdas"
    ;; The expansion of (<-seq [v (my-fn arg)] body) inside a defun/k should
    ;; produce (my-fn/k arg (lambda (--v--) (let ((v --v--)) body)) on-not-found)
    (let* ((form '(defun/k nskk-cps-test--seq-expand (x)
                    "Expansion test."
                    (<-seq [result (my-fn x)]
                      (succeed result))))
           (expanded (macroexpand-1 form))
           ;; expanded = (progn (defun nskk-cps-test--seq-expand/k ...) (defun ...) (put ...))
           (k-defun  (nth 1 expanded))
           (k-body   (nth 4 k-defun))  ; (<-seq ...) transformed body
           )
      ;; The /k body should be a call to my-fn/k
      (should (eq (car k-body) 'my-fn/k))
      ;; Second arg is a lambda (on-found)
      (should (eq (car (nth 2 k-body)) 'lambda))
      ;; Third arg is the outer on-not-found symbol (a lambda or symbol)
      (should (nth 3 k-body)))))

;;; -------------------------------------------------------------------
;;; New helper function tests
;;; -------------------------------------------------------------------

(nskk-describe "nskk--cps-parse-interactive"
  (nskk-it "returns nil form and unchanged body when no :interactive"
    (let ((result (nskk--cps-parse-interactive '((foo) (bar)))))
      (should (null (car result)))
      (should (equal (cdr result) '((foo) (bar))))))

  (nskk-it "returns (interactive) for :interactive t"
    (let ((result (nskk--cps-parse-interactive '(:interactive t (foo)))))
      (should (equal (car result) '(interactive)))
      (should (equal (cdr result) '((foo))))))

  (nskk-it "returns (interactive SPEC) for string spec"
    (let ((result (nskk--cps-parse-interactive '(:interactive "p" (foo)))))
      (should (equal (car result) '(interactive "p")))
      (should (equal (cdr result) '((foo))))))

  (nskk-it "handles empty body with :interactive"
    (let ((result (nskk--cps-parse-interactive '(:interactive t))))
      (should (equal (car result) '(interactive)))
      (should (null (cdr result)))))

  (nskk-it "returns (interactive FORM) for arbitrary form spec"
    (let ((result (nskk--cps-parse-interactive '(:interactive (list "f") (foo)))))
      (should (equal (car result) '(interactive (list "f"))))
      (should (equal (cdr result) '((foo)))))))

(nskk-describe "nskk--cps-parse-kw-args"
  (nskk-it "splits :found and :fail from rest args"
    (let* ((rest '(arg1 :found found-form :fail fail-form))
           (result (nskk--cps-parse-kw-args rest '(:found :fail))))
      (should (equal (car result) '(arg1)))
      (should (equal (cdr (assq :found (cdr result))) 'found-form))
      (should (equal (cdr (assq :fail  (cdr result))) 'fail-form))))

  (nskk-it "handles :fail before :found"
    (let* ((rest '(arg1 :fail fail-form :found found-form))
           (result (nskk--cps-parse-kw-args rest '(:found :fail))))
      (should (equal (car result) '(arg1)))
      (should (equal (cdr (assq :found (cdr result))) 'found-form))
      (should (equal (cdr (assq :fail  (cdr result))) 'fail-form))))

  (nskk-it "errors when keyword missing"
    (should-error (nskk--cps-parse-kw-args '(arg1 :found f) '(:found :fail))))

  (nskk-it "errors when keyword has no following form"
    (should-error (nskk--cps-parse-kw-args '(:found) '(:found :fail))))

  (nskk-it "handles zero positional args"
    (let* ((rest '(:found found-form :fail fail-form))
           (result (nskk--cps-parse-kw-args rest '(:found :fail))))
      (should (null (car result)))
      (should (equal (cdr (assq :found (cdr result))) 'found-form))
      (should (equal (cdr (assq :fail  (cdr result))) 'fail-form)))))

(nskk-describe "nskk--cps-args-info simplified"
  (nskk-it "handles plain args"
    (should (equal (nskk--cps-args-info '(a b c)) '((a b c)))))

  (nskk-it "strips &optional keyword"
    (should (equal (nskk--cps-args-info '(a &optional b)) '((a b)))))

  (nskk-it "extracts &rest symbol"
    (should (equal (nskk--cps-args-info '(a &rest rest)) (cons '(a) 'rest))))

  (nskk-it "handles &optional and &rest together"
    (should (equal (nskk--cps-args-info '(a &optional b &rest rest))
                   (cons '(a b) 'rest)))))

(nskk-describe "nskk--cps-define-handler"
  (nskk-it "registers handler in dispatch table"
    (let ((nskk--cps-form-dispatch nil))
      (nskk--cps-define-handler test-form #'identity)
      (should (assq 'test-form nskk--cps-form-dispatch))))

  (nskk-it "registered handler is called by nskk--cps-transform-form"
    (let* ((called nil)
           (handler (lambda (form _f _nf) (setq called form) 'result))
           (nskk--cps-form-dispatch (list (cons 'my-form handler))))
      (nskk--cps-transform-form '(my-form arg) 'found 'not-found)
      (should (equal called '(my-form arg))))))

;;; -------------------------------------------------------------------
;;; nskk-it-k macro tests
;;; -------------------------------------------------------------------

(nskk-describe "nskk-it-k macro"
  (nskk-it "nskk--parse-it-k-clauses extracts found binding and body"
    (let* ((rest '(:found (val) (should (equal val 42))))
           (parsed (nskk--parse-it-k-clauses rest)))
      (should (eq (plist-get parsed :found-binding) 'val))
      (should (equal (plist-get parsed :found-body) '((should (equal val 42)))))
      (should (equal (plist-get parsed :not-found-body)
                     '((ert-fail "on-not-found called unexpectedly"))))))

  (nskk-it "nskk--parse-it-k-clauses uses default not-found body when omitted"
    (let* ((rest '(:found (result) (should result)))
           (parsed (nskk--parse-it-k-clauses rest)))
      (should (equal (plist-get parsed :not-found-body)
                     '((ert-fail "on-not-found called unexpectedly"))))))

  (nskk-it "nskk--parse-it-k-clauses extracts explicit not-found body"
    (let* ((rest '(:found (v) (should v) :not-found () (should nil)))
           (parsed (nskk--parse-it-k-clauses rest)))
      (should (eq  (plist-get parsed :found-binding) 'v))
      (should (equal (plist-get parsed :found-body) '((should v))))
      (should (equal (plist-get parsed :not-found-body) '((should nil))))))

  (nskk-it "nskk-it-k :found body executes when on-found is called"
    (let ((found-val nil))
      (cl-flet ((fake/k (on-found _on-not-found)
                  (funcall on-found 99)))
        (fake/k
          (lambda (v) (setq found-val v))
          (lambda () (ert-fail "on-not-found should not be called"))))
      (should (equal found-val 99))))

  (nskk-it "nskk-it-k :not-found body executes when on-not-found is called"
    (let ((nf-called nil))
      (cl-flet ((fake/k (_on-found on-not-found)
                  (funcall on-not-found)))
        (fake/k
          (lambda (_v) (ert-fail "on-found should not be called"))
          (lambda () (setq nf-called t))))
      (should nf-called))))

(provide 'nskk-cps-macros-test)

;;; nskk-cps-macros-test.el ends here
