;;; nskk-dict-transaction-test.el --- Transaction API tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'nskk-dict-transaction)

(ert-deftest nskk-dict-transaction-snapshot-round-trip ()
  (nskk-prolog-retract-all 'nskk-transaction-test 2)
  (nskk-prolog-assert '((nskk-transaction-test "before" value)))
  (let ((snapshot (nskk-dict-transaction-predicate-snapshot
                   (nskk--prolog-clause-key 'nskk-transaction-test 2))))
    (nskk-prolog-retract-all 'nskk-transaction-test 2)
    (nskk-dict-transaction-apply-predicate-snapshot snapshot)
    (should (nskk-prolog-query '(nskk-transaction-test "before" value)))))

(ert-deftest nskk-dict-transaction-rollback-resignals-primary ()
  (let ((owner 'nskk-dict-transaction-test)
        (called nil))
    (should-error
     (nskk-dict-transaction-rollback-and-resignal
      owner '(error "primary")
      (list (cons 'state (lambda () (setq called t)))))
     :type 'error)
    (should called)))

(ert-deftest nskk-dict-transaction-failed-rollback-is-retained ()
  (let ((owner 'nskk-dict-transaction-failed-test))
    (should-error
     (nskk-dict-transaction-rollback-and-resignal
      owner '(error "primary")
      (list (cons 'state (lambda () (error "rollback")))))
     :type 'error)
    (should-error (nskk-dict-transaction-ensure-rollback-complete owner)
                  :type 'nskk-dict-rollback-incomplete)))

(provide 'nskk-dict-transaction-test)
