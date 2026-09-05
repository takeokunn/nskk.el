;;; nskk-dict-transaction-test.el --- Transaction API tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'nskk-dict-transaction)
(require 'nskk-dictionary)

(ert-deftest nskk-dict-transaction-snapshot-round-trip ()
  (nskk-prolog-retract-all 'nskk-transaction-test 2)
  (nskk-prolog-assert '((nskk-transaction-test "before" value)))
  (let ((snapshot (nskk-dict-transaction-predicate-snapshot
                   (nskk-prolog-clause-key 'nskk-transaction-test 2))))
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

;;; nskk-dict-transaction-load-entries

(defun nskk-dict-transaction-test--identity-parser (entry)
  "Return ENTRY unchanged, standing in for a caller's fact builder."
  entry)

(ert-deftest nskk-dict-transaction-load-entries-wraps-empty-file ()
  "A valid file holding no entries is distinguishable from a skipped one."
  (let ((file (make-temp-file "nskk-tx-empty-")))
    (unwind-protect
        (progn
          (with-temp-file file (prin1 '() (current-buffer)))
          (let ((result (nskk-dict-transaction-load-entries
                         file 4096
                         #'nskk-dict-transaction-test--identity-parser)))
            (should (consp result))
            (should (null (cdr result)))))
      (delete-file file))))

(ert-deftest nskk-dict-transaction-load-entries-skips-absent-file ()
  "An absent file yields nil rather than an empty-entry wrapper."
  (should-not
   (nskk-dict-transaction-load-entries
    (expand-file-name "nskk-tx-absent" (make-temp-file "nskk-tx-dir-" t))
    4096 #'nskk-dict-transaction-test--identity-parser)))

(ert-deftest nskk-dict-transaction-load-entries-reports-oversize ()
  "ON-OVERSIZE receives the observed size and suppresses the read."
  (let ((file (make-temp-file "nskk-tx-oversize-"))
        (reported nil))
    (unwind-protect
        (progn
          (with-temp-file file (prin1 '((a) (b)) (current-buffer)))
          (should-not
           (nskk-dict-transaction-load-entries
            file 2 #'nskk-dict-transaction-test--identity-parser
            (lambda (size) (setq reported size))))
          (should (integerp reported))
          (should (> reported 2)))
      (delete-file file))))

(ert-deftest nskk-dict-transaction-load-entries-signals-oversize-without-handler ()
  "Without ON-OVERSIZE an excessive file is signaled, never silently skipped."
  (let ((file (make-temp-file "nskk-tx-oversize-signal-")))
    (unwind-protect
        (progn
          (with-temp-file file (prin1 '((a) (b)) (current-buffer)))
          (should-error
           (nskk-dict-transaction-load-entries
            file 2 #'nskk-dict-transaction-test--identity-parser)
           :type 'error))
      (delete-file file))))

(ert-deftest nskk-dict-transaction-load-entries-rejects-symlink ()
  "A symbolic link is refused before any read."
  (let* ((directory (make-temp-file "nskk-tx-link-" t))
         (target (expand-file-name "target.dat" directory))
         (link (expand-file-name "link.dat" directory)))
    (unwind-protect
        (progn
          (with-temp-file target (prin1 '((a)) (current-buffer)))
          (make-symbolic-link target link)
          (should-error
           (nskk-dict-transaction-load-entries
            link 4096 #'nskk-dict-transaction-test--identity-parser)
           :type 'error))
      (delete-directory directory t))))

(ert-deftest nskk-dict-transaction-load-entries-rejects-post-stat-symlink ()
  "A file that turns into a symbolic link after the stat is refused.
The assertion matches the recheck's own message rather than merely
demanding some error: the pinned read re-validates the same property, so
a test that accepts any error passes even when the recheck is gone."
  (let ((file (make-temp-file "nskk-tx-post-stat-"))
        (checks 0)
        (read-called nil))
    (unwind-protect
        (progn
          (with-temp-file file (prin1 '((a)) (current-buffer)))
          (let ((condition
                 (cl-letf (((symbol-function 'nskk-dict-transaction-read-entries)
                            (lambda (&rest _)
                              (setq read-called t)
                              nil))
                           ((symbol-function 'file-symlink-p)
                            (lambda (f)
                              (and (equal f file)
                                   (>= (cl-incf checks) 2)))))
                   (condition-case err
                       (progn
                         (nskk-dict-transaction-load-entries
                          file 4096
                          #'nskk-dict-transaction-test--identity-parser)
                         nil)
                     (error err)))))
            (should condition)
            (should (string-match-p "changed to a symbolic link"
                                    (error-message-string condition))))
          (should-not read-called))
      (delete-file file))))

(ert-deftest nskk-dict-transaction-load-entries-rejects-post-stat-non-regular ()
  "A file that stops being regular after the stat is refused."
  (let ((file (make-temp-file "nskk-tx-post-stat-regular-"))
        (checks 0))
    (unwind-protect
        (progn
          (with-temp-file file (prin1 '((a)) (current-buffer)))
          (let ((condition
                 (cl-letf (((symbol-function 'file-regular-p)
                            (lambda (f)
                              (not (and (equal f file)
                                        (>= (cl-incf checks) 2))))))
                   (condition-case err
                       (progn
                         (nskk-dict-transaction-load-entries
                          file 4096
                          #'nskk-dict-transaction-test--identity-parser)
                         nil)
                     (error err)))))
            (should condition)
            (should (string-match-p "changed to a non-regular file"
                                    (error-message-string condition)))))
      (delete-file file))))

;;; nskk-dict-transaction-publish-facts

(ert-deftest nskk-dict-transaction-publish-facts-replaces-whole-predicate ()
  "Publishing replaces every existing clause rather than adding to them."
  (nskk-prolog-retract-all 'nskk-publish-test 2)
  (nskk-prolog-assert '((nskk-publish-test "old" value)))
  (nskk-dict-transaction-publish-facts
   'nskk-publish-owner 'nskk-publish-test 2
   '((nskk-publish-test "new" value)))
  (should (nskk-prolog-holds-p '(nskk-publish-test "new" value)))
  (should-not (nskk-prolog-holds-p '(nskk-publish-test "old" value)))
  (should (= 1 (length (nskk-prolog-query '(nskk-publish-test \?k \?v))))))

(ert-deftest nskk-dict-transaction-publish-facts-runs-on-commit ()
  "ON-COMMIT runs once the facts have landed."
  (nskk-prolog-retract-all 'nskk-publish-commit-test 2)
  (let (observed)
    (nskk-dict-transaction-publish-facts
     'nskk-publish-commit-owner 'nskk-publish-commit-test 2
     '((nskk-publish-commit-test "new" value))
     (lambda ()
       (setq observed
             (nskk-prolog-holds-p '(nskk-publish-commit-test "new" value)))))
    (should observed)))

(ert-deftest nskk-dict-transaction-publish-facts-restores-clauses-on-error ()
  "A failure mid-publication restores the original clauses."
  (nskk-prolog-retract-all 'nskk-publish-rollback-test 2)
  (nskk-prolog-assert '((nskk-publish-rollback-test "old" value)))
  (let ((key (nskk-prolog-clause-key 'nskk-publish-rollback-test 2)))
    (let ((before (nskk-dict-transaction-predicate-snapshot key)))
      (should-error
       (cl-letf (((symbol-function 'nskk-prolog-assert)
                  (lambda (&rest _) (error "publication failure"))))
         (nskk-dict-transaction-publish-facts
          'nskk-publish-rollback-owner 'nskk-publish-rollback-test 2
          '((nskk-publish-rollback-test "new" value))))
       :type 'error)
      (should (equal before (nskk-dict-transaction-predicate-snapshot key)))
      (should (nskk-prolog-holds-p '(nskk-publish-rollback-test "old" value))))))

(ert-deftest nskk-dict-transaction-publish-facts-runs-caller-rollback-actions ()
  "Caller-supplied restorers run alongside the clause restore."
  (nskk-prolog-retract-all 'nskk-publish-actions-test 2)
  (nskk-prolog-assert '((nskk-publish-actions-test "old" value)))
  (let ((restored nil))
    (should-error
     (cl-letf (((symbol-function 'nskk-prolog-assert)
                (lambda (&rest _) (error "publication failure"))))
       (nskk-dict-transaction-publish-facts
        'nskk-publish-actions-owner 'nskk-publish-actions-test 2
        '((nskk-publish-actions-test "new" value))
        nil
        (list (cons 'caller-state (lambda () (setq restored t))))))
     :type 'error)
    (should restored)))

;;; nskk-dict-write-private-file

(ert-deftest nskk-dict-write-private-file-creates-directory-privately ()
  "A directory this code creates is unreadable by other users."
  (let* ((parent (make-temp-file "nskk-private-parent-" t))
         (target (expand-file-name "created/data.dat" parent)))
    (unwind-protect
        (progn
          (nskk-dict-write-private-file target '(("a" "b" "c")))
          (should (file-regular-p target))
          (should (equal (file-modes (file-name-directory target)) #o700)))
      (delete-directory parent t))))

(ert-deftest nskk-dict-write-private-file-keeps-existing-directory-modes ()
  "An existing directory keeps whatever modes it already had."
  (let* ((parent (make-temp-file "nskk-private-existing-" t))
         (directory (expand-file-name "existing" parent))
         (target (expand-file-name "data.dat" directory)))
    (unwind-protect
        (progn
          (make-directory directory)
          (set-file-modes directory #o755)
          (nskk-dict-write-private-file target '(("a" "b" "c")))
          (should (file-regular-p target))
          (should (equal (file-modes directory) #o755)))
      (delete-directory parent t))))

(ert-deftest nskk-dict-write-private-file-round-trips-payload ()
  "The payload is written as the single serialized form the readers expect."
  (let* ((parent (make-temp-file "nskk-private-payload-" t))
         (target (expand-file-name "data.dat" parent))
         (payload '(("prev" "reading" "candidate") ("p2" "r2" "c2"))))
    (unwind-protect
        (progn
          (nskk-dict-write-private-file target payload)
          (should (equal payload
                         (with-temp-buffer
                           (insert-file-contents target)
                           (read (current-buffer))))))
      (delete-directory parent t))))

(ert-deftest nskk-dict-serialize-solutions-orders-variables ()
  "Each solution yields its variables in the order requested."
  (nskk-prolog-retract-all 'nskk-serialize-test 3)
  (nskk-prolog-assert '((nskk-serialize-test "a" "b" "c")))
  (should (equal (nskk-dict-serialize-solutions
                  '(nskk-serialize-test \?x \?y \?z) '(\?z \?y \?x))
                 '(("c" "b" "a")))))

(provide 'nskk-dict-transaction-test)
