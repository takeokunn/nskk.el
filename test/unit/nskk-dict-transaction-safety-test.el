;;; nskk-dict-transaction-safety-test.el --- Filesystem safety predicate characterization -*- lexical-binding: t; -*-

;; Characterizes the four filesystem safety predicates in nskk-dict-transaction.el
;; as they behave today, so that deduplicating their shared acquisition preamble can
;; be shown not to move the boundary between the directories each one accepts.

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'nskk-dict-transaction)

(defun nskk-dict-transaction-safety--ancestor-directories (directory)
  "Return every ancestor of DIRECTORY, root first."
  (let ((current (directory-file-name (expand-file-name directory)))
        parent
        result)
    (setq parent (directory-file-name (file-name-directory current)))
    (while (not (equal parent current))
      (push parent result)
      (setq current parent
            parent (directory-file-name (file-name-directory current))))
    result))

(defun nskk-dict-transaction-safety--controlled-ancestor-p (directory)
  "Return non-nil when DIRECTORY meets the controller rule.
This restates the rule rather than calling
`nskk-dict-transaction--safe-directory-controller-p', so that breaking that
predicate cannot turn the tests guarded by it into skips."
  (let* ((attributes (file-attributes directory 'integer))
         (modes (and attributes (file-modes directory))))
    (and attributes
         (integerp modes)
         (file-directory-p directory)
         (null (file-acl directory))
         (memq (file-attribute-user-id attributes) (list 0 (user-uid)))
         (or (zerop (logand modes #o022))
             (not (zerop (logand modes #o1000)))))))

(defun nskk-dict-transaction-safety--shadowing-handler (directory)
  "Return a file-name handler shadowing DIRECTORY, or nil."
  (cl-some (lambda (operation) (find-file-name-handler directory operation))
           nskk-dict-transaction--file-handler-operations))

(defun nskk-dict-transaction-safety--check-environment (root)
  "Skip the running test unless ROOT can exercise the predicates at all."
  (when (zerop (user-uid))
    (ert-skip "running as root: the owner and `file-writable-p' clauses are vacuous"))
  (dolist (directory (cons root (nskk-dict-transaction-safety--ancestor-directories root)))
    (when-let* ((handler (nskk-dict-transaction-safety--shadowing-handler directory)))
      (ert-skip (format "file-name handler %S shadows %s, so every predicate returns nil"
                        handler directory))))
  (dolist (directory (nskk-dict-transaction-safety--ancestor-directories root))
    (unless (nskk-dict-transaction-safety--controlled-ancestor-p directory)
      (ert-skip (format "ancestor %s is not owner/mode-controlled, so the ancestry clause rejects everything under %s"
                        directory root))))
  (when (file-acl root)
    (ert-skip (format "%s carries an ACL, so the (null acl) clause rejects it" root)))
  (set-file-modes root #o750)
  (unless (equal (file-modes root) #o750)
    (ert-skip (format "%s does not preserve modes: set #o750, read back %S"
                      root (file-modes root))))
  (set-file-modes root #o700))

(defun nskk-dict-transaction-safety--delete-tree (root)
  "Delete ROOT, restoring a traversable mode on each entry first."
  (ignore-errors (set-file-modes root #o700))
  (dolist (entry (ignore-errors
                   (directory-files root t directory-files-no-dot-files-regexp)))
    (cond ((file-symlink-p entry) (ignore-errors (delete-file entry)))
          ((file-directory-p entry) (nskk-dict-transaction-safety--delete-tree entry))
          (t (ignore-errors (delete-file entry)))))
  (ignore-errors (delete-directory root)))

(defmacro nskk-dict-transaction-safety--with-root (root &rest body)
  "Bind ROOT to a fresh private temporary directory and run BODY."
  (declare (indent 1))
  `(let ((,root (make-temp-file "nskk-dict-transaction-safety-" t)))
     (unwind-protect
         (progn
           (set-file-modes ,root #o700)
           (nskk-dict-transaction-safety--check-environment ,root)
           ,@body)
       (nskk-dict-transaction-safety--delete-tree ,root))))

(defun nskk-dict-transaction-safety--subdirectory (root name modes)
  "Create NAME under ROOT with MODES and return its path."
  (let ((directory (expand-file-name name root)))
    (make-directory directory)
    (set-file-modes directory modes)
    (unless (equal (file-modes directory) modes)
      (ert-skip (format "%s does not preserve mode #o%o: read back %S"
                        directory modes (file-modes directory))))
    directory))

(defun nskk-dict-transaction-safety--symlink (root name target)
  "Create a symbolic link NAME under ROOT pointing at TARGET and return it."
  (let ((link (expand-file-name name root)))
    (condition-case error
        (make-symbolic-link target link)
      (error (ert-skip (format "symbolic links are unavailable: %S" error))))
    link))

(defun nskk-dict-transaction-safety--regular-file (root name)
  "Create an empty regular file NAME under ROOT and return its path."
  (let ((file (expand-file-name name root)))
    (with-temp-file file)
    (set-file-modes file #o600)
    file))

(defun nskk-dict-transaction-safety--refusing-handler (operation &rest arguments)
  "Report locality for OPERATION but fail on any real file access.
ARGUMENTS are only reported, never acted on."
  (if (eq operation 'file-remote-p)
      nil
    (ert-fail (format "a predicate performed %S on a handled path"
                      (cons operation arguments)))))

(defun nskk-dict-transaction-safety--transparent-handler (operation &rest arguments)
  "Run OPERATION on ARGUMENTS exactly as if no handler were installed.
A handler that refuses access cannot show that the handler *check* works,
because every later step fails for its own reasons and the predicate would
return nil either way.  This one changes no answer, so the only thing left
that can reject the path is the check itself."
  (let ((inhibit-file-name-handlers
         (cons 'nskk-dict-transaction-safety--transparent-handler
               (and (eq inhibit-file-name-operation operation)
                    inhibit-file-name-handlers)))
        (inhibit-file-name-operation operation))
    (apply operation arguments)))

(ert-deftest nskk-dict-transaction-safety-controller-accepts-owner-controlled-modes ()
  (nskk-dict-transaction-safety--with-root root
    (dolist (modes (list #o700 #o755 #o750 #o701 #o500))
      (let ((directory (nskk-dict-transaction-safety--subdirectory
                        root (format "ctl-%o" modes) modes)))
        (should (nskk-dict-transaction--safe-directory-controller-p directory))))))

(ert-deftest nskk-dict-transaction-safety-controller-rejects-shared-write-without-sticky ()
  (nskk-dict-transaction-safety--with-root root
    (dolist (modes (list #o775 #o770 #o707 #o702 #o720 #o777))
      (let ((directory (nskk-dict-transaction-safety--subdirectory
                        root (format "open-%o" modes) modes)))
        (should-not (nskk-dict-transaction--safe-directory-controller-p directory))))))

(ert-deftest nskk-dict-transaction-safety-controller-accepts-shared-write-with-sticky ()
  ;; #o1000 is the whole reason a world-writable directory such as /tmp is
  ;; usable at all, so the sticky alternative is the clause that must survive.
  (nskk-dict-transaction-safety--with-root root
    (dolist (modes (list #o1777 #o1775 #o1702))
      (let ((directory (nskk-dict-transaction-safety--subdirectory
                        root (format "sticky-%o" modes) modes)))
        (should (nskk-dict-transaction--safe-directory-controller-p directory))
        (should-not (zerop (logand (file-modes directory) #o022)))))))

(ert-deftest nskk-dict-transaction-safety-controller-ignores-ancestry ()
  (nskk-dict-transaction-safety--with-root root
    (let ((directory (nskk-dict-transaction-safety--subdirectory root "child" #o700)))
      (set-file-modes root #o775)
      (unless (equal (file-modes root) #o775)
        (ert-skip (format "%s does not preserve mode #o775" root)))
      (should (nskk-dict-transaction--safe-directory-controller-p directory)))))

(ert-deftest nskk-dict-transaction-safety-ancestry-clause-rejects-shared-writable-parent ()
  (nskk-dict-transaction-safety--with-root root
    (let ((directory (nskk-dict-transaction-safety--subdirectory root "child" #o700))
          (source (file-attributes root 'integer)))
      (should (nskk-dict-transaction--safe-snapshot-base-p directory))
      (should (nskk-dict-transaction--safe-source-snapshot-parent-p directory source))
      (should (nskk-dict-transaction--private-snapshot-directory-p directory))
      (set-file-modes root #o775)
      (unless (equal (file-modes root) #o775)
        (ert-skip (format "%s does not preserve mode #o775" root)))
      (should (file-writable-p directory))
      (should-not (nskk-dict-transaction--safe-snapshot-base-p directory))
      (should-not (nskk-dict-transaction--safe-source-snapshot-parent-p directory source))
      (should-not (nskk-dict-transaction--private-snapshot-directory-p directory)))))

(ert-deftest nskk-dict-transaction-safety-snapshot-base-mode-matrix ()
  (nskk-dict-transaction-safety--with-root root
    (dolist (row '((#o700 t) (#o755 t) (#o750 t) (#o701 t)
                   (#o775 nil) (#o770 nil) (#o707 nil)
                   (#o1777 nil) (#o1775 nil)))
      (let ((directory (nskk-dict-transaction-safety--subdirectory
                        root (format "base-%o" (car row)) (car row))))
        (should (eq (and (nskk-dict-transaction--safe-snapshot-base-p directory) t)
                    (cadr row)))))))

(ert-deftest nskk-dict-transaction-safety-snapshot-base-requires-writability ()
  (nskk-dict-transaction-safety--with-root root
    (let ((directory (nskk-dict-transaction-safety--subdirectory root "ro" #o500)))
      (should (nskk-dict-transaction--safe-directory-controller-p directory))
      (should-not (file-writable-p directory))
      (should-not (nskk-dict-transaction--safe-snapshot-base-p directory)))))

(ert-deftest nskk-dict-transaction-safety-snapshot-base-accepts-symlink ()
  (nskk-dict-transaction-safety--with-root root
    (let* ((target (nskk-dict-transaction-safety--subdirectory root "target" #o700))
           (link (nskk-dict-transaction-safety--symlink root "link" target)))
      (should (file-symlink-p link))
      (should (nskk-dict-transaction--safe-snapshot-base-p link)))))

(ert-deftest nskk-dict-transaction-safety-source-snapshot-parent-mode-matrix ()
  (nskk-dict-transaction-safety--with-root root
    (let ((source (file-attributes root 'integer)))
      (dolist (row '((#o700 t) (#o755 t) (#o750 t) (#o701 t)
                     (#o775 nil) (#o770 nil) (#o707 nil)
                     (#o1777 nil) (#o500 nil)))
        (let ((directory (nskk-dict-transaction-safety--subdirectory
                          root (format "src-%o" (car row)) (car row))))
          (should (eq (and (nskk-dict-transaction--safe-source-snapshot-parent-p
                            directory source)
                           t)
                      (cadr row))))))))

(ert-deftest nskk-dict-transaction-safety-source-snapshot-parent-rejects-symlink ()
  (nskk-dict-transaction-safety--with-root root
    (let* ((target (nskk-dict-transaction-safety--subdirectory root "target" #o700))
           (link (nskk-dict-transaction-safety--symlink root "link" target))
           (source (file-attributes root 'integer)))
      (should (nskk-dict-transaction--safe-source-snapshot-parent-p target source))
      (should-not (nskk-dict-transaction--safe-source-snapshot-parent-p link source)))))

(ert-deftest nskk-dict-transaction-safety-source-snapshot-parent-requires-same-device ()
  (nskk-dict-transaction-safety--with-root root
    (let* ((directory (nskk-dict-transaction-safety--subdirectory root "parent" #o700))
           (same-device (file-attributes
                         (nskk-dict-transaction-safety--regular-file directory "source")
                         'integer))
           (foreign (file-attributes "/dev/null" 'integer)))
      (unless foreign
        (ert-skip "/dev/null is unavailable, so no second device number is reachable"))
      (when (equal (file-attribute-device-number foreign)
                   (file-attribute-device-number same-device))
        (ert-skip "/dev/null shares a device number with the temporary directory"))
      (should (nskk-dict-transaction--safe-source-snapshot-parent-p directory same-device))
      (should-not (nskk-dict-transaction--safe-source-snapshot-parent-p directory foreign)))))

(ert-deftest nskk-dict-transaction-safety-private-requires-exactly-700 ()
  (nskk-dict-transaction-safety--with-root root
    ;; #o1700 is accepted: the mask in `(= (logand modes #o777) #o700)' discards
    ;; the sticky bit, so a sticky private directory reads as an ordinary one.
    ;; The group/other-WRITE rows are the ones that matter for a directory
    ;; whose contract is that nobody else can touch the snapshot inside it;
    ;; a mask that only checked the read and execute bits would pass them.
    (dolist (row '((#o700 t) (#o1700 t)
                   (#o755 nil) (#o750 nil) (#o701 nil) (#o710 nil)
                   (#o770 nil) (#o500 nil) (#o600 nil)
                   (#o720 nil) (#o702 nil) (#o722 nil)))
      (let ((directory (nskk-dict-transaction-safety--subdirectory
                        root (format "priv-%o" (car row)) (car row))))
        (should (eq (and (nskk-dict-transaction--private-snapshot-directory-p directory) t)
                    (cadr row)))))))

(ert-deftest nskk-dict-transaction-safety-private-rejects-symlink ()
  (nskk-dict-transaction-safety--with-root root
    (let* ((target (nskk-dict-transaction-safety--subdirectory root "target" #o700))
           (link (nskk-dict-transaction-safety--symlink root "link" target)))
      (should (nskk-dict-transaction--private-snapshot-directory-p target))
      (should-not (nskk-dict-transaction--private-snapshot-directory-p link)))))

(ert-deftest nskk-dict-transaction-safety-predicates-are-pairwise-distinguishable ()
  (nskk-dict-transaction-safety--with-root root
    (let* ((source (file-attributes root 'integer))
           (private (nskk-dict-transaction-safety--subdirectory root "m700" #o700))
           (probes (list private
                         (nskk-dict-transaction-safety--subdirectory root "m755" #o755)
                         (nskk-dict-transaction-safety--subdirectory root "m1777" #o1777)
                         (nskk-dict-transaction-safety--symlink root "link" private)))
           (columns
            (list (mapcar (lambda (directory)
                            (and (nskk-dict-transaction--safe-directory-controller-p
                                  directory)
                                 t))
                          probes)
                  (mapcar (lambda (directory)
                            (and (nskk-dict-transaction--safe-snapshot-base-p directory) t))
                          probes)
                  (mapcar (lambda (directory)
                            (and (nskk-dict-transaction--safe-source-snapshot-parent-p
                                  directory source)
                                 t))
                          probes)
                  (mapcar (lambda (directory)
                            (and (nskk-dict-transaction--private-snapshot-directory-p
                                  directory)
                                 t))
                          probes))))
      (should (equal (nth 0 columns) '(t t t t)))
      (should (equal (nth 1 columns) '(t t nil t)))
      (should (equal (nth 2 columns) '(t t nil nil)))
      (should (equal (nth 3 columns) '(t nil nil nil)))
      ;; Four distinct columns is what makes the rows above impossible to satisfy
      ;; by substituting one predicate for another.
      (should (= 4 (length (delete-dups (copy-sequence columns))))))))

(ert-deftest nskk-dict-transaction-safety-all-predicates-reject-handled-paths ()
  (nskk-dict-transaction-safety--with-root root
    (let* ((directory (nskk-dict-transaction-safety--subdirectory root "handled" #o700))
           (source (file-attributes root 'integer)))
      ;; Control.  Unhandled, this directory satisfies every other clause, so
      ;; the rejections below are attributable to the handler check and to
      ;; nothing else.  Without this the `should-not's would hold even if the
      ;; check were deleted and the path merely failed for some other reason.
      (should (nskk-dict-transaction--private-snapshot-directory-p directory))
      (should (nskk-dict-transaction--safe-directory-controller-p directory))
      (let ((file-name-handler-alist
             (cons (cons (concat "\\`" (regexp-quote directory))
                         #'nskk-dict-transaction-safety--transparent-handler)
                   file-name-handler-alist)))
        (should-not (nskk-dict-transaction--safe-directory-controller-p directory))
        (should-not (nskk-dict-transaction--safe-snapshot-base-p directory))
        (should-not (nskk-dict-transaction--safe-source-snapshot-parent-p directory source))
        (should-not (nskk-dict-transaction--private-snapshot-directory-p directory))))))

(ert-deftest nskk-dict-transaction-safety-all-predicates-reject-regular-file ()
  (nskk-dict-transaction-safety--with-root root
    (let ((file (nskk-dict-transaction-safety--regular-file root "plain"))
          (source (file-attributes root 'integer)))
      (should-not (nskk-dict-transaction--safe-directory-controller-p file))
      (should-not (nskk-dict-transaction--safe-snapshot-base-p file))
      (should-not (nskk-dict-transaction--safe-source-snapshot-parent-p file source))
      (should-not (nskk-dict-transaction--private-snapshot-directory-p file)))))

(ert-deftest nskk-dict-transaction-safety-all-predicates-reject-missing-directory ()
  (nskk-dict-transaction-safety--with-root root
    (let ((missing (expand-file-name "absent" root))
          (source (file-attributes root 'integer)))
      (should-not (file-exists-p missing))
      (should-not (nskk-dict-transaction--safe-directory-controller-p missing))
      (should-not (nskk-dict-transaction--safe-snapshot-base-p missing))
      (should-not (nskk-dict-transaction--safe-source-snapshot-parent-p missing source))
      (should-not (nskk-dict-transaction--private-snapshot-directory-p missing)))))

(provide 'nskk-dict-transaction-safety-test)
;;; nskk-dict-transaction-safety-test.el ends here
