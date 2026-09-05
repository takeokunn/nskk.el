;;; nskk-dict-transaction.el --- Public dictionary transaction API -*- lexical-binding: t; -*-

;; Layer position: L1 (Core Engine) -- depends on nskk-prolog and nskk-cps-macros.

;;; Code:
(require 'nskk-prolog)
(require 'nskk-cps-macros)

(define-error 'nskk-dict-error "Dictionary error")
(define-error 'nskk-dict-rollback-incomplete
              "Dictionary rollback remains incomplete"
              'nskk-dict-error)

(defun nskk-dict-transaction-read-entries (file truename attributes max-size parser)
  "Read FILE transactionally and transform each entry with PARSER.
PARSER is called for each entry in the single serialized data form."
  (let ((read-circle nil))
    (with-temp-buffer
      (nskk-dict-transaction-insert-file-contents-pinned
       file truename attributes max-size)
      (set-buffer-multibyte t)
      (decode-coding-region (point-min) (point-max) 'undecided)
      (goto-char (point-min))
      (let ((data (read (current-buffer))))
        (condition-case nil
            (progn
              (read (current-buffer))
              (error "Expected exactly one data form"))
          (end-of-file
           (unless (proper-list-p data)
             (error "Expected proper list, got %s" (type-of data)))
           (mapcar parser data)))))))

(defun nskk-dict-transaction-load-entries
    (file max-size parser &optional on-oversize)
  "Validate FILE, then read and parse it with PARSER.
Return a cons whose cdr holds the parsed entries, or nil when FILE is
absent or unreadable.  Wrapping the entries lets a caller tell a valid
empty file from one that was skipped.

FILE is rejected when it is a symbolic link or not a regular file, and
again when either property changes between the metadata read and the
pinned read -- the window an attacker would have to swap the path.

A FILE larger than MAX-SIZE is passed to ON-OVERSIZE, which receives the
observed size and whose return value is discarded; the load is then
skipped.  Without ON-OVERSIZE the excess is signaled instead."
  (cond
   ((file-symlink-p file)
    (error "Refusing symbolic-link file: %s" file))
   ((not (file-exists-p file)) nil)
   ((not (file-regular-p file))
    (error "Refusing non-regular file: %s" file))
   ((not (file-readable-p file)) nil)
   (t
    (let* ((attributes (file-attributes file 'integer))
           (size (and attributes (file-attribute-size attributes))))
      (unless attributes
        (error "File disappeared before it could be read: %s" file))
      (unless (integerp size)
        (error "Invalid size for %s: %S" file size))
      (when (file-symlink-p file)
        (error "File changed to a symbolic link before read: %s" file))
      (unless (file-regular-p file)
        (error "File changed to a non-regular file before read: %s" file))
      (cond
       ((<= size max-size)
        (cons t
              (nskk-dict-transaction-read-entries
               file (file-truename file) attributes max-size parser)))
       (on-oversize
        (funcall on-oversize size)
        nil)
       (t
        (error "File exceeds %d-byte limit: %s" max-size file)))))))

(defun nskk-dict-transaction--commit-facts
    (owner predicate arity facts on-commit)
  "Replace PREDICATE/ARITY clauses with FACTS, then settle OWNER's rollback.
ON-COMMIT, when non-nil, runs after the facts are asserted and before the
retained rollback state is discarded."
  (nskk-prolog-retract-all predicate arity)
  (dolist (fact facts)
    (nskk-prolog-assert (list fact)))
  (when on-commit
    (funcall on-commit))
  (nskk-dict-transaction-clear-pending-rollback owner))

(defun nskk-dict-transaction-publish-facts
    (owner predicate arity facts &optional on-commit rollback-actions)
  "Publish FACTS as the whole of PREDICATE/ARITY for OWNER, transactionally.
Snapshot the current clauses first, so an error or a quit anywhere in the
replacement restores them before the original condition is resignaled.

ON-COMMIT runs after the facts land, for state a caller must update in the
same transaction.  ROLLBACK-ACTIONS is an alist of (KEY . RESTORER) for
state outside the clause store; each RESTORER is called during rollback
alongside the clause restore.  The key `predicate' is reserved for the
clause restore this function contributes -- reusing it costs nothing but a
duplicated label in the incomplete-rollback diagnostic."
  (let* ((key (nskk-prolog-clause-key predicate arity))
         (previous (nskk-dict-transaction-predicate-snapshot key)))
    (condition-case condition
        (nskk-dict-transaction--commit-facts
         owner predicate arity facts on-commit)
      ((error quit)
       (nskk-dict-transaction-rollback-and-resignal
        owner
        condition
        (cons (cons 'predicate
                    (lambda ()
                      (nskk-dict-transaction-apply-predicate-snapshot
                       previous)))
              rollback-actions))))))

(defvar nskk-dict-transaction--pending-rollbacks (make-hash-table :test #'equal)
  "Rollback state retained until every failed storage region is restored.")

(defun nskk-dict-transaction--pending-rollback (owner)
  "Return pending rollback state for OWNER, or nil."
  (gethash owner nskk-dict-transaction--pending-rollbacks))

(defun nskk-dict-transaction--clear-pending-rollback (owner)
  "Discard retained rollback state for OWNER."
  (remhash owner nskk-dict-transaction--pending-rollbacks))

(defun nskk-dict-transaction--rollback-diagnostic (owner primary failures)
  "Format a rollback diagnostic for OWNER, PRIMARY, and FAILURES."
  (format "Rollback for %S after %S is incomplete; unrestored regions: %s"
          owner primary
          (mapconcat (lambda (failure)
                       (format "%S=%S" (car failure) (cdr failure)))
                     failures ", ")))

(defun nskk-dict-transaction--warn-rollback-incomplete (owner primary failures)
  "Warn that rollback for OWNER after PRIMARY left FAILURES."
  (condition-case nil
      (let ((inhibit-quit t))
        (display-warning 'nskk
                         (nskk-dict-transaction--rollback-diagnostic owner primary failures)
                         :error))
    ((error quit) nil)))

(defun nskk-dict-transaction--run-rollback (owner primary restorers)
  "Run RESTORERS independently after PRIMARY for OWNER."
  (let (failed-restorers failures)
    (dolist (restorer restorers)
      (condition-case failure
          (let ((inhibit-quit t)) (funcall (cdr restorer)))
        ((error quit)
         (push restorer failed-restorers)
         (push (cons (car restorer) failure) failures))))
    (setq failed-restorers (nreverse failed-restorers)
          failures (nreverse failures))
    (if failures
        (let ((pending (list :primary primary :restorers failed-restorers
                             :failures failures)))
          (puthash owner pending nskk-dict-transaction--pending-rollbacks)
          (nskk-dict-transaction--warn-rollback-incomplete owner primary failures)
          pending)
      (nskk-dict-transaction--clear-pending-rollback owner)
      nil)))

(defun nskk-dict-transaction--retry-pending-rollback (owner)
  "Retry retained failed rollback regions for OWNER."
  (when-let* ((pending (nskk-dict-transaction--pending-rollback owner)))
    (nskk-dict-transaction--run-rollback owner (plist-get pending :primary)
                              (plist-get pending :restorers))))

(defun nskk-dict-transaction--ensure-rollback-complete (owner)
  "Retry OWNER rollback and signal if any region remains unrestored."
  (when-let* ((pending (nskk-dict-transaction--retry-pending-rollback owner)))
    (signal 'nskk-dict-rollback-incomplete
            (list (nskk-dict-transaction--rollback-diagnostic
                   owner (plist-get pending :primary)
                   (plist-get pending :failures))
                  (list :owner owner :primary (plist-get pending :primary)
                        :failures (plist-get pending :failures))))))

(defun nskk-dict-transaction--rollback-and-resignal (owner primary restorers)
  "Rollback OWNER with RESTORERS, then re-signal PRIMARY unchanged."
  (nskk-dict-transaction--run-rollback owner primary restorers)
  (signal (car primary) (cdr primary)))

(defconst nskk-dict-transaction--storage-missing (make-symbol "missing")
  "Sentinel for an absent predicate storage entry.")

(defun nskk-dict-transaction--predicate-snapshot (key)
  "Return an exact snapshot of Prolog storage entries for KEY only."
  (let ((missing nskk-dict-transaction--storage-missing))
    (vector missing key
            (gethash key (nskk-prolog-database) missing)
            (gethash key (nskk-prolog-database-tails) missing)
            (gethash key (nskk-prolog-index-config) missing)
            (gethash key (nskk-prolog-hash-indices) missing)
            (gethash key (nskk-prolog-trie-indices) missing)
            (gethash key (nskk-prolog-index-bucket-tail-cache) missing))))

(defun nskk-dict-transaction--apply-predicate-snapshot (snapshot)
  "Apply SNAPSHOT without changing storage for any other predicate."
  (let ((missing (aref snapshot 0))
        (key (aref snapshot 1))
        (inhibit-quit t))
    (dolist (entry
             (list (cons (nskk-prolog-database) (aref snapshot 2))
                   (cons (nskk-prolog-database-tails) (aref snapshot 3))
                   (cons (nskk-prolog-index-config) (aref snapshot 4))
                   (cons (nskk-prolog-hash-indices) (aref snapshot 5))
                   (cons (nskk-prolog-trie-indices) (aref snapshot 6))
                   (cons (nskk-prolog-index-bucket-tail-cache) (aref snapshot 7))))
      (if (eq (cdr entry) missing)
          (remhash key (car entry))
        (puthash key (cdr entry) (car entry))))))

(defun nskk-dict-transaction--pin-identity (attributes)
  "Return the hard-link identity fields from ATTRIBUTES."
  (and attributes
       (list (file-attribute-device-number attributes)
             (file-attribute-inode-number attributes)
             (file-attribute-size attributes)
             (file-attribute-modification-time attributes))))

(defun nskk-dict-transaction--full-identity (attributes)
  "Return all identity fields checked while reading ATTRIBUTES."
  (and attributes
       (append (nskk-dict-transaction--pin-identity attributes)
               (list (file-attribute-status-change-time attributes)))))

(defun nskk-dict-transaction--resolved-file (file)
  "Return the canonical path for FILE, or nil when it cannot be resolved."
  (condition-case nil
      (file-truename file)
    (file-error nil)))

(defun nskk-dict-transaction--parent-directories (path)
  "Return every parent directory of PATH from root to leaf."
  (let ((directory (directory-file-name
                    (file-name-directory (expand-file-name path))))
        parent
        result)
    (while directory
      (push directory result)
      (setq parent (directory-file-name (file-name-directory directory))
            directory (unless (equal parent directory) parent)))
    (nreverse result)))

(defconst nskk-dict-transaction--file-handler-operations
  '(insert-file-contents file-attributes file-truename file-writable-p
    file-symlink-p file-regular-p file-directory-p file-modes file-acl
    make-temp-file set-file-modes add-name-to-file delete-directory)
  "File operations used by pinned dictionary reads.")

(defun nskk-dict-transaction--local-unhandled-file-p (path)
  "Return non-nil when PATH is local and none of its operations are handled."
  (and (not (file-remote-p path))
       (not (cl-some (lambda (operation)
                       (find-file-name-handler path operation))
                     nskk-dict-transaction--file-handler-operations))))

(defun nskk-dict-transaction--safe-directory-controller-p (directory)
  "Return non-nil when DIRECTORY has a trusted entry controller."
  (condition-case nil
      (let* ((attributes
              (and (nskk-dict-transaction--local-unhandled-file-p directory)
                   (file-attributes directory 'integer)))
             (modes (and attributes (file-modes directory)))
             (acl (and attributes (file-acl directory)))
             (owner (and attributes (file-attribute-user-id attributes))))
        (and attributes
             (integerp modes)
             (file-directory-p directory)
             (null acl)
             (or (equal owner 0) (equal owner (user-uid)))
             (or (zerop (logand modes #o022))
                 (not (zerop (logand modes #o1000))))))
    (file-error nil)))

(defun nskk-dict-transaction--safe-directory-ancestry-p (directory)
  "Return non-nil when every lexical and canonical parent is controlled."
  (let ((canonical
         (and (nskk-dict-transaction--local-unhandled-file-p directory)
              (nskk-dict-transaction--resolved-file directory))))
    (and canonical
         (nskk-dict-transaction--local-unhandled-file-p canonical)
         (cl-every
          #'nskk-dict-transaction--safe-directory-controller-p
          (delete-dups
           (append (nskk-dict-transaction--parent-directories directory)
                   (nskk-dict-transaction--parent-directories canonical)))))))

(defun nskk-dict-transaction--safe-snapshot-base-p (directory)
  "Return non-nil when DIRECTORY can safely contain a temporary snapshot."
  (condition-case nil
      (let* ((attributes
              (and (nskk-dict-transaction--local-unhandled-file-p directory)
                   (file-attributes directory 'integer)))
             (modes (and attributes (file-modes directory)))
             (acl (and attributes (file-acl directory))))
        (and attributes
             (integerp modes)
             (file-directory-p directory)
             (file-writable-p directory)
             (null acl)
             (nskk-dict-transaction--safe-directory-ancestry-p directory)
             (or (and (equal (file-attribute-user-id attributes) (user-uid))
                      (zerop (logand modes #o022)))
                 (and (zerop (file-attribute-user-id attributes))
                      (not (zerop (logand modes #o1000)))))))
    (file-error nil)))

(defun nskk-dict-transaction--safe-source-snapshot-parent-p
    (directory source-attributes)
  "Return non-nil when DIRECTORY can snapshot a source with SOURCE-ATTRIBUTES."
  (condition-case nil
      (let* ((attributes
              (and (nskk-dict-transaction--local-unhandled-file-p directory)
                   (file-attributes directory 'integer)))
             (modes (and attributes (file-modes directory)))
             (acl (and attributes (file-acl directory))))
        (and attributes
             (integerp modes)
             (file-directory-p directory)
             (not (file-symlink-p directory))
             (file-writable-p directory)
             (null acl)
             (nskk-dict-transaction--safe-directory-ancestry-p directory)
             (equal (file-attribute-user-id attributes) (user-uid))
             (zerop (logand modes #o022))
             (equal (file-attribute-device-number attributes)
                    (file-attribute-device-number source-attributes))))
    (file-error nil)))

(defun nskk-dict-transaction--private-snapshot-directory-p (directory)
  "Return non-nil when DIRECTORY is a private snapshot directory."
  (condition-case nil
      (let* ((attributes
              (and (nskk-dict-transaction--local-unhandled-file-p directory)
                   (file-attributes directory 'integer)))
             (modes (and attributes (file-modes directory)))
             (acl (and attributes (file-acl directory))))
        (and attributes
             (integerp modes)
             (file-directory-p directory)
             (not (file-symlink-p directory))
             (null acl)
             (nskk-dict-transaction--safe-directory-ancestry-p directory)
             (equal (file-attribute-user-id attributes) (user-uid))
             (= (logand modes #o777) #o700)))
    (file-error nil)))

(defun nskk-dict-transaction--stable-direct-source-p
    (file resolved-file expected-identity max-bytes allow-symlink)
  "Return non-nil when FILE can be read directly without a pinned snapshot."
  (let* ((current-resolved (nskk-dict-transaction--resolved-file file))
         (current-attributes
          (and current-resolved (file-attributes resolved-file 'integer)))
         (current-size
          (and current-attributes (file-attribute-size current-attributes))))
    (and (nskk-dict-transaction--local-unhandled-file-p file)
         (nskk-dict-transaction--local-unhandled-file-p resolved-file)
         (nskk-dict-transaction--safe-directory-ancestry-p file)
         (nskk-dict-transaction--safe-directory-ancestry-p resolved-file)
         current-attributes
         (integerp current-size)
         (<= current-size max-bytes)
         (file-regular-p resolved-file)
         (or allow-symlink (not (file-symlink-p file)))
         (equal resolved-file current-resolved)
         (equal expected-identity
                (nskk-dict-transaction--full-identity current-attributes))
         (not (file-writable-p resolved-file))
         (cl-every
          (lambda (directory) (not (file-writable-p directory)))
          (append (nskk-dict-transaction--parent-directories file)
                  (nskk-dict-transaction--parent-directories resolved-file))))))

(defun nskk-dict-transaction--validate-pinned-snapshot
    (snapshot-file file resolved-file expected-pin-identity allow-symlink)
  "Validate SNAPSHOT-FILE before reading and return its full identity."
  (when (file-symlink-p snapshot-file)
    (error "NSKK: Pinned snapshot is a symbolic link"))
  (let* ((snapshot-attributes (file-attributes snapshot-file 'integer))
         (source-attributes (file-attributes resolved-file 'integer))
         (pinned-identity
          (nskk-dict-transaction--full-identity snapshot-attributes)))
    (unless
        (and snapshot-attributes
             source-attributes
             (file-regular-p snapshot-file)
             (file-regular-p resolved-file)
             (not (file-symlink-p snapshot-file))
             (or allow-symlink (not (file-symlink-p file)))
             (equal resolved-file
                    (nskk-dict-transaction--resolved-file file))
             (equal expected-pin-identity
                    (nskk-dict-transaction--pin-identity
                     snapshot-attributes))
             (equal pinned-identity
                    (nskk-dict-transaction--full-identity source-attributes)))
      (error "NSKK: File changed before pinned read"))
    pinned-identity))

(defun nskk-dict-transaction--pinned-snapshot-unchanged-p
    (snapshot-file file resolved-file pinned-identity allow-symlink)
  "Return non-nil when the pinned snapshot and its source are unchanged."
  (let ((snapshot-attributes (file-attributes snapshot-file 'integer))
        (source-attributes (file-attributes resolved-file 'integer)))
    (and snapshot-attributes
         source-attributes
         (file-regular-p snapshot-file)
         (file-regular-p resolved-file)
         (not (file-symlink-p snapshot-file))
         (or allow-symlink (not (file-symlink-p file)))
         (equal resolved-file
                (nskk-dict-transaction--resolved-file file))
         (equal pinned-identity
                (nskk-dict-transaction--full-identity snapshot-attributes))
         (equal pinned-identity
                (nskk-dict-transaction--full-identity source-attributes)))))

(defun nskk-dict-transaction--read-pinned-snapshot
    (snapshot-file file resolved-file expected-pin-identity max-bytes
                   allow-symlink)
  "Read and validate SNAPSHOT-FILE, returning unibyte contents."
  (let ((pinned-identity
         (nskk-dict-transaction--validate-pinned-snapshot
          snapshot-file file resolved-file expected-pin-identity
          allow-symlink)))
    (with-temp-buffer
     (set-buffer-multibyte nil)
     (let ((coding-system-for-read 'no-conversion))
       (insert-file-contents snapshot-file nil 0 (1+ max-bytes)))
     (when (> (buffer-size) max-bytes)
       (error "NSKK: File exceeds %d-byte limit" max-bytes))
     (unless (nskk-dict-transaction--pinned-snapshot-unchanged-p
              snapshot-file file resolved-file pinned-identity allow-symlink)
       (error "NSKK: File changed during pinned read"))
     (buffer-string))))

(defun nskk-dict-transaction--try-pinned-snapshot
    (snapshot-base file resolved-file expected-pin-identity max-bytes
                   allow-symlink)
  "Return (t . CONTENTS) after reading through SNAPSHOT-BASE, or nil."
  (let (snapshot-directory snapshot-file linked)
    (unwind-protect
        (progn
          (condition-case nil
              (progn
                (setq snapshot-directory
                      (make-temp-file
                       (expand-file-name "nskk-pinned-read-" snapshot-base)
                       t))
                (set-file-modes snapshot-directory #o700)
                (unless
                    (nskk-dict-transaction--private-snapshot-directory-p
                     snapshot-directory)
                  (error "NSKK: Snapshot directory is not private"))
                (setq snapshot-file
                      (expand-file-name "contents" snapshot-directory))
                (add-name-to-file resolved-file snapshot-file)
                (setq linked t))
            (error nil))
          (when linked
            (cons t
                  (nskk-dict-transaction--read-pinned-snapshot
                   snapshot-file file resolved-file expected-pin-identity
                   max-bytes allow-symlink))))
      (when snapshot-directory
        (ignore-errors (delete-directory snapshot-directory t))))))

(defun nskk-dict-transaction--read-stable-direct-source
    (file resolved-file expected-identity max-bytes allow-symlink)
  "Read an immutable source directly, returning unibyte contents."
  (unless (nskk-dict-transaction--stable-direct-source-p
           file resolved-file expected-identity max-bytes allow-symlink)
    (error "NSKK: Cannot safely read unpinned file"))
  (with-temp-buffer
   (set-buffer-multibyte nil)
   (let ((coding-system-for-read 'no-conversion))
     (insert-file-contents resolved-file nil 0 (1+ max-bytes)))
   (when (> (buffer-size) max-bytes)
     (error "NSKK: File exceeds %d-byte limit" max-bytes))
   (unless (nskk-dict-transaction--stable-direct-source-p
            file resolved-file expected-identity max-bytes allow-symlink)
     (error "NSKK: File changed during unpinned read"))
   (buffer-string)))

(defun nskk-dict-transaction--insert-file-contents-pinned
    (file resolved-file attributes max-bytes &optional allow-symlink)
  "Insert a validated regular FILE through a pinned hard-link snapshot.
RESOLVED-FILE and ATTRIBUTES describe the target validated by the caller.
Read at most MAX-BYTES bytes.  Reject symbolic FILE unless ALLOW-SYMLINK
is non-nil.  If no safe hard-link snapshot can be made, read directly only
when both the lexical and canonical local paths are immutable."
  (let* ((source-local-p
          (and (nskk-dict-transaction--local-unhandled-file-p file)
               (nskk-dict-transaction--local-unhandled-file-p resolved-file)))
         (size (and attributes (file-attribute-size attributes)))
         (expected-pin-identity
          (nskk-dict-transaction--pin-identity attributes))
         (expected-full-identity
          (nskk-dict-transaction--full-identity attributes))
         (source-snapshot-directories
          (and source-local-p
               (cl-remove-if-not
                (lambda (directory)
                  (nskk-dict-transaction--safe-source-snapshot-parent-p
                   directory attributes))
                (nskk-dict-transaction--parent-directories resolved-file))))
         (snapshot-bases
          (and source-local-p
               (cl-remove-if-not
                #'nskk-dict-transaction--safe-snapshot-base-p
                (delete-dups
                 (cons
                  (directory-file-name
                   (expand-file-name temporary-file-directory))
                  source-snapshot-directories))))))
    (unless (and source-local-p
                 attributes
                 expected-full-identity
                 (integerp size)
                 (integerp max-bytes)
                 (>= max-bytes 0)
                 (file-regular-p resolved-file)
                 (or allow-symlink (not (file-symlink-p file)))
                 (equal resolved-file
                        (nskk-dict-transaction--resolved-file file)))
      (error "NSKK: File is not a stable local regular file"))
    (when (> size max-bytes)
      (error "NSKK: File exceeds %d-byte limit" max-bytes))
    (let ((contents
           (catch 'read-through-snapshot
             (dolist (snapshot-base snapshot-bases)
               (let ((result
                      (nskk-dict-transaction--try-pinned-snapshot
                       snapshot-base file resolved-file expected-pin-identity
                       max-bytes allow-symlink)))
                 (when result
                   (throw 'read-through-snapshot (cdr result)))))
             (nskk-dict-transaction--read-stable-direct-source
              file resolved-file expected-full-identity max-bytes
              allow-symlink))))
      (set-buffer-multibyte nil)
      (insert contents))))


(defalias 'nskk-dict-transaction-predicate-snapshot 'nskk-dict-transaction--predicate-snapshot)
(defalias 'nskk-dict-transaction-apply-predicate-snapshot 'nskk-dict-transaction--apply-predicate-snapshot)
(defalias 'nskk-dict-transaction-ensure-rollback-complete 'nskk-dict-transaction--ensure-rollback-complete)
(defalias 'nskk-dict-transaction-clear-pending-rollback 'nskk-dict-transaction--clear-pending-rollback)
(defalias 'nskk-dict-transaction-rollback-and-resignal 'nskk-dict-transaction--rollback-and-resignal)
(defalias 'nskk-dict-transaction-insert-file-contents-pinned 'nskk-dict-transaction--insert-file-contents-pinned)
(defalias 'nskk-dict-transaction-pending-rollback 'nskk-dict-transaction--pending-rollback)
(defalias 'nskk-dict-transaction-retry-pending-rollback 'nskk-dict-transaction--retry-pending-rollback)

(provide 'nskk-dict-transaction)
;;; nskk-dict-transaction.el ends here
