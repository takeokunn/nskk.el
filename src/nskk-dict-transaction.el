;;; nskk-dict-transaction.el --- Public dictionary transaction API -*- lexical-binding: t; -*-

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

;; Layer position: L1 (Core Engine) -- depends on nskk-prolog, and on
;;   nskk-cps-macros at compile time only.
;;
;; Two facilities are provided.  Predicate snapshots and rollback let a
;; caller restore exact Prolog storage after a failed publication.  Pinned
;; reads insert a dictionary file's bytes only when the path that was
;; validated is provably the path that is read, preferring a hard-link
;; snapshot in a directory no other user controls and falling back to a
;; direct read only from an immutable location.

;;; Code:

(require 'cl-lib)
(require 'nskk-prolog)
(eval-when-compile (require 'nskk-cps-macros))

(define-error 'nskk-dict-error "Dictionary error")

;; One level, unlike the two-level `nskk-dict-<domain>-<condition>' scheme the
;; rest of this block follows: the name predates that scheme and is asserted on
;; by `:type' in the test suite, so renaming it would be a breaking change.
(define-error 'nskk-dict-rollback-incomplete
              "Dictionary rollback remains incomplete"
              'nskk-dict-error)

(define-error 'nskk-dict-serialization-error
              "Dictionary serialization error"
              'nskk-dict-error)

(define-error 'nskk-dict-serialization-trailing-data
              "Serialized dictionary holds more than one data form"
              'nskk-dict-serialization-error)

(define-error 'nskk-dict-serialization-malformed
              "Serialized dictionary data is not a proper list"
              'nskk-dict-serialization-error)

;; `file-error' is a co-parent so that `error-message-string' renders these
;; the way the untyped `error' calls they replaced did.  Without it the
;; condition's own message is interposed and the datum is printed with
;; `prin1', so a user sees Dictionary file exceeds the read limit: "NSKK: ..."
;; -- quotes and all -- instead of the message the caller wrote.
(define-error 'nskk-dict-pin-error
              "Dictionary pinned read error"
              '(nskk-dict-error file-error))

(define-error 'nskk-dict-pin-unstable-source
              "Dictionary source cannot be read stably"
              'nskk-dict-pin-error)

(define-error 'nskk-dict-pin-size-exceeded
              "Dictionary file exceeds the read limit"
              'nskk-dict-pin-error)

(define-error 'nskk-dict-pin-source-changed
              "Dictionary file changed across a pinned read"
              'nskk-dict-pin-error)

(define-error 'nskk-dict-pin-snapshot-symlink
              "Pinned dictionary snapshot is a symbolic link"
              'nskk-dict-pin-error)

(defun nskk-dict-transaction--at-input-end-p ()
  "Return non-nil when only whitespace and comments follow point.
This is the question `read' answers by signalling `end-of-file', asked
without running the reader a second time over attacker-controlled bytes."
  ;; The reader skips every character up to and including space, plus
  ;; NO_BREAK_SPACE.  Matching that set exactly is what stops this predicate
  ;; rejecting a dictionary `read' would have accepted -- a trailing NBSP is
  ;; the realistic case, since `decode-coding-region' turns UTF-8 C2 A0 into
  ;; one.  Narrower than the reader means a valid dictionary fails to load.
  (skip-chars-forward "\0-\s ")
  (while (and (not (eobp)) (eq (char-after) ?\;))
    (forward-line 1)
    (skip-chars-forward "\0-\s "))
  (eobp))

(defun nskk-dict-transaction-read-entries (file truename attributes max-size parser)
  "Read FILE transactionally and transform each entry with PARSER.
TRUENAME and ATTRIBUTES describe the target the caller already validated,
and at most MAX-SIZE bytes are read.  PARSER is called for each entry in
the single serialized data form."
  ;; `read-circle' is a live dynamic variable, so a crafted dictionary could
  ;; otherwise use #N= / #N# to hand back shared or circular structure that
  ;; PARSER and every later traversal fail to terminate on.
  (let ((read-circle nil))
    (with-temp-buffer
      (nskk-dict-transaction-insert-file-contents-pinned
       file truename attributes max-size)
      (set-buffer-multibyte t)
      (decode-coding-region (point-min) (point-max) 'undecided)
      (goto-char (point-min))
      (let ((data (read (current-buffer))))
        (unless (nskk-dict-transaction--at-input-end-p)
          (signal 'nskk-dict-serialization-trailing-data
                  (list "Expected exactly one data form")))
        (unless (proper-list-p data)
          (signal 'nskk-dict-serialization-malformed
                  (list (format "Expected proper list, got %s" (type-of data)))))
        (mapcar parser data)))))

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

(defun nskk-dict-transaction-pending-rollback (owner)
  "Return pending rollback state for OWNER, or nil."
  (gethash owner nskk-dict-transaction--pending-rollbacks))

(defun nskk-dict-transaction-clear-pending-rollback (owner)
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
      (nskk-dict-transaction-clear-pending-rollback owner)
      nil)))

(defun nskk-dict-transaction-retry-pending-rollback (owner)
  "Retry retained failed rollback regions for OWNER."
  (when-let* ((pending (nskk-dict-transaction-pending-rollback owner)))
    (nskk-dict-transaction--run-rollback owner (plist-get pending :primary)
                              (plist-get pending :restorers))))

(defun nskk-dict-transaction-ensure-rollback-complete (owner)
  "Retry OWNER rollback and signal if any region remains unrestored."
  (when-let* ((pending (nskk-dict-transaction-retry-pending-rollback owner)))
    (signal 'nskk-dict-rollback-incomplete
            (list (nskk-dict-transaction--rollback-diagnostic
                   owner (plist-get pending :primary)
                   (plist-get pending :failures))
                  (list :owner owner :primary (plist-get pending :primary)
                        :failures (plist-get pending :failures))))))

(defun nskk-dict-transaction-rollback-and-resignal (owner primary restorers)
  "Rollback OWNER with RESTORERS, then re-signal PRIMARY unchanged."
  (nskk-dict-transaction--run-rollback owner primary restorers)
  (signal (car primary) (cdr primary)))

(defconst nskk-dict-transaction--storage-missing (make-symbol "missing")
  "Sentinel for an absent predicate storage entry.
Stays private because its identity never has to cross a module boundary:
it is stored at index 0 of every snapshot vector, so
`nskk-dict-transaction-apply-predicate-snapshot' recovers it from the
snapshot rather than reading this constant.  A module that probes the same
tables owns its own sentinel, as `nskk--prolog-cache-missing' does.")

(defun nskk-dict-transaction-predicate-snapshot (key)
  "Return an exact snapshot of Prolog storage entries for KEY only."
  (let ((missing nskk-dict-transaction--storage-missing))
    (vector missing key
            (gethash key (nskk-prolog-database) missing)
            (gethash key (nskk-prolog-database-tails) missing)
            (gethash key (nskk-prolog-index-config) missing)
            (gethash key (nskk-prolog-hash-indices) missing)
            (gethash key (nskk-prolog-trie-indices) missing)
            (gethash key (nskk-prolog-index-bucket-tail-cache) missing))))

(defun nskk-dict-transaction-apply-predicate-snapshot (snapshot)
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

(defun nskk-dict-transaction--directory-satisfies-p (directory predicate)
  "Call PREDICATE with DIRECTORY's attributes and mode bits, or return nil.
PREDICATE is reached only for a local, unhandled, ACL-free directory, which
`file-directory-p' resolves, so a symbolic link to one qualifies here and any
caller that must reject links tests for that itself.
Those three are prerequisites shared by every caller rather than part of any
one threat model: a handled name routes the later stat and read through
different code, and an ACL can grant write access the mode bits do not show,
so neither can be reasoned about from modes alone.  A `file-error' means the
question could not be answered, which fails closed."
  (condition-case nil
      (let* ((attributes
              (and (nskk-dict-transaction--local-unhandled-file-p directory)
                   (file-attributes directory 'integer)))
             (modes (and attributes (file-modes directory)))
             (acl (and attributes (file-acl directory))))
        (and attributes
             (integerp modes)
             (file-directory-p directory)
             (null acl)
             (funcall predicate attributes modes)))
    (file-error nil)))

(defun nskk-dict-transaction--safe-directory-controller-p (directory)
  "Return non-nil when DIRECTORY has a trusted entry controller.
Only root or this user may create entries here, because a third party who
can write the directory could substitute the entry between validation and
read.  A sticky bit substitutes for exclusive write, since it stops one user
renaming or unlinking another's entry."
  (nskk-dict-transaction--directory-satisfies-p
   directory
   (lambda (attributes modes)
     (let ((owner (file-attribute-user-id attributes)))
       (and (or (equal owner 0) (equal owner (user-uid)))
            (or (zerop (logand modes #o022))
                (not (zerop (logand modes #o1000)))))))))

(defun nskk-dict-transaction--safe-directory-ancestry-p (directory)
  "Return non-nil when every parent of DIRECTORY is controlled.
Both the lexical parents and those of the canonical path are checked,
because a symbolic link anywhere along the way puts the real entry under a
directory the lexical walk never visits."
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
  "Return non-nil when DIRECTORY can safely contain a temporary snapshot.
Writability is required because a snapshot is created here.  Either this
user owns it and no one else may write, or root owns it and the sticky bit
protects entries, which is what makes a shared temporary directory usable."
  (nskk-dict-transaction--directory-satisfies-p
   directory
   (lambda (attributes modes)
     (and (file-writable-p directory)
          (nskk-dict-transaction--safe-directory-ancestry-p directory)
          (or (and (equal (file-attribute-user-id attributes) (user-uid))
                   (zerop (logand modes #o022)))
              (and (zerop (file-attribute-user-id attributes))
                   (not (zerop (logand modes #o1000)))))))))

(defun nskk-dict-transaction--safe-source-snapshot-parent-p
    (directory source-attributes)
  "Return non-nil when DIRECTORY can snapshot a source with SOURCE-ATTRIBUTES.
Stricter than a plain snapshot base: this user must own it outright, and its
device must match the source's, because a hard link cannot cross devices and
a mismatch would silently fall back to a copy that is not pinned."
  (nskk-dict-transaction--directory-satisfies-p
   directory
   (lambda (attributes modes)
     (and (not (file-symlink-p directory))
          (file-writable-p directory)
          (nskk-dict-transaction--safe-directory-ancestry-p directory)
          (equal (file-attribute-user-id attributes) (user-uid))
          (zerop (logand modes #o022))
          (equal (file-attribute-device-number attributes)
                 (file-attribute-device-number source-attributes))))))

(defun nskk-dict-transaction--private-snapshot-directory-p (directory)
  "Return non-nil when DIRECTORY is a private snapshot directory.
Exactly 0700 and owned by this user: anything readable by others would
expose the snapshot's contents for as long as the read takes."
  (nskk-dict-transaction--directory-satisfies-p
   directory
   (lambda (attributes modes)
     (and (not (file-symlink-p directory))
          (nskk-dict-transaction--safe-directory-ancestry-p directory)
          (equal (file-attribute-user-id attributes) (user-uid))
          (= (logand modes #o777) #o700)))))

(defun nskk-dict-transaction--stable-direct-source-p
    (file resolved-file expected-identity max-bytes allow-symlink)
  "Return non-nil when FILE can be read directly without a pinned snapshot.
RESOLVED-FILE is FILE's canonical target and must still match EXPECTED-IDENTITY
within MAX-BYTES.  A symbolic FILE is rejected unless ALLOW-SYMLINK is non-nil.
Nothing along either path may be writable, since that is what makes the read
reproducible without a snapshot to pin it."
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

(defun nskk-dict-transaction--pinned-snapshot-shape-ok-p
    (snapshot-file file resolved-file snapshot-attributes source-attributes
                   allow-symlink)
  "Return non-nil when snapshot and source still have the required shape.
SNAPSHOT-ATTRIBUTES and SOURCE-ATTRIBUTES describe SNAPSHOT-FILE and
RESOLVED-FILE.  Covers only what the checks before and after a pinned read
both demand: both paths are regular files, SNAPSHOT-FILE is not a symlink,
FILE is one only when ALLOW-SYMLINK permits, and FILE still resolves to
RESOLVED-FILE.  Which identity to compare is left to the caller, because
that is the one question the two checks answer differently."
  (and snapshot-attributes
       source-attributes
       (file-regular-p snapshot-file)
       (file-regular-p resolved-file)
       (not (file-symlink-p snapshot-file))
       (or allow-symlink (not (file-symlink-p file)))
       (equal resolved-file
              (nskk-dict-transaction--resolved-file file))))

(defun nskk-dict-transaction--validate-pinned-snapshot
    (snapshot-file file resolved-file expected-pin-identity allow-symlink)
  "Validate SNAPSHOT-FILE before reading and return its full identity.
SNAPSHOT-FILE must still carry EXPECTED-PIN-IDENTITY and share its identity
with RESOLVED-FILE, the canonical target of FILE.  A symbolic FILE is
rejected unless ALLOW-SYMLINK is non-nil."
  (when (file-symlink-p snapshot-file)
    (signal 'nskk-dict-pin-snapshot-symlink
            (list "NSKK: Pinned snapshot is a symbolic link")))
  (let* ((snapshot-attributes (file-attributes snapshot-file 'integer))
         (source-attributes (file-attributes resolved-file 'integer))
         (pinned-identity
          (nskk-dict-transaction--full-identity snapshot-attributes)))
    (unless
        (and (nskk-dict-transaction--pinned-snapshot-shape-ok-p
              snapshot-file file resolved-file snapshot-attributes
              source-attributes allow-symlink)
             (equal expected-pin-identity
                    (nskk-dict-transaction--pin-identity
                     snapshot-attributes))
             (equal pinned-identity
                    (nskk-dict-transaction--full-identity source-attributes)))
      (signal 'nskk-dict-pin-source-changed
              (list "NSKK: File changed before pinned read")))
    pinned-identity))

(defun nskk-dict-transaction--pinned-snapshot-unchanged-p
    (snapshot-file file resolved-file pinned-identity allow-symlink)
  "Return non-nil when the pinned snapshot and its source are unchanged.
Re-checks after the bytes are read that SNAPSHOT-FILE and RESOLVED-FILE, the
canonical target of FILE, both still carry PINNED-IDENTITY.  A symbolic FILE
is rejected unless ALLOW-SYMLINK is non-nil."
  (let ((snapshot-attributes (file-attributes snapshot-file 'integer))
        (source-attributes (file-attributes resolved-file 'integer)))
    (and (nskk-dict-transaction--pinned-snapshot-shape-ok-p
          snapshot-file file resolved-file snapshot-attributes
          source-attributes allow-symlink)
         (equal pinned-identity
                (nskk-dict-transaction--full-identity snapshot-attributes))
         (equal pinned-identity
                (nskk-dict-transaction--full-identity source-attributes)))))

(defun nskk-dict-transaction--read-pinned-snapshot
    (snapshot-file file resolved-file expected-pin-identity max-bytes
                   allow-symlink)
  "Read and validate SNAPSHOT-FILE, returning unibyte contents.
SNAPSHOT-FILE must carry EXPECTED-PIN-IDENTITY before the read; afterwards it
and RESOLVED-FILE, the canonical target of FILE, must both still carry the
full identity captured at that point, so a change that races the read is
caught rather than returned.  At most MAX-BYTES bytes are accepted; a
symbolic FILE is rejected unless ALLOW-SYMLINK is non-nil."
  (let ((pinned-identity
         (nskk-dict-transaction--validate-pinned-snapshot
          snapshot-file file resolved-file expected-pin-identity
          allow-symlink)))
    (with-temp-buffer
     (set-buffer-multibyte nil)
     (let ((coding-system-for-read 'no-conversion))
       (insert-file-contents snapshot-file nil 0 (1+ max-bytes)))
     (when (> (buffer-size) max-bytes)
       (signal 'nskk-dict-pin-size-exceeded
               (list (format "NSKK: File exceeds %d-byte limit" max-bytes))))
     (unless (nskk-dict-transaction--pinned-snapshot-unchanged-p
              snapshot-file file resolved-file pinned-identity allow-symlink)
       (signal 'nskk-dict-pin-source-changed
               (list "NSKK: File changed during pinned read")))
     (buffer-string))))

(defun nskk-dict-transaction--make-snapshot-directory (snapshot-base)
  "Create a temporary directory under SNAPSHOT-BASE, or nil on failure."
  (condition-case nil
      (make-temp-file
       (expand-file-name "nskk-pinned-read-" snapshot-base)
       t)
    (error nil)))

(defun nskk-dict-transaction--link-pinned-snapshot
    (snapshot-directory resolved-file)
  "Hard-link RESOLVED-FILE inside SNAPSHOT-DIRECTORY and return the link.
Return nil when SNAPSHOT-DIRECTORY cannot be made private or the link
cannot be created.  Every error here is absorbed into nil because an
unusable base is a fallback condition, not evidence of interference; the
caller moves on to the next base and still deletes SNAPSHOT-DIRECTORY.
A non-private directory is reported the same way, as a plain nil, for the
same reason: no caller can observe the difference."
  (condition-case nil
      (let ((snapshot-file (expand-file-name "contents" snapshot-directory)))
        (set-file-modes snapshot-directory #o700)
        (when (nskk-dict-transaction--private-snapshot-directory-p
               snapshot-directory)
          (add-name-to-file resolved-file snapshot-file)
          snapshot-file))
    (error nil)))

(defun nskk-dict-transaction--read-through-snapshot-directory
    (snapshot-directory file resolved-file expected-pin-identity max-bytes
                        allow-symlink)
  "Read RESOLVED-FILE through a hard link made in SNAPSHOT-DIRECTORY.
Return nil when SNAPSHOT-DIRECTORY is nil or cannot hold the link.  FILE,
EXPECTED-PIN-IDENTITY, MAX-BYTES and ALLOW-SYMLINK are passed through to the
validating read."
  (when-let* ((snapshot-file
               (and snapshot-directory
                    (nskk-dict-transaction--link-pinned-snapshot
                     snapshot-directory resolved-file))))
    (nskk-dict-transaction--read-pinned-snapshot
     snapshot-file file resolved-file expected-pin-identity max-bytes
     allow-symlink)))

(defun nskk-dict-transaction--try-pinned-snapshot
    (snapshot-base file resolved-file expected-pin-identity max-bytes
                   allow-symlink)
  "Return the contents of RESOLVED-FILE read through SNAPSHOT-BASE, or nil.
Nil means SNAPSHOT-BASE could not host a snapshot at all, so the caller
should try the next base.  A snapshot that is created and then fails
validation signals instead, because that is evidence of interference.
FILE, EXPECTED-PIN-IDENTITY, MAX-BYTES and ALLOW-SYMLINK are passed through
to the validating read."
  (let ((snapshot-directory
         (nskk-dict-transaction--make-snapshot-directory snapshot-base)))
    (unwind-protect
        (nskk-dict-transaction--read-through-snapshot-directory
         snapshot-directory file resolved-file expected-pin-identity
         max-bytes allow-symlink)
      (when snapshot-directory
        (ignore-errors (delete-directory snapshot-directory t))))))

(defun nskk-dict-transaction--read-stable-direct-source
    (file resolved-file expected-identity max-bytes allow-symlink)
  "Read an immutable source directly, returning unibyte contents.
Used only when no snapshot base was usable.  FILE and its canonical target
RESOLVED-FILE must match EXPECTED-IDENTITY both before and after the read,
which is what stands in for the absent snapshot.  At most MAX-BYTES bytes
are accepted; a symbolic FILE is rejected unless ALLOW-SYMLINK is non-nil."
  (unless (nskk-dict-transaction--stable-direct-source-p
           file resolved-file expected-identity max-bytes allow-symlink)
    (signal 'nskk-dict-pin-unstable-source
            (list "NSKK: Cannot safely read unpinned file")))
  (with-temp-buffer
   (set-buffer-multibyte nil)
   (let ((coding-system-for-read 'no-conversion))
     (insert-file-contents resolved-file nil 0 (1+ max-bytes)))
   (when (> (buffer-size) max-bytes)
     (signal 'nskk-dict-pin-size-exceeded
             (list (format "NSKK: File exceeds %d-byte limit" max-bytes))))
   (unless (nskk-dict-transaction--stable-direct-source-p
            file resolved-file expected-identity max-bytes allow-symlink)
     (signal 'nskk-dict-pin-source-changed
             (list "NSKK: File changed during unpinned read")))
   (buffer-string)))

(defun nskk-dict-transaction--snapshot-bases (resolved-file attributes)
  "Return directories that can host a hard-link snapshot of RESOLVED-FILE.
ATTRIBUTES describes RESOLVED-FILE.  The system temporary directory is
tried first; a parent of the source qualifies only under the stricter rule
in `nskk-dict-transaction--safe-source-snapshot-parent-p'."
  (cl-remove-if-not
   #'nskk-dict-transaction--safe-snapshot-base-p
   (delete-dups
    (cons
     (directory-file-name (expand-file-name temporary-file-directory))
     (cl-remove-if-not
      (lambda (directory)
        (nskk-dict-transaction--safe-source-snapshot-parent-p
         directory attributes))
      (nskk-dict-transaction--parent-directories resolved-file))))))

(defun nskk-dict-transaction--validate-pinned-source
    (file resolved-file attributes max-bytes source-local-p allow-symlink)
  "Signal unless FILE is a stable local regular file within MAX-BYTES.
RESOLVED-FILE and ATTRIBUTES describe the target the caller validated, and
SOURCE-LOCAL-P records that both names are local and unhandled.  Reject a
symbolic FILE unless ALLOW-SYMLINK is non-nil."
  (let ((size (and attributes (file-attribute-size attributes))))
    (unless (and source-local-p
                 attributes
                 (nskk-dict-transaction--full-identity attributes)
                 (integerp size)
                 (integerp max-bytes)
                 (>= max-bytes 0)
                 (file-regular-p resolved-file)
                 (or allow-symlink (not (file-symlink-p file)))
                 (equal resolved-file
                        (nskk-dict-transaction--resolved-file file)))
      (signal 'nskk-dict-pin-unstable-source
              (list "NSKK: File is not a stable local regular file")))
    (when (> size max-bytes)
      (signal 'nskk-dict-pin-size-exceeded
              (list (format "NSKK: File exceeds %d-byte limit" max-bytes))))))

(defun/k nskk-dict-transaction--snapshot-contents
  (snapshot-bases file resolved-file expected-pin-identity max-bytes
                  allow-symlink)
  "Return contents read through the first usable base in SNAPSHOT-BASES.
Fail when no base can host a snapshot at all, which leaves the caller to
decide whether an unpinned read is admissible.  A base that hosts a
snapshot and then fails validation signals rather than failing over."
  ;; `dolist' is not a form the CPS transformer rewrites, so (succeed ...)
  ;; inside it would survive as a call to an undefined function.  `escape'
  ;; binds K to an ordinary lambda, which is callable from anywhere.
  (escape found
    (dolist (snapshot-base snapshot-bases)
      (when-let* ((contents (nskk-dict-transaction--try-pinned-snapshot
                             snapshot-base file resolved-file
                             expected-pin-identity max-bytes allow-symlink)))
        (funcall found contents)))
    (fail)))

(defun nskk-dict-transaction-insert-file-contents-pinned
    (file resolved-file attributes max-bytes &optional allow-symlink)
  "Insert a validated regular FILE through a pinned hard-link snapshot.
RESOLVED-FILE and ATTRIBUTES describe the target validated by the caller.
Read at most MAX-BYTES bytes.  Reject symbolic FILE unless ALLOW-SYMLINK
is non-nil.  If no safe hard-link snapshot can be made, read directly only
when both the lexical and canonical local paths are immutable."
  (let* ((source-local-p
          (and (nskk-dict-transaction--local-unhandled-file-p file)
               (nskk-dict-transaction--local-unhandled-file-p resolved-file)))
         (expected-pin-identity
          (nskk-dict-transaction--pin-identity attributes))
         (expected-full-identity
          (nskk-dict-transaction--full-identity attributes))
         (snapshot-bases
          (and source-local-p
               (nskk-dict-transaction--snapshot-bases resolved-file attributes))))
    (nskk-dict-transaction--validate-pinned-source
     file resolved-file attributes max-bytes source-local-p allow-symlink)
    (let ((contents
           (or (nskk-dict-transaction--snapshot-contents
                snapshot-bases file resolved-file expected-pin-identity
                max-bytes allow-symlink)
               (nskk-dict-transaction--read-stable-direct-source
                file resolved-file expected-full-identity max-bytes
                allow-symlink))))
      (set-buffer-multibyte nil)
      (insert contents))))

(provide 'nskk-dict-transaction)
;;; nskk-dict-transaction.el ends here
