;;; nskk-trie.el --- Trie data structure for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

;; Author: takeokunn <bararararatty@gmail.com>
;; Maintainer: takeokunn <bararararatty@gmail.com>
;; URL: https://github.com/takeokunn/nskk.el
;; Version: 0.1.0
;; Keywords: i18n

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Trie (prefix tree) data structure for NSKK (Layer 0: Foundation).
;;
;; Layer position: L0 (Foundation) -- compile-time dependency on
;; nskk-cps-macros (also L0); no runtime nskk-* dependencies.
;;
;; Provides an efficient character-keyed trie for string prefix lookup,
;; insertion, deletion, and prefix search operations used by the dictionary
;; and Prolog index layers.

;;; Code:

(require 'cl-lib)
(eval-when-compile (require 'nskk-cps-macros))

;;;; Trie Node Structure

(cl-defstruct (nskk-trie-node
               (:constructor nskk-trie-node--create)
               (:copier nil))
  "Trie node for NSKK.
Slots:
  children - hash-table of child nodes (char -> node)
  value    - value stored at this node (terminal nodes only)
  is-end   - non-nil if this node terminates a key
  count    - number of keys passing through this node"
  (children nil :type (or null hash-table))
  (value nil)
  (is-end nil :type boolean)
  (count 0 :type integer))

(cl-defstruct (nskk-trie
               (:constructor nskk-trie--create-internal)
               (:copier nil))
  "Trie structure for NSKK.
Slots:
  root  - root node
  size  - total number of stored keys"
  (root nil :type nskk-trie-node)
  (size 0 :type integer))

;;;; Trie Operations

(defun nskk-trie-create ()
  "Create and return a new empty trie."
  (nskk-trie--create-internal
   :root (nskk-trie-node--create)
   :size 0))

(defsubst nskk--trie-node-leaf-p (node)
  "Return non-nil if NODE is a childless non-terminal (safe to prune)."
  (and (not (nskk-trie-node-is-end node))
       (or (null (nskk-trie-node-children node))
           (zerop (hash-table-count (nskk-trie-node-children node))))))

(defconst nskk--trie-initial-children-size 50
  "Initial hash-table capacity for trie node children.
Romaji trie nodes typically branch over the ASCII letter set (26-52
entries); 50 avoids early rehash for most nodes while keeping memory
modest for leaf-heavy subtrees.")

(defun nskk--trie-get-or-create-child (node char)
  "Return the child of NODE for CHAR, creating it if absent."
  (unless (nskk-trie-node-children node)
    (setf (nskk-trie-node-children node)
          (make-hash-table :test 'eq :size nskk--trie-initial-children-size)))
  (let ((child (gethash char (nskk-trie-node-children node))))
    (unless child
      (setq child (nskk-trie-node--create))
      (puthash char child (nskk-trie-node-children node)))
    (cl-incf (nskk-trie-node-count child))
    child))

(defun nskk-trie-insert (trie key value)
  "Insert KEY with VALUE into TRIE."
  (unless (stringp key)
    (error "Key must be a string: %s" key))
  (when (zerop (length key))
    (error "Key cannot be empty"))
  (let ((node (nskk-trie-root trie))
        (was-new nil))
    (dotimes (i (length key))
      (setq node (nskk--trie-get-or-create-child node (aref key i))))
    (setq was-new (not (nskk-trie-node-is-end node)))
    (setf (nskk-trie-node-is-end node) t)
    (setf (nskk-trie-node-value node) value)
    (when was-new
      (cl-incf (nskk-trie-size trie)))
    trie))

(defun nskk--trie-find-node (trie key)
  "Return the trie node for KEY in TRIE, or nil if not found."
  (let ((node (nskk-trie-root trie))
        (i 0)
        (len (length key)))
    (while (and (< i len)
                (nskk-trie-node-children node)
                (setq node (gethash (aref key i) (nskk-trie-node-children node))))
      (cl-incf i))
    (and (= i len) node)))

(defun/k nskk-trie-lookup (trie key)
  "Look up KEY in TRIE.
Sync wrapper returns the value if found, nil otherwise.
To distinguish a stored nil from not-found, use the /k variant."
  (unless (stringp key)
    (error "Key must be a string: %s" key))
  (let ((node (nskk--trie-find-node trie key)))
    (if (and node (nskk-trie-node-is-end node))
        (succeed (nskk-trie-node-value node))
      (fail))))

(defun nskk--trie-cleanup-path (trie key)
  "Remove orphaned nodes along the path for KEY in TRIE."
  (let ((root (nskk-trie-root trie)))
    (cl-labels
        ((prune (node depth)
           (when (< depth (length key))
             (let* ((char (aref key depth))
                    (children (nskk-trie-node-children node))
                    (child (and children (gethash char children))))
               (when child
                 (prune child (1+ depth))
                 (when (nskk--trie-node-leaf-p child)
                   (remhash char children)))))))
      (prune root 0))))

(defun nskk-trie-delete (trie key)
  "Delete KEY from TRIE.
Returns t if deleted, nil if not present."
  (unless (stringp key)
    (error "Key must be a string: %s" key))
  (let ((node (nskk--trie-find-node trie key)))
    (when (and node (nskk-trie-node-is-end node))
      (setf (nskk-trie-node-is-end node) nil)
      (setf (nskk-trie-node-value node) nil)
      (cl-decf (nskk-trie-size trie))
      (nskk--trie-cleanup-path trie key)
      t)))

(defun nskk--trie-collect-all (node prefix limit)
  "Collect all (key . value) pairs reachable from NODE with PREFIX.
LIMIT is the maximum number of pairs to collect, or nil for no limit.
Uses an iterative DFS with an explicit stack."
  (let ((stack (list (cons node prefix)))
        (results nil)
        (count 0))
    (while (and stack (or (null limit) (< count limit)))
      (let* ((entry (pop stack))
             (n (car entry))
             (pfx (cdr entry)))
        (when (nskk-trie-node-is-end n)
          (push (cons pfx (nskk-trie-node-value n)) results)
          (cl-incf count))
        (when-let* ((children (nskk-trie-node-children n)))
          (maphash (lambda (char child)
                     (push (cons child (concat pfx (char-to-string char)))
                           stack))
                   children))))
    (nreverse results)))

(defun nskk-trie-prefix-search (trie prefix &optional limit)
  "Search TRIE for all keys starting with PREFIX.
Returns a list of (key . value) pairs.
If LIMIT is non-nil, return at most LIMIT results."
  (unless (stringp prefix)
    (error "Prefix must be a string: %s" prefix))
  (let ((node (if (zerop (length prefix))
                  (nskk-trie-root trie)
                (nskk--trie-find-node trie prefix))))
    (when node
      (nskk--trie-collect-all node prefix limit))))

(defun/k nskk-trie-longest-match (trie input)
  "Walk TRIE character-by-character over INPUT, return longest match.
INPUT is a string.  Sync wrapper returns (VALUE . CONSUMED-LENGTH) or nil.
To distinguish stored nil from no match, use the /k variant.
O(k) where k = length of INPUT."
  (unless (stringp input)
    (error "Input must be a string: %s" input))
  (let ((node (nskk-trie-root trie))
        (best nil)
        (i 0)
        (len (length input)))
    (while (and (< i len)
                (nskk-trie-node-children node)
                (setq node (gethash (aref input i) (nskk-trie-node-children node))))
      (cl-incf i)
      (when (nskk-trie-node-is-end node)
        (setq best (cons (nskk-trie-node-value node) i))))
    (if best
        (succeed best)
      (fail))))

(defun nskk-trie-has-prefix-p (trie prefix)
  "Return non-nil if PREFIX leads to a node in TRIE (has children or is-end).
This means PREFIX is either a complete key or a proper prefix of some key.
O(k) where k = length of PREFIX."
  (unless (stringp prefix)
    (error "Prefix must be a string: %s" prefix))
  (if (zerop (length prefix))
      (nskk-trie-root trie)
    (nskk--trie-find-node trie prefix)))

(provide 'nskk-trie)

;;; nskk-trie.el ends here
