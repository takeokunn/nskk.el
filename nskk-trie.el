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
;; Layer position: L0 (Foundation) -- no dependencies on other nskk-* modules.
;;
;; Provides an efficient character-keyed trie for string prefix lookup,
;; insertion, deletion, and prefix search operations used by the dictionary
;; and Prolog index layers.

;;; Code:

(require 'cl-lib)

;;;; Trie Node Structure

(cl-defstruct (nskk-trie-node
               (:constructor nskk-trie-node--create)
               (:copier nil))
  "Trie node for NSKK.
Slots:
  char     - character this node represents (nil for root)
  children - hash-table of child nodes (char -> node)
  value    - value stored at this node (terminal nodes only)
  is-end   - non-nil if this node terminates a key
  count    - number of keys passing through this node"
  (char nil :type (or null character))
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

(defun nskk--trie-get-or-create-child (node char)
  "Return the child of NODE for CHAR, creating it if absent."
  (unless (nskk-trie-node-children node)
    (setf (nskk-trie-node-children node)
          (make-hash-table :test 'eq :size 50)))
  (let ((child (gethash char (nskk-trie-node-children node))))
    (unless child
      (setq child (nskk-trie-node--create :char char))
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
        (key-len (length key)))
    (catch 'not-found
      (dotimes (i key-len)
        (let ((char (aref key i)))
          (unless (and (nskk-trie-node-children node)
                       (setq node (gethash char (nskk-trie-node-children node))))
            (throw 'not-found nil))))
      node)))

(defun nskk-trie-lookup (trie key)
  "Look up KEY in TRIE.
Returns (value . t) if found, (nil . nil) otherwise."
  (unless (stringp key)
    (error "Key must be a string: %s" key))
  (let ((node (nskk--trie-find-node trie key)))
    (if (and node (nskk-trie-node-is-end node))
        (cons (nskk-trie-node-value node) t)
      (cons nil nil))))

(defun nskk--trie-cleanup-path (trie key)
  "Remove leaf nodes no longer needed after deleting KEY from TRIE."
  (let ((node (nskk-trie-root trie))
        (parent-stack nil)
        (key-len (length key)))
    (dotimes (i key-len)
      (let ((char (aref key i)))
        (push node parent-stack)
        (setq node (gethash char (nskk-trie-node-children node)))))
    (while (and node
                parent-stack
                (nskk--trie-node-leaf-p node))
      (let ((parent (pop parent-stack)))
        (when (nskk-trie-node-children parent)
          (remhash (nskk-trie-node-char node)
                   (nskk-trie-node-children parent)))
        (setq node parent)))))

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

(defun nskk--trie-collect-all (node prefix limit collected-count)
  "Collect all (key . value) pairs reachable from NODE with PREFIX."
  (let ((results nil)
        (count collected-count))
    (cl-labels
        ((dfs (n pfx)
           (when (nskk-trie-node-is-end n)
             (when (and limit (>= count limit))
               (throw 'limit-reached nil))
             (push (cons pfx (nskk-trie-node-value n)) results)
             (cl-incf count))
           (when-let* ((children (nskk-trie-node-children n)))
             (maphash (lambda (char child)
                        (dfs child (concat pfx (char-to-string char))))
                      children))))
      (catch 'limit-reached
        (dfs node prefix)))
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
      (nskk--trie-collect-all node prefix limit 0))))

(provide 'nskk-trie)

;;; nskk-trie.el ends here
