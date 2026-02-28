;;; nskk-macros.el --- Core macros for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

;; Author: takeokunn <bararararatty@gmail.com>
;; Maintainer: takeokunn <bararararatty@gmail.com>
;; URL: https://github.com/takeokunn/nskk.el
;; Keywords: i18n

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

;; Shared macros used across multiple NSKK files.
;; Single-use macros are defined in their consuming files.
;;
;; Usage: (eval-when-compile (require 'nskk-macros))

;;; Code:

;;;; Hash Table Batch Fill

(defmacro nskk-fill-hash-table (table &rest entries)
  "Fill TABLE with ENTRIES.
Each entry is (KEY VALUE).
Example: (nskk-fill-hash-table tbl (\"a\" \"あ\") (\"i\" \"い\"))"
  (declare (indent 1) (debug t))
  `(progn
     ,@(mapcar (lambda (entry)
                 `(puthash ,(car entry) ,(cadr entry) ,table))
               entries)))

;;;; Buffer Modification Guard

(defmacro nskk-without-modification (&rest body)
  "Execute BODY without triggering modification hooks or undo recording."
  (declare (indent 0) (debug t))
  `(let ((inhibit-modification-hooks t)
         (buffer-undo-list t))
     ,@body))

;;;; Provide

(provide 'nskk-macros)

;;; nskk-macros.el ends here
