;;; nskk-dict-struct.el --- Dictionary structures (stub) -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

;; Author: NSKK Contributors
;; Maintainer: takeokunn <bararararatty@gmail.com>
;; URL: https://github.com/takeokunn/nskk.el
;; Version: 0.1.0
;; Keywords: japanese, input, mule

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

;; Dictionary structure definitions - stub for missing dependencies.
;; TODO: Implement full dictionary structure functionality.

;;; Code:

(require 'cl-lib)

(cl-defstruct nskk-dict-entry
  "Dictionary entry structure."
  (key nil)
  (candidates nil)
  (okuri nil))

(cl-defstruct nskk-dict-index
  "Dictionary index structure."
  (entries nil)
  (by-prefix nil)
  (by-freq nil))

(defun nskk-dict-struct-entry-count (index _okuri-type)
  "Return count of entries in INDEX."
  (length (nskk-dict-index-entries index)))

(defun nskk-search-reset-stats ()
  "Reset search statistics."
  nil)

(provide 'nskk-dict-struct)

;;; nskk-dict-struct.el ends here
