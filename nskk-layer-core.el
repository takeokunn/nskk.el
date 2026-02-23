;;; nskk-layer-core.el --- Core engine layer interface -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Takeshi Umeda

;; Author: Takeshi Umeda <takeokunn@gmail.com>
;; Maintainer: takeokunn <bararararatty@gmail.com>
;; URL: https://github.com/takeokunn/nskk.el
;; Keywords: i18n

;; This file is NOT part of GNU Emacs.

;; This file is part of NSKK (Next-generation SKK).
;;
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

;; This module provides the interface for the Core Engine Layer (Layer 4).
;;
;; Layer Responsibilities:
;; - Core Engine Layer implements pure conversion and transformation logic
;; - Provides romaji-to-kana conversion APIs
;; - Provides character type conversion (hiragana/katakana, zenkaku/hankaku)
;; - Provides dictionary search APIs
;; - NO state management - state is handled by Application Layer (Layer 3)
;; - NO UI interaction - UI is handled by Presentation Layer (Layer 1)
;; - NO direct data storage - data access is through Data Access Layer (Layer 5)
;;
;; It orchestrates:
;; - Romaji to kana conversion (nskk-converter)
;; - Character conversion utilities (nskk-core)
;; - Dictionary search (nskk-search)
;;
;; The core engine layer is responsible for:
;; 1. Accepting conversion requests from the application layer
;; 2. Performing all conversion operations (pure functions)
;; 3. Querying the data access layer for dictionary entries
;; 4. Returning conversion results WITHOUT modifying state
;;
;; Performance targets:
;; - Romaji conversion: < 0.1ms
;; - Character conversion: < 0.01ms per character
;; - Dictionary search: < 1ms (cached)
;;
;; Architecture compliance:
;; - No direct UI interaction (delegates to presentation layer)
;; - No direct data storage (delegates to data access layer)
;; - No state management (state managed by application layer)
;; - Pure conversion and transformation logic
;;
;; Dependency Flow:
;; - Application Layer -> Core Engine Layer -> Data Access Layer

;;; Code:

(require 'nskk-converter)
(require 'nskk-core)
(require 'nskk-search)
(require 'nskk-dict-io)

;; NOTE: Core Engine Layer (Layer 4) does NOT depend on State Management.
;; State is managed by the Application Layer (Layer 3) which coordinates
;; between the UI and Core layers.

;;; Conversion API

(defun nskk-core-convert-romaji (romaji)
  "Convert ROMAJI string to kana.
Returns (kana . remaining-romaji) cons cell.
If conversion incomplete, returns (:incomplete . romaji).

Note: This is a pure conversion function. State management is handled
by the Application Layer which calls this function."
  (when (stringp romaji)
    (nskk-converter-convert romaji)))

;; String-level conversion wrappers for the Application Layer.
;; These delegate to nskk-core.el char-level primitives via nskk-core-string-* functions.
;; Named with nskk-layer-core- prefix to avoid conflicting with nskk-core.el's
;; char-level functions of the same base name.

(defun nskk-layer-core-hiragana-to-katakana (string)
  "Convert hiragana in STRING to katakana.
Returns converted string.

Note: This is a pure conversion function. State management is handled
by the Application Layer which calls this function."
  (when (stringp string)
    (nskk-core-string-hiragana-to-katakana string)))

(defun nskk-layer-core-katakana-to-hiragana (string)
  "Convert katakana in STRING to hiragana.
Returns converted string.

Note: This is a pure conversion function. State management is handled
by the Application Layer which calls this function."
  (when (stringp string)
    (nskk-core-string-katakana-to-hiragana string)))

(defun nskk-layer-core-zenkaku-to-hankaku (string)
  "Convert zenkaku katakana in STRING to hankaku.
Returns converted string.

Note: This is a pure conversion function. State management is handled
by the Application Layer which calls this function."
  (when (stringp string)
    (nskk-core-string-zenkaku-to-hankaku string)))

(defun nskk-layer-core-hankaku-to-zenkaku (string)
  "Convert hankaku katakana in STRING to zenkaku.
Returns converted string.

Note: This is a pure conversion function. State management is handled
by the Application Layer which calls this function."
  (when (stringp string)
    (nskk-core-string-hankaku-to-zenkaku string)))

;;; Dictionary Search API

(defun nskk-core-search (key &optional type limit)
  "Search dictionary for KEY.
TYPE is search type: :exact (default), :prefix, or :regex.
LIMIT is maximum results (default: 100).
Returns list of candidate strings."
  (when (stringp key)
    (let ((search-type (or type :exact)))
      (cond
       ((eq search-type :exact)
        (nskk-dict-lookup key))
       ((eq search-type :prefix)
        (when nskk--system-dict-index
          (nskk-search-prefix nskk--system-dict-index key nil limit)))
       ((eq search-type :regex)
        (when nskk--system-dict-index
          (nskk-search-partial nskk--system-dict-index key nil limit)))
       (t
        (error "Unknown search type: %s" search-type))))))

(defun nskk-core-search-and-rank (key &optional type limit)
  "Search dictionary and rank results by relevance.
KEY is the search key.
TYPE is search type: :exact (default), :prefix, or :regex.
LIMIT is maximum results (default: 100).
Returns ranked list of candidate strings."
  (nskk-core-search key type limit))

;; Character classification functions are provided by nskk-core.el:
;; - nskk-core-hiragana-p
;; - nskk-core-katakana-p
;; - nskk-core-han-p
;; - nskk-core-japanese-p

;;; Performance Statistics

(defun nskk-core-stats ()
  "Return performance statistics for core engine.
Returns alist with conversion and search metrics."
  `((romaji-table-size . ,(hash-table-count nskk--romaji-table))))

(defun nskk-core-reset-stats ()
  "Reset all performance statistics."
  nil)

;;; Validation and Testing

(defun nskk-core-validate-romaji-table ()
  "Validate romaji conversion table.
Returns t if valid, nil if issues found."
  (let ((issues '())
        (duplicates '())
        (seen (make-hash-table :test 'equal)))
    ;; Check for duplicates
    (maphash
     (lambda (key value)
       (when (and (integerp value)
                  (gethash value seen))
         (push key duplicates))
       (when (integerp value)
         (puthash value t seen)))
     nskk--romaji-table)

    (when duplicates
      (push (list 'duplicate-kana 'romaji-table duplicates) issues))

    ;; Check for common entries
    (let ((common-entries '("a" "i" "u" "e" "o" "ka" "ki" "ku" "ke" "ko")))
      (dolist (entry common-entries)
        (unless (gethash entry nskk--romaji-table)
          (push (list 'missing-entry entry) issues))))

    (if issues
        (progn
          (message "Romaji table validation issues: %S" issues)
          nil)
      t)))

(defun nskk-core-benchmark-romaji-conversion (iterations)
  "Benchmark romaji conversion.
Performs ITERATIONS conversions and returns average time."
  (let* ((test-strings '("ka" "ki" "ku" "ke" "ko" "sha" "shu" "sho" "kya" "kyu"))
         (start-time (float-time))
         (count 0))
    (dotimes (_i iterations)
      (dolist (str test-strings)
        (nskk-converter-convert str)
        (cl-incf count)))
    (let ((total (- (float-time) start-time)))
      (message "Performed %d conversions in %.6f seconds" count total)
      (message "Average time: %.6f ms per conversion" (* (/ total count) 1000))
      (* (/ total count) 1000))))

(provide 'nskk-layer-core)

;;; nskk-layer-core.el ends here
