;;; nskk-study.el --- Contextual word association learning for NSKK -*- lexical-binding: t; -*-

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

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Contextual word association learning for NSKK (Layer 2: Domain).
;;
;; Layer position: L2 (Domain) -- depends on nskk-prolog (L0) and nskk-custom (L0).
;;
;; Implements skk-study-style contextual learning: when a user confirms a
;; candidate, the system records an association between the previously
;; confirmed word and the current (reading, candidate) pair.  On subsequent
;; conversions, candidates are reordered based on these associations,
;; prioritizing candidates that were previously selected in similar context.
;;
;; Example: after confirming 雨 then converting ふる → 降る, the association
;; (雨, ふる) → 降る is stored.  Next time ふる is converted after 雨,
;; 降る is promoted to the front.
;;
;; Prolog predicates maintained by this module:
;; - `study-association/3' -- (previous-word reading candidate) context association
;;
;; Key public API:
;; - `nskk-study-record'        -- record a study association after kakutei
;; - `nskk-study-reorder'       -- reorder candidates based on study associations
;; - `nskk-study-save'          -- persist study data to file
;; - `nskk-study-load'          -- load study data from file
;; - `nskk-study-after-kakutei' -- entry point called from henkan commit path

;;; Code:

(require 'subr-x)
(require 'seq)
(require 'nskk-prolog)

;;;; Customization

(defgroup nskk-study nil
  "Contextual word association learning for NSKK."
  :prefix "nskk-study-"
  :group 'nskk)

(defcustom nskk-study-file
  (expand-file-name "nskk/study.dat" user-emacs-directory)
  "File path for persisting study association data."
  :type 'file
  :safe #'stringp
  :package-version '(nskk . "0.1.0")
  :group 'nskk-study)

(defcustom nskk-study-search-times 5
  "Number of previous confirmations to search for associations.
When reordering candidates, this many recent kakutei entries are
checked for matching associations."
  :type 'natnum
  :safe #'natnump
  :package-version '(nskk . "0.1.0")
  :group 'nskk-study)

(defcustom nskk-study-max-distance 30
  "Maximum buffer distance for recording study associations.
When the current conversion point exceeds this distance (in characters)
from the previous kakutei position, no association is recorded.
Set to nil to disable the distance check."
  :type '(choice natnum (const :tag "No limit" nil))
  :safe (lambda (v) (or (null v) (natnump v)))
  :package-version '(nskk . "0.1.0")
  :group 'nskk-study)

(defcustom nskk-study-first-candidate t
  "Whether to record study associations when the first candidate is selected.
When nil, associations are only recorded when the user cycles past
the first candidate before confirming."
  :type 'boolean
  :safe #'booleanp
  :package-version '(nskk . "0.1.0")
  :group 'nskk-study)

;;;; Prolog Infrastructure

;; Study association facts: (study-association previous-word reading candidate)
;; Hash-indexed on first arg (previous-word) for O(1) lookup
(nskk-prolog-set-index 'study-association 3 :hash)

;;;; Kakutei History Ring (global)

(defvar nskk--study-kakutei-ring nil
  "Ring of recent kakutei entries for study context.
Each entry is a plist (:word WORD :point POINT :buffer BUFFER).
Most recent entry is at the head.  Length is capped at
`nskk-study-search-times'.")

(defun nskk--study-push-kakutei (word point buffer)
  "Push a kakutei entry onto the history ring.
WORD is the confirmed text, POINT is the buffer position,
BUFFER is the buffer where confirmation occurred."
  (push (list :word word :point point :buffer buffer)
        nskk--study-kakutei-ring)
  (when (> (length nskk--study-kakutei-ring) nskk-study-search-times)
    (setq nskk--study-kakutei-ring
          (seq-take nskk--study-kakutei-ring nskk-study-search-times))))

(defun nskk--study-recent-words ()
  "Return a list of recent kakutei words from the history ring."
  (mapcar (lambda (entry) (plist-get entry :word))
          nskk--study-kakutei-ring))

(defun nskk--study-distance-ok-p (current-point current-buffer)
  "Check if CURRENT-POINT in CURRENT-BUFFER is within max-distance of last kakutei."
  (or (null nskk-study-max-distance)
      (null nskk--study-kakutei-ring)
      (let ((last (car nskk--study-kakutei-ring)))
        (and (eq current-buffer (plist-get last :buffer))
             (<= (abs (- current-point (plist-get last :point)))
                 nskk-study-max-distance)))))

;;;; Core API

;;;###autoload
(defun nskk-study-record (reading candidate &optional index)
  "Record study associations for READING and CANDIDATE.
Associates each recent kakutei word with this (READING, CANDIDATE) pair.
INDEX is the candidate index (0-based); when `nskk-study-first-candidate'
is nil and INDEX is 0, no association is recorded.

Candidates with the `nskk-no-learn' text property are silently skipped."
  (when-let* ((word (if (stringp candidate) candidate (car candidate))))
    (when (and (not (get-text-property 0 'nskk-no-learn word))
               (or nskk-study-first-candidate (and index (> index 0)))
               (nskk--study-distance-ok-p (point) (current-buffer))
               nskk--study-kakutei-ring)
      (when-let* ((prev-word (plist-get (car nskk--study-kakutei-ring) :word)))
        (when-let* ((old (nskk-prolog-query-value
                          `(study-association ,prev-word ,reading \?c) '\?c)))
          (nskk-prolog-retract `(study-association ,prev-word ,reading ,old)))
        (nskk-prolog-assert
         (list `(study-association ,prev-word ,reading ,word)))))))

;;;###autoload
(defun nskk-study-after-kakutei (reading candidate &optional index)
  "Entry point called after kakutei to update study state.
Records the study association and pushes the confirmed word
onto the kakutei history ring.
READING is the dictionary lookup key.
CANDIDATE is the confirmed word string.
INDEX is the candidate index (0-based, optional)."
  (nskk-study-record reading candidate index)
  (when-let* ((word (if (stringp candidate) candidate (car candidate))))
    (nskk--study-push-kakutei word (point) (current-buffer))))

;;;###autoload
(defun nskk-study-reorder (reading candidates)
  "Reorder CANDIDATES for READING based on study associations.
Searches the kakutei history ring for associations matching READING.
If a match is found, the associated candidate is promoted to the front
of the list.  Returns the (possibly reordered) candidate list."
  (if (or (null nskk--study-kakutei-ring) (null candidates))
      candidates
    (let* ((match (seq-find
                   (lambda (entry)
                     (let* ((prev-word (plist-get entry :word))
                            (assoc-candidate
                             (and prev-word
                                  (nskk-prolog-query-value
                                   `(study-association ,prev-word ,reading \?c)
                                   '\?c))))
                       (and assoc-candidate (member assoc-candidate candidates))))
                   nskk--study-kakutei-ring))
           (promoted (and match
                          (let ((prev-word (plist-get match :word)))
                            (nskk-prolog-query-value
                             `(study-association ,prev-word ,reading \?c)
                             '\?c)))))
      (if promoted
          (cons promoted (remove promoted candidates))
        candidates))))

;;;; Persistence

;;;###autoload
(defun nskk-study-save ()
  "Save study association data to `nskk-study-file'."
  (interactive)
  (condition-case err
      (progn
        (let ((dir (file-name-directory nskk-study-file)))
          (unless (file-directory-p dir)
            (make-directory dir t)))
        (with-temp-file nskk-study-file
          (let ((solutions (nskk-prolog-query '(study-association \?p \?r \?c))))
            (prin1
             (mapcar (lambda (sol)
                       (list (nskk-prolog-walk '\?p sol)
                             (nskk-prolog-walk '\?r sol)
                             (nskk-prolog-walk '\?c sol)))
                     solutions)
             (current-buffer)))
          (message "NSKK: Study data saved")))
    (error
     (message "NSKK: Failed to save study data: %s" (error-message-string err)))))

;;;###autoload
(defun nskk-study-load ()
  "Load study association data from `nskk-study-file'."
  (interactive)
  (when (file-readable-p nskk-study-file)
    (condition-case err
        (when-let* ((data (with-temp-buffer
                            (insert-file-contents nskk-study-file)
                            (read (current-buffer)))))
          (dolist (entry data)
            (pcase entry
              (`(,(and (pred stringp) prev)
                 ,(and (pred stringp) reading)
                 ,(and (pred stringp) cand))
               (nskk-prolog-assert
                (list `(study-association ,prev ,reading ,cand)))))))
      (error
       (message "NSKK: Failed to load study data: %s" (error-message-string err))))))

(provide 'nskk-study)

;;; nskk-study.el ends here
