;;; nskk-study.el --- Contextual word association learning for NSKK -*- lexical-binding: t; -*-
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
;; Contextual word association learning for NSKK (Layer 2: Domain).
;;
;; Layer position: L2 (Domain) -- depends on nskk-prolog, nskk-dictionary,
;;   nskk-dict-transaction, and nskk-cps-macros.
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
(require 'nskk-cps-macros)
(require 'nskk-prolog)
(require 'nskk-dictionary)
(require 'nskk-dict-transaction)

;;;; Customization
(defgroup nskk-study nil
  "Contextual word association learning for NSKK."
  :prefix "nskk-study-"
  :group 'nskk)

(defcustom nskk-study-file
  (expand-file-name "nskk/study.dat" user-emacs-directory)
  "File path for persisting study association data."
  :type 'file
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
Associations require a position after the previous kakutei and strictly
less than this distance (in characters) from it.
Set to nil to disable the distance check."
  :type '(choice natnum (const :tag "No limit" nil))
  :safe (lambda (v)
          (or (null v) (natnump v)))
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
(defconst nskk--study-max-file-size (* 10 1024 1024)
  "Maximum number of bytes accepted from the study data file.")

(defvar nskk--study-kakutei-ring nil
  "Ring of recent kakutei entries for study context.
Each entry is a plist (:word WORD :point POINT :buffer BUFFER).
Most recent entry is at the head.  Length is capped at
`nskk-study-search-times'.")

(defun nskk--study-candidate-word (candidate)
  "Extract the word string from CANDIDATE.
CANDIDATE is either the word itself or a cons whose car is the word.
Return nil for anything else, so a malformed candidate is skipped rather
than recorded."
  (pcase candidate
    ((pred stringp) candidate)
    (`(,(pred stringp) . ,_) (car candidate))
    (_ nil)))

(defun/done nskk--study-push-kakutei (word point buffer)
  "Push a kakutei entry onto the history ring.
WORD is the confirmed text, POINT is the buffer position,
BUFFER is the buffer where confirmation occurred."
  (push (list :word word :point point :buffer buffer) nskk--study-kakutei-ring)
  (when (> (length nskk--study-kakutei-ring) nskk-study-search-times)
    (setq nskk--study-kakutei-ring
          (seq-take nskk--study-kakutei-ring nskk-study-search-times))))

(defun nskk--study-distance-ok-p (current-point current-buffer)
  "Check if CURRENT-POINT in CURRENT-BUFFER is within max-distance of last kakutei."
  (or (null nskk-study-max-distance)
      (null nskk--study-kakutei-ring)
      (let ((last (car nskk--study-kakutei-ring)))
        (and (eq current-buffer (plist-get last :buffer))
             (< 0 (- current-point (plist-get last :point))
                nskk-study-max-distance)))))

;;;; Core API
(defun nskk--study-record-allowed-p (word index)
  "Non-nil when an association for WORD chosen at INDEX may be recorded.
WORD carrying the `nskk-no-learn' text property is never recorded."
  (and (not (get-text-property 0 'nskk-no-learn word))
       (or nskk-study-first-candidate (and index (> index 0)))
       (nskk--study-distance-ok-p (point) (current-buffer))
       nskk--study-kakutei-ring))

(defun/done nskk--study-associate (prev-word reading word)
  "Make WORD the only association held for PREV-WORD and READING.
Any candidate previously associated with the pair is retracted first, so
the predicate never accumulates competing answers for one context."
  (when-let* ((old (nskk-prolog-query-value
                    `(study-association ,prev-word ,reading \?c)
                    '\?c)))
    (nskk-prolog-retract `(study-association ,prev-word ,reading ,old)))
  (nskk-prolog-assert (list `(study-association ,prev-word ,reading ,word))))

;;;###autoload
(defun nskk-study-record (reading candidate &optional index)
  "Record study associations for READING and CANDIDATE.
Associates the most recent kakutei word with this (READING, CANDIDATE)
pair.  INDEX is the candidate index (0-based); when
`nskk-study-first-candidate' is nil and INDEX is 0, no association is
recorded.

Candidates with the `nskk-no-learn' text property are silently skipped."
  (when-let* ((word (nskk--study-candidate-word candidate))
              ((nskk--study-record-allowed-p word index))
              (prev-word (plist-get (car nskk--study-kakutei-ring) :word)))
    (nskk--study-associate prev-word reading word)))

;;;###autoload
(defun nskk-study-after-kakutei (reading candidate &optional index)
  "Entry point called after kakutei to update study state.
Records the study association and pushes the confirmed word
onto the kakutei history ring.
READING is the dictionary lookup key.
CANDIDATE is the confirmed word string.
INDEX is the candidate index (0-based, optional)."
  (nskk-study-record reading candidate index)
  (when-let* ((word (nskk--study-candidate-word candidate)))
    (nskk--study-push-kakutei word (point) (current-buffer))))

(defun nskk--study-associated-candidate (prev-word reading candidates)
  "Return the candidate associated with PREV-WORD and READING.
Return nil when no association exists, or when the associated candidate
is not among CANDIDATES -- a stored answer the dictionary no longer
offers must not displace one it does."
  (when-let* ((prev-word)
              (associated (nskk-prolog-query-value
                           `(study-association ,prev-word ,reading \?c)
                           '\?c)))
    (car (member associated candidates))))

;;;###autoload
(defun nskk-study-reorder (reading candidates)
  "Reorder CANDIDATES for READING based on study associations.
Searches the kakutei history ring, most recent entry first, for an
association naming one of CANDIDATES.  The first such candidate is
promoted to the front.  Returns the (possibly reordered) candidate list."
  (if (or (null nskk--study-kakutei-ring) (null candidates))
      candidates
    (let ((promoted
           (seq-some (lambda (entry)
                       (nskk--study-associated-candidate
                        (plist-get entry :word) reading candidates))
                     nskk--study-kakutei-ring)))
      (if promoted
          (cons promoted (cl-remove promoted candidates :test #'eq :count 1))
        candidates))))

;;;; Persistence
(defun nskk--study-entry-to-fact (entry)
  "Convert a validated study ENTRY to a Prolog fact."
  (pcase entry
    (`(,(and (pred stringp) prev) ,(and (pred stringp) reading)
       ,(and (pred stringp) candidate))
     `(study-association ,prev ,reading ,candidate))
    (_ (error "Invalid study entry: %S" entry))))

(defun nskk--study-report-oversize (size)
  "Report that a study file of SIZE bytes exceeds the load limit."
  (message "NSKK: Study file too large (%d bytes), skipping load" size))

(defun/done nskk--study-write-file ()
  "Serialize every study association to `nskk-study-file' and announce it.
The announcement follows the atomic rename, so a publication that fails
is never reported as a completed save."
  (nskk-dict-write-private-file
   nskk-study-file
   (nskk-dict-serialize-solutions '(study-association \?p \?r \?c)
                                  '(\?p \?r \?c)))
  (message "NSKK: Study data saved"))

;;;###autoload
(defun nskk-study-save ()
  "Save study association data to `nskk-study-file'."
  (interactive)
  (if nskk--persistence-inhibited
      (message "NSKK: Study data save inhibited (tutorial active)")
    (condition-case err
        (nskk--study-write-file)
      (error
       (message "NSKK: Failed to save study data: %s"
                (error-message-string err))))))

(defun/done nskk--study-load-file (owner)
  "Replace the stored study associations with `nskk-study-file' for OWNER.
A file that is absent, unreadable, or over the size limit leaves the
existing associations in place."
  (nskk-dict-transaction-ensure-rollback-complete owner)
  (when-let* ((result (nskk-dict-transaction-load-entries
                       nskk-study-file
                       nskk--study-max-file-size
                       #'nskk--study-entry-to-fact
                       #'nskk--study-report-oversize)))
    (nskk-dict-transaction-publish-facts owner 'study-association 3
                                         (cdr result))))

;;;###autoload
(defun nskk-study-load ()
  "Load study association data from `nskk-study-file'."
  (interactive)
  (let ((owner 'nskk-study-load))
    (condition-case err
        (nskk--study-load-file owner)
      (error
       (message "NSKK: Failed to load study data: %s"
                (error-message-string err))))))

(provide 'nskk-study)

;;; nskk-study.el ends here
