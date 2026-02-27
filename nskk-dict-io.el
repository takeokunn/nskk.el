;;; nskk-dict-io.el --- Dictionary I/O -*- lexical-binding: t; -*-

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

;; Dictionary I/O module for loading and saving SKK dictionary files.
;; Supports the standard SKK dictionary format with EUC-JP and UTF-8 encoding.

;;; Code:

(require 'cl-lib)
(require 'nskk-dict-struct)
(require 'nskk-trie)
(require 'nskk-custom)

;;; Dictionary Parsing

(defun nskk-dict-parse-line (line)
  "Parse a single SKK dictionary LINE.
Returns (key . candidates-list) or nil for comments/invalid lines."
  ;; Skip comment lines and empty lines
  (when (and (stringp line)
             (> (length line) 0)
             (not (string-prefix-p ";;" line)))
    (let ((space-pos (string-search " " line)))
      (when (and space-pos
                 (> space-pos 0)
                 (> (length line) (+ space-pos 2))
                 (= (aref line (1+ space-pos)) ?/))
        (let* ((key (substring line 0 space-pos))
               (candidates-str (substring line (1+ space-pos)))
               (candidates (nskk-dict--parse-candidates candidates-str)))
          (when candidates
            (cons key candidates)))))))

(defun nskk-dict--parse-candidates (str)
  "Parse candidates from STR like \"/candidate1/candidate2/...\"."
  (when (and (stringp str) (> (length str) 1) (= (aref str 0) ?/))
    (let ((parts (split-string (substring str 1) "/" t)))
      ;; Strip annotations (e.g., "漢字;annotation" -> "漢字")
      (mapcar (lambda (c)
                (let ((semi (string-search ";" c)))
                  (if semi (substring c 0 semi) c)))
              parts))))

;;; Dictionary Loading

(defun nskk-dict-load-file (file &optional coding-system)
  "Load SKK dictionary from FILE.
CODING-SYSTEM defaults to nil which lets Emacs auto-detect encoding
from the file's coding cookie (e.g. -*- coding: euc-jp -*-).
Returns a `nskk-dict-index' struct, or nil on failure."
  (when (and (stringp file) (file-readable-p file))
    (let ((entries (make-hash-table :test 'equal :size 50000))
          (trie (nskk-trie-create))
          (coding coding-system))
      (with-temp-buffer
        (let ((coding-system-for-read coding))
          (insert-file-contents file))
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((line (buffer-substring-no-properties
                        (line-beginning-position) (line-end-position)))
                 (parsed (nskk-dict-parse-line line)))
            (when parsed
              (let ((key (car parsed))
                    (candidates (cdr parsed)))
                ;; Add to hash-table
                (let ((existing (gethash key entries)))
                  (if existing
                      ;; Merge candidates (avoid duplicates)
                      (puthash key (cl-union existing candidates :test #'equal) entries)
                    (puthash key candidates entries)))
                ;; Add to trie for prefix search
                (nskk-trie-insert trie key candidates))))
          (forward-line 1)))
      ;; Build and return index
      (make-nskk-dict-index
       :entries entries
       :by-prefix trie
       :by-freq nil))))

(defun nskk-dict-load-system-dictionaries ()
  "Load all system dictionaries configured in `nskk-dict-system-dictionary-files'.
Returns a merged `nskk-dict-index', or nil if no dictionaries found."
  (let ((merged-entries (make-hash-table :test 'equal :size 100000))
        (merged-trie (nskk-trie-create))
        (loaded 0))
    (dolist (file nskk-dict-system-dictionary-files)
      (when (file-readable-p file)
        (let ((index (nskk-dict-load-file file)))
          (when index
            ;; Merge entries
            (maphash (lambda (key candidates)
                       (let ((existing (gethash key merged-entries)))
                         (if existing
                             (puthash key (cl-union existing candidates :test #'equal)
                                      merged-entries)
                           (puthash key candidates merged-entries))))
                     (nskk-dict-index-entries index))
            ;; Merge trie data
            (let ((keys (nskk-trie-keys (nskk-dict-index-by-prefix index))))
              (dolist (k keys)
                (let ((vals (nskk-trie-lookup-values
                             (nskk-dict-index-by-prefix index) k)))
                  (nskk-trie-insert merged-trie k vals))))
            (cl-incf loaded)))))
    (if (> loaded 0)
        (progn
          (message "NSKK: Loaded %d system dictionar%s (%d entries)"
                   loaded (if (= loaded 1) "y" "ies")
                   (hash-table-count merged-entries))
          (make-nskk-dict-index
           :entries merged-entries
           :by-prefix merged-trie
           :by-freq nil))
      (message "NSKK: No system dictionaries found")
      nil)))

(defun nskk-dict-load-user-dictionary ()
  "Load user dictionary from `nskk-dict-user-dictionary-file'.
Returns a `nskk-dict-index', or nil if not found."
  (when (and nskk-dict-user-dictionary-file
             (file-readable-p nskk-dict-user-dictionary-file))
    (message "NSKK: Loading user dictionary from %s"
             nskk-dict-user-dictionary-file)
    (nskk-dict-load-file nskk-dict-user-dictionary-file)))

;;; Global Dictionary State

(defvar nskk--system-dict-index nil
  "Loaded system dictionary index.")

(defvar nskk--user-dict-index nil
  "Loaded user dictionary index.")

(defun nskk-dict--detect-system-dictionaries ()
  "Auto-detect system dictionary files.
Probes nix profiles, common system paths, and homebrew locations.
Also checks `nskk-large-dictionary' as a fallback.
Returns a list of readable dictionary file paths."
  (let ((candidates
         (append
          ;; Nix profile paths
          (list (expand-file-name "~/.nix-profile/share/skk/SKK-JISYO.L")
                "/run/current-system/sw/share/skk/SKK-JISYO.L")
          ;; NIX_PROFILES env var paths
          (let ((nix-profiles (getenv "NIX_PROFILES")))
            (when nix-profiles
              (mapcar (lambda (p) (expand-file-name "share/skk/SKK-JISYO.L" p))
                      (split-string nix-profiles))))
          ;; Common system paths
          (list "/usr/share/skk/SKK-JISYO.L"
                "/usr/local/share/skk/SKK-JISYO.L")
          ;; Homebrew paths
          (list "/opt/homebrew/share/skk/SKK-JISYO.L")
          ;; nskk-large-dictionary fallback
          (when nskk-large-dictionary
            (list nskk-large-dictionary))))
        (found nil))
    (dolist (path candidates)
      (when (and (stringp path) (file-readable-p path))
        (push path found)))
    (nreverse found)))

(defun nskk-dict-initialize ()
  "Initialize dictionaries by loading system and user dictionaries.
When `nskk-dict-system-dictionary-files' is nil, auto-detects
dictionary paths from nix profiles and common system locations."
  (interactive)
  (let ((dict-files (or nskk-dict-system-dictionary-files
                        (nskk-dict--detect-system-dictionaries))))
    (when dict-files
      (let ((nskk-dict-system-dictionary-files dict-files))
        (setq nskk--system-dict-index (nskk-dict-load-system-dictionaries)))))
  (setq nskk--user-dict-index (nskk-dict-load-user-dictionary))
  (message "NSKK: Dictionary initialization complete"))

(defun nskk-dict-lookup (key)
  "Look up KEY in loaded dictionaries.
Returns list of candidates or nil."
  (let ((user-result (when nskk--user-dict-index
                       (gethash key (nskk-dict-index-entries nskk--user-dict-index))))
        (system-result (when nskk--system-dict-index
                         (gethash key (nskk-dict-index-entries nskk--system-dict-index)))))
    ;; User dictionary results come first
    (if user-result
        (if system-result
            (cl-union user-result system-result :test #'equal)
          user-result)
      system-result)))

;;; User Dictionary Modification

(defvar nskk-dict-modified nil
  "Non-nil when the user dictionary has unsaved modifications.")

(defun nskk-dict-register-word (reading word)
  "Register WORD as a conversion candidate for READING in user dictionary.
Adds the entry to the in-memory user dictionary index and marks it for saving.
If no user dictionary index exists, creates one.
READING is the headword (e.g., hiragana reading).
WORD is the conversion result to register."
  (when (and (stringp reading) (stringp word)
             (not (string-empty-p reading))
             (not (string-empty-p word)))
    ;; Ensure user dict index exists
    (unless nskk--user-dict-index
      (setq nskk--user-dict-index
            (make-nskk-dict-index
             :entries (make-hash-table :test 'equal :size 1000)
             :by-prefix nil
             :by-freq nil)))
    (let* ((entries (nskk-dict-index-entries nskk--user-dict-index))
           (existing (gethash reading entries)))
      (if existing
          ;; Prepend to existing entry (higher priority) unless already present
          (unless (member word existing)
            (puthash reading (cons word existing) entries))
        ;; Create new entry
        (puthash reading (list word) entries)))
    (setq nskk-dict-modified t)
    (message "NSKK: Registered %s -> %s" reading word)))

;;; User Dictionary Save

(defun nskk-dict-save-user-dictionary ()
  "Save user dictionary to `nskk-dict-user-dictionary-file'."
  (interactive)
  (when (and nskk-dict-user-dictionary-file nskk--user-dict-index)
    (let ((dir (file-name-directory nskk-dict-user-dictionary-file)))
      (unless (file-directory-p dir)
        (make-directory dir t)))
    (with-temp-file nskk-dict-user-dictionary-file
      (insert ";; -*- mode: fundamental; coding: utf-8 -*-\n")
      (insert ";; NSKK user dictionary\n")
      (insert ";; okuri-nasi entries.\n")
      (maphash (lambda (key candidates)
                 (insert (format "%s /%s/\n"
                                 key
                                 (mapconcat #'identity candidates "/"))))
               (nskk-dict-index-entries nskk--user-dict-index)))
    (message "NSKK: User dictionary saved to %s"
             nskk-dict-user-dictionary-file)))

(provide 'nskk-dict-io)

;;; nskk-dict-io.el ends here
