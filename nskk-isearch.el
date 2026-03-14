;;; nskk-isearch.el --- Isearch integration for NSKK -*- lexical-binding: t; -*-

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

;; Isearch (incremental search) integration for NSKK (Layer 5: Presentation).
;;
;; Layer position: L5 (Presentation) -- depends on nskk-state, nskk-custom,
;;   nskk-modeline, and nskk-cps-macros.
;;
;; Provides Japanese incremental search using Emacs' built-in `isearch-mode'.
;; When enabled, C-s/C-r activates isearch with NSKK input support, allowing
;; the user to type Japanese text in the isearch prompt.
;;
;; The isearch prompt shows the current NSKK input mode:
;;   I-search: [か]    -- hiragana mode
;;   I-search: [ア]    -- katakana mode
;;   I-search: [英]    -- full-width latin mode
;;   I-search: [aa]    -- ASCII mode
;;   I-search: [aあ]   -- abbrev mode
;;
;; This is the nskk.el equivalent of ddskk's `skk-isearch.el'.
;;
;; Usage:
;;   (require 'nskk-isearch)
;;   (nskk-isearch-setup)   ; or set nskk-isearch-enable to t
;;
;; Or in nskk-mode startup, use:
;;   (setq nskk-isearch-enable t)
;;
;; Prolog predicates maintained by this module: none.

;;; Code:

(require 'isearch)
(require 'nskk-state)
(require 'nskk-custom)
(require 'nskk-cps-macros)

;;;; Customization

(defgroup nskk-isearch nil
  "Isearch integration settings for NSKK."
  :prefix "nskk-isearch-"
  :group 'nskk)

(defcustom nskk-isearch-enable nil
  "When non-nil, integrate NSKK with Emacs isearch.
When enabled, isearch will use the NSKK input mode from the originating
buffer, allowing Japanese text search via the normal isearch keybindings.
The isearch prompt shows the current NSKK mode indicator."
  :type 'boolean
  :safe #'booleanp
  :package-version '(nskk . "0.1.0")
  :group 'nskk-isearch)

(defcustom nskk-isearch-mode-string-alist
  '((hiragana      . "[か]")
    (katakana      . "[ア]")
    (jisx0208-latin . "[英]")
    (ascii         . "[aa]")
    (latin         . "[aa]")
    (abbrev        . "[aあ]"))
  "Alist mapping NSKK mode symbols to isearch prompt strings.
Used to show the current input mode in the isearch prompt."
  :type '(alist :key-type symbol :value-type string)
  :package-version '(nskk . "0.1.0")
  :group 'nskk-isearch)

;;;; Internal State

(defvar nskk--isearch-orig-buffer nil
  "The buffer where isearch was initiated.")

;;;; Mode Indicator

(defun nskk--isearch-mode-string ()
  "Return isearch prompt mode string for the current NSKK mode.
Returns the appropriate string from `nskk-isearch-mode-string-alist',
or nil if NSKK is not active in the originating buffer."
  (when-let* ((buf nskk--isearch-orig-buffer)
              (_ (buffer-live-p buf)))
    (let ((mode (with-current-buffer buf
                  (when (and (boundp 'nskk-current-state)
                             nskk-current-state)
                    (nskk-state-mode nskk-current-state)))))
      (when mode
        (cdr (assq mode nskk-isearch-mode-string-alist))))))

(defun nskk--isearch-prompt-advice (orig-fun)
  "Advice for `isearch-message-prefix' to add NSKK mode indicator.
ORIG-FUN is the original `isearch-message-prefix' function."
  (let* ((orig-prompt (funcall orig-fun))
         (mode-str (when nskk-isearch-enable
                     (nskk--isearch-mode-string))))
    (if mode-str
        (concat mode-str " " orig-prompt)
      orig-prompt)))

;;;; Hook Functions

(defun nskk--isearch-setup ()
  "Set up NSKK state when isearch begins."
  (setq nskk--isearch-orig-buffer (current-buffer)))

(defun nskk--isearch-teardown ()
  "Clean up NSKK state when isearch ends."
  (setq nskk--isearch-orig-buffer nil))

;;;; Setup/Teardown

(defun nskk-isearch-setup ()
  "Install NSKK isearch integration.
Adds hooks and advice to enable Japanese isearch."
  (add-hook 'isearch-mode-hook #'nskk--isearch-setup)
  (add-hook 'isearch-mode-end-hook #'nskk--isearch-teardown)
  (advice-add 'isearch-message-prefix :around #'nskk--isearch-prompt-advice))

(defun nskk-isearch-teardown ()
  "Remove NSKK isearch integration."
  (remove-hook 'isearch-mode-hook #'nskk--isearch-setup)
  (remove-hook 'isearch-mode-end-hook #'nskk--isearch-teardown)
  (advice-remove 'isearch-message-prefix #'nskk--isearch-prompt-advice))

;;;; Auto-enable

;; Auto-enable when nskk-isearch-enable changes
(add-variable-watcher 'nskk-isearch-enable
                      (lambda (_sym val op _buf)
                        (when (eq op 'set)
                          (if val
                              (nskk-isearch-setup)
                            (nskk-isearch-teardown)))))

(provide 'nskk-isearch)

;;; nskk-isearch.el ends here
