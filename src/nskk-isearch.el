;;; nskk-isearch.el --- Isearch integration for NSKK -*- lexical-binding: t; -*-

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

(require 'cl-lib)
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
  :safe (lambda (v) (and (listp v) (cl-every (lambda (e) (and (consp e) (symbolp (car e)) (stringp (cdr e)))) v)))
  :package-version '(nskk . "0.1.0")
  :group 'nskk-isearch)

;;;; Internal State

(progn
  (defvar nskk--isearch-orig-buffer nil
    "The buffer where isearch was initiated.")

  (defvar nskk--isearch-orig-buffer-stack nil
    "Stack of previous originating buffers for nested isearch sessions.")

  (defvar nskk--isearch-mode-hook-owned nil
    "Non-nil when NSKK installed its isearch mode hook.")

  (defvar nskk--isearch-mode-end-hook-owned nil
    "Non-nil when NSKK installed its isearch mode end hook.")

  (defvar nskk--isearch-prompt-advice-owned nil
    "Non-nil when NSKK installed its isearch prompt advice.")

  (defvar nskk--isearch-enable-watcher-owned nil
    "Non-nil when NSKK installed its enable-option watcher."))

;;;; Mode Indicator

(defun nskk--isearch-mode-string ()
  "Return isearch prompt mode string for the current NSKK mode.
Returns the appropriate string from `nskk-isearch-mode-string-alist',
or nil if NSKK is not active in the originating buffer."
  (let ((buf nskk--isearch-orig-buffer))
    (when (and buf (buffer-live-p buf))
      (let ((mode (with-current-buffer buf
                    (when (and (boundp 'nskk-current-state)
                               nskk-current-state)
                      (nskk-state-mode nskk-current-state)))))
        (when mode
          (cdr (assq mode nskk-isearch-mode-string-alist)))))))

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
  "Push the previous origin and record the current isearch buffer."
  (push nskk--isearch-orig-buffer nskk--isearch-orig-buffer-stack)
  (setq nskk--isearch-orig-buffer (current-buffer)))

(defun nskk--isearch-teardown ()
  "Restore the previous originating buffer when isearch ends."
  (if nskk--isearch-orig-buffer-stack
      (setq nskk--isearch-orig-buffer
            (pop nskk--isearch-orig-buffer-stack))
    (setq nskk--isearch-orig-buffer nil)))

;;;; Setup/Teardown

;;;###autoload
(defun nskk-isearch-setup ()
  "Install NSKK isearch integration transactionally.
Adds hooks and advice to enable Japanese isearch."
  (let* ((transaction-state (nskk--isearch-transaction-state))
         (physical-before (nth 0 transaction-state))
         (owned-before (nth 1 transaction-state)))
    (condition-case condition-data
        (progn
          (unless (nth 0 physical-before)
            (add-hook 'isearch-mode-hook #'nskk--isearch-setup))
          (unless (nth 1 physical-before)
            (add-hook 'isearch-mode-end-hook #'nskk--isearch-teardown))
          (unless (nth 2 physical-before)
            (advice-add 'isearch-message-prefix :around
                        #'nskk--isearch-prompt-advice))
          (nskk--isearch-reconcile-resource-state '(t t t))
          (unless (equal (nskk--isearch-resource-state) '(t t t))
            (error "Failed to install NSKK isearch integration"))
          (nskk--isearch-set-ownership-state
           (cl-mapcar (lambda (was-present was-owned)
                        (or was-owned (not was-present)))
                      physical-before
                      owned-before)))
      ((error quit)
       (let ((condition-symbol (car condition-data))
             (condition-payload (cdr condition-data)))
         (condition-case nil
             (nskk--isearch-restore-transaction-state transaction-state)
           ((error quit) nil))
         (signal condition-symbol condition-payload))))))

(progn
  (defun nskk--isearch-resource-state ()
    "Return the installed state of NSKK's three isearch resources."
    (list (and (memq #'nskk--isearch-setup isearch-mode-hook) t)
          (and (memq #'nskk--isearch-teardown isearch-mode-end-hook) t)
          (and (advice-member-p #'nskk--isearch-prompt-advice
                                'isearch-message-prefix)
               t)))

  (defun nskk--isearch-ownership-state ()
    "Return NSKK's ownership state for the three isearch resources."
    (list nskk--isearch-mode-hook-owned
          nskk--isearch-mode-end-hook-owned
          nskk--isearch-prompt-advice-owned))

  (defun nskk--isearch-set-ownership-state (state)
    "Set NSKK's three isearch ownership flags from STATE."
    (setq nskk--isearch-mode-hook-owned (nth 0 state)
          nskk--isearch-mode-end-hook-owned (nth 1 state)
          nskk--isearch-prompt-advice-owned (nth 2 state)))

  (defun nskk--isearch-transaction-state ()
    "Return physical and ownership state for isearch resources."
    (list (nskk--isearch-resource-state)
          (nskk--isearch-ownership-state)))

  (defun nskk--isearch-desired-resource-state (transaction-state)
    "Return teardown's desired physical state for TRANSACTION-STATE."
    (cl-mapcar (lambda (present owned)
                 (and present (not owned)))
               (nth 0 transaction-state)
               (nth 1 transaction-state)))

  (defun nskk--isearch-finalize-ownership (owned-before)
    "Reconcile ownership from OWNED-BEFORE with current physical state."
    (nskk--isearch-set-ownership-state
     (cl-mapcar (lambda (owned present)
                  (and owned present))
                owned-before
                (nskk--isearch-resource-state))))

  (defun nskk--isearch-watcher-present-p ()
    "Return non-nil when NSKK's enable watcher is installed."
    (and (memq #'nskk--isearch-enable-watcher
               (get-variable-watchers 'nskk-isearch-enable))
         t))

  (defun nskk--isearch-watcher-state ()
    "Return physical and ownership state for the enable watcher."
    (list (nskk--isearch-watcher-present-p)
          nskk--isearch-enable-watcher-owned))

  (defun nskk--isearch-restore-watcher-presence (present)
    "Restore the enable watcher to physical presence PRESENT."
    (if present
        (unless (nskk--isearch-watcher-present-p)
          (add-variable-watcher 'nskk-isearch-enable
                                #'nskk--isearch-enable-watcher))
      (when (nskk--isearch-watcher-present-p)
        (remove-variable-watcher 'nskk-isearch-enable
                                 #'nskk--isearch-enable-watcher))))

  (defun nskk--isearch-restore-watcher-state (state)
    "Restore the enable watcher to physical and ownership STATE."
    (let (condition-data)
      (condition-case new-condition
          (nskk--isearch-restore-watcher-presence (nth 0 state))
        ((error quit)
         (setq condition-data new-condition)))
      (setq nskk--isearch-enable-watcher-owned (nth 1 state))
      (when condition-data
        (signal (car condition-data) (cdr condition-data)))))

  (defun nskk--isearch-restore-transaction-state (state)
    "Restore isearch resources to physical and ownership STATE."
    (let (condition-data)
      (condition-case new-condition
          (nskk--isearch-restore-resource-state (nth 0 state))
        ((error quit)
         (setq condition-data new-condition)))
      (nskk--isearch-set-ownership-state (nth 1 state))
      (when condition-data
        (signal (car condition-data) (cdr condition-data)))))

  (defun nskk--isearch-reconcile-resource-state (state)
    "Reconcile all isearch resources to physical STATE.
Repeat only while cleanup makes progress, so a later callback that
re-pollutes an earlier resource is repaired without looping forever."
    (let ((attempts 0)
          (keep-going t)
          condition-data)
      (while (and keep-going
                  (< attempts 4)
                  (not (equal (nskk--isearch-resource-state) state)))
        (let ((before (nskk--isearch-resource-state)))
          (condition-case new-condition
              (nskk--isearch-restore-resource-state state)
            ((error quit)
             (unless condition-data
               (setq condition-data new-condition))))
          (setq attempts (1+ attempts))
          (let ((after (nskk--isearch-resource-state)))
            (when (or (equal after state)
                      (equal after before))
              (setq keep-going nil)))))
      (when condition-data
        (signal (car condition-data) (cdr condition-data)))))

  (defun nskk--isearch-reconcile-lifecycle-state
      (resource-state watcher-present)
    "Reconcile resources and watcher to their requested physical states."
    (let ((attempts 0)
          (keep-going t)
          condition-data)
      (while (and keep-going
                  (< attempts 5)
                  (not (equal
                        (append (nskk--isearch-resource-state)
                                (list (nskk--isearch-watcher-present-p)))
                        (append resource-state (list watcher-present)))))
        (let ((before
               (append (nskk--isearch-resource-state)
                       (list (nskk--isearch-watcher-present-p)))))
          (condition-case new-condition
              (nskk--isearch-reconcile-resource-state resource-state)
            ((error quit)
             (unless condition-data
               (setq condition-data new-condition))))
          (condition-case new-condition
              (nskk--isearch-restore-watcher-presence watcher-present)
            ((error quit)
             (unless condition-data
               (setq condition-data new-condition))))
          (setq attempts (1+ attempts))
          (let ((after
                 (append (nskk--isearch-resource-state)
                         (list (nskk--isearch-watcher-present-p)))))
            (when (or (equal after
                             (append resource-state
                                     (list watcher-present)))
                      (equal after before))
              (setq keep-going nil)))))
      (when condition-data
        (signal (car condition-data) (cdr condition-data))))))

(defun nskk--isearch-restore-resource-state (state)
  "Restore NSKK isearch resources to presence STATE.
Continue restoring independent resources after a cleanup failure.
Signal the first cleanup condition after all restore attempts."
  (let (condition-data)
    (cl-labels ((attempt (function)
                  (condition-case new-condition
                      (funcall function)
                    ((error quit)
                     (unless condition-data
                       (setq condition-data new-condition))))))
      (attempt
       (lambda ()
         (if (nth 0 state)
             (unless (memq #'nskk--isearch-setup isearch-mode-hook)
               (add-hook 'isearch-mode-hook #'nskk--isearch-setup))
           (when (memq #'nskk--isearch-setup isearch-mode-hook)
             (remove-hook 'isearch-mode-hook #'nskk--isearch-setup)))))
      (attempt
       (lambda ()
         (if (nth 1 state)
             (unless (memq #'nskk--isearch-teardown isearch-mode-end-hook)
               (add-hook 'isearch-mode-end-hook #'nskk--isearch-teardown))
           (when (memq #'nskk--isearch-teardown isearch-mode-end-hook)
             (remove-hook 'isearch-mode-end-hook #'nskk--isearch-teardown)))))
      (attempt
       (lambda ()
         (if (nth 2 state)
             (unless (advice-member-p #'nskk--isearch-prompt-advice
                                      'isearch-message-prefix)
               (advice-add 'isearch-message-prefix :around
                           #'nskk--isearch-prompt-advice))
           (when (advice-member-p #'nskk--isearch-prompt-advice
                                  'isearch-message-prefix)
             (advice-remove 'isearch-message-prefix
                            #'nskk--isearch-prompt-advice)))))
      (when condition-data
        (signal (car condition-data) (cdr condition-data))))))

;;;###autoload
(defun nskk-isearch-teardown ()
  "Remove only isearch integration owned by this NSKK instance."
  (let* ((transaction-state (nskk--isearch-transaction-state))
         (physical-before (nth 0 transaction-state))
         (owned-before (nth 1 transaction-state))
         (desired-state
          (nskk--isearch-desired-resource-state transaction-state))
         condition-data)
    (cl-labels ((attempt (function)
                  (condition-case new-condition
                      (funcall function)
                    ((error quit)
                     (unless condition-data
                       (setq condition-data new-condition))))))
      (when (and (nth 0 owned-before) (nth 0 physical-before))
        (attempt
         (lambda ()
           (remove-hook 'isearch-mode-hook #'nskk--isearch-setup))))
      (when (and (nth 1 owned-before) (nth 1 physical-before))
        (attempt
         (lambda ()
           (remove-hook 'isearch-mode-end-hook #'nskk--isearch-teardown))))
      (when (and (nth 2 owned-before) (nth 2 physical-before))
        (attempt
         (lambda ()
           (advice-remove 'isearch-message-prefix
                          #'nskk--isearch-prompt-advice))))
      (attempt
       (lambda ()
         (nskk--isearch-reconcile-resource-state desired-state)))
      (setq nskk--isearch-orig-buffer nil
            nskk--isearch-orig-buffer-stack nil)
      (nskk--isearch-finalize-ownership owned-before)
      (when condition-data
        (signal (car condition-data) (cdr condition-data))))))

;;;; Auto-enable

;; Auto-enable when nskk-isearch-enable changes
(defun nskk--isearch-enable-watcher (_symbol new-value operation _where)
  "Update isearch integration after setting the enable option.
NEW-VALUE is the new option value.  OPERATION identifies the variable change."
  (when (eq operation 'set)
    (if new-value
        (nskk-isearch-setup)
      (nskk-isearch-teardown))))

(defun nskk--isearch-register-enable-watcher ()
  "Register the enable-option watcher without taking preexisting ownership."
  (let ((watcher-state (nskk--isearch-watcher-state)))
    (condition-case condition-data
        (progn
          (unless (nth 0 watcher-state)
            (add-variable-watcher 'nskk-isearch-enable
                                  #'nskk--isearch-enable-watcher))
          (nskk--isearch-restore-watcher-presence t)
          (unless (nskk--isearch-watcher-present-p)
            (error "Failed to register NSKK isearch enable watcher"))
          (setq nskk--isearch-enable-watcher-owned
                (or (nth 1 watcher-state)
                    (not (nth 0 watcher-state)))))
      ((error quit)
       (let ((condition-symbol (car condition-data))
             (condition-payload (cdr condition-data)))
         (condition-case nil
             (nskk--isearch-restore-watcher-state watcher-state)
           ((error quit) nil))
         (signal condition-symbol condition-payload))))))

(defun nskk-isearch-unload-function ()
  "Remove all integration owned by nskk-isearch."
  (let* ((transaction-state (nskk--isearch-transaction-state))
         (owned-before (nth 1 transaction-state))
         (desired-resource-state
          (nskk--isearch-desired-resource-state transaction-state))
         (watcher-state (nskk--isearch-watcher-state))
         (desired-watcher-present
          (and (nth 0 watcher-state)
               (not (nth 1 watcher-state))))
         condition-data)
    (cl-labels ((attempt (function)
                  (condition-case new-condition
                      (funcall function)
                    ((error quit)
                     (unless condition-data
                       (setq condition-data new-condition))))))
      (attempt #'nskk-isearch-teardown)
      (when (and (nth 0 watcher-state) (nth 1 watcher-state))
        (attempt
         (lambda ()
           (remove-variable-watcher 'nskk-isearch-enable
                                    #'nskk--isearch-enable-watcher))))
      (setq nskk--isearch-orig-buffer nil
            nskk--isearch-orig-buffer-stack nil)
      (attempt
       (lambda ()
         (nskk--isearch-reconcile-lifecycle-state
          desired-resource-state
          desired-watcher-present)))
      (nskk--isearch-finalize-ownership owned-before)
      (setq nskk--isearch-enable-watcher-owned
            (and (nth 1 watcher-state)
                 (nskk--isearch-watcher-present-p)))
      (when condition-data
        (signal (car condition-data) (cdr condition-data)))
      nil)))

(nskk--isearch-register-enable-watcher)

(provide 'nskk-isearch)

;;; nskk-isearch.el ends here
