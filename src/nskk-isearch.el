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

;; Isearch integration for NSKK.

;;; Code:

(require 'cl-lib)
(require 'isearch)
(require 'nskk-state)
(require 'nskk-cps-macros)

(declare-function nskk-mode "nskk" (&optional arg))
(declare-function nskk-set-mode "nskk-input" (mode))
(declare-function nskk-reset-romaji-buffer "nskk-henkan" ())

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
  '((hiragana       . "[か]")
    (katakana       . "[ア]")
    (katakana-半角   . "[ｱ]")
    (jisx0208-latin . "[英]")
    (ascii          . "[aa]")
    (latin          . "[aa]")
    (abbrev         . "[aあ]"))
  "Alist mapping NSKK mode symbols to isearch prompt strings.
Used to show the current input mode in the isearch prompt.  The default
covers every symbol in `nskk--valid-modes'; a mode absent from this alist
searches normally but shows no indicator."
  :type '(alist :key-type symbol :value-type string)
  :safe (lambda (v)
          (and (listp v)
               (cl-every (lambda (e)
                           (and (consp e)
                                (symbolp (car e))
                                (stringp (cdr e))))
                         v)))
  :package-version '(nskk . "0.1.0")
  :group 'nskk-isearch)

;;;; Internal State

(defvar nskk--isearch-orig-buffer nil
  "The buffer where isearch was initiated.")

(defvar nskk--isearch-orig-buffer-stack nil
  "Stack of previous originating buffers for nested isearch sessions.")

(defvar nskk--isearch-input-sessions nil
  "Stack of search input buffers and their previous overriding maps.")

(defvar nskk--isearch-pending-buffers nil
  "Input buffers whose failed disposal must be retried at teardown.")

(defvar nskk--isearch-mode-hook-owned nil
  "Non-nil when NSKK installed its isearch mode hook.")

(defvar nskk--isearch-mode-end-hook-owned nil
  "Non-nil when NSKK installed its isearch mode end hook.")

(defvar nskk--isearch-prompt-advice-owned nil
  "Non-nil when NSKK installed its isearch prompt advice.")

(defvar nskk--isearch-enable-watcher-owned nil
  "Non-nil when NSKK installed its enable-option watcher.")

;;;; Condition Plumbing

(defun nskk--isearch-collect-condition (thunks)
  "Call every non-nil function in THUNKS, in order.
Return the first error or quit condition signalled, or nil when none was.
Later thunks run even after an earlier one fails, so a cleanup failure on
one resource cannot strand the resources that follow it."
  (let (condition-data)
    (dolist (thunk thunks condition-data)
      (when thunk
        (condition-case new-condition
            (funcall thunk)
          ((error quit)
           (unless condition-data
             (setq condition-data new-condition))))))))

(defun nskk--isearch-resignal (condition-data)
  "Re-signal CONDITION-DATA when it is non-nil."
  (when condition-data
    (signal (car condition-data) (cdr condition-data))))

(defun nskk--isearch-ignore-condition (thunk)
  "Call THUNK, discarding any error or quit it signals.
Used on a rollback path, where the condition that triggered the rollback
is the one the caller must see."
  (condition-case nil
      (funcall thunk)
    ((error quit) nil)))

(defun nskk--isearch-converge (read-state restore target)
  "Call RESTORE until READ-STATE returns TARGET, or until progress stops.
Return the first condition RESTORE signalled, or nil.

A callback that reinstalls an already-removed resource is repaired by a
later pass, so one bounded retry per element of TARGET is enough, plus a
final pass to observe the result.  Stopping as soon as a pass changes
nothing keeps a permanently re-polluting callback from looping forever."
  (let ((limit (1+ (length target)))
        (attempts 0)
        (keep-going t)
        condition-data)
    (while (and keep-going
                (< attempts limit)
                (not (equal (funcall read-state) target)))
      (let ((before (funcall read-state)))
        (condition-case new-condition
            (funcall restore)
          ((error quit)
           (unless condition-data
             (setq condition-data new-condition))))
        (setq attempts (1+ attempts))
        (let ((after (funcall read-state)))
          (when (or (equal after target)
                    (equal after before))
            (setq keep-going nil)))))
    condition-data))

;;;; Mode Indicator

(defun nskk--isearch-origin-mode (buffer)
  "Return the NSKK input mode active in BUFFER, or nil when there is none."
  (with-current-buffer buffer
    (when (and (boundp 'nskk-current-state)
               nskk-current-state)
      (nskk-state-mode nskk-current-state))))

(defun/k nskk--isearch-mode-string ()
	 "Return the isearch prompt indicator for the originating buffer's mode.
Fails when no live originating buffer holds an NSKK state whose mode has
an entry in `nskk-isearch-mode-string-alist'."
	 (let* ((buffer (or (caar nskk--isearch-input-sessions)
			    nskk--isearch-orig-buffer))
		(mode (and buffer
			   (buffer-live-p buffer)
			   (nskk--isearch-origin-mode buffer)))
		(indicator (and mode
				(cdr (assq mode nskk-isearch-mode-string-alist)))))
	   (if indicator
               (succeed indicator)
	     (fail))))

(defun nskk--isearch-prompt-advice (orig-fun &rest args)
  "Advice for `isearch-message-prefix' to add NSKK mode indicator.
ORIG-FUN is the original function; ARGS are forwarded unchanged."
  (let ((orig-prompt (apply orig-fun args)))
    (if nskk-isearch-enable
        (nskk--isearch-mode-string/k
         (lambda (indicator) (concat indicator " " orig-prompt))
         (lambda () orig-prompt))
      orig-prompt)))

;;;; Hook Functions

(defun nskk--isearch-input-pending-p ()
  "Return non-nil when the current search input is not yet committed."
  (or (nskk-state-henkan-phase nskk-current-state)
      (not (string-empty-p nskk--romaji-buffer))))

(defun nskk--isearch-pending-message ()
  "Display uncommitted input without adding it to the search string."
  (let* ((pending
          (with-current-buffer (caar nskk--isearch-input-sessions)
            (let ((overlay (nskk-state-conversion-overlay)))
              (concat
               (if (and (overlayp overlay) (overlay-buffer overlay)
                        (stringp (overlay-get overlay 'display)))
                   (concat (buffer-substring (point-min) (overlay-start overlay))
                           (overlay-get overlay 'display)
                           (buffer-substring (overlay-end overlay) (point-max)))
                 (buffer-string))
               nskk--romaji-buffer))))
         (isearch-message (concat isearch-message pending)))
    (isearch-message)))

(defun nskk--isearch-input (event)
  "Process EVENT in the isolated search input buffer."
  (let ((text
         (with-current-buffer (caar nskk--isearch-input-sessions)
           (let* ((overriding-terminal-local-map nil)
                  (overriding-local-map nil)
                  (last-command-event event)
                  (this-command (key-binding (vector event))))
             (command-execute this-command))
           (unless (nskk--isearch-input-pending-p)
             (prog1 (buffer-string) (erase-buffer))))))
    (if (and text (not (string-empty-p text)))
        (isearch-process-search-string text text)
      (isearch-update)
      (nskk--isearch-pending-message))))

(defun nskk--isearch-printing-char ()
  "Enter a character using the search session's NSKK mode."
  (interactive)
  (nskk--isearch-input last-command-event))

(defun nskk--isearch-delete-char ()
  "Delete pending input, or remove the last committed search character."
  (interactive)
  (if (with-current-buffer (caar nskk--isearch-input-sessions)
        (nskk-state-henkan-phase nskk-current-state))
      (nskk--isearch-input 127)
    (isearch-delete-char)))

(defun nskk--isearch-exit ()
  "Commit pending NSKK input before exiting the search."
  (interactive)
  (if (with-current-buffer (caar nskk--isearch-input-sessions)
        (nskk-state-henkan-phase nskk-current-state))
      (nskk--isearch-input 10)
    (isearch-exit)))

(defun nskk--isearch-newline ()
  "Commit conversion, switch from Latin input, or search for a newline."
  (interactive)
  (if (with-current-buffer (caar nskk--isearch-input-sessions)
        (or (memq (nskk-state-mode nskk-current-state)
                  '(ascii latin jisx0208-latin))
            (nskk-state-henkan-phase nskk-current-state)))
      (nskk--isearch-input 10)
    (isearch-printing-char)))

(defun nskk--isearch-abort ()
  "Cancel pending input before aborting the search."
  (interactive)
  (if (with-current-buffer (caar nskk--isearch-input-sessions)
        (and (not (nskk-state-henkan-phase nskk-current-state))
             (not (string-empty-p nskk--romaji-buffer))))
      (progn
        (with-current-buffer (caar nskk--isearch-input-sessions)
          (nskk-reset-romaji-buffer))
        (nskk--isearch-pending-message))
    (condition-case nil
        (nskk--isearch-input 7)
      (quit (isearch-abort)))))

(defun nskk--isearch-dispose-input (buffer)
  "Dispose of owned input BUFFER, retaining any cleanup failure.
Retry without hooks only for this disposable buffer.  If even that fails,
retain ownership for a later teardown instead of losing the buffer."
  (let (conditions)
    (when (buffer-live-p buffer)
      (condition-case err
          (unless (kill-buffer buffer) (error "Input buffer disposal refused"))
        ((error quit) (push err conditions)))
      (when (buffer-live-p buffer)
        (condition-case err
            (with-current-buffer buffer
              (let ((kill-buffer-hook nil)
                    (kill-buffer-query-functions nil))
                (unless (kill-buffer buffer)
                  (error "Input buffer disposal refused"))))
          ((error quit) (push err conditions)))))
    (when (buffer-live-p buffer)
      (cl-pushnew buffer nskk--isearch-pending-buffers))
    (when conditions
      (setq conditions (nreverse conditions))
      (signal (caar conditions)
              (append (cdar conditions)
                      (when (cdr conditions)
                        (list :cleanup-errors (cdr conditions))))))))

(defun nskk--isearch-setup ()
  "Push the previous origin and record the current isearch buffer."
  (let ((mode (and (bound-and-true-p nskk-mode)
                   (nskk--isearch-origin-mode (current-buffer))))
        (previous-map overriding-terminal-local-map)
        input-buffer input-map)
    (push nskk--isearch-orig-buffer nskk--isearch-orig-buffer-stack)
    (setq nskk--isearch-orig-buffer (current-buffer))
    (condition-case err
        (progn
	  (when mode
	    (setq input-buffer (generate-new-buffer " *nskk-isearch-input*"))
	    (with-current-buffer input-buffer
              (nskk-mode 1)
              (nskk-set-mode (if (eq mode 'katakana-半角) 'ascii mode)))
	    (let ((map (make-sparse-keymap)))
              (set-keymap-parent map previous-map)
              (dotimes (offset 95)
		(define-key map (vector (+ 32 offset)) #'nskk--isearch-printing-char))
              (define-key map (kbd "DEL") #'nskk--isearch-delete-char)
              (define-key map (kbd "RET") #'nskk--isearch-exit)
              (define-key map (kbd "C-g") #'nskk--isearch-abort)
              (define-key map (kbd "C-j") #'nskk--isearch-newline)
              (setq input-map map
		    overriding-terminal-local-map map)))
	  (push (list input-buffer input-map previous-map)
		nskk--isearch-input-sessions))
      ((error quit)
       (setq overriding-terminal-local-map previous-map
             nskk--isearch-orig-buffer
             (pop nskk--isearch-orig-buffer-stack))
       (let ((cleanup
              (nskk--isearch-collect-condition
               (list (lambda () (nskk--isearch-dispose-input input-buffer))))))
         (signal (car err)
                 (append (cdr err)
                         (when cleanup (list :cleanup-errors (list cleanup))))))))))

(defun nskk--isearch-teardown ()
  "Restore the previous originating buffer when isearch ends."
  (let ((session (pop nskk--isearch-input-sessions))
        (pending nskk--isearch-pending-buffers))
    (setq nskk--isearch-pending-buffers nil)
    ;; `isearch-done' normally restores its map before running this hook.
    (when (and (nth 1 session)
               (eq overriding-terminal-local-map (nth 1 session)))
      (setq overriding-terminal-local-map (nth 2 session)))
    (setq nskk--isearch-orig-buffer
          (and nskk--isearch-orig-buffer-stack
               (pop nskk--isearch-orig-buffer-stack)))
    (nskk--isearch-resignal
     (nskk--isearch-collect-condition
      (mapcar (lambda (buffer)
                (lambda () (nskk--isearch-dispose-input buffer)))
              (cons (car session) pending))))))

;;;; Resource And Ownership State

(defun nskk-isearch-resource-state ()
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
  (list (nskk-isearch-resource-state)
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
              (nskk-isearch-resource-state))))

;;;; Watcher State

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
  (let ((condition-data
         (nskk--isearch-collect-condition
          (list (lambda ()
                  (nskk--isearch-restore-watcher-presence (nth 0 state)))))))
    (setq nskk--isearch-enable-watcher-owned (nth 1 state))
    (nskk--isearch-resignal condition-data)))

;;;; Restore And Reconcile

(defun nskk--isearch-restore-hook (present hook function)
  "Add or remove FUNCTION on HOOK so its membership matches PRESENT."
  (if present
      (unless (memq function (symbol-value hook))
        (add-hook hook function))
    (when (memq function (symbol-value hook))
      (remove-hook hook function))))

(defun nskk--isearch-restore-advice (present)
  "Add or remove the isearch prompt advice so its presence matches PRESENT."
  (if present
      (unless (advice-member-p #'nskk--isearch-prompt-advice
                               'isearch-message-prefix)
        (advice-add 'isearch-message-prefix :around
                    #'nskk--isearch-prompt-advice))
    (when (advice-member-p #'nskk--isearch-prompt-advice
                           'isearch-message-prefix)
      (advice-remove 'isearch-message-prefix
                     #'nskk--isearch-prompt-advice))))

(defun nskk--isearch-restore-resource-state (state)
  "Restore NSKK isearch resources to presence STATE.
Continue restoring independent resources after a cleanup failure.
Signal the first cleanup condition after all restore attempts."
  (nskk--isearch-resignal
   (nskk--isearch-collect-condition
    (list (lambda ()
            (nskk--isearch-restore-hook (nth 0 state)
                                        'isearch-mode-hook
                                        #'nskk--isearch-setup))
          (lambda ()
            (nskk--isearch-restore-hook (nth 1 state)
                                        'isearch-mode-end-hook
                                        #'nskk--isearch-teardown))
          (lambda ()
            (nskk--isearch-restore-advice (nth 2 state)))))))

(defun nskk--isearch-restore-transaction-state (state)
  "Restore isearch resources to physical and ownership STATE."
  (let ((condition-data
         (nskk--isearch-collect-condition
          (list (lambda ()
                  (nskk--isearch-restore-resource-state (nth 0 state)))))))
    (nskk--isearch-set-ownership-state (nth 1 state))
    (nskk--isearch-resignal condition-data)))

(defun nskk--isearch-lifecycle-state ()
  "Return the three resource presences followed by the watcher presence."
  (append (nskk-isearch-resource-state)
          (list (nskk--isearch-watcher-present-p))))

(defun nskk--isearch-reconcile-resource-state (state)
  "Reconcile all isearch resources to physical STATE."
  (nskk--isearch-resignal
   (nskk--isearch-converge
    #'nskk-isearch-resource-state
    (lambda () (nskk--isearch-restore-resource-state state))
    state)))

(defun nskk--isearch-reconcile-lifecycle-state (resource-state watcher-present)
  "Reconcile resources to RESOURCE-STATE and the watcher to WATCHER-PRESENT."
  (nskk--isearch-resignal
   (nskk--isearch-converge
    #'nskk--isearch-lifecycle-state
    (lambda ()
      (nskk--isearch-resignal
       (nskk--isearch-collect-condition
        (list (lambda ()
                (nskk--isearch-reconcile-resource-state resource-state))
              (lambda ()
                (nskk--isearch-restore-watcher-presence watcher-present))))))
    (append resource-state (list watcher-present)))))

;;;; Setup/Teardown

(defun nskk--isearch-acquire-resources (physical-before)
  "Install each isearch resource that PHYSICAL-BEFORE reports absent."
  (unless (nth 0 physical-before)
    (add-hook 'isearch-mode-hook #'nskk--isearch-setup))
  (unless (nth 1 physical-before)
    (add-hook 'isearch-mode-end-hook #'nskk--isearch-teardown))
  (unless (nth 2 physical-before)
    (advice-add 'isearch-message-prefix :around
                #'nskk--isearch-prompt-advice)))

(defun nskk--isearch-ownership-after-acquisition (physical-before owned-before)
  "Return ownership gained by installing what PHYSICAL-BEFORE lacked.
OWNED-BEFORE is the ownership state held before acquisition; a resource
already present and not owned stays unowned, so teardown leaves it alone."
  (cl-mapcar (lambda (was-present was-owned)
               (or was-owned (not was-present)))
             physical-before
             owned-before))

(defun nskk--isearch-release-thunks (physical-before owned-before)
  "Return a removal thunk per resource both owned and installed.
PHYSICAL-BEFORE and OWNED-BEFORE are the snapshotted presence and
ownership triples; an entry is nil where nothing should be released."
  (list (and (nth 0 owned-before)
             (nth 0 physical-before)
             (lambda ()
               (remove-hook 'isearch-mode-hook #'nskk--isearch-setup)))
        (and (nth 1 owned-before)
             (nth 1 physical-before)
             (lambda ()
               (remove-hook 'isearch-mode-end-hook #'nskk--isearch-teardown)))
        (and (nth 2 owned-before)
             (nth 2 physical-before)
             (lambda ()
               (advice-remove 'isearch-message-prefix
                              #'nskk--isearch-prompt-advice)))))

;;;###autoload
(defun nskk-isearch-setup ()
  "Install NSKK isearch integration transactionally.
Adds hooks and advice to enable Japanese isearch."
  (let ((transaction-state (nskk--isearch-transaction-state)))
    (condition-case condition-data
        (let ((physical-before (nth 0 transaction-state)))
          (nskk--isearch-acquire-resources physical-before)
          (nskk--isearch-reconcile-resource-state '(t t t))
          (unless (equal (nskk-isearch-resource-state) '(t t t))
            (error "Failed to install NSKK isearch integration"))
          (nskk--isearch-set-ownership-state
           (nskk--isearch-ownership-after-acquisition
            physical-before
            (nth 1 transaction-state))))
      ((error quit)
       (nskk--isearch-ignore-condition
        (lambda ()
          (nskk--isearch-restore-transaction-state transaction-state)))
       (signal (car condition-data) (cdr condition-data))))))

;;;###autoload
(defun nskk-isearch-teardown ()
  "Remove only isearch integration owned by this NSKK instance."
  (let* ((transaction-state (nskk--isearch-transaction-state))
         (owned-before (nth 1 transaction-state))
         (desired-state
          (nskk--isearch-desired-resource-state transaction-state))
         (condition-data
          (nskk--isearch-collect-condition
           (append
            (nskk--isearch-release-thunks (nth 0 transaction-state)
                                          owned-before)
            (make-list (max (length nskk--isearch-input-sessions)
                            (if nskk--isearch-pending-buffers 1 0))
                       #'nskk--isearch-teardown)
            (list (lambda ()
                    (nskk--isearch-reconcile-resource-state
                     desired-state)))))))
    (setq nskk--isearch-orig-buffer nil
          nskk--isearch-orig-buffer-stack nil)
    (nskk--isearch-finalize-ownership owned-before)
    (nskk--isearch-resignal condition-data)))

;;;; Auto-enable

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
          ;; Two sweeps: a watcher that another callback removes during the
          ;; first add is reinstated by the second, which is what the
          ;; presence check below then observes.
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
       (nskk--isearch-ignore-condition
        (lambda ()
          (nskk--isearch-restore-watcher-state watcher-state)))
       (signal (car condition-data) (cdr condition-data))))))

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
         (condition-data
          (nskk--isearch-collect-condition
           (list #'nskk-isearch-teardown
                 (and (nth 0 watcher-state)
                      (nth 1 watcher-state)
                      (lambda ()
                        (remove-variable-watcher
                         'nskk-isearch-enable
                         #'nskk--isearch-enable-watcher)))))))
    (setq nskk--isearch-orig-buffer nil
          nskk--isearch-orig-buffer-stack nil)
    (let ((reconcile-condition
           (nskk--isearch-collect-condition
            (list (lambda ()
                    (nskk--isearch-reconcile-lifecycle-state
                     desired-resource-state
                     desired-watcher-present))))))
      (unless condition-data
        (setq condition-data reconcile-condition)))
    (nskk--isearch-finalize-ownership owned-before)
    (setq nskk--isearch-enable-watcher-owned
          (and (nth 1 watcher-state)
               (nskk--isearch-watcher-present-p)))
    (nskk--isearch-resignal condition-data)
    nil))

(nskk--isearch-register-enable-watcher)

(provide 'nskk-isearch)

;;; nskk-isearch.el ends here
