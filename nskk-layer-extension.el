;;; nskk-layer-extension.el --- Extension layer interface  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  NSKK Contributors

;; Author: NSKK Contributors
;; Keywords: Japanese, input method, extensions

;; This file is part of NSKK.

;; NSKK is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; NSKK is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with NSKK.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Extension layer provides:
;; - Hook interface compatible with DDSKK
;; - Event subscription mechanism
;; - Extension lifecycle management
;;
;; DDSKK Hook Compatibility:
;; This layer maps DDSKK hook names to NSKK event system,
;; allowing existing DDSKK extensions to work with minimal changes.

;;; Code:

(require 'nskk-events)

;;; Hook Compatibility Layer (DDSKK -> NSKK)

(defvar nskk-extensions-loaded nil
  "List of loaded extensions.")

;; Input hooks
(defvar nskk-mode-hook nil
  "Hook run when NSKK mode is enabled.
DDSKK equivalent: skk-mode-hook")

(defvar nskk-mode-off-hook nil
  "Hook run when NSKK mode is disabled.
DDSKK equivalent: skk-mode-off-hook")

(defvar nskk-input-mode-hook nil
  "Hook run when input mode changes.
DDSKK equivalent: skk-input-mode-hook")

;; Conversion hooks
(defvar nskk-start-henkan-hook nil
  "Hook run before conversion starts.
DDSKK equivalent: skk-start-henkan-hook")
(put 'nskk-start-henkan-hook 'permanent-local t)

(defvar nskk-henkan-hook nil
  "Hook run during conversion.
DDSKK equivalent: skk-henkan-hook")

(defvar nskk-post-henkan-hook nil
  "Hook run after conversion completes.
DDSKK equivalent: skk-post-henkan-hook")

(defvar nskk-after-henkan-hook nil
  "Hook run after conversion is committed.
DDSKK equivalent: skk-after-henkan-hook")

;; Candidate hooks
(defvar nskk-henkan-select-hook nil
  "Hook run when a candidate is selected.
DDSKK equivalent: skk-henkan-select-hook")

;; Dictionary hooks
(defvar nskk-search-jisyo-hook nil
  "Hook run during dictionary search.
DDSKK equivalent: skk-search-jisyo-hook")

(defvar nskk-jisyo-update-hook nil
  "Hook run when dictionary is updated.
DDSKK equivalent: skk-jisyo-update-hook")

;; State hooks
(defvar nskk-save-history-hook nil
  "Hook run when history is saved.
DDSKK equivalent: skk-save-history-hook")

;; Keymap hooks
(defvar nskk-annotate-mode-map-hook nil
  "Hook run when annotating mode map.
DDSKK equivalent: skk-annotate-mode-map-hook")

(defvar nskk-annotate-minibuffer-map-hook nil
  "Hook run when annotating minibuffer map.
DDSKK equivalent: skk-annotate-minibuffer-map-hook")

;;; Hook to Event Bridge

(defun nskk-hook-to-event-bridge (hook-name event-type)
  "Convert HOOK-NAME to EVENT-TYPE subscription.
This allows DDSKK-style hooks to emit NSKK events."
  (add-hook hook-name
            (lambda (&rest args)
              (apply #'nskk-event-emit event-type args))))

;; Establish bridge for standard hooks
(dolist (hook-spec '((nskk-mode-hook . mode-enter)
                     (nskk-mode-off-hook . mode-exit)
                     (nskk-start-henkan-hook . conversion-start)
                     (nskk-henkan-hook . conversion-candidate)
                     (nskk-post-henkan-hook . conversion-end)
                     (nskk-after-henkan-hook . conversion-commit)
                     (nskk-henkan-select-hook . conversion-candidate)
                     (nskk-search-jisyo-hook . dict-search)
                     (nskk-jisyo-update-hook . dict-update)
                     (nskk-save-history-hook . state-save)))
  (nskk-hook-to-event-bridge (car hook-spec) (cdr hook-spec)))

;;; Extension Management

(defun nskk-extension-load (extension)
  "Load EXTENSION and initialize it.
EXTENSION should be a symbol naming the extension feature.
Returns t if successful, nil otherwise."
  (condition-case err
      (progn
        (require extension)
        (push extension nskk-extensions-loaded)
        (nskk-event-emit 'extension-load :extension extension)
        t)
    (error
     (message "NSKK failed to load extension %s: %S" extension err)
     (nskk-event-emit 'extension-error
                     :extension extension
                     :error err)
     nil)))

(defun nskk-extension-unload (extension)
  "Unload EXTENSION and cleanup.
Returns t if successful, nil otherwise."
  (when (memq extension nskk-extensions-loaded)
    (setq nskk-extensions-loaded
          (delq extension nskk-extensions-loaded))
    (nskk-event-emit 'extension-unload :extension extension)
    t))

(defun nskk-extension-loaded-p (extension)
  "Check if EXTENSION is loaded."
  (memq extension nskk-extensions-loaded))

;;; Event Subscription Helpers

(defmacro nskk-on (event-type &rest body)
  "Subscribe to EVENT-TYPE and run BODY on event.
BODY is wrapped in a lambda and receives event data as implicit plist.
Access event properties with `nskk-event-prop' or destructuring-bind."
  (declare (indent 1))
  (let ((callback (gensym "callback")))
    `(let ((,callback (lambda (data) ,@body)))
       (nskk-event-subscribe ',event-type ,callback))))

(defun nskk-event-prop (prop data)
  "Get PROP from event DATA plist."
  (plist-get data prop))

(defmacro nskk-define-extension (name &rest body)
  "Define an extension named NAME.
BODY is executed when extension loads.
Use `nskk-on' inside to subscribe to events."
  (declare (indent 1))
  `(progn
     (eval-after-load 'nskk-events
       '(progn
          ,@body))
     (provide ',name)))

;;; Initialization

(defun nskk-extension-init ()
  "Initialize extension layer."
  (nskk-event-emit 'init))

(add-hook 'after-init-hook #'nskk-extension-init)

(provide 'nskk-layer-extension)

;;; nskk-layer-extension.el ends here
