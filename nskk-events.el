;;; nskk-events.el --- Event system for NSKK extensions  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  NSKK Contributors

;; Author: NSKK Contributors
;; URL: https://github.com/takeokunn/nskk.el
;; Keywords: i18n

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

;; Event system for extension integration.
;; Provides pub/sub mechanism for extension communication.

;;; Code:

;;;; Token Generation
(defun generate-new-token ()
  "Generate a unique token for event subscription."
  (cl-gensym))

;;;; Event Types
(defconst nskk-event-types
  '(;; Input events
    (input-start . "Emitted when input begins")
    (input-end . "Emitted when input ends")
    (key-pressed . "Emitted on key press")
    (key-rejected . "Emitted when key is rejected")

    ;; Mode events
    (mode-change . "Emitted on mode change")
    (mode-enter . "Emitted when entering a mode")
    (mode-exit . "Emitted when exiting a mode")

    ;; Conversion events
    (conversion-start . "Emitted when conversion begins")
    (conversion-end . "Emitted when conversion ends")
    (conversion-candidate . "Emitted when candidate is selected")
    (conversion-commit . "Emitted when conversion is committed")

    ;; State events
    (state-change . "Emitted on state change")
    (state-save . "Emitted when state is saved")
    (state-restore . "Emitted when state is restored")

    ;; UI events
    (ui-update . "Emitted when UI updates")
    (ui-show . "Emitted when UI is shown")
    (ui-hide . "Emitted when UI is hidden")

    ;; Dictionary events
    (dict-load . "Emitted when dictionary loads")
    (dict-save . "Emitted when dictionary saves")
    (dict-search . "Emitted on dictionary search")
    (dict-update . "Emitted when dictionary updates")

    ;; Extension lifecycle
    (extension-load . "Emitted when extension loads")
    (extension-unload . "Emitted when extension unloads")
    (extension-error . "Emitted on extension error")

    ;; System events
    (init . "Emitted on initialization")
    (shutdown . "Emitted on shutdown")
    (error . "Emitted on error"))
  "Event types supported by NSKK.
Each event is a cons cell (SYMBOL . DESCRIPTION).")

(defvar nskk-event-subscribers (make-hash-table :test 'eq)
  "Hash table mapping event types to lists of subscribers.
Each subscriber is a function that receives the event data.")

(defvar nskk-event-history nil
  "List of recent events for debugging.
Each entry is (TIMESTAMP EVENT-TYPE DATA).")

(defvar nskk-event-history-max 100
  "Maximum number of events to keep in history.")

(defun nskk-event-subscribe (event-type callback)
  "Subscribe to EVENT-TYPE with CALLBACK function.
Returns a token that can be used with `nskk-event-unsubscribe'.

EVENT-TYPE should be a symbol from `nskk-event-types'.
CALLBACK is a function that receives a single argument (event data plist)."
  (unless (assq event-type nskk-event-types)
    (error "Unknown event type: %s" event-type))
  (unless callback
    (error "Callback must not be nil"))

  (let* ((subscribers (gethash event-type nskk-event-subscribers))
         (token (generate-new-token))
         (subscriber-info (cons token callback)))
    (puthash event-type
             (append subscribers (list subscriber-info))
             nskk-event-subscribers)
    token))

(defun nskk-event-unsubscribe (event-type token)
  "Unsubscribe from EVENT-TYPE using TOKEN.
TOKEN should be the value returned by `nskk-event-subscribe'."
  (let* ((subscribers (gethash event-type nskk-event-subscribers))
         (filtered (seq-remove (lambda (s) (eq (car s) token))
                              subscribers)))
    (puthash event-type filtered nskk-event-subscribers)))

(defun nskk-event-emit (event-type &rest data)
  "Emit EVENT-TYPE with DATA as event properties.
All subscribers to this event type will be called with DATA."
  (unless (assq event-type nskk-event-types)
    (error "Unknown event type: %s" event-type))

  ;; Record in history
  (push (list (current-time) event-type data)
        nskk-event-history)
  (when (> (length nskk-event-history) nskk-event-history-max)
    (setq nskk-event-history
          (nthcdr nskk-event-history-max nskk-event-history)))

  ;; Notify subscribers
  (dolist (subscriber (gethash event-type nskk-event-subscribers))
    (condition-case err
        (funcall (cdr subscriber) data)
      (error
       (message "NSKK event subscriber error: %S" err)
       (nskk-event-emit 'extension-error
                       :event-type event-type
                       :error err)))))

(defun nskk-event-history-clear ()
  "Clear event history."
  (setq nskk-event-history nil))

(defun nskk-event-history-get (&optional event-type limit)
  "Get recent events from history.
If EVENT-TYPE is specified, filter by that type.
LIMIT specifies maximum number of events (default 10)."
  (let* ((limit (or limit 10))
         (events (if event-type
                     (seq-filter (lambda (e)
                                   (eq (cadr e) event-type))
                                 nskk-event-history)
                   nskk-event-history))
         (count (min limit (length events))))
    (seq-take events count)))

(defun nskk-event-history-format (event)
  "Format EVENT for display."
  (cl-destructuring-bind (timestamp _type _data) event
    (format-time-string "%H:%M:%S" timestamp)))

(provide 'nskk-events)

;;; nskk-events.el ends here
