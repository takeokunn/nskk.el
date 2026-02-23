;;; nskk-events-test.el --- Event system tests -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Authors

;; Author: NSKK Developers
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Comprehensive tests for nskk-events.el covering:
;; - Event subscription
;; - Event emission
;; - Event unsubscription
;; - Event history tracking
;; - Event type validation
;; - Subscriber error handling
;; - Multiple subscribers
;; - Event data propagation

;;; Code:

(require 'ert)
(require 'nskk-events)
(require 'nskk-test-framework)

;; Test Variables

(defvar nskk-events-test-received-events nil
  "List of events received during test.")

(defvar nskk-events-test-call-count nil
  "Number of times a callback was called.")

;;;
;;; Helper Functions
;;;

(defun nskk-events-test-reset ()
  "Reset test state."
  (setq nskk-events-test-received-events nil
        nskk-events-test-call-count 0))

(defun nskk-events-test-recorder (data)
  "Record event data for testing."
  (push data nskk-events-test-received-events))

(defun nskk-events-test-counter (_data)
  "Increment call counter."
  (cl-incf nskk-events-test-call-count))

(defun nskk-events-test-clear-all-subscribers ()
  "Clear all event subscribers for clean test state."
  (setq nskk-event-subscribers (make-hash-table :test 'eq))
  (nskk-event-history-clear))

;;;
;;; Event Subscription Tests
;;;

(nskk-deftest-unit event-subscribe-basic
  "Test basic event subscription."
  (nskk-events-test-reset)
  (nskk-events-test-clear-all-subscribers)

  (let ((token (nskk-event-subscribe 'input-start #'nskk-events-test-recorder)))
    (should token)))

(nskk-deftest-unit event-subscribe-receive
  "Test receiving subscribed event."
  (nskk-events-test-reset)
  (nskk-events-test-clear-all-subscribers)

  (nskk-event-subscribe 'input-start #'nskk-events-test-recorder)
  (nskk-event-emit 'input-start :test-data "value")

  (should (= (length nskk-events-test-received-events) 1))
  (should (equal (car nskk-events-test-received-events) '(:test-data "value"))))

(nskk-deftest-unit event-subscribe-multiple
  "Test subscribing to multiple event types."
  (nskk-events-test-reset)
  (nskk-events-test-clear-all-subscribers)

  (nskk-event-subscribe 'input-start #'nskk-events-test-recorder)
  (nskk-event-subscribe 'input-end #'nskk-events-test-recorder)
  (nskk-event-subscribe 'mode-change #'nskk-events-test-recorder)

  (nskk-event-emit 'input-start :event "start")
  (nskk-event-emit 'input-end :event "end")
  (nskk-event-emit 'mode-change :event "change")

  (should (= (length nskk-events-test-received-events) 3)))

(nskk-deftest-unit event-subscribe-invalid-type
  "Test error when subscribing to invalid event type."
  (nskk-events-test-clear-all-subscribers)
  (should-error (nskk-event-subscribe 'invalid-event-type #'ignore)))

(nskk-deftest-unit event-subscribe-nil-callback
  "Test error when subscribing with nil callback."
  (nskk-events-test-clear-all-subscribers)
  (should-error (nskk-event-subscribe 'input-start nil)))

(nskk-deftest-unit event-subscribe-duplicate
  "Test subscribing same callback multiple times."
  (nskk-events-test-reset)
  (nskk-events-test-clear-all-subscribers)

  (nskk-event-subscribe 'input-start #'nskk-events-test-recorder)
  (nskk-event-subscribe 'input-start #'nskk-events-test-recorder)

  (nskk-event-emit 'input-start :test "data")

  ;; Should receive twice since subscribed twice
  (should (= (length nskk-events-test-received-events) 2)))

;;;
;;; Event Unsubscription Tests
;;;

(nskk-deftest-unit event-unsubscribe-basic
  "Test basic event unsubscription."
  (nskk-events-test-reset)
  (nskk-events-test-clear-all-subscribers)

  (let ((token (nskk-event-subscribe 'input-start #'nskk-events-test-recorder)))
    (nskk-event-emit 'input-start :test "1")
    (nskk-event-unsubscribe 'input-start token)
    (nskk-event-emit 'input-start :test "2")

    (should (= (length nskk-events-test-received-events) 1))
    (should (equal (car nskk-events-test-received-events) '(:test "1")))))

(nskk-deftest-unit event-unsubscribe-multiple
  "Test unsubscribing multiple callbacks."
  (nskk-events-test-reset)
  (nskk-events-test-clear-all-subscribers)

  (let ((token1 (nskk-event-subscribe 'input-start #'nskk-events-test-recorder)))
    (let ((token2 (nskk-event-subscribe 'input-start #'nskk-events-test-recorder)))
      (nskk-event-emit 'input-start :test "1")

      (nskk-event-unsubscribe 'input-start token1)
      (nskk-event-emit 'input-start :test "2")

      (nskk-event-unsubscribe 'input-start token2)
      (nskk-event-emit 'input-start :test "3")

      ;; token1 received 1 and 2, token2 only received 1
      (should (= (length nskk-events-test-received-events) 3)))))

(nskk-deftest-unit event-unsubscribe-invalid-token
  "Test unsubscription with invalid token (should not error)."
  (nskk-events-test-reset)
  (nskk-events-test-clear-all-subscribers)
  (nskk-event-subscribe 'input-start #'nskk-events-test-recorder)

  ;; Should not error
  (nskk-event-unsubscribe 'input-start :invalid-token)

  (nskk-event-emit 'input-start :test "data")

  ;; Original subscriber should still work
  (should (= (length nskk-events-test-received-events) 1)))

(nskk-deftest-unit event-unsubscribe-wrong-event-type
  "Test unsubscription with wrong event type."
  (nskk-events-test-reset)
  (nskk-events-test-clear-all-subscribers)

  (let ((token (nskk-event-subscribe 'input-start #'nskk-events-test-recorder)))
    ;; Try to unsubscribe from different event type
    (nskk-event-unsubscribe 'input-end token)

    (nskk-event-emit 'input-start :test "data")

    ;; Should still receive event since unsubscribe didn't work
    (should (= (length nskk-events-test-received-events) 1))))

;;;
;;; Event Emission Tests
;;;

(nskk-deftest-unit event-emit-no-subscribers
  "Test emitting event with no subscribers."
  (nskk-events-test-clear-all-subscribers)
  ;; Should not error
  (nskk-event-emit 'input-start :test "data"))

(nskk-deftest-unit event-emit-with-data
  "Test emitting event with data."
  (nskk-events-test-reset)
  (nskk-events-test-clear-all-subscribers)

  (nskk-event-subscribe 'input-start #'nskk-events-test-recorder)
  (nskk-event-emit 'input-start :key1 "value1" :key2 123 :key3 '(a b c))

  (should (equal (car nskk-events-test-received-events)
                 '(:key1 "value1" :key2 123 :key3 (a b c)))))

(nskk-deftest-unit event-emit-invalid-type
  "Test error when emitting invalid event type."
  (nskk-events-test-clear-all-subscribers)
  (should-error (nskk-event-emit 'invalid-event-type :data "test")))

(nskk-deftest-unit event-emit-empty-data
  "Test emitting event with no data."
  (nskk-events-test-reset)
  (nskk-events-test-clear-all-subscribers)

  (nskk-event-subscribe 'input-start #'nskk-events-test-recorder)
  (nskk-event-emit 'input-start)

  (should (equal (car nskk-events-test-received-events) nil)))

;;;
;;; Event History Tests
;;;

(nskk-deftest-unit event-history-basic
  "Test event history tracking."
  (nskk-events-test-clear-all-subscribers)

  (nskk-event-emit 'input-start :data "1")
  (nskk-event-emit 'input-end :data "2")
  (nskk-event-emit 'mode-change :data "3")

  (let ((history (nskk-event-history-get nil 10)))
    (should (= (length history) 3))))

(nskk-deftest-unit event-history-limit
  "Test event history limit parameter."
  (nskk-events-test-clear-all-subscribers)

  (dotimes (i 10)
    (nskk-event-emit 'input-start :index i))

  (let ((history-3 (nskk-event-history-get nil 3))
        (history-5 (nskk-event-history-get nil 5)))
    (should (= (length history-3) 3))
    (should (= (length history-5) 5))))

(nskk-deftest-unit event-history-filter-by-type
  "Test filtering event history by type."
  (nskk-events-test-clear-all-subscribers)

  (nskk-event-emit 'input-start :data "1")
  (nskk-event-emit 'input-end :data "2")
  (nskk-event-emit 'input-start :data "3")
  (nskk-event-emit 'mode-change :data "4")
  (nskk-event-emit 'input-start :data "5")

  (let ((history (nskk-event-history-get 'input-start 10)))
    (should (= (length history) 3))
    ;; All should be input-start events
    (dolist (event history)
      (should (eq (cadr event) 'input-start)))))

(nskk-deftest-unit event-history-max-size
  "Test event history maximum size enforcement."
  (nskk-events-test-clear-all-subscribers)

  ;; Emit more than max history size (100)
  (dotimes (i 150)
    (nskk-event-emit 'input-start :index i))

  (let ((history (nskk-event-history-get nil 200)))
    ;; Should be limited to max size
    (should (<= (length history) nskk-event-history-max))))

(nskk-deftest-unit event-history-clear
  "Test clearing event history."
  (nskk-events-test-clear-all-subscribers)

  (nskk-event-emit 'input-start :data "1")
  (nskk-event-emit 'input-end :data "2")

  (nskk-event-history-clear)

  (let ((history (nskk-event-history-get nil 10)))
    (should (= (length history) 0))))

(nskk-deftest-unit event-history-structure
  "Test event history entry structure."
  (nskk-events-test-clear-all-subscribers)

  (nskk-event-emit 'input-start :test "data")

  (let ((history (nskk-event-history-get nil 1)))
    (should (= (length history) 1))
    (let ((entry (car history)))
      ;; Entry should be (timestamp event-type data)
      (should (= (length entry) 3))
      (should (listp (car entry)))  ; timestamp is a time value (list)
      (should (eq (cadr entry) 'input-start))
      (should (equal (caddr entry) '(:test "data"))))))

;;;
;;; Multiple Subscribers Tests
;;;

(nskk-deftest-unit event-multiple-subscribers
  "Test multiple subscribers to same event."
  (nskk-events-test-reset)
  (nskk-events-test-clear-all-subscribers)

  (let ((count1 0)
        (count2 0)
        (count3 0))
    (nskk-event-subscribe 'input-start (lambda (_) (cl-incf count1)))
    (nskk-event-subscribe 'input-start (lambda (_) (cl-incf count2)))
    (nskk-event-subscribe 'input-start (lambda (_) (cl-incf count3)))

    (nskk-event-emit 'input-start :data "test")

    (should (= count1 1))
    (should (= count2 1))
    (should (= count3 1))))

(nskk-deftest-unit event-different-event-types
  "Test subscribers to different event types."
  (nskk-events-test-reset)
  (nskk-events-test-clear-all-subscribers)

  (let ((input-count 0)
        (mode-count 0))
    (nskk-event-subscribe 'input-start (lambda (_) (cl-incf input-count)))
    (nskk-event-subscribe 'mode-change (lambda (_) (cl-incf mode-count)))

    (nskk-event-emit 'input-start :data "1")
    (nskk-event-emit 'mode-change :data "2")
    (nskk-event-emit 'input-start :data "3")
    (nskk-event-emit 'input-end :data "4")

    (should (= input-count 2))
    (should (= mode-count 1))))

;;;
;;; Subscriber Error Handling Tests
;;;

(nskk-deftest-unit event-subscriber-error-caught
  "Test that subscriber errors are caught and don't stop emission."
  (nskk-events-test-reset)
  (nskk-events-test-clear-all-subscribers)

  (let ((normal-called nil))
    (nskk-event-subscribe 'input-start (lambda (_) (error "Test error in callback")))
    (nskk-event-subscribe 'input-start (lambda (_) (setq normal-called t)))

    (nskk-event-emit 'input-start :data "test")

    ;; Normal callback should still be called despite error
    (should normal-called)))

(nskk-deftest-unit event-subscriber-error-emits-error-event
  "Test that subscriber error emits extension-error event."
  (nskk-events-test-reset)
  (nskk-events-test-clear-all-subscribers)

  (let ((error-event-received nil))
    (nskk-event-subscribe 'input-start (lambda (_) (error "Test error")))
    (nskk-event-subscribe 'extension-error (lambda (_) (setq error-event-received t)))

    (nskk-event-emit 'input-start :data "test")

    (should error-event-received)))

;;;
;;; Event Type Validation Tests
;;;

(nskk-deftest-unit event-types-all-valid
  "Test that all defined event types are valid."
  (dolist (event-def nskk-event-types)
    (let ((event-type (car event-def)))
      ;; Should not error when subscribing to defined types
      (let ((token (nskk-event-subscribe event-type #'ignore)))
        (nskk-event-unsubscribe event-type token)))))

(nskk-deftest-unit event-type-categories
  "Test event type categories."
  (let ((input-events '(input-start input-end key-pressed key-rejected))
        (mode-events '(mode-change mode-enter mode-exit))
        (conversion-events '(conversion-start conversion-end
                                            conversion-candidate conversion-commit))
        (state-events '(state-change state-save state-restore))
        (ui-events '(ui-update ui-show ui-hide))
        (dict-events '(dict-load dict-save dict-search dict-update))
        (extension-events '(extension-load extension-unload extension-error))
        (system-events '(init shutdown error)))

    ;; Verify these are all valid event types
    (dolist (event-type (append input-events mode-events conversion-events
                                state-events ui-events dict-events
                                extension-events system-events))
      (should (assq event-type nskk-event-types)))))

;;;
;;; Integration Tests
;;;

(nskk-deftest-integration event-full-lifecycle
  "Test full event lifecycle: subscribe, emit, unsubscribe."
  (nskk-events-test-reset)
  (nskk-events-test-clear-all-subscribers)

  ;; Subscribe to multiple events
  (let ((start-token (nskk-event-subscribe 'input-start #'nskk-events-test-recorder)))
    (let ((end-token (nskk-event-subscribe 'input-end #'nskk-events-test-recorder)))
      (nskk-event-emit 'input-start :phase "start")
      (nskk-event-emit 'input-end :phase "end")

      (should (= (length nskk-events-test-received-events) 2))

      ;; Unsubscribe from input-start
      (nskk-event-unsubscribe 'input-start start-token)

      (nskk-event-emit 'input-start :phase "start2")
      (nskk-event-emit 'input-end :phase "end2")

      ;; Should only receive input-end events now
      (should (= (length nskk-events-test-received-events) 3))

      ;; Cleanup
      (nskk-event-unsubscribe 'input-end end-token))))

(nskk-deftest-integration event-mode-change-workflow
  "Test realistic mode change event workflow."
  (nskk-events-test-reset)
  (nskk-events-test-clear-all-subscribers)

  (let ((mode-changes nil)
        (current-mode 'ascii))

    ;; Track mode changes
    (nskk-event-subscribe 'mode-change
                          (lambda (data)
                            (push (plist-get data :to) mode-changes)))

    ;; Simulate mode transitions
    (nskk-event-emit 'mode-change :from 'ascii :to 'hiragana)
    (setq current-mode 'hiragana)

    (nskk-event-emit 'mode-change :from 'hiragana :to 'katakana)
    (setq current-mode 'katakana)

    (nskk-event-emit 'mode-change :from 'katakana :to 'ascii)
    (setq current-mode 'ascii)

    (should (equal mode-changes '(ascii katakana hiragana))))

  ;; Verify history
  (let ((history (nskk-event-history-get 'mode-change 10)))
    (should (= (length history) 3))))

(nskk-deftest-integration event-conversion-workflow
  "Test realistic conversion event workflow."
  (nskk-events-test-reset)
  (nskk-events-test-clear-all-subscribers)

  (let ((conversion-states nil)
        (candidates nil))

    ;; Track conversion lifecycle
    (nskk-event-subscribe 'conversion-start
                          (lambda (data)
                            (push 'start conversion-states)))
    (nskk-event-subscribe 'conversion-candidate
                          (lambda (data)
                            (push (plist-get data :candidate) candidates)))
    (nskk-event-subscribe 'conversion-commit
                          (lambda (data)
                            (push 'commit conversion-states)))

    ;; Simulate conversion flow
    (nskk-event-emit 'conversion-start :input "かんじ")
    (nskk-event-emit 'conversion-candidate :candidate "漢字" :index 0)
    (nskk-event-emit 'conversion-candidate :candidate "感じ" :index 1)
    (nskk-event-emit 'conversion-commit :candidate "漢字" :final t)

    (should (equal conversion-states '(commit start)))
    (should (equal candidates '("感じ" "漢字")))))

(nskk-deftest-integration event-history-query-workflow
  "Test querying event history for debugging."
  (nskk-events-test-clear-all-subscribers)

  ;; Simulate activity
  (nskk-event-emit 'input-start :char ?a)
  (nskk-event-emit 'key-pressed :key ?i)
  (nskk-event-emit 'key-pressed :key ?u)
  (nskk-event-emit 'input-end :result "あいう")

  ;; Query different event types
  (let ((input-events (nskk-event-history-get 'input-start 10))
        (key-events (nskk-event-history-get 'key-pressed 10))
        (all-events (nskk-event-history-get nil 10)))

    (should (= (length input-events) 1))
    (should (= (length key-events) 2))
    (should (= (length all-events) 4))))

(provide 'nskk-events-test)

;;; nskk-events-test.el ends here
