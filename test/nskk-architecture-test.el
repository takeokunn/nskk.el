;;; nskk-architecture-test.el --- Tests for nskk-architecture.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Authors

;; Author: NSKK Developers
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Tests for nskk-architecture.el covering:
;; - Layer state management
;; - Layer state queries
;; - Dependency resolution
;; - Service registration and lookup
;; - Logging and tracing
;; - Event emission
;; - Architecture configuration

;;; Code:

(require 'ert)
(require 'nskk-architecture)
(require 'nskk-test-framework)

;;;
;;; Layer State Struct Tests
;;;

(nskk-deftest-unit architecture-layer-state-struct-exists
  "Test that nskk-layer-state struct is defined."
  (should (fboundp 'nskk-layer-state-p)))

(nskk-deftest-unit architecture-layer-state-create
  "Test creating a layer state."
  (let ((state (nskk-layer-state-create :name 'test-layer)))
    (should (nskk-layer-state-p state))
    (should (eq (nskk-layer-state-name state) 'test-layer))
    (should (eq (nskk-layer-state-status state) 'uninitialized))
    (should (null (nskk-layer-state-initialized-at state)))
    (should (null (nskk-layer-state-startup-time-ms state)))
    (should (null (nskk-layer-state-error-message state)))
    (should (eq (nskk-layer-state-health-status state) 'unknown))))

(nskk-deftest-unit architecture-layer-state-status-transitions
  "Test layer state status can be changed."
  (let ((state (nskk-layer-state-create :name 'test)))
    (should (eq (nskk-layer-state-status state) 'uninitialized))
    (setf (nskk-layer-state-status state) 'initializing)
    (should (eq (nskk-layer-state-status state) 'initializing))
    (setf (nskk-layer-state-status state) 'initialized)
    (should (eq (nskk-layer-state-status state) 'initialized))
    (setf (nskk-layer-state-status state) 'failed)
    (should (eq (nskk-layer-state-status state) 'failed))
    (setf (nskk-layer-state-status state) 'shutdown)
    (should (eq (nskk-layer-state-status state) 'shutdown))))

;;;
;;; Layer Registry Tests
;;;

(nskk-deftest-unit architecture-layers-defined
  "Test that layers are defined."
  (should (listp nskk-architecture--layers))
  (should (> (length nskk-architecture--layers) 0)))

(nskk-deftest-unit architecture-all-layers-present
  "Test that all expected layers are present."
  (let ((expected-layers '(infrastructure data core application extension presentation qa)))
    (dolist (layer expected-layers)
      (should (assq layer nskk-architecture--layers)))))

(nskk-deftest-unit architecture-layer-has-required-fields
  "Test that each layer spec has required fields."
  (dolist (layer-spec nskk-architecture--layers)
    (let ((layer-data (cdr layer-spec)))
      (should (plist-get layer-data :init-function))
      (should (plist-get layer-data :shutdown-function))
      (should (plist-get layer-data :health-function))
      (should (plist-member layer-data :dependencies))
      (should (plist-get layer-data :priority)))))

(nskk-deftest-unit architecture-layer-priorities-unique
  "Test that layer priorities are unique."
  (let ((priorities (mapcar (lambda (spec)
                              (plist-get (cdr spec) :priority))
                            nskk-architecture--layers)))
    (should (= (length priorities) (length (delete-dups (copy-sequence priorities)))))))

;;;
;;; Layer State Query Tests
;;;

(nskk-deftest-unit architecture-get-layer-state-nil-when-empty
  "Test that querying non-existent layer state returns nil."
  (let ((nskk-architecture--layer-states (make-hash-table :test 'eq)))
    (should (null (nskk-architecture-get-layer-state 'nonexistent)))))

(nskk-deftest-unit architecture-get-layer-state-after-set
  "Test retrieving layer state that was stored."
  (let ((nskk-architecture--layer-states (make-hash-table :test 'eq))
        (state (nskk-layer-state-create :name 'test)))
    (puthash 'test state nskk-architecture--layer-states)
    (should (eq (nskk-architecture-get-layer-state 'test) state))))

(nskk-deftest-unit architecture-layer-status-returns-symbol
  "Test that layer-status returns the status symbol."
  (let ((nskk-architecture--layer-states (make-hash-table :test 'eq))
        (state (nskk-layer-state-create :name 'test)))
    (setf (nskk-layer-state-status state) 'initialized)
    (puthash 'test state nskk-architecture--layer-states)
    (should (eq (nskk-architecture-layer-status 'test) 'initialized))))

(nskk-deftest-unit architecture-layer-initialized-p-true
  "Test layer-initialized-p returns t for initialized layer."
  (let ((nskk-architecture--layer-states (make-hash-table :test 'eq))
        (state (nskk-layer-state-create :name 'test)))
    (setf (nskk-layer-state-status state) 'initialized)
    (puthash 'test state nskk-architecture--layer-states)
    (should (nskk-architecture-layer-initialized-p 'test))))

(nskk-deftest-unit architecture-layer-initialized-p-false
  "Test layer-initialized-p returns nil for uninitialized layer."
  (let ((nskk-architecture--layer-states (make-hash-table :test 'eq))
        (state (nskk-layer-state-create :name 'test)))
    (puthash 'test state nskk-architecture--layer-states)
    (should (not (nskk-architecture-layer-initialized-p 'test)))))

(nskk-deftest-unit architecture-layer-healthy-p-true
  "Test layer-healthy-p returns t when health is ok."
  (let ((nskk-architecture--layer-states (make-hash-table :test 'eq))
        (state (nskk-layer-state-create :name 'test)))
    (setf (nskk-layer-state-health-status state) 'ok)
    (puthash 'test state nskk-architecture--layer-states)
    (should (nskk-architecture-layer-healthy-p 'test))))

(nskk-deftest-unit architecture-layer-healthy-p-false
  "Test layer-healthy-p returns nil when health is not ok."
  (let ((nskk-architecture--layer-states (make-hash-table :test 'eq))
        (state (nskk-layer-state-create :name 'test)))
    (setf (nskk-layer-state-health-status state) 'critical)
    (puthash 'test state nskk-architecture--layer-states)
    (should (not (nskk-architecture-layer-healthy-p 'test)))))

;;;
;;; Dependency Resolution Tests
;;;

(nskk-deftest-unit architecture-build-dependency-graph
  "Test dependency graph construction."
  (let ((graph (nskk-architecture--build-dependency-graph)))
    (should (hash-table-p graph))
    ;; Infrastructure should have no dependencies
    (should (null (gethash 'infrastructure graph)))
    ;; Core should depend on infrastructure and data
    (should (member 'infrastructure (gethash 'core graph)))
    (should (member 'data (gethash 'core graph)))))

(nskk-deftest-unit architecture-get-dependencies-infrastructure
  "Test getting dependencies for infrastructure layer."
  (should (null (nskk-architecture--get-dependencies 'infrastructure))))

(nskk-deftest-unit architecture-get-dependencies-core
  "Test getting dependencies for core layer."
  (let ((deps (nskk-architecture--get-dependencies 'core)))
    (should (listp deps))
    (should (member 'infrastructure deps))
    (should (member 'data deps))))

(nskk-deftest-unit architecture-get-dependencies-nonexistent
  "Test getting dependencies for non-existent layer."
  (should (null (nskk-architecture--get-dependencies 'nonexistent-layer))))

;;;
;;; Service Registration Tests
;;;

(nskk-deftest-unit architecture-register-service
  "Test service registration."
  (let ((nskk-architecture--dependency-registry nil))
    (nskk-architecture-register-service 'test-service #'identity)
    (should nskk-architecture--dependency-registry)
    (should (eq (nskk-architecture-get-service 'test-service) #'identity))))

(nskk-deftest-unit architecture-get-service-nil-when-empty
  "Test getting service from empty registry."
  (let ((nskk-architecture--dependency-registry nil))
    (should (null (nskk-architecture-get-service 'nonexistent)))))

(nskk-deftest-unit architecture-register-multiple-services
  "Test registering multiple services."
  (let ((nskk-architecture--dependency-registry nil))
    (nskk-architecture-register-service 'service-a #'car)
    (nskk-architecture-register-service 'service-b #'cdr)
    (should (eq (nskk-architecture-get-service 'service-a) #'car))
    (should (eq (nskk-architecture-get-service 'service-b) #'cdr))))

(nskk-deftest-unit architecture-register-service-overwrites
  "Test that registering a service with same name overwrites."
  (let ((nskk-architecture--dependency-registry nil))
    (nskk-architecture-register-service 'test #'car)
    (nskk-architecture-register-service 'test #'cdr)
    (should (eq (nskk-architecture-get-service 'test) #'cdr))))

;;;
;;; Logging Tests
;;;

(nskk-deftest-unit architecture-log-debug-disabled
  "Test that logging does nothing when debug is disabled."
  (let ((nskk-architecture-enable-debug nil))
    ;; Should not error
    (nskk-architecture--log "INFO" "Test message %s" "arg")))

(nskk-deftest-unit architecture-log-debug-enabled
  "Test that logging works when debug is enabled."
  (let ((nskk-architecture-enable-debug t))
    ;; Should not error
    (nskk-architecture--log "INFO" "Test message %s" "arg")))

;;;
;;; Tracing Tests
;;;

(nskk-deftest-unit architecture-trace-disabled
  "Test that tracing does nothing when disabled."
  (let ((nskk-architecture-enable-tracing nil)
        (nskk-architecture--communication-log nil))
    (nskk-architecture--trace-communication 'layer-a 'layer-b 'test nil)
    (should (null nskk-architecture--communication-log))))

(nskk-deftest-unit architecture-trace-enabled
  "Test that tracing records when enabled."
  (let ((nskk-architecture-enable-tracing t)
        (nskk-architecture-enable-debug nil)
        (nskk-architecture--communication-log nil))
    (nskk-architecture--trace-communication 'layer-a 'layer-b 'test-event '(:data "test"))
    (should (= (length nskk-architecture--communication-log) 1))
    (let ((entry (car nskk-architecture--communication-log)))
      (should (eq (plist-get entry :from) 'layer-a))
      (should (eq (plist-get entry :to) 'layer-b))
      (should (eq (plist-get entry :event) 'test-event)))))

;;;
;;; Event Hook Tests
;;;

(nskk-deftest-unit architecture-event-hook-exists
  "Test that the architecture event hook variable exists."
  (should (boundp 'nskk-architecture-event-hook)))

;;;
;;; Configuration Tests
;;;

(nskk-deftest-unit architecture-custom-group-exists
  "Test that the nskk-architecture custom group exists."
  (should (get 'nskk-architecture 'custom-group)))

(nskk-deftest-unit architecture-debug-default-nil
  "Test that debug is disabled by default."
  (should (eq (default-value 'nskk-architecture-enable-debug) nil)))

(nskk-deftest-unit architecture-tracing-default-nil
  "Test that tracing is disabled by default."
  (should (eq (default-value 'nskk-architecture-enable-tracing) nil)))

(nskk-deftest-unit architecture-auto-recovery-default-t
  "Test that auto-recovery is enabled by default."
  (should (eq (default-value 'nskk-architecture-auto-recovery) t)))

(nskk-deftest-unit architecture-init-timeout-default
  "Test that init timeout has a sensible default."
  (should (integerp (default-value 'nskk-architecture-init-timeout-seconds)))
  (should (> (default-value 'nskk-architecture-init-timeout-seconds) 0)))

;;;
;;; Function Existence Tests
;;;

(nskk-deftest-unit architecture-public-functions-exist
  "Test that all public functions exist."
  (should (fboundp 'nskk-architecture-initialize))
  (should (fboundp 'nskk-architecture-shutdown))
  (should (fboundp 'nskk-architecture-health-check))
  (should (fboundp 'nskk-architecture-restart-layer))
  (should (fboundp 'nskk-architecture-reload))
  (should (fboundp 'nskk-architecture-show-statistics))
  (should (fboundp 'nskk-architecture-show-diagram))
  (should (fboundp 'nskk-architecture-get-communication-log))
  (should (fboundp 'nskk-architecture-enable-debug))
  (should (fboundp 'nskk-architecture-disable-debug)))

;;;
;;; Module Feature Tests
;;;

(nskk-deftest-unit architecture-provides-feature
  "Test that nskk-architecture provides its feature."
  (should (featurep 'nskk-architecture)))

(provide 'nskk-architecture-test)

;;; nskk-architecture-test.el ends here
