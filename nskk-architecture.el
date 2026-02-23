;;; nskk-architecture.el --- Architecture integration and orchestration layer -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Authors

;; Author: NSKK Developers
;; Keywords: Japanese, input, method, architecture, layer
;; Homepage: https://github.com/takeokunn/nskk.el

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

;; This file provides the architecture integration layer for NSKK,
;; orchestrating all 7 layers in the correct initialization sequence.
;;
;; Architecture Overview:
;; ┌─────────────────────────────────────────────────────────┐
;; │                Layer 1: Presentation                    │
;; │              UI integration, event handling              │
;; ├─────────────────────────────────────────────────────────┤
;; │                Layer 2: Extension                       │
;; │          Hooks, event bus, inter-layer messaging        │
;; ├─────────────────────────────────────────────────────────┤
;; │                Layer 3: Application                     │
;; │           Business logic, conversion control            │
;; ├─────────────────────────────────────────────────────────┤
;; │                Layer 4: Core Engine                     │
;; │           Conversion engine, dictionary engine          │
;; ├─────────────────────────────────────────────────────────┤
;; │                Layer 5: Data Access                     │
;; │              Persistence, sync, dictionary access       │
;; ├─────────────────────────────────────────────────────────┤
;; │                Layer 6: Infrastructure                  │
;; │         Thread management, memory, resource management  │
;; ├─────────────────────────────────────────────────────────┤
;; │                Layer 7: QA                              │
;; │          Testing, benchmarking, quality assurance       │
;; └─────────────────────────────────────────────────────────┘
;;
;; Layer Initialization Order (bottom-up):
;; 1. Infrastructure (nskk-infrastructure-initialize)
;; 2. Data (nskk-data-initialize)
;; 3. Core (nskk-core-initialize)
;; 4. Application (nskk-application-initialize)
;; 5. Extension (nskk-extension-initialize)
;; 6. Presentation (nskk-presentation-initialize)
;; 7. QA (nskk-qa-initialize)
;;
;; Shutdown Order (top-down):
;; 1. QA
;; 2. Presentation
;; 3. Extension
;; 4. Application
;; 5. Core
;; 6. Data
;; 7. Infrastructure

;;; Code:

(require 'cl-lib)

;;;; Customization

(defgroup nskk-architecture nil
  "NSKK architecture layer configuration."
  :prefix "nskk-architecture-"
  :group 'nskk)

(defcustom nskk-architecture-enable-debug nil
  "Enable debug logging for architecture operations."
  :type 'boolean
  :group 'nskk-architecture)

(defcustom nskk-architecture-enable-tracing nil
  "Enable tracing of inter-layer communication."
  :type 'boolean
  :group 'nskk-architecture)

(defcustom nskk-architecture-auto-recovery t
  "Enable automatic recovery from layer failures."
  :type 'boolean
  :group 'nskk-architecture)

(defcustom nskk-architecture-init-timeout-seconds 30
  "Maximum time to wait for layer initialization."
  :type 'integer
  :group 'nskk-architecture)

;;;; Layer State Management

(cl-defstruct (nskk-layer-state
              (:constructor nskk-layer-state-create)
              (:copier nil))
  "State tracking for a single layer."
  (name nil :read-only t :type symbol)
  (status 'uninitialized :type symbol)  ; uninitialized, initializing, initialized, failed, shutdown
  (initialized-at nil :type (or null integer))
  (startup-time-ms nil :type (or null number))
  (error-message nil :type (or null string))
  (health-status 'unknown :type symbol)  ; ok, warning, critical
  (metadata nil :type (or null hash-table)))

(defvar nskk-architecture--layer-states (make-hash-table :test 'eq)
  "Registry of all layer states.")

(defvar nskk-architecture--initialized nil
  "Whether architecture has been initialized.")

(defvar nskk-architecture--communication-log nil
  "Log of inter-layer communications when tracing is enabled.")

(defvar nskk-architecture--dependency-registry nil
  "Registry of layer dependencies.")

;;;; Layer Definition

(defvar nskk-architecture--layers
  '(;; Layer 6: Infrastructure (bottom layer)
    (infrastructure
     . (:init-function nskk-infrastructure-initialize
        :shutdown-function nskk-infrastructure-shutdown
        :health-function nskk-infrastructure-health-check
        :dependencies nil
        :priority 100))

    ;; Layer 5: Data Access
    (data
     . (:init-function nskk-data-initialize
        :shutdown-function nskk-data-shutdown
        :health-function nskk-data-health-check
        :dependencies (infrastructure)
        :priority 90))

    ;; Layer 4: Core Engine
    (core
     . (:init-function nskk-core-initialize
        :shutdown-function nskk-core-shutdown
        :health-function nskk-core-health-check
        :dependencies (infrastructure data)
        :priority 80))

    ;; Layer 3: Application
    (application
     . (:init-function nskk-application-initialize
        :shutdown-function nskk-application-shutdown
        :health-function nskk-application-health-check
        :dependencies (core extension)
        :priority 70))

    ;; Layer 2: Extension
    (extension
     . (:init-function nskk-extension-initialize
        :shutdown-function nskk-extension-shutdown
        :health-function nskk-extension-health-check
        :dependencies nil
        :priority 60))

    ;; Layer 1: Presentation
    (presentation
     . (:init-function nskk-presentation-initialize
        :shutdown-function nskk-presentation-shutdown
        :health-function nskk-presentation-health-check
        :dependencies (application extension)
        :priority 50))

    ;; Layer 7: QA (top layer, but independent)
    (qa
     . (:init-function nskk-qa-initialize
        :shutdown-function nskk-qa-shutdown
        :health-function nskk-qa-health-check
        :dependencies nil
        :priority 40)))
  "Registry of all layers with their lifecycle functions.")

;;;; Logging and Tracing

(defun nskk-architecture--log (level &rest args)
  "Log message at LEVEL with ARGS."
  (when nskk-architecture-enable-debug
    (let ((message-string (apply #'format args))
          (timestamp (format-time-string "%H:%M:%S.%3N")))
      (message "[NSKK-Arch %s] %s: %s"
               timestamp level message-string))))

(defun nskk-architecture--trace-communication (from-layer to-layer event-type data)
  "Trace communication FROM-LAYER to TO-LAYER with EVENT-TYPE and DATA."
  (when nskk-architecture-enable-tracing
    (let ((entry (list :timestamp (current-time)
                       :from from-layer
                       :to to-layer
                       :event event-type
                       :data data)))
      (push entry nskk-architecture--communication-log)
      (nskk-architecture--log "TRACE" "%s -> %s: %s"
                              from-layer to-layer event-type))))

;;;; Layer State Queries

(defun nskk-architecture-get-layer-state (layer-name)
  "Get state for LAYER-NAME."
  (gethash layer-name nskk-architecture--layer-states))

(defun nskk-architecture-layer-status (layer-name)
  "Get status symbol for LAYER-NAME."
  (let ((state (nskk-architecture-get-layer-state layer-name)))
    (when state
      (nskk-layer-state-status state))))

(defun nskk-architecture-layer-initialized-p (layer-name)
  "Check if LAYER-NAME is initialized."
  (eq (nskk-architecture-layer-status layer-name) 'initialized))

(defun nskk-architecture-layer-healthy-p (layer-name)
  "Check if LAYER-NAME is healthy."
  (let ((state (nskk-architecture-get-layer-state layer-name)))
    (when state
      (eq (nskk-layer-state-health-status state) 'ok))))

;;;; Dependency Resolution

(defun nskk-architecture--build-dependency-graph ()
  "Build dependency graph from layer definitions."
  (let ((graph (make-hash-table :test 'eq)))
    (dolist (layer-spec nskk-architecture--layers)
      (let ((layer-name (car layer-spec))
            (layer-data (cdr layer-spec)))
        (puthash layer-name
                 (plist-get layer-data :dependencies)
                 graph)))
    graph))

(defun nskk-architecture--get-dependencies (layer-name)
  "Get list of LAYER-NAME's dependencies."
  (let ((layer-spec (assq layer-name nskk-architecture--layers)))
    (when layer-spec
      (plist-get (cdr layer-spec) :dependencies))))

(defun nskk-architecture--sort-layers-by-dependency ()
  "Sort layers topologically by dependencies (bottom-up init order)."
  (let ((result '())
        (visited (make-hash-table :test 'eq))
        (visiting (make-hash-table :test 'eq))
        (graph (nskk-architecture--build-dependency-graph)))

    (cl-labels ((visit (layer)
                      (when (gethash layer visiting)
                        (error "Circular dependency detected involving layer: %s" layer))
                      (unless (gethash layer visited)
                        (puthash layer t visiting)
                        (dolist (dep (gethash layer graph))
                          (visit dep))
                        (puthash layer t visited)
                        (push layer result))))
      (dolist (layer (mapcar #'car nskk-architecture--layers))
        (visit layer)))

    (nreverse result)))

;;;; Layer Initialization

;;;###autoload (autoload 'nskk-architecture-initialize "nskk-architecture" nil t)
(defun nskk-architecture-initialize (&optional force)
  "Initialize NSKK architecture in correct dependency order.
With FORCE, reinitialize even if already initialized.
Returns t if successful, nil on failure."
  (interactive "P")

  (if (and nskk-architecture--initialized (not force))
      (progn
        (nskk-architecture--log "WARN" "Architecture already initialized. Use force to reinitialize.")
        t)

    (nskk-architecture--log "INFO" "Starting architecture initialization...")

    ;; Initialize layer states
    (dolist (layer-spec nskk-architecture--layers)
      (let* ((layer-name (car layer-spec))
             (state (nskk-layer-state-create :name layer-name)))
        (puthash layer-name state nskk-architecture--layer-states)))

    ;; Sort layers by dependency (bottom-up for initialization)
    (let ((init-order (nskk-architecture--sort-layers-by-dependency))
          (failed-layers '())
          (start-time (float-time)))

      (nskk-architecture--log "INFO" "Initialization order: %S" init-order)

      ;; Initialize each layer
      (dolist (layer-name init-order)
        (condition-case err
            (if (nskk-architecture--initialize-layer layer-name)
                (nskk-architecture--log "INFO" "Layer %s initialized successfully" layer-name)
              (progn
                (push layer-name failed-layers)
                (nskk-architecture--log "ERROR" "Layer %s initialization failed" layer-name)))
          (error
           (push layer-name failed-layers)
           (nskk-architecture--log "ERROR" "Layer %s initialization error: %S"
                                   layer-name err))))

      ;; Check if critical layers failed
      (let* ((critical-layers '(infrastructure data core application))
             (critical-failed (cl-intersection critical-layers failed-layers)))
        (if critical-failed
            (progn
              (nskk-architecture--log "ERROR" "Critical layer initialization failed: %S"
                                     critical-failed)
              ;; Attempt cleanup
              (nskk-architecture-shutdown)
              (error "Architecture initialization failed: %S" critical-failed))
          ;; Success
          (setq nskk-architecture--initialized t)
          (let ((elapsed (* 1000.0 (- (float-time) start-time))))
            (nskk-architecture--log "INFO" "Architecture initialization complete in %.2fms" elapsed)
            (nskk-architecture--emit-event :architecture-initialized
                                          :elapsed-ms elapsed
                                          :layers init-order))
          t)))))

(defun nskk-architecture--initialize-layer (layer-name)
  "Initialize single LAYER-NAME.
Returns t if successful, nil on failure."
  (let* ((layer-spec (assq layer-name nskk-architecture--layers))
         (layer-data (cdr layer-spec))
         (init-function (plist-get layer-data :init-function))
         (state (nskk-architecture-get-layer-state layer-name))
         (start-time (float-time)))

    (cond
     ;; Check if already initialized
     ((eq (nskk-layer-state-status state) 'initialized)
      (nskk-architecture--log "DEBUG" "Layer %s already initialized, skipping" layer-name)
      t)

     ;; Check dependencies
     ((cl-some (lambda (dep)
                 (not (nskk-architecture-layer-initialized-p dep)))
               (nskk-architecture--get-dependencies layer-name))
      (nskk-architecture--log "ERROR" "Layer %s has uninitialized dependencies" layer-name)
      nil)

     ;; Proceed with initialization
     (t
      ;; Mark as initializing
      (setf (nskk-layer-state-status state) 'initializing)
      (nskk-architecture--log "DEBUG" "Initializing layer: %s" layer-name)

      ;; Call init function if it exists
      (cond
       ((fboundp init-function)
        (condition-case err
            (progn
              (funcall init-function)
              (let ((elapsed (* 1000.0 (- (float-time) start-time))))
                (setf (nskk-layer-state-status state) 'initialized)
                (setf (nskk-layer-state-initialized-at state) (float-time))
                (setf (nskk-layer-state-startup-time-ms state) elapsed)
                (nskk-architecture--log "DEBUG" "Layer %s initialized in %.2fms"
                                       layer-name elapsed)
                t))
          (error
           (setf (nskk-layer-state-status state) 'failed)
           (setf (nskk-layer-state-error-message state) (format "%s" err))
           (nskk-architecture--log "ERROR" "Layer %s init failed: %S"
                                  layer-name err)
           nil)))
       ;; No init function - mark as initialized
       (t
        (nskk-architecture--log "DEBUG" "Layer %s has no init function, marking initialized"
                               layer-name)
        (setf (nskk-layer-state-status state) 'initialized)
        (setf (nskk-layer-state-initialized-at state) (float-time))
        t))))))

;;;; Layer Shutdown

;;;###autoload (autoload 'nskk-architecture-shutdown "nskk-architecture" nil t)
(defun nskk-architecture-shutdown ()
  "Shutdown NSKK architecture in reverse dependency order."
  (interactive)

  (if (not nskk-architecture--initialized)
      (nskk-architecture--log "WARN" "Architecture not initialized")

    (nskk-architecture--log "INFO" "Starting architecture shutdown...")

    ;; Shutdown in reverse initialization order (top-down)
    (let* ((init-order (nskk-architecture--sort-layers-by-dependency))
           (shutdown-order (nreverse init-order))
           (start-time (float-time)))

      (nskk-architecture--log "INFO" "Shutdown order: %S" shutdown-order)

      (dolist (layer-name shutdown-order)
        (nskk-architecture--shutdown-layer layer-name))

      (let ((elapsed (* 1000.0 (- (float-time) start-time))))
        (nskk-architecture--log "INFO" "Architecture shutdown complete in %.2fms" elapsed)
        (nskk-architecture--emit-event :architecture-shutdown-complete
                                      :elapsed-ms elapsed))

      ;; Reset state
      (setq nskk-architecture--initialized nil)
      (clrhash nskk-architecture--layer-states)
      (setq nskk-architecture--communication-log nil))))

(defun nskk-architecture--shutdown-layer (layer-name)
  "Shutdown single LAYER-NAME."
  (let* ((layer-spec (assq layer-name nskk-architecture--layers))
         (layer-data (cdr layer-spec))
         (shutdown-function (plist-get layer-data :shutdown-function))
         (state (nskk-architecture-get-layer-state layer-name)))

    (if (not state)
        (nskk-architecture--log "DEBUG" "Layer %s has no state, skipping shutdown" layer-name)

      (nskk-architecture--log "DEBUG" "Shutting down layer: %s" layer-name)

      ;; Call shutdown function if it exists
      (when (fboundp shutdown-function)
        (condition-case err
            (funcall shutdown-function)
          (error
           (nskk-architecture--log "ERROR" "Layer %s shutdown error: %S"
                                  layer-name err))))

      ;; Mark as shutdown
      (setf (nskk-layer-state-status state) 'shutdown))))

;;;; Health Checking

;;;###autoload (autoload 'nskk-architecture-health-check "nskk-architecture" nil t)
(defun nskk-architecture-health-check ()
  "Perform health check across all layers.
Returns alist with overall status and per-layer status."
  (interactive)

  (nskk-architecture--log "INFO" "Running architecture health check...")

  (let ((results '())
        (overall-status 'ok)
        (issues '())
        (warnings '()))

    (dolist (layer-spec nskk-architecture--layers)
      (let* ((layer-name (car layer-spec))
             (layer-data (cdr layer-spec))
             (health-function (plist-get layer-data :health-function))
             (state (nskk-architecture-get-layer-state layer-name))
             (layer-health 'unknown))

        ;; Call health function if available
        (cond
         ((fboundp health-function)
          (condition-case err
              (let ((result (funcall health-function)))
                (setq layer-health (plist-get result :status))
                (when (eq layer-health 'critical)
                  (push (list layer-name result) issues))
                (when (eq layer-health 'warning)
                  (push (list layer-name result) warnings)))
            (error
             (setq layer-health 'error)
             (push (list layer-name :error (format "%s" err)) issues))))
         ;; No health function - use state status
         ((eq (nskk-layer-state-status state) 'initialized)
          (setq layer-health 'ok))
         (t
          (setq layer-health 'error)
          (push (list layer-name :status 'not-initialized) issues)))

        ;; Update layer state health
        (when state
          (setf (nskk-layer-state-health-status state) layer-health))

        ;; Collect result
        (push (cons layer-name layer-health) results)))

    ;; Determine overall status
    (when issues
      (setq overall-status 'critical))
    (when (and (eq overall-status 'ok) warnings)
      (setq overall-status 'warning))

    ;; Log results
    (nskk-architecture--log "INFO" "Health check complete: %s" overall-status)

    ;; Display in interactive mode
    (when (called-interactively-p 'interactive)
      (message "NSKK Architecture Health: %s" overall-status)
      (dolist (result (nreverse results))
        (message "  %s: %s" (car result) (cdr result))))

    `((status . ,overall-status)
      (layers . ,(nreverse results))
      (issues . ,issues)
      (warnings . ,warnings)
      (timestamp . ,(current-time)))))

;;;; Dependency Injection

(defun nskk-architecture-register-service (service-name service-function)
  "Register SERVICE-NAME with SERVICE-FUNCTION for dependency injection."
  (unless nskk-architecture--dependency-registry
    (setq nskk-architecture--dependency-registry (make-hash-table :test 'eq)))
  (puthash service-name service-function nskk-architecture--dependency-registry))

(defun nskk-architecture-get-service (service-name)
  "Get service implementation for SERVICE-NAME."
  (when nskk-architecture--dependency-registry
    (gethash service-name nskk-architecture--dependency-registry)))

;;;; Event Emission

(defun nskk-architecture--emit-event (event-type &rest data)
  "Emit architecture-level EVENT-TYPE with DATA."
  ;; Check if events layer is available
  (when (fboundp 'nskk-event-emit)
    (apply #'nskk-event-emit event-type data))

  ;; Also run hooks for backward compatibility
  (run-hook-with-args 'nskk-architecture-event-hook event-type data))

(defvar nskk-architecture-event-hook nil
  "Hook run on architecture events.
Each function receives (EVENT-TYPE DATA plist).")

;;;; Lifecycle Management

(defun nskk-architecture-restart-layer (layer-name)
  "Restart a single LAYER-NAME."
  (interactive (list (intern (completing-read "Layer to restart: "
                                              (mapcar #'car nskk-architecture--layers)))))

  (nskk-architecture--log "INFO" "Restarting layer: %s" layer-name)

  ;; Shutdown
  (nskk-architecture--shutdown-layer layer-name)

  ;; Reset state
  (let ((state (nskk-architecture-get-layer-state layer-name)))
    (when state
      (setf (nskk-layer-state-status state) 'uninitialized)
      (setf (nskk-layer-state-error-message state) nil)))

  ;; Reinitialize
  (if (nskk-architecture--initialize-layer layer-name)
      (progn
        (nskk-architecture--log "INFO" "Layer %s restarted successfully" layer-name)
        (nskk-architecture--emit-event :layer-restarted
                                      :layer layer-name)
        t)
    (progn
      (nskk-architecture--log "ERROR" "Layer %s restart failed" layer-name)
      nil)))

(defun nskk-architecture-reload ()
  "Reload and reinitialize entire architecture."
  (interactive)

  (nskk-architecture--log "INFO" "Reloading architecture...")

  ;; Shutdown
  (when nskk-architecture--initialized
    (nskk-architecture-shutdown))

  ;; Reload layer files
  (dolist (layer-spec nskk-architecture--layers)
    (let* ((layer-name (car layer-spec))
           (filename (format "nskk-layer-%s" layer-name)))
      (when (locate-library filename)
        (load-file filename)
        (nskk-architecture--log "DEBUG" "Reloaded %s" filename))))

  ;; Reinitialize
  (nskk-architecture-initialize 'force))

;;;; Statistics and Debugging

(defun nskk-architecture-show-statistics ()
  "Display architecture statistics."
  (interactive)

  (if (not nskk-architecture--initialized)
      (message "NSKK Architecture not initialized")

    (with-output-to-temp-buffer "*NSKK Architecture Statistics*"
      (princ "NSKK Architecture Statistics\n")
      (princ "---------------------------\n\n")

      (princ "Layer Status:\n")
      (dolist (layer-spec nskk-architecture--layers)
        (let* ((layer-name (car layer-spec))
               (state (nskk-architecture-get-layer-state layer-name)))
          (princ (format "  %s: %s\n" layer-name
                        (if state
                            (nskk-layer-state-status state)
                          'no-state)))))

      (princ "\nStartup Times:\n")
      (dolist (layer-spec nskk-architecture--layers)
        (let* ((layer-name (car layer-spec))
               (state (nskk-architecture-get-layer-state layer-name)))
          (when (and state (nskk-layer-state-startup-time-ms state))
            (princ (format "  %s: %.2fms\n" layer-name
                          (nskk-layer-state-startup-time-ms state))))))

      (princ "\nHealth Status:\n")
      (dolist (layer-spec nskk-architecture--layers)
        (let* ((layer-name (car layer-spec))
               (state (nskk-architecture-get-layer-state layer-name)))
          (when state
            (princ (format "  %s: %s\n" layer-name
                          (nskk-layer-state-health-status state)))))))))

(defun nskk-architecture-show-diagram ()
  "Display architecture diagram."
  (interactive)

  (with-output-to-temp-buffer "*NSKK Architecture Diagram*"
    (princ "NSKK 7-Layer Architecture\n")
    (princ "-------------------------\n\n")
    (princ "┌─────────────────────────────────────────────────────────┐\n")
    (princ "│                Layer 1: Presentation                    │\n")
    (princ "│              UI integration, event handling              │\n")
    (princ "│  nskk-layer-presentation.el                             │\n")
    (princ "├─────────────────────────────────────────────────────────┤\n")
    (princ "│                Layer 2: Extension                       │\n")
    (princ "│          Hooks, event bus, inter-layer messaging        │\n")
    (princ "│  nskk-layer-extension.el                                │\n")
    (princ "├─────────────────────────────────────────────────────────┤\n")
    (princ "│                Layer 3: Application                     │\n")
    (princ "│           Business logic, conversion control            │\n")
    (princ "│  nskk-layer-application.el                              │\n")
    (princ "├─────────────────────────────────────────────────────────┤\n")
    (princ "│                Layer 4: Core Engine                     │\n")
    (princ "│           Conversion engine, dictionary engine          │\n")
    (princ "│  nskk-layer-core.el                                     │\n")
    (princ "├─────────────────────────────────────────────────────────┤\n")
    (princ "│                Layer 5: Data Access                     │\n")
    (princ "│              Persistence, sync, dictionary access       │\n")
    (princ "│  nskk-layer-data.el                                     │\n")
    (princ "├─────────────────────────────────────────────────────────┤\n")
    (princ "│                Layer 6: Infrastructure                  │\n")
    (princ "│         Thread management, memory, resource management  │\n")
    (princ "│  nskk-layer-infrastructure.el                           │\n")
    (princ "├─────────────────────────────────────────────────────────┤\n")
    (princ "│                Layer 7: QA                              │\n")
    (princ "│          Testing, benchmarking, quality assurance       │\n")
    (princ "│  nskk-layer-qa.el                                       │\n")
    (princ "└─────────────────────────────────────────────────────────┘\n\n")

    (princ "Integration Layer: nskk-architecture.el\n\n")

    (princ "Initialization Order (bottom-up):\n")
    (princ "  1. Infrastructure\n")
    (princ "  2. Data\n")
    (princ "  3. Core\n")
    (princ "  4. Application\n")
    (princ "  5. Extension\n")
    (princ "  6. Presentation\n")
    (princ "  7. QA\n\n")

    (princ "Shutdown Order (top-down):\n")
    (princ "  1. QA\n")
    (princ "  2. Presentation\n")
    (princ "  3. Extension\n")
    (princ "  4. Application\n")
    (princ "  5. Core\n")
    (princ "  6. Data\n")
    (princ "  7. Infrastructure\n")))

(defun nskk-architecture-get-communication-log ()
  "Get inter-layer communication log."
  (interactive)

  (cond
   ((not nskk-architecture-enable-tracing)
    (message "Tracing not enabled. Set `nskk-architecture-enable-tracing' to t."))
   (nskk-architecture--communication-log
    (with-output-to-temp-buffer "*NSKK Communication Log*"
      (princ "NSKK Inter-Layer Communication Log\n")
      (princ "-----------------------------------\n\n")
      (dolist (entry (reverse nskk-architecture--communication-log))
        (let* ((timestamp (plist-get entry :timestamp))
               (from (plist-get entry :from))
               (to (plist-get entry :to))
               (event (plist-get entry :event))
               (data (plist-get entry :data)))
          (princ (format "[%s] %s -> %s: %s\n"
                        (format-time-string "%H:%M:%S.%3N" timestamp)
                        from to event))
          (when data
            (princ (format "    Data: %S\n" data))))))
    (length nskk-architecture--communication-log))
   (t
    (message "No communication log entries"))))

(defun nskk-architecture-enable-debug ()
  "Enable debug mode for all layers."
  (interactive)

  (nskk-architecture--log "INFO" "Enabling debug mode...")

  ;; Enable architecture debug
  (setq nskk-architecture-enable-debug t)
  (setq nskk-architecture-enable-tracing t)

  ;; Enable layer-specific debug
  (when (boundp 'nskk-infrastructure-debug)
    (setq nskk-infrastructure-debug t))

  ;; Notify other layers via event
  (nskk-architecture--emit-event :debug-enabled)

  (message "NSKK Architecture debug mode enabled"))

(defun nskk-architecture-disable-debug ()
  "Disable debug mode for all layers."
  (interactive)

  (nskk-architecture--log "INFO" "Disabling debug mode...")

  (setq nskk-architecture-enable-debug nil)
  (setq nskk-architecture-enable-tracing nil)

  (when (boundp 'nskk-infrastructure-debug)
    (setq nskk-infrastructure-debug nil))

  (nskk-architecture--emit-event :debug-disabled)

  (message "NSKK Architecture debug mode disabled"))

;;;; Provide

(provide 'nskk-architecture)

;;; nskk-architecture.el ends here
