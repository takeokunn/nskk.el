;;; perf-analysis.el --- Performance profiling for NSKK error handling -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'nskk-state)
(require 'nskk-mode-switch)
(require 'nskk-input-commands)

;; Performance measurement utilities
(defun perf-measure-time (fn &optional iterations)
  "Measure execution time of FN over ITERATIONS (default 10000).
Returns (total-time average-time min-time max-time)."
  (let ((iterations (or iterations 10000))
        (times nil))
    (dotimes (_ iterations)
      (let ((start (current-time)))
        (funcall fn)
        (let* ((end (current-time))
               (elapsed (* 1000.0 (float-time (time-subtract end start)))))
          (push elapsed times))))
    (let ((total (apply #'+ times))
          (min-time (apply #'min times))
          (max-time (apply #'max times)))
      (list total
            (/ total iterations)
            min-time
            max-time))))

(defun perf-print-results (name results)
  "Print profiling results for NAME."
  (let ((total (nth 0 results))
        (avg (nth 1 results))
        (min-time (nth 2 results))
        (max-time (nth 3 results)))
    (message "%s: total=%.3fms avg=%.6fms min=%.6fms max=%.6fms"
             name total avg min-time max-time)))

;; Test fixtures
(defun setup-test-state ()
  "Create a test state for benchmarking."
  (nskk-state-create 'ascii))

(defun setup-global-state ()
  "Setup global nskk-current-state for benchmarking."
  (setq nskk-current-state (nskk-state-create 'ascii)))

;; Benchmark: Mode validation
(defun bench-mode-validation ()
  "Benchmark nskk-state-valid-mode-p function."
  (message "\n=== Mode Validation Benchmarks ===")
  (let ((valid-results (perf-measure-time (lambda () (nskk-state-valid-mode-p 'hiragana)) 100000))
        (invalid-results (perf-measure-time (lambda () (nskk-state-valid-mode-p 'invalid-mode)) 100000)))
    (perf-print-results "Valid mode check (hiragana)" valid-results)
    (perf-print-results "Invalid mode check" invalid-results)))

;; Benchmark: State type checking
(defun bench-state-type-check ()
  "Benchmark nskk-state-p function."
  (message "\n=== State Type Checking Benchmarks ===")
  (let ((state (setup-test-state)))
    (let ((valid-state-results (perf-measure-time (lambda () (nskk-state-p state)) 100000))
          (nil-results (perf-measure-time (lambda () (nskk-state-p nil)) 100000)))
      (perf-print-results "State type check (valid)" valid-state-results)
      (perf-print-results "State type check (nil)" nil-results))))

;; Benchmark: Boundp checks
(defun bench-boundp-checks ()
  "Benchmark variable binding checks."
  (message "\n=== Variable Binding Checks ===")
  (setup-global-state)
  (let ((boundp-results (perf-measure-time (lambda () (boundp 'nskk-current-state)) 100000)))
    (perf-print-results "boundp check (nskk-current-state)" boundp-results))
  (setq nskk-current-state nil)
  (let ((unbound-results (perf-measure-time (lambda () (boundp 'nskk-current-state)) 100000)))
    (perf-print-results "boundp check (unbound)" unbound-results)))

;; Benchmark: Mode getter with fallback
(defun bench-mode-getter-fallback ()
  "Benchmark mode getter with fallback (hot path)."
  (message "\n=== Mode Getter with Fallback (Hot Path) ===")
  (setup-global-state)
  (let ((direct-results (perf-measure-time (lambda () (nskk-state-get-mode)) 100000))
        (fallback-results (perf-measure-time (lambda () (or (nskk-state-get-mode) 'ascii)) 100000)))
    (perf-print-results "Direct mode getter" direct-results)
    (perf-print-results "Mode getter with fallback" fallback-results)))

;; Benchmark: State setter with validation
(defun bench-state-setter ()
  "Benchmark nskk-state-set function with mode validation."
  (message "\n=== State Setter with Validation ===")
  (let ((state (setup-test-state)))
    (let ((setter-results (perf-measure-time
                           (lambda () (nskk-state-set state 'mode 'hiragana))
                           10000)))
      (perf-print-results "State setter (mode change)" setter-results)))
  (let ((state (setup-test-state)))
    (let ((invalid-setter-results (perf-measure-time
                                   (lambda () (nskk-state-set state 'mode 'invalid))
                                   10000)))
      (perf-print-results "State setter (invalid mode)" invalid-setter-results))))

;; Benchmark: Mode switch function
(defun bench-mode-switch ()
  "Benchmark nskk--set-mode function."
  (message "\n=== Mode Switch Function ===")
  (setup-global-state)
  (let ((switch-results (perf-measure-time
                        (lambda () (nskk--set-mode 'hiragana))
                        5000)))
    (perf-print-results "Mode switch (nskk--set-mode)" switch-results)))

;; Benchmark: Input routing decision
(defun bench-input-routing ()
  "Benchmark the input routing logic in nskk-self-insert."
  (message "\n=== Input Routing Decision ===")
  (setup-global-state)
  ;; Simulate the key decision logic from nskk-self-insert
  (let* ((current-mode-direct (perf-measure-time
                               (lambda ()
                                 (nskk-state-get-mode))
                               100000))
         (current-mode-fallback (perf-measure-time
                                (lambda ()
                                  (or (nskk-state-get-mode) 'ascii))
                                100000))
         (mode-dispatch (perf-measure-time
                        (lambda ()
                          (let ((mode (or (nskk-state-get-mode) 'ascii)))
                            (cond
                             ((memq mode '(ascii latin)) 'ascii-path)
                             ((eq mode 'abbrev) 'abbrev-path)
                             (t 'japanese-path))))
                        100000)))
    (perf-print-results "Mode query (direct)" current-mode-direct)
    (perf-print-results "Mode query (with fallback)" current-mode-fallback)
    (perf-print-results "Mode-based dispatch" mode-dispatch)))

;; Benchmark: Generic getter vs direct accessor
(defun bench-generic-vs-direct-getter ()
  "Benchmark generic getter vs direct accessor."
  (message "\n=== Generic Getter vs Direct Accessor ===")
  (let ((state (setup-test-state)))
    (let ((generic-results (perf-measure-time
                           (lambda () (nskk-state-get state 'mode))
                           50000))
          (direct-results (perf-measure-time
                          (lambda () (nskk-state-mode state))
                          50000)))
      (perf-print-results "Generic getter (nskk-state-get)" generic-results)
      (perf-print-results "Direct accessor (nskk-state-mode)" direct-results))))

;; Benchmark: Memory allocations in hot path
(defun bench-string-concatenation ()
  "Benchmark string operations in hot path."
  (message "\n=== String Operations (Buffer Append) ===")
  (let ((state (setup-test-state)))
    (let ((append-results (perf-measure-time
                          (lambda () (nskk-state-append-input state ?a))
                          50000)))
      (perf-print-results "Buffer append (nskk-state-append-input)" append-results))))

;; Benchmark: Symbol interning for dynamic accessor
(defun bench-symbol-interning ()
  "Benchmark symbol creation for dynamic accessors."
  (message "\n=== Symbol Interning ===")
  (let ((intern-results (perf-measure-time
                        (lambda () (intern (format "nskk-state-%s" 'mode)))
                        100000)))
    (perf-print-results "Symbol intern (format + intern)" intern-results)))

;; Complete benchmark suite
(defun perf-run-all-benchmarks ()
  "Run all performance benchmarks."
  (interactive)
  (message "=== NSKK Performance Analysis: Error Handling Overhead ===")
  (message "Iterations tuned for statistical significance (5000-100000 iterations)")
  (bench-mode-validation)
  (bench-state-type-check)
  (bench-boundp-checks)
  (bench-mode-getter-fallback)
  (bench-mode-switch)
  (bench-state-setter)
  (bench-input-routing)
  (bench-generic-vs-direct-getter)
  (bench-string-concatenation)
  (bench-symbol-interning)
  (message "\n=== Benchmark Complete ==="))

(provide 'perf-analysis)

;;; perf-analysis.el ends here
