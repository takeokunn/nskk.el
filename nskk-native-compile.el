;;; nskk-native-compile.el --- Native compilation support for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Takeshi Umeda

;; Author: NSKK Contributors
;; Maintainer: takeokunn <bararararatty@gmail.com>
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

;; Native compilation support for NSKK.
;; Requires Emacs 28+ with native compilation enabled.

;; This module provides:
;; - Native compilation configuration
;; - Compilation function declarations with type hints
;; - Async compilation utilities
;; - Performance monitoring

;;; Code:

;;;; Requirements

(eval-when-compile
  (require 'cl-lib))

;;;; Native Compilation Detection

(defconst nskk-native-compile-available
  (and (fboundp 'native-comp-available-p)
       (native-comp-available-p))
  "Non-nil if native compilation is available.")

(defconst nskk-native-compile-enabled
  (and nskk-native-compile-available
       (featurep 'native-compile))
  "Non-nil if native compilation is enabled.")

;;;; Native Compilation Configuration

(defun nskk-native-compile-configure ()
  "Configure native compilation settings for NSKK.
Call this explicitly if you want optimized native compilation."
  (interactive)
  (when nskk-native-compile-available
    (when (boundp 'native-comp-speed)
      (setq native-comp-speed 3))
    (when (boundp 'native-comp-debug)
      (setq native-comp-debug 0))
    (when (boundp 'native-comp-verbose)
      (setq native-comp-verbose nil))
    (when (boundp 'native-comp-async-report-warnings-errors)
      (setq native-comp-async-report-warnings-errors nil))
    (when (boundp 'native-comp-jit-compilation)
      (setq native-comp-jit-compilation t))
    (when (boundp 'native-comp-enable-subr-trampolines)
      (setq native-comp-enable-subr-trampolines t))))

;;;; Type Hints for Compiler

;; Input processing functions
(declare-function nskk-convert-romaji "nskk-converter" t)
(put 'nskk-convert-romaji 'side-effect-free t)

;; State management functions
(declare-function nskk-state-get-mode "nskk-state" t)
(put 'nskk-state-get-mode 'side-effect-free t)

(declare-function nskk-state-set-mode "nskk-state" t)
(put 'nskk-state-set-mode 'side-effect-free nil)

;; Mode switching functions
(declare-function nskk-switch-to-hiragana "nskk" t)

;; Conversion helper functions
(declare-function nskk-core-hiragana-to-katakana "nskk-core" t)
(put 'nskk-core-hiragana-to-katakana 'side-effect-free t)

(declare-function nskk-core-katakana-to-hiragana "nskk-core" t)
(put 'nskk-core-katakana-to-hiragana 'side-effect-free t)

(declare-function nskk-core-hankaku-to-zenkaku "nskk-core" t)
(put 'nskk-core-hankaku-to-zenkaku 'side-effect-free t)

(declare-function nskk-core-zenkaku-to-hankaku "nskk-core" t)
(put 'nskk-core-zenkaku-to-hankaku 'side-effect-free t)

;;;; Compiler Macros for Native Compilation

;; Note: nskk-converter-lookup is already defined as inline in nskk-converter.el
;; No additional compiler macro needed here

;; Note: nskk-char-alphabetic-p is not currently used in the codebase
;; Compiler macro removed to avoid reference to undefined function

;;;; Async Compilation Utilities

(defun nskk-native-compile-async (&optional recursively load)
  "Asynchronously compile NSKK files.
If RECURSIVELY is non-nil, compile subdirectories too.
If LOAD is non-nil, load compiled files after compilation."
  (interactive "P")
  (unless nskk-native-compile-available
    (error "Native compilation is not available in this Emacs build"))

  (let ((nskk-dir (file-name-directory (locate-library "nskk-native-compile"))))
    (message "NSKK: Starting native compilation...")
    (native-compile-async nskk-dir recursively load)
    (message "NSKK: Native compilation scheduled")))

(defun nskk-native-compile-refresh ()
  "Recompile all NSKK files that have been modified."
  (interactive)
  (unless nskk-native-compile-available
    (error "Native compilation is not available in this Emacs build"))

  (let ((nskk-dir (file-name-directory (locate-library "nskk-native-compile"))))
    (message "NSKK: Refreshing native compilation...")
    (native-compile-async nskk-dir t t) ; Recursively compile and load
    (message "NSKK: Native compilation refresh scheduled")))

;;;; Compilation Status Reporting

(defun nskk-native-compile-status ()
  "Report native compilation status."
  (interactive)
  (message "=== NSKK Native Compilation Status ===")
  (message "Native compilation available: %s"
           (if nskk-native-compile-available "Yes" "No"))
  (message "Native compilation enabled: %s"
           (if nskk-native-compile-enabled "Yes" "No"))
  (message "Native compilation speed: %d"
           (if (bound-and-true-p native-comp-speed)
               native-comp-speed
             0))
  (message "JIT compilation: %s"
           (if (bound-and-true-p native-comp-jit-compilation) "Yes" "No"))

  (when nskk-native-compile-enabled
    (let ((nskk-dir (file-name-directory (locate-library "nskk-native-compile"))))
      (message "NSKK directory: %s" nskk-dir)
      (message "Compiled files: %d"
               (length (directory-files-recursively
                        nskk-dir "\\.eln$" nil))))))

;;;; Performance Monitoring

(defvar nskk--native-comp-baseline nil
  "Baseline performance metrics before native compilation.")

(defun nskk-native-compile-benchmark ()
  "Benchmark NSKK with and without native compilation."
  (interactive)
  (unless nskk-native-compile-available
    (error "Native compilation is not available"))

  (message "NSKK: Running native compilation benchmark...")

  ;; Run benchmark suite
  (let ((results (nskk--run-compile-benchmark)))
    (nskk--report-benchmark results)))

(defun nskk--run-compile-benchmark ()
  "Execute compilation benchmark suite."
  (let ((results (make-hash-table :test 'equal)))
    ;; Benchmark romaji conversion
    (puthash 'romaji-conversion
             (nskk--benchmark-function
              (lambda () (nskk-convert-romaji "konnichiwa"))
              1000)
             results)

    ;; Benchmark mode switching
    (puthash 'mode-switch
             (nskk--benchmark-function
              (lambda () (nskk-switch-to-hiragana))
              100)
             results)

    results))

(defun nskk--benchmark-function (func iterations)
  "Benchmark FUNC over ITERATIONS run cycles."
  (let ((times nil))
    ;; Warmup
    (dotimes (_ 10)
      (funcall func))

    ;; Measurement
    (dotimes (_ iterations)
      (let ((start (current-time)))
        (funcall func)
        (push (float-time (time-subtract (current-time) start)) times)))

    ;; Calculate statistics
    (let* ((sorted (sort times #'<))
           (total (apply #'+ times))
           (count (length times))
           (mean (/ total count))
           (median (nth (/ count 2) sorted))
           (min-time (car sorted))
           (max-time (car (last sorted))))
      (list :mean mean
            :median median
            :min min-time
            :max max-time
            :iterations count))))

(defun nskk--report-benchmark (results)
  "Report benchmark RESULTS in a temporary buffer."
  (with-output-to-temp-buffer "*NSKK Native Compile Benchmark*"
    (princ "=== NSKK Native Compilation Benchmark ===\n\n")

    (maphash
     (lambda (name data)
       (princ (format "--- %s ---\n" name))
       (princ (format "  Mean:   %.6f ms\n" (* 1000 (plist-get data :mean))))
       (princ (format "  Median: %.6f ms\n" (* 1000 (plist-get data :median))))
       (princ (format "  Min:    %.6f ms\n" (* 1000 (plist-get data :min))))
       (princ (format "  Max:    %.6f ms\n" (* 1000 (plist-get data :max))))
       (princ (format "  Runs:   %d\n\n" (plist-get data :iterations))))
     results)))

;;;; Optimization Hints for Native Compiler

;; Type declarations (using cl-deftype for better optimization)
(cl-deftype nskk-string () 'string)
(cl-deftype nskk-character () '(integer 0 1114111))
(cl-deftype nskk-mode () '(member hiragana katakana ascii))

;;;; Compilation Warnings Suppression

;; Suppress known warnings in native compilation
(with-eval-after-load 'warnings
  (add-to-list 'warning-suppress-types '(nskk)))

;;;; Initialize

(defun nskk-native-compile-initialize ()
  "Initialize native compilation support for NSKK.
Call `nskk-native-compile-async-refresh' to trigger recompilation."
  nil)

(defun nskk-native-compile-async-refresh ()
  "Refresh native compilation on startup if needed."
  (when nskk-native-compile-available
    (let ((nskk-dir (file-name-directory (locate-library "nskk-native-compile"))))
      ;; Check if any .el files are newer than their .eln counterparts
      (let ((needs-recompile nil))
        (dolist (el-file (directory-files-recursively nskk-dir "\\.el$"))
          (let ((eln-file (concat el-file "n")))
            (when (or (not (file-exists-p eln-file))
                      (file-newer-than-file-p el-file eln-file))
              (setq needs-recompile t)
              (cl-return))))
        (when needs-recompile
          (nskk-native-compile-refresh))))))

;;;; Auto-compilation on Save

(defun nskk-native-compile-after-save ()
  "Automatically recompile after saving NSKK files."
  (when (and nskk-native-compile-available
             (buffer-file-name)
             (string-match-p "nskk-.*\\.el\\'" (buffer-file-name)))
    (let ((eln-file (concat (buffer-file-name) "n")))
      (when (file-exists-p eln-file)
        (native-compile-async (buffer-file-name))))))

(defun nskk-native-compile-enable-auto-recompile ()
  "Enable automatic recompilation of NSKK files on save."
  (interactive)
  (add-hook 'after-save-hook #'nskk-native-compile-after-save))

;;;; Provide

(defalias 'nskk-compile-all #'nskk-native-compile-async
  "Compile all NSKK modules using native compilation.
This is an alias for `nskk-native-compile-async'.")

(provide 'nskk-native-compile)

;;; nskk-native-compile.el ends here
