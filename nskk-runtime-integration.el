;;; nskk-runtime-integration.el --- Runtime integration orchestrator -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, integration
;; Version: 0.9.0
;; Package-Requires: ((emacs "30.0"))

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

;; ランタイム機能の統合を担当するモジュールです。
;;
;; 以下のコンポーネント群をまとめて初期化・停止します:
;; - 並列処理スタック (thread-pool, parallel-search, async-learning, sync-primitives)
;; - 非同期UI (async-candidates, progress, background)
;; - パフォーマンス支援 (profiler, bottleneck-detector, auto-tune)
;; - アーキテクチャ検証 (nskk-architecture)
;; - Transient UI (transient-config / transient-plugins / transient-debug)
;; - 最適化群 (macro-optimize, native-compile, memory-optimize, multi-cache)
;; - プラグイン統合 (ロード可能な場合のみ)
;;
;; `nskk-runtime-integration-initialize` と
;; `nskk-runtime-integration-shutdown` を通じてライフサイクルを管理します。
;;; Code:

(require 'cl-lib)

;;; Track N: Threading

(require 'nskk-thread-pool)
(require 'nskk-parallel-search)
(require 'nskk-async-learning)
(require 'nskk-sync-primitives)

;;; Track O: Async UI

(require 'nskk-async-candidates)
(require 'nskk-progress)
(require 'nskk-background)

;;; Track P: Profiling

(require 'nskk-profiler)
(require 'nskk-bottleneck-detector)
(require 'nskk-auto-tune)

;;; Track Q: Architecture

(require 'nskk-architecture)

;;; Track R: Plugin System (optional - handle gracefully)

(defvar nskk-runtime-integration-plugin-system-available nil
  "Track Rのプラグインシステムが利用可能かどうか。")

(condition-case err
    (progn
      (require 'nskk-plugin-system nil t)
      (require 'nskk-plugin-loader nil t)
      (require 'nskk-plugin-sandbox nil t)
      (require 'nskk-plugin-hooks nil t)
      (setq nskk-runtime-integration-plugin-system-available t))
  (error
   (message "NSKK Runtime: Plugin system (Track R) not available: %S" err)
   (setq nskk-runtime-integration-plugin-system-available nil)))

;;; Track S: Transient UI

(require 'nskk-transient-config)
(require 'nskk-transient-plugins)
(require 'nskk-transient-debug)

;;; Track T: Optimization

(require 'nskk-macro-optimize)
(require 'nskk-native-compile)
(require 'nskk-memory-optimize)
(require 'nskk-multi-cache)

;;; カスタマイズ変数

(defgroup nskk-runtime-integration nil
  "NSKK Runtime integration settings."
  :group 'nskk
  :prefix "nskk-runtime-integration-")

(defcustom nskk-runtime-integration-enable-threading t
  "非nilの場合、並列処理機能を有効化する。"
  :type 'boolean
  :group 'nskk-runtime-integration)

(defcustom nskk-runtime-integration-enable-async-ui t
  "非nilの場合、非同期UI機能を有効化する。"
  :type 'boolean
  :group 'nskk-runtime-integration)

(defcustom nskk-runtime-integration-enable-profiling nil
  "非nilの場合、プロファイリング機能を有効化する。"
  :type 'boolean
  :group 'nskk-runtime-integration)

(defcustom nskk-runtime-integration-enable-auto-tune t
  "非nilの場合、自動チューニング機能を有効化する。"
  :type 'boolean
  :group 'nskk-runtime-integration)

(defcustom nskk-runtime-integration-enable-optimization t
  "非nilの場合、最適化機能を有効化する。"
  :type 'boolean
  :group 'nskk-runtime-integration)

;;; グローバル状態

(defvar nskk-runtime-integration-initialized nil
  "ランタイム統合が初期化済みかどうか。")

(defvar nskk-runtime-integration-thread-pool nil
  "ランタイム統合で使用するグローバルスレッドプール。")

(defvar nskk-runtime-integration-profiling-active nil
  "プロファイリングがアクティブかどうか。")

;;; 公開ヘルパー

(defun nskk-runtime-integration-initialized-p ()
  "ランタイム統合が初期化済みかどうかを返す。"
  nskk-runtime-integration-initialized)

(defun nskk-runtime-integration-thread-pool ()
  "ランタイム統合で管理しているスレッドプールを返す。
存在しない場合はnilを返す。"
  nskk-runtime-integration-thread-pool)

(defun nskk-runtime-integration-profiling-active-p ()
  "プロファイリングが作動中かどうかを返す。"
  nskk-runtime-integration-profiling-active)

;;; 初期化関数

(defun nskk-runtime-integration-initialize ()
  "ランタイム統合の全モジュールを初期化する。"
  (interactive)
  (cl-block nskk-runtime-integration-initialize
    (when nskk-runtime-integration-initialized
      (message "NSKK Runtime already initialized")
      (cl-return-from nskk-runtime-integration-initialize))

    (message "NSKK Runtime: Initializing...")

    ;; 1. Infrastructure Layer (Threading & Optimization)
    (when nskk-runtime-integration-enable-threading
      (message "NSKK Runtime: Initializing threading system...")
      (when (nskk-thread-pool-available-p)
        (setq nskk-runtime-integration-thread-pool (nskk-thread-pool-create))
        (message "NSKK Runtime: Thread pool created with %d workers"
                 (nskk-thread-pool-size nskk-runtime-integration-thread-pool))))

    (when nskk-runtime-integration-enable-optimization
      (message "NSKK Runtime: Initializing optimization system...")
      ;; メモリ最適化設定
      (when (fboundp 'nskk-memory-optimize-enable)
        (nskk-memory-optimize-enable))
      ;; マルチレベルキャッシュ初期化
      (when (fboundp 'nskk-multi-cache-initialize)
        (nskk-multi-cache-initialize))
      ;; ネイティブコンパイル設定
      (when (fboundp 'nskk-native-compile-setup)
        (nskk-native-compile-setup)))

    ;; 2. Core Engine Layer (Profiling)
    (when nskk-runtime-integration-enable-profiling
      (message "NSKK Runtime: Initializing profiling system...")
      (setq nskk-runtime-integration-profiling-active nil))

    ;; 3. Application Layer (Async UI)
    (when nskk-runtime-integration-enable-async-ui
      (message "NSKK Runtime: Initializing async UI system...")
      ;; プログレス表示初期化
      (when (fboundp 'nskk-progress-initialize)
        (nskk-progress-initialize))
      ;; バックグラウンド処理初期化
      (when (fboundp 'nskk-background-initialize)
        (nskk-background-initialize)))

    ;; 4. Presentation Layer (Transient UI)
    (message "NSKK Runtime: Initializing transient UI...")
    ;; Transient UIは必要に応じて起動されるため、ここでは何もしない

    ;; 5. Extension Layer (Architecture)
    (message "NSKK Runtime: Initializing architecture layer...")
    (when (fboundp 'nskk-architecture-validate)
      (nskk-architecture-validate))

    ;; 6. Plugin Layer (optional)
    (when nskk-runtime-integration-plugin-system-available
      (message "NSKK Runtime: Initializing plugin system...")
      (when (fboundp 'nskk-plugin-system-initialize)
        (nskk-plugin-system-initialize)))

    ;; 7. Auto-tuning (最後に実行)
    (when nskk-runtime-integration-enable-auto-tune
      (message "NSKK Runtime: Running auto-tune...")
      (when (fboundp 'nskk-auto-tune-run)
        (nskk-auto-tune-run)))

    (setq nskk-runtime-integration-initialized t)
    (message "NSKK Runtime: Initialization complete")))

(defun nskk-runtime-integration-shutdown ()
  "ランタイム統合の全モジュールをシャットダウンする。"
  (interactive)
  (cl-block nskk-runtime-integration-shutdown
    (unless nskk-runtime-integration-initialized
      (message "NSKK Runtime not initialized")
      (cl-return-from nskk-runtime-integration-shutdown))

    (message "NSKK Runtime: Shutting down...")

    ;; プロファイリング停止
    (when nskk-runtime-integration-profiling-active
      (nskk-profile-stop))

    ;; スレッドプールシャットダウン
    (when (and nskk-runtime-integration-thread-pool
               (nskk-thread-pool-p nskk-runtime-integration-thread-pool))
      (nskk-thread-pool-shutdown nskk-runtime-integration-thread-pool)
      (setq nskk-runtime-integration-thread-pool nil))

    ;; バックグラウンド処理停止
    (when (fboundp 'nskk-background-shutdown)
      (nskk-background-shutdown))

    ;; プラグインシステムシャットダウン
    (when (and nskk-runtime-integration-plugin-system-available
               (fboundp 'nskk-plugin-system-shutdown))
      (nskk-plugin-system-shutdown))

    (setq nskk-runtime-integration-initialized nil)
    (message "NSKK Runtime: Shutdown complete")))

(defun nskk-runtime-integration-status ()
  "ランタイム統合の現在の状態を表示する。"
  (interactive)
  (let ((status-buffer (get-buffer-create "*NSKK Runtime Status*")))
    (with-current-buffer status-buffer
      (erase-buffer)
      (insert "NSKK Runtime Status\n")
      (insert "==================\n\n")

      ;; 初期化状態
      (insert (format "Initialized: %s\n\n"
                      (if nskk-runtime-integration-initialized "Yes" "No")))

      ;; Track N: Threading
      (insert "Track N: Threading\n")
      (insert "------------------\n")
      (insert (format "Enabled: %s\n" nskk-runtime-integration-enable-threading))
      (when nskk-runtime-integration-thread-pool
        (insert (format "Thread pool size: %d\n"
                        (nskk-thread-pool-size nskk-runtime-integration-thread-pool)))
        (insert (format "Active threads: %d\n"
                        (nskk-thread-pool-active-count nskk-runtime-integration-thread-pool))))
      (insert "\n")

      ;; Track O: Async UI
      (insert "Track O: Async UI\n")
      (insert "-----------------\n")
      (insert (format "Enabled: %s\n" nskk-runtime-integration-enable-async-ui))
      (insert "\n")

      ;; Track P: Profiling
      (insert "Track P: Profiling\n")
      (insert "------------------\n")
      (insert (format "Enabled: %s\n" nskk-runtime-integration-enable-profiling))
      (insert (format "Active: %s\n" nskk-runtime-integration-profiling-active))
      (insert "\n")

      ;; Track Q: Architecture
      (insert "Track Q: Architecture\n")
      (insert "---------------------\n")
      (insert "7-Layer Architecture: Active\n")
      (insert "\n")

      ;; Track R: Plugin System
      (insert "Track R: Plugin System\n")
      (insert "----------------------\n")
      (insert (format "Available: %s\n" nskk-runtime-integration-plugin-system-available))
      (insert "\n")

      ;; Track S: Transient UI
      (insert "Track S: Transient UI\n")
      (insert "---------------------\n")
      (insert "Transient menus: Available\n")
      (insert "\n")

      ;; Track T: Optimization
      (insert "Track T: Optimization\n")
      (insert "---------------------\n")
      (insert (format "Enabled: %s\n" nskk-runtime-integration-enable-optimization))
      (insert (format "Auto-tune: %s\n" nskk-runtime-integration-enable-auto-tune))
      (insert "\n"))

    (pop-to-buffer status-buffer)))

(defun nskk-runtime-integration-benchmark ()
  "ランタイム統合のパフォーマンスベンチマークを実行する。"
  (interactive)
  (message "Running NSKK Runtime benchmark...")
  (message "Please run: emacs -batch -l tests/nskk-runtime-integration-benchmark.el"))

(defun nskk-runtime-integration-verify ()
  "ランタイム統合の統合状態を検証する。"
  (interactive)
  (let ((errors nil))

    ;; Track N検証
    (unless (featurep 'nskk-thread-pool)
      (push "Track N: nskk-thread-pool not loaded" errors))

    ;; Track O検証
    (unless (featurep 'nskk-async-candidates)
      (push "Track O: nskk-async-candidates not loaded" errors))

    ;; Track P検証
    (unless (featurep 'nskk-profiler)
      (push "Track P: nskk-profiler not loaded" errors))

    ;; Track Q検証
    (unless (featurep 'nskk-architecture)
      (push "Track Q: nskk-architecture not loaded" errors))

    ;; Track S検証
    (unless (featurep 'nskk-transient-config)
      (push "Track S: nskk-transient-config not loaded" errors))

    ;; Track T検証
    (unless (featurep 'nskk-multi-cache)
      (push "Track T: nskk-multi-cache not loaded" errors))

    (if errors
        (progn
          (message "NSKK Runtime verification FAILED:")
          (dolist (err errors)
            (message "  - %s" err))
          nil)
      (message "NSKK Runtime verification PASSED")
      t)))

(provide 'nskk-runtime-integration)

;;; nskk-runtime-integration.el ends here
