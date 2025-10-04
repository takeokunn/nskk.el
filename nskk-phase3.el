;;; nskk-phase3.el --- Phase 3 Integration for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, integration
;; Version: 0.9.0
;; Package-Requires: ((emacs "31.0"))

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

;; Phase 3統合モジュール
;;
;; このモジュールはPhase 3の全トラックを統合します:
;;
;; Track N: Threading (並列処理)
;;   - nskk-thread-pool: スレッドプール管理
;;   - nskk-parallel-search: 並列辞書検索
;;   - nskk-async-learning: 非同期学習
;;   - nskk-sync-primitives: 同期プリミティブ
;;
;; Track O: Async UI (非同期UI)
;;   - nskk-async-candidates: 非同期候補表示
;;   - nskk-progress: プログレス表示
;;   - nskk-background: バックグラウンド処理
;;
;; Track P: Profiling (プロファイリング)
;;   - nskk-profiler: パフォーマンスプロファイラー
;;   - nskk-bottleneck-detector: ボトルネック検出
;;   - nskk-auto-tune: 自動チューニング
;;
;; Track Q: Architecture (アーキテクチャ)
;;   - nskk-architecture: 7層アーキテクチャ統合
;;
;; Track R: Plugin System (プラグインシステム)
;;   - Note: Track Rのモジュールは実装未完了の可能性があるため、
;;           安全にハンドリングします
;;
;; Track S: Transient UI (TransientUI)
;;   - nskk-transient-config: 設定UI
;;   - nskk-transient-plugins: プラグインUI
;;   - nskk-transient-debug: デバッグUI
;;
;; Track T: Optimization (最適化)
;;   - nskk-macro-optimize: マクロ最適化
;;   - nskk-native-compile: ネイティブコンパイル
;;   - nskk-memory-optimize: メモリ最適化
;;   - nskk-multi-cache: マルチレベルキャッシュ
;;
;; 初期化順序:
;; 1. Infrastructure Layer (Track N, T)
;; 2. Core Engine Layer (Track P)
;; 3. Application Layer (Track O)
;; 4. Presentation Layer (Track S)
;; 5. Extension Layer (Track Q)
;; 6. Plugin Layer (Track R - optional)

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

(defvar nskk-phase3-plugin-system-available nil
  "Track Rのプラグインシステムが利用可能かどうか。")

(condition-case err
    (progn
      (require 'nskk-plugin-system nil t)
      (require 'nskk-plugin-loader nil t)
      (require 'nskk-plugin-sandbox nil t)
      (require 'nskk-plugin-hooks nil t)
      (setq nskk-phase3-plugin-system-available t))
  (error
   (message "NSKK Phase 3: Plugin system (Track R) not available: %S" err)
   (setq nskk-phase3-plugin-system-available nil)))

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

(defgroup nskk-phase3 nil
  "NSKK Phase 3 integration settings."
  :group 'nskk
  :prefix "nskk-phase3-")

(defcustom nskk-phase3-enable-threading t
  "非nilの場合、並列処理機能を有効化する。"
  :type 'boolean
  :group 'nskk-phase3)

(defcustom nskk-phase3-enable-async-ui t
  "非nilの場合、非同期UI機能を有効化する。"
  :type 'boolean
  :group 'nskk-phase3)

(defcustom nskk-phase3-enable-profiling nil
  "非nilの場合、プロファイリング機能を有効化する。"
  :type 'boolean
  :group 'nskk-phase3)

(defcustom nskk-phase3-enable-auto-tune t
  "非nilの場合、自動チューニング機能を有効化する。"
  :type 'boolean
  :group 'nskk-phase3)

(defcustom nskk-phase3-enable-optimization t
  "非nilの場合、最適化機能を有効化する。"
  :type 'boolean
  :group 'nskk-phase3)

;;; グローバル状態

(defvar nskk-phase3-initialized nil
  "Phase 3が初期化済みかどうか。")

(defvar nskk-phase3-thread-pool nil
  "Phase 3で使用するグローバルスレッドプール。")

(defvar nskk-phase3-profiling-active nil
  "プロファイリングがアクティブかどうか。")

;;; 初期化関数

(defun nskk-phase3-initialize ()
  "Phase 3の全モジュールを初期化する。"
  (interactive)
  (when nskk-phase3-initialized
    (message "NSKK Phase 3 already initialized")
    (cl-return-from nskk-phase3-initialize))

  (message "NSKK Phase 3: Initializing...")

  ;; 1. Infrastructure Layer (Threading & Optimization)
  (when nskk-phase3-enable-threading
    (message "NSKK Phase 3: Initializing threading system...")
    (when (nskk-thread-pool-available-p)
      (setq nskk-phase3-thread-pool (nskk-thread-pool-create))
      (message "NSKK Phase 3: Thread pool created with %d workers"
               (nskk-thread-pool-size nskk-phase3-thread-pool))))

  (when nskk-phase3-enable-optimization
    (message "NSKK Phase 3: Initializing optimization system...")
    ;; メモリ最適化設定
    (nskk-memory-optimize-enable)
    ;; マルチレベルキャッシュ初期化
    (nskk-multi-cache-initialize)
    ;; ネイティブコンパイル設定
    (when (fboundp 'nskk-native-compile-setup)
      (nskk-native-compile-setup)))

  ;; 2. Core Engine Layer (Profiling)
  (when nskk-phase3-enable-profiling
    (message "NSKK Phase 3: Initializing profiling system...")
    (setq nskk-phase3-profiling-active nil))

  ;; 3. Application Layer (Async UI)
  (when nskk-phase3-enable-async-ui
    (message "NSKK Phase 3: Initializing async UI system...")
    ;; プログレス表示初期化
    (nskk-progress-initialize)
    ;; バックグラウンド処理初期化
    (nskk-background-initialize))

  ;; 4. Presentation Layer (Transient UI)
  (message "NSKK Phase 3: Initializing transient UI...")
  ;; Transient UIは必要に応じて起動されるため、ここでは何もしない

  ;; 5. Extension Layer (Architecture)
  (message "NSKK Phase 3: Initializing architecture layer...")
  (when (fboundp 'nskk-architecture-validate)
    (nskk-architecture-validate))

  ;; 6. Plugin Layer (optional)
  (when nskk-phase3-plugin-system-available
    (message "NSKK Phase 3: Initializing plugin system...")
    (when (fboundp 'nskk-plugin-system-initialize)
      (nskk-plugin-system-initialize)))

  ;; 7. Auto-tuning (最後に実行)
  (when nskk-phase3-enable-auto-tune
    (message "NSKK Phase 3: Running auto-tune...")
    (when (fboundp 'nskk-auto-tune-run)
      (nskk-auto-tune-run)))

  (setq nskk-phase3-initialized t)
  (message "NSKK Phase 3: Initialization complete"))

(defun nskk-phase3-shutdown ()
  "Phase 3の全モジュールをシャットダウンする。"
  (interactive)
  (unless nskk-phase3-initialized
    (message "NSKK Phase 3 not initialized")
    (cl-return-from nskk-phase3-shutdown))

  (message "NSKK Phase 3: Shutting down...")

  ;; プロファイリング停止
  (when nskk-phase3-profiling-active
    (nskk-profile-stop))

  ;; スレッドプールシャットダウン
  (when (and nskk-phase3-thread-pool
             (nskk-thread-pool-p nskk-phase3-thread-pool))
    (nskk-thread-pool-shutdown nskk-phase3-thread-pool)
    (setq nskk-phase3-thread-pool nil))

  ;; バックグラウンド処理停止
  (when (fboundp 'nskk-background-shutdown)
    (nskk-background-shutdown))

  ;; プラグインシステムシャットダウン
  (when (and nskk-phase3-plugin-system-available
             (fboundp 'nskk-plugin-system-shutdown))
    (nskk-plugin-system-shutdown))

  (setq nskk-phase3-initialized nil)
  (message "NSKK Phase 3: Shutdown complete"))

;;; 統合ユーティリティ関数

(defun nskk-phase3-status ()
  "Phase 3の現在の状態を表示する。"
  (interactive)
  (let ((status-buffer (get-buffer-create "*NSKK Phase 3 Status*")))
    (with-current-buffer status-buffer
      (erase-buffer)
      (insert "NSKK Phase 3 Status\n")
      (insert "==================\n\n")

      ;; 初期化状態
      (insert (format "Initialized: %s\n\n"
                      (if nskk-phase3-initialized "Yes" "No")))

      ;; Track N: Threading
      (insert "Track N: Threading\n")
      (insert "------------------\n")
      (insert (format "Enabled: %s\n" nskk-phase3-enable-threading))
      (when nskk-phase3-thread-pool
        (insert (format "Thread pool size: %d\n"
                        (nskk-thread-pool-size nskk-phase3-thread-pool)))
        (insert (format "Active threads: %d\n"
                        (nskk-thread-pool-active-count nskk-phase3-thread-pool))))
      (insert "\n")

      ;; Track O: Async UI
      (insert "Track O: Async UI\n")
      (insert "-----------------\n")
      (insert (format "Enabled: %s\n" nskk-phase3-enable-async-ui))
      (insert "\n")

      ;; Track P: Profiling
      (insert "Track P: Profiling\n")
      (insert "------------------\n")
      (insert (format "Enabled: %s\n" nskk-phase3-enable-profiling))
      (insert (format "Active: %s\n" nskk-phase3-profiling-active))
      (insert "\n")

      ;; Track Q: Architecture
      (insert "Track Q: Architecture\n")
      (insert "---------------------\n")
      (insert "7-Layer Architecture: Active\n")
      (insert "\n")

      ;; Track R: Plugin System
      (insert "Track R: Plugin System\n")
      (insert "----------------------\n")
      (insert (format "Available: %s\n" nskk-phase3-plugin-system-available))
      (insert "\n")

      ;; Track S: Transient UI
      (insert "Track S: Transient UI\n")
      (insert "---------------------\n")
      (insert "Transient menus: Available\n")
      (insert "\n")

      ;; Track T: Optimization
      (insert "Track T: Optimization\n")
      (insert "---------------------\n")
      (insert (format "Enabled: %s\n" nskk-phase3-enable-optimization))
      (insert (format "Auto-tune: %s\n" nskk-phase3-enable-auto-tune))
      (insert "\n"))

    (pop-to-buffer status-buffer)))

(defun nskk-phase3-benchmark ()
  "Phase 3のパフォーマンスベンチマークを実行する。"
  (interactive)
  (message "Running NSKK Phase 3 benchmark...")
  (message "Please run: emacs -batch -l tests/nskk-phase3-benchmark.el"))

(defun nskk-phase3-verify ()
  "Phase 3の統合状態を検証する。"
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
          (message "NSKK Phase 3 verification FAILED:")
          (dolist (err errors)
            (message "  - %s" err))
          nil)
      (message "NSKK Phase 3 verification PASSED")
      t)))

;;; 自動初期化

;; Phase 3は明示的な初期化が必要なため、自動初期化は行わない
;; ユーザーは (nskk-phase3-initialize) を呼び出す必要がある

(provide 'nskk-phase3)

;;; nskk-phase3.el ends here
