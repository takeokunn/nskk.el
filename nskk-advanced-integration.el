;;; nskk-advanced-integration.el --- Advanced feature integration for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, integration, ai, sync, analytics
;; Version: 1.0.0
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

;; 拡張機能群 (AI / 同期 / 分析) の初期化とライフサイクルをまとめて管理する
;; モジュールです。各機能は個別に有効化できます。
;;
;; 主なエントリポイント:
;; - `nskk-advanced-integration-initialize`
;; - `nskk-advanced-integration-shutdown`
;; - `nskk-advanced-integration-status`
;;
;; 互換性のため、従来の Phase4 API へのエイリアスも提供しています。

;;; Code:

(require 'cl-lib)

;; AI
(require 'nskk-ai-context)
(require 'nskk-ai-pattern)
(require 'nskk-ai-candidates)
(require 'nskk-ai-learning)

;; Sync
(require 'nskk-sync-protocol)
(require 'nskk-sync-crypto)
(require 'nskk-sync-diff)
(require 'nskk-sync-conflict)

;; Analytics
(require 'nskk-analytics-pattern)
(require 'nskk-analytics-optimize)
(require 'nskk-analytics-report)
(require 'nskk-analytics-dashboard)

;;; Customization

(defgroup nskk-advanced-integration nil
  "NSKK の拡張機能 (AI / 同期 / 分析) を統合する設定。"
  :group 'nskk
  :prefix "nskk-advanced-integration-")

(defcustom nskk-advanced-integration-enable-ai t
  "非nilの場合、AI統合機能を起動する。"
  :type 'boolean
  :group 'nskk-advanced-integration)

(defcustom nskk-advanced-integration-enable-sync nil
  "非nilの場合、同期機能を起動する。サーバーURLの指定が必要。"
  :type 'boolean
  :group 'nskk-advanced-integration)

(defcustom nskk-advanced-integration-enable-analytics t
  "非nilの場合、分析・最適化機能を起動する。"
  :type 'boolean
  :group 'nskk-advanced-integration)

(defcustom nskk-advanced-integration-ai-context-window-size 100
  "AI文脈解析で使用する文字数ウィンドウ。"
  :type 'integer
  :group 'nskk-advanced-integration)

(defcustom nskk-advanced-integration-sync-server-url nil
  "同期サーバーのURL。nil の場合は同期を無効化する。"
  :type '(choice (const :tag "Disabled" nil)
                 (string :tag "Server URL"))
  :group 'nskk-advanced-integration)

(defcustom nskk-advanced-integration-analytics-report-interval 'weekly
  "分析レポートを自動生成する頻度。"
  :type '(choice (const :tag "Daily" daily)
                 (const :tag "Weekly" weekly)
                 (const :tag "Monthly" monthly)
                 (const :tag "Disabled" nil))
  :group 'nskk-advanced-integration)

;;; Internal state

(defvar nskk-advanced-integration--initialized nil
  "拡張機能統合が初期化済みかどうか。")

(defvar nskk-advanced-integration--ai-enabled nil
  "AI機能が実際に有効化されているかどうか。")

(defvar nskk-advanced-integration--sync-enabled nil
  "同期機能が実際に有効化されているかどうか。")

(defvar nskk-advanced-integration--analytics-enabled nil
  "分析機能が実際に有効化されているかどうか。")

;;; Helpers

(defun nskk-advanced-integration-initialized-p ()
  "拡張機能統合が初期化済みであれば非nilを返す。"
  nskk-advanced-integration--initialized)

(defun nskk-advanced-integration-ai-enabled-p ()
  "AI機能が有効化済みであれば非nilを返す。"
  nskk-advanced-integration--ai-enabled)

(defun nskk-advanced-integration-sync-enabled-p ()
  "同期機能が有効化済みであれば非nilを返す。"
  nskk-advanced-integration--sync-enabled)

(defun nskk-advanced-integration-analytics-enabled-p ()
  "分析機能が有効化済みであれば非nilを返す。"
  nskk-advanced-integration--analytics-enabled)

(defun nskk-advanced-integration--initialize-ai ()
  "AI統合機能を初期化する。成功したら non-nil を返す。"
  (when nskk-advanced-integration-enable-ai
    (condition-case err
        (progn
          (when (fboundp 'nskk-ai-context-set-window-size)
            (nskk-ai-context-set-window-size
             nskk-advanced-integration-ai-context-window-size))
          (when (fboundp 'nskk-ai-context-initialize)
            (nskk-ai-context-initialize))
          (when (fboundp 'nskk-ai-pattern-initialize)
            (nskk-ai-pattern-initialize))
          (when (fboundp 'nskk-ai-learning-initialize)
            (nskk-ai-learning-initialize))
          (when (fboundp 'nskk-ai-candidates-initialize)
            (nskk-ai-candidates-initialize))
          (message "NSKK Advanced: AI stack ready")
          t)
      (error
       (message "NSKK Advanced: AI initialization failed: %S" err)
       nil))))

(defun nskk-advanced-integration--shutdown-ai ()
  "AI統合機能をシャットダウンする。"
  (when nskk-advanced-integration--ai-enabled
    (when (fboundp 'nskk-ai-learning-save)
      (nskk-ai-learning-save))
    (when (fboundp 'nskk-ai-pattern-save)
      (nskk-ai-pattern-save))
    (setq nskk-advanced-integration--ai-enabled nil)
    (message "NSKK Advanced: AI stack shut down")))

(defun nskk-advanced-integration--initialize-sync ()
  "同期機能を初期化する。成功したら non-nil を返す。"
  (when (and nskk-advanced-integration-enable-sync
             nskk-advanced-integration-sync-server-url)
    (condition-case err
        (progn
          (when (fboundp 'nskk-sync-crypto-initialize)
            (nskk-sync-crypto-initialize))
          (when (fboundp 'nskk-sync-protocol-initialize)
            (nskk-sync-protocol-initialize
             nskk-advanced-integration-sync-server-url))
          (message "NSKK Advanced: Sync connected to %s"
                   nskk-advanced-integration-sync-server-url)
          t)
      (error
       (message "NSKK Advanced: Sync initialization failed: %S" err)
       nil))))

(defun nskk-advanced-integration--shutdown-sync ()
  "同期機能をシャットダウンする。"
  (when nskk-advanced-integration--sync-enabled
    (when (fboundp 'nskk-sync-protocol-flush)
      (nskk-sync-protocol-flush))
    (when (fboundp 'nskk-sync-protocol-shutdown)
      (nskk-sync-protocol-shutdown))
    (setq nskk-advanced-integration--sync-enabled nil)
    (message "NSKK Advanced: Sync shut down")))

(defun nskk-advanced-integration--initialize-analytics ()
  "分析機能を初期化する。成功したら non-nil を返す。"
  (when nskk-advanced-integration-enable-analytics
    (condition-case err
        (progn
          (when (fboundp 'nskk-analytics-pattern-start-collection)
            (nskk-analytics-pattern-start-collection))
          (when (fboundp 'nskk-analytics-optimize-initialize)
            (nskk-analytics-optimize-initialize))
          (when (and nskk-advanced-integration-analytics-report-interval
                     (fboundp 'nskk-analytics-report-schedule))
            (nskk-analytics-report-schedule
             nskk-advanced-integration-analytics-report-interval))
          (message "NSKK Advanced: Analytics ready")
          t)
      (error
       (message "NSKK Advanced: Analytics initialization failed: %S" err)
       nil))))

(defun nskk-advanced-integration--shutdown-analytics ()
  "分析機能をシャットダウンする。"
  (when nskk-advanced-integration--analytics-enabled
    (when (fboundp 'nskk-analytics-pattern-stop-collection)
      (nskk-analytics-pattern-stop-collection))
    (when (fboundp 'nskk-analytics-pattern-save)
      (nskk-analytics-pattern-save))
    (setq nskk-advanced-integration--analytics-enabled nil)
    (message "NSKK Advanced: Analytics shut down")))

;;; Lifecycle

(defun nskk-advanced-integration-initialize ()
  "拡張機能群を初期化する。"
  (interactive)
  (if (nskk-advanced-integration-initialized-p)
      (message "NSKK Advanced: already initialized")
    (progn
      (setq nskk-advanced-integration--ai-enabled
            (nskk-advanced-integration--initialize-ai))
      (setq nskk-advanced-integration--sync-enabled
            (nskk-advanced-integration--initialize-sync))
      (setq nskk-advanced-integration--analytics-enabled
            (nskk-advanced-integration--initialize-analytics))
      (setq nskk-advanced-integration--initialized t)
      (message "NSKK Advanced: initialization complete")))
  nskk-advanced-integration--initialized)

(defun nskk-advanced-integration-shutdown ()
  "拡張機能群をシャットダウンする。"
  (interactive)
  (when (nskk-advanced-integration-initialized-p)
    (nskk-advanced-integration--shutdown-analytics)
    (nskk-advanced-integration--shutdown-sync)
    (nskk-advanced-integration--shutdown-ai)
    (setq nskk-advanced-integration--initialized nil)
    (message "NSKK Advanced: shutdown complete")))

;;; Status reporting

(defun nskk-advanced-integration-status ()
  "現在の有効状態をメッセージで表示する。"
  (interactive)
  (message
   (string-join
    (delq nil
          (list
           (format "NSKK Advanced initialized: %s"
                   (if (nskk-advanced-integration-initialized-p) 'yes 'no))
           (format "  AI: %s" (if (nskk-advanced-integration-ai-enabled-p) 'on 'off))
           (when nskk-advanced-integration-enable-sync
             (format "  Sync: %s (server=%s)"
                     (if (nskk-advanced-integration-sync-enabled-p) 'on 'off)
                     (or nskk-advanced-integration-sync-server-url "n/a")))
           (format "  Analytics: %s"
                   (if (nskk-advanced-integration-analytics-enabled-p) 'on 'off))))
    "\n")))

(defun nskk-advanced-integration-health-check ()
  "必要な機能がロード済みかを確認する。"
  (interactive)
  (let* ((modules '(nskk-ai-context
                    nskk-ai-pattern
                    nskk-ai-candidates
                    nskk-ai-learning
                    nskk-sync-protocol
                    nskk-sync-crypto
                    nskk-sync-diff
                    nskk-sync-conflict
                    nskk-analytics-pattern
                    nskk-analytics-optimize
                    nskk-analytics-report
                    nskk-analytics-dashboard))
         (missing (cl-remove-if #'featurep modules)))
    (if missing
        (progn
          (message "NSKK Advanced: missing modules %s" missing)
          nil)
      (message "NSKK Advanced: all modules loaded")
      t)))

(defun nskk-advanced-integration-integration-test-ready-p ()
  "統合テスト実行に必要な関数が揃っているか確認する。"
  (and (nskk-advanced-integration-health-check)
       (cl-every #'fboundp
                 '(nskk-ai-context-analyze
                   nskk-ai-pattern-detect
                   nskk-ai-candidates-rank
                   nskk-ai-learning-update
                   nskk-sync-protocol-connect
                   nskk-sync-crypto-encrypt
                   nskk-sync-diff-compute
                   nskk-sync-conflict-resolve
                   nskk-analytics-pattern-record
                   nskk-analytics-optimize-tune
                   nskk-analytics-report-generate
                   nskk-analytics-dashboard-show))))

;;; Convenience wrappers

(defun nskk-advanced-integration-show-dashboard ()
  "分析ダッシュボードを表示する。"
  (interactive)
  (if (and (nskk-advanced-integration-analytics-enabled-p)
           (fboundp 'nskk-analytics-dashboard-show))
      (nskk-analytics-dashboard-show)
    (message "NSKK Advanced: analytics dashboard is unavailable")))

(defun nskk-advanced-integration-generate-report ()
  "分析レポートを生成する。"
  (interactive)
  (if (and (nskk-advanced-integration-analytics-enabled-p)
           (fboundp 'nskk-analytics-report-generate))
      (call-interactively 'nskk-analytics-report-generate)
    (message "NSKK Advanced: analytics report is unavailable")))

(defun nskk-advanced-integration-sync-now ()
  "同期を即時実行する。"
  (interactive)
  (if (and (nskk-advanced-integration-sync-enabled-p)
           (fboundp 'nskk-sync-protocol-sync))
      (nskk-sync-protocol-sync)
    (message "NSKK Advanced: sync is unavailable")))

;;; Compatibility aliases (Phase4)

(define-obsolete-function-alias 'nskk-phase4-initialize
  'nskk-advanced-integration-initialize "1.0.0")
(define-obsolete-function-alias 'nskk-phase4-shutdown
  'nskk-advanced-integration-shutdown "1.0.0")
(define-obsolete-function-alias 'nskk-phase4-status
  'nskk-advanced-integration-status "1.0.0")
(define-obsolete-function-alias 'nskk-phase4-health-check
  'nskk-advanced-integration-health-check "1.0.0")
(define-obsolete-function-alias 'nskk-phase4-integration-test-ready-p
  'nskk-advanced-integration-integration-test-ready-p "1.0.0")
(define-obsolete-function-alias 'nskk-phase4-show-dashboard
  'nskk-advanced-integration-show-dashboard "1.0.0")
(define-obsolete-function-alias 'nskk-phase4-generate-report
  'nskk-advanced-integration-generate-report "1.0.0")
(define-obsolete-function-alias 'nskk-phase4-sync-now
  'nskk-advanced-integration-sync-now "1.0.0")

(define-obsolete-variable-alias 'nskk-phase4-enable-ai
  'nskk-advanced-integration-enable-ai "1.0.0")
(define-obsolete-variable-alias 'nskk-phase4-enable-sync
  'nskk-advanced-integration-enable-sync "1.0.0")
(define-obsolete-variable-alias 'nskk-phase4-enable-analytics
  'nskk-advanced-integration-enable-analytics "1.0.0")
(define-obsolete-variable-alias 'nskk-phase4-ai-context-window-size
  'nskk-advanced-integration-ai-context-window-size "1.0.0")
(define-obsolete-variable-alias 'nskk-phase4-sync-server-url
  'nskk-advanced-integration-sync-server-url "1.0.0")
(define-obsolete-variable-alias 'nskk-phase4-analytics-report-interval
  'nskk-advanced-integration-analytics-report-interval "1.0.0")
(define-obsolete-variable-alias 'nskk-phase4--initialized
  'nskk-advanced-integration--initialized "1.0.0")
(define-obsolete-variable-alias 'nskk-phase4--ai-enabled
  'nskk-advanced-integration--ai-enabled "1.0.0")
(define-obsolete-variable-alias 'nskk-phase4--sync-enabled
  'nskk-advanced-integration--sync-enabled "1.0.0")
(define-obsolete-variable-alias 'nskk-phase4--analytics-enabled
  'nskk-advanced-integration--analytics-enabled "1.0.0")

(provide 'nskk-advanced-integration)

;;; nskk-advanced-integration.el ends here
