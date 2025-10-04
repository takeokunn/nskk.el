;;; nskk-architecture.el --- 7-Layer Architecture Integration for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, architecture
;; Version: 0.1.0
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

;; 7層アーキテクチャ統合モジュール
;;
;; NSKKの7層アーキテクチャ:
;;
;; Layer 1: Presentation Layer    - UI統合、イベントハンドリング
;; Layer 2: Extension Layer       - フック、イベントバス、レイヤー間通信
;; Layer 3: Application Layer     - ビジネスロジック、変換制御
;; Layer 4: Core Engine Layer     - 変換エンジン、辞書エンジン
;; Layer 5: Data Access Layer     - 永続化、同期
;; Layer 6: Infrastructure Layer  - スレッド管理、メモリ管理
;; Layer 7: QA Layer              - テスト、ベンチマーク統合
;;
;; レイヤー間通信フロー:
;;
;; ユーザー入力
;;   ↓
;; Presentation Layer (UI)
;;   ↓ (Extension Layerを経由)
;; Application Layer (ビジネスロジック)
;;   ↓
;; Core Engine Layer (変換処理)
;;   ↓
;; Data Access Layer (辞書検索)
;;   ↓
;; Infrastructure Layer (リソース管理)
;;
;; 依存性注入とメッセージング:
;; - すべてのレイヤー間通信はExtension Layerを経由
;; - イベント駆動アーキテクチャによる疎結合
;; - 依存性逆転の原則に基づく設計
;;
;; 使用例:
;; (nskk-architecture-initialize)
;; (nskk-architecture-health-check)
;; (nskk-architecture-get-layer-status 'application)

;;; Code:

(require 'cl-lib)
(require 'nskk-layer-presentation)
(require 'nskk-layer-extension)
(require 'nskk-layer-application)
(require 'nskk-layer-core)
(require 'nskk-layer-data)
(require 'nskk-layer-infrastructure)
(require 'nskk-layer-qa)

;;; カスタマイズ可能変数

(defgroup nskk-architecture nil
  "7-layer architecture settings for NSKK."
  :group 'nskk
  :prefix "nskk-architecture-")

(defcustom nskk-architecture-enable-validation t
  "レイヤー間通信の検証を有効にするか。"
  :type 'boolean
  :group 'nskk-architecture)

(defcustom nskk-architecture-enable-tracing nil
  "レイヤー間通信のトレースを有効にするか。"
  :type 'boolean
  :group 'nskk-architecture)

;;; 内部変数

(defvar nskk-architecture--layers
  '(infrastructure data core application extension presentation qa)
  "レイヤーのリスト（初期化順）。")

(defvar nskk-architecture--layer-status (make-hash-table :test 'eq)
  "各レイヤーの状態。")

(defvar nskk-architecture--initialized nil
  "アーキテクチャが初期化済みかどうか。")

(defvar nskk-architecture--communication-log nil
  "レイヤー間通信ログ。")

;;; 初期化・シャットダウン

(defun nskk-architecture-initialize ()
  "7層アーキテクチャを初期化する。"
  (interactive)
  (unless nskk-architecture--initialized
    (nskk-architecture--log "Initializing 7-layer architecture...")

    ;; レイヤーを下から順に初期化
    (dolist (layer nskk-architecture--layers)
      (nskk-architecture--initialize-layer layer))

    ;; レイヤー間通信をセットアップ
    (nskk-architecture--setup-layer-communication)

    ;; イベントフローをセットアップ
    (nskk-architecture--setup-event-flow)

    (setq nskk-architecture--initialized t)
    (nskk-architecture--log "7-layer architecture initialized successfully")
    (message "NSKK 7-layer architecture initialized")))

(defun nskk-architecture-shutdown ()
  "7層アーキテクチャをシャットダウンする。"
  (interactive)
  (when nskk-architecture--initialized
    (nskk-architecture--log "Shutting down 7-layer architecture...")

    ;; レイヤーを上から順にシャットダウン
    (dolist (layer (reverse nskk-architecture--layers))
      (nskk-architecture--shutdown-layer layer))

    ;; 通信ログをクリア
    (setq nskk-architecture--communication-log nil)

    (setq nskk-architecture--initialized nil)
    (nskk-architecture--log "7-layer architecture shutdown complete")
    (message "NSKK 7-layer architecture shutdown")))

;;; レイヤー初期化

(defun nskk-architecture--initialize-layer (layer)
  "レイヤーを初期化する。
LAYERはレイヤー名。"
  (nskk-architecture--log "Initializing layer: %s" layer)
  (condition-case err
      (progn
        (pcase layer
          ('infrastructure (nskk-infrastructure-initialize))
          ('data (nskk-data-initialize))
          ('core (nskk-core-initialize))
          ('application (nskk-application-initialize))
          ('extension (nskk-extension-initialize))
          ('presentation (nskk-presentation-initialize))
          ('qa (nskk-qa-initialize)))
        (puthash layer :initialized nskk-architecture--layer-status)
        (nskk-architecture--log "Layer %s initialized" layer))
    (error
     (nskk-architecture--log "Failed to initialize layer %s: %s" layer err)
     (puthash layer :error nskk-architecture--layer-status)
     (error "Layer initialization failed: %s" layer))))

(defun nskk-architecture--shutdown-layer (layer)
  "レイヤーをシャットダウンする。
LAYERはレイヤー名。"
  (nskk-architecture--log "Shutting down layer: %s" layer)
  (condition-case err
      (progn
        (pcase layer
          ('infrastructure (nskk-infrastructure-shutdown))
          ('data (nskk-data-shutdown))
          ('core (nskk-core-shutdown))
          ('application (nskk-application-shutdown))
          ('extension (nskk-extension-shutdown))
          ('presentation (nskk-presentation-shutdown))
          ('qa (nskk-qa-shutdown)))
        (puthash layer :shutdown nskk-architecture--layer-status))
    (error
     (nskk-architecture--log "Failed to shutdown layer %s: %s" layer err))))

;;; レイヤー間通信セットアップ

(defun nskk-architecture--setup-layer-communication ()
  "レイヤー間通信をセットアップする。"
  (nskk-architecture--log "Setting up layer communication...")

  ;; Presentation → Application
  (nskk-extension-register-route
   'presentation 'application
   #'nskk-architecture--route-presentation-to-application)

  ;; Application → Core
  (nskk-extension-register-route
   'application 'core
   #'nskk-architecture--route-application-to-core)

  ;; Application → Data
  (nskk-extension-register-route
   'application 'data
   #'nskk-architecture--route-application-to-data)

  ;; Core → Data
  (nskk-extension-register-route
   'core 'data
   #'nskk-architecture--route-core-to-data)

  ;; Data → Infrastructure
  (nskk-extension-register-route
   'data 'infrastructure
   #'nskk-architecture--route-data-to-infrastructure)

  (nskk-architecture--log "Layer communication setup complete"))

;;; ルーティングハンドラー

(defun nskk-architecture--route-presentation-to-application (action &rest args)
  "Presentation → Application ルーティング。
ACTIONはアクション、ARGSは引数。"
  (nskk-architecture--trace-communication 'presentation 'application action args)
  (pcase action
    (:process-key (apply #'nskk-application-process-input args))
    (:commit-candidate (apply #'nskk-application-commit-candidate args))
    (_ (nskk-architecture--log "Unknown action: %s" action))))

(defun nskk-architecture--route-application-to-core (action &rest args)
  "Application → Core ルーティング。
ACTIONはアクション、ARGSは引数。"
  (nskk-architecture--trace-communication 'application 'core action args)
  (pcase action
    (:convert-romaji (apply #'nskk-core-convert-romaji args))
    (:hiragana-to-katakana (apply #'nskk-core-hiragana-to-katakana args))
    (:hankaku-to-zenkaku (apply #'nskk-core-hankaku-to-zenkaku args))
    (_ (nskk-architecture--log "Unknown action: %s" action))))

(defun nskk-architecture--route-application-to-data (action &rest args)
  "Application → Data ルーティング。
ACTIONはアクション、ARGSは引数。"
  (nskk-architecture--trace-communication 'application 'data action args)
  (pcase action
    (:search (apply #'nskk-data-search args))
    (:learn (apply #'nskk-data-learn args))
    (_ (nskk-architecture--log "Unknown action: %s" action))))

(defun nskk-architecture--route-core-to-data (action &rest args)
  "Core → Data ルーティング。
ACTIONはアクション、ARGSは引数。"
  (nskk-architecture--trace-communication 'core 'data action args)
  (pcase action
    (:lookup (apply #'nskk-data-search args))
    (_ (nskk-architecture--log "Unknown action: %s" action))))

(defun nskk-architecture--route-data-to-infrastructure (action &rest args)
  "Data → Infrastructure ルーティング。
ACTIONはアクション、ARGSは引数。"
  (nskk-architecture--trace-communication 'data 'infrastructure action args)
  (pcase action
    (:read-file (apply #'nskk-infrastructure-read-file args))
    (:write-file (apply #'nskk-infrastructure-write-file args))
    (_ (nskk-architecture--log "Unknown action: %s" action))))

;;; イベントフロー

(defun nskk-architecture--setup-event-flow ()
  "イベントフローをセットアップする。"
  (nskk-architecture--log "Setting up event flow...")

  ;; Application Layerのイベントを Presentation Layerで受信
  (nskk-extension-subscribe :conversion-started
                            #'nskk-architecture--on-conversion-started)
  (nskk-extension-subscribe :conversion-committed
                            #'nskk-architecture--on-conversion-committed)
  (nskk-extension-subscribe :mode-changed
                            #'nskk-architecture--on-mode-changed)

  (nskk-architecture--log "Event flow setup complete"))

(defun nskk-architecture--on-conversion-started (&rest data)
  "変換開始イベントハンドラー。
DATAはイベントデータ。"
  (let ((candidates (plist-get data :candidates)))
    (nskk-presentation-show-candidates candidates)))

(defun nskk-architecture--on-conversion-committed (&rest data)
  "変換確定イベントハンドラー。
DATAはイベントデータ。"
  (nskk-presentation-hide-candidates))

(defun nskk-architecture--on-mode-changed (&rest data)
  "モード変更イベントハンドラー。
DATAはイベントデータ。"
  (let ((new-mode (plist-get data :to)))
    (nskk-presentation-update-mode-line new-mode)))

;;; 通信トレース

(defun nskk-architecture--trace-communication (from to action args)
  "レイヤー間通信をトレースする。
FROMは送信元レイヤー、TOは送信先レイヤー、
ACTIONはアクション、ARGSは引数。"
  (when nskk-architecture-enable-tracing
    (let ((trace (list :timestamp (float-time)
                       :from from
                       :to to
                       :action action
                       :args args)))
      (push trace nskk-architecture--communication-log)
      (nskk-architecture--log "Communication: %s -> %s : %s" from to action))))

(defun nskk-architecture-get-communication-log ()
  "通信ログを取得する。"
  (interactive)
  (if nskk-architecture--communication-log
      (with-output-to-temp-buffer "*NSKK Communication Log*"
        (princ "NSKK Layer Communication Log\n")
        (princ "============================\n\n")
        (dolist (entry (reverse nskk-architecture--communication-log))
          (princ (format "[%.3f] %s -> %s : %s\n"
                         (plist-get entry :timestamp)
                         (plist-get entry :from)
                         (plist-get entry :to)
                         (plist-get entry :action)))))
    (message "Communication log is empty")))

(defun nskk-architecture-clear-communication-log ()
  "通信ログをクリアする。"
  (interactive)
  (setq nskk-architecture--communication-log nil)
  (message "Communication log cleared"))

;;; ヘルスチェック

(defun nskk-architecture-health-check ()
  "7層アーキテクチャのヘルスチェックを実行する。"
  (interactive)
  (nskk-architecture--log "Running architecture health check...")

  (let ((issues '()))
    ;; 初期化状態チェック
    (unless nskk-architecture--initialized
      (push "Architecture not initialized" issues))

    ;; 各レイヤーの状態チェック
    (dolist (layer nskk-architecture--layers)
      (let ((status (gethash layer nskk-architecture--layer-status)))
        (unless (eq status :initialized)
          (push (format "Layer %s not initialized (status: %s)" layer status)
                issues))))

    ;; 結果表示
    (if issues
        (message "Architecture issues:\n%s" (string-join issues "\n"))
      (message "Architecture: All systems operational"))

    (not issues)))

(defun nskk-architecture-get-layer-status (layer)
  "レイヤーの状態を取得する。
LAYERはレイヤー名。"
  (gethash layer nskk-architecture--layer-status))

;;; アーキテクチャ図生成

(defun nskk-architecture-show-diagram ()
  "アーキテクチャ図を表示する。"
  (interactive)
  (with-output-to-temp-buffer "*NSKK Architecture*"
    (princ "NSKK 7-Layer Architecture\n")
    (princ "=========================\n\n")
    (princ "┌─────────────────────────────────────┐\n")
    (princ "│   Layer 1: Presentation Layer       │\n")
    (princ "│   - UI統合、イベントハンドリング     │\n")
    (princ "├─────────────────────────────────────┤\n")
    (princ "│   Layer 2: Extension Layer          │\n")
    (princ "│   - フック、イベントバス             │\n")
    (princ "├─────────────────────────────────────┤\n")
    (princ "│   Layer 3: Application Layer        │\n")
    (princ "│   - ビジネスロジック、変換制御       │\n")
    (princ "├─────────────────────────────────────┤\n")
    (princ "│   Layer 4: Core Engine Layer        │\n")
    (princ "│   - 変換エンジン、辞書エンジン       │\n")
    (princ "├─────────────────────────────────────┤\n")
    (princ "│   Layer 5: Data Access Layer        │\n")
    (princ "│   - 永続化、同期                     │\n")
    (princ "├─────────────────────────────────────┤\n")
    (princ "│   Layer 6: Infrastructure Layer     │\n")
    (princ "│   - スレッド管理、メモリ管理         │\n")
    (princ "├─────────────────────────────────────┤\n")
    (princ "│   Layer 7: QA Layer                 │\n")
    (princ "│   - テスト、ベンチマーク統合         │\n")
    (princ "└─────────────────────────────────────┘\n\n")
    (princ "Layer Status:\n")
    (princ "-------------\n")
    (dolist (layer nskk-architecture--layers)
      (let ((status (gethash layer nskk-architecture--layer-status)))
        (princ (format "[%s] %s: %s\n"
                       (if (eq status :initialized) "✓" "✗")
                       layer
                       (or status "not initialized")))))))

;;; 統計情報

(defun nskk-architecture-get-statistics ()
  "アーキテクチャ全体の統計情報を取得する。"
  (interactive)
  (let ((stats (make-hash-table :test 'eq)))
    (puthash 'infrastructure (nskk-infrastructure-get-statistics) stats)
    (puthash 'data (nskk-data-get-statistics) stats)
    (puthash 'core (nskk-core-get-statistics) stats)
    (puthash 'application (nskk-application-get-statistics) stats)
    (puthash 'extension (nskk-extension-get-statistics) stats)
    (puthash 'qa (nskk-qa-get-metrics) stats)
    stats))

;;; デバッグ・ロギング

(defvar nskk-architecture--debug-enabled nil
  "デバッグモードが有効かどうか。")

(defun nskk-architecture-enable-debug ()
  "デバッグモードを有効にする。"
  (interactive)
  (setq nskk-architecture--debug-enabled t)
  ;; 各レイヤーのデバッグも有効化
  (nskk-infrastructure-enable-debug)
  (nskk-data-enable-debug)
  (nskk-core-enable-debug)
  (nskk-application-enable-debug)
  (nskk-extension-enable-debug)
  (nskk-presentation-enable-debug)
  (nskk-qa-enable-debug)
  (message "NSKK Architecture: Debug mode enabled for all layers"))

(defun nskk-architecture-disable-debug ()
  "デバッグモードを無効にする。"
  (interactive)
  (setq nskk-architecture--debug-enabled nil)
  ;; 各レイヤーのデバッグも無効化
  (nskk-infrastructure-disable-debug)
  (nskk-data-disable-debug)
  (nskk-core-disable-debug)
  (nskk-application-disable-debug)
  (nskk-extension-disable-debug)
  (nskk-presentation-disable-debug)
  (nskk-qa-disable-debug)
  (message "NSKK Architecture: Debug mode disabled for all layers"))

(defun nskk-architecture--log (format-string &rest args)
  "デバッグログを出力する。
FORMAT-STRINGはフォーマット文字列、ARGSは引数。"
  (when nskk-architecture--debug-enabled
    (apply #'message (concat "[NSKK-Architecture] " format-string) args)))

(provide 'nskk-architecture)
;;; nskk-architecture.el ends here
