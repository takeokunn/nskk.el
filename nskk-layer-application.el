;;; nskk-layer-application.el --- Application Layer for NSKK -*- lexical-binding: t; -*-

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

;; Application Layer - ビジネスロジックと変換制御
;;
;; 責務:
;; - 日本語入力のビジネスロジック
;; - 変換プロセス制御
;; - 候補選択ロジック
;; - 学習機能統合
;; - ユーザーインタラクションフロー
;;
;; レイヤー依存:
;; - Core Engine Layer (変換エンジン利用)
;; - Data Access Layer (辞書アクセス)
;; - Extension Layer (イベント発行)
;;
;; 主要コンポーネント:
;; - 変換コントローラー
;; - 候補管理
;; - モード管理
;; - 学習統合
;; - セッション管理
;;
;; 使用例:
;; (nskk-application-process-input "ka")
;; (nskk-application-start-conversion "kanji")
;; (nskk-application-select-candidate 0)
;; (nskk-application-switch-mode 'katakana)

;;; Code:

(require 'cl-lib)

;;; カスタマイズ可能変数

(defgroup nskk-application nil
  "Application layer settings for NSKK."
  :group 'nskk
  :prefix "nskk-application-")

(defcustom nskk-application-default-mode 'hiragana
  "デフォルトの入力モード。"
  :type '(choice (const :tag "ひらがな" hiragana)
                 (const :tag "カタカナ" katakana)
                 (const :tag "英数" latin)
                 (const :tag "全角英数" zenkaku-latin))
  :group 'nskk-application)

(defcustom nskk-application-enable-learning t
  "学習機能を有効にするか。"
  :type 'boolean
  :group 'nskk-application)

(defcustom nskk-application-candidate-limit 20
  "表示する候補の最大数。"
  :type 'integer
  :group 'nskk-application)

;;; 内部変数

(defvar nskk-application--current-mode nil
  "現在の入力モード。")

(defvar nskk-application--input-buffer ""
  "入力バッファ。")

(defvar nskk-application--conversion-state nil
  "変換状態。")

(defvar nskk-application--candidate-list nil
  "候補リスト。")

(defvar nskk-application--candidate-index 0
  "現在選択中の候補インデックス。")

(defvar nskk-application--session-id nil
  "現在のセッションID。")

;;; 変換状態構造

(cl-defstruct nskk-application-conversion-state
  "変換状態を表す構造体。"
  (query nil :documentation "変換クエリ")
  (candidates nil :documentation "候補リスト")
  (index 0 :documentation "選択中のインデックス")
  (context nil :documentation "変換コンテキスト")
  (timestamp nil :documentation "開始タイムスタンプ"))

;;; セッション管理

(defun nskk-application-initialize ()
  "Application Layerを初期化する。"
  (setq nskk-application--current-mode nskk-application-default-mode)
  (setq nskk-application--input-buffer "")
  (setq nskk-application--conversion-state nil)
  (setq nskk-application--candidate-list nil)
  (setq nskk-application--candidate-index 0)
  (setq nskk-application--session-id (nskk-application--generate-session-id))
  (nskk-application--log "Application Layer initialized with session %s"
                         nskk-application--session-id))

(defun nskk-application-shutdown ()
  "Application Layerをシャットダウンする。"
  (nskk-application--cleanup-session)
  (setq nskk-application--current-mode nil)
  (setq nskk-application--session-id nil)
  (nskk-application--log "Application Layer shutdown"))

(defun nskk-application--generate-session-id ()
  "新しいセッションIDを生成する。"
  (format "nskk-session-%s" (format-time-string "%Y%m%d-%H%M%S")))

(defun nskk-application--cleanup-session ()
  "現在のセッションをクリーンアップする。"
  (setq nskk-application--input-buffer "")
  (setq nskk-application--conversion-state nil)
  (setq nskk-application--candidate-list nil))

;;; 入力処理

(defun nskk-application-process-input (input)
  "入力を処理する。
INPUTは入力文字またはキーイベント。"
  ;; 拡張ポイント: 入力前処理
  (nskk-application--run-extension-point :before-input-processing input)

  ;; 現在のモードに応じて処理
  (let ((result
         (pcase nskk-application--current-mode
           ('hiragana (nskk-application--process-hiragana-input input))
           ('katakana (nskk-application--process-katakana-input input))
           ('latin (nskk-application--process-latin-input input))
           ('zenkaku-latin (nskk-application--process-zenkaku-latin-input input))
           (_ (nskk-application--process-default-input input)))))

    ;; 拡張ポイント: 入力後処理
    (nskk-application--run-extension-point :after-input-processing result)
    result))

(defun nskk-application--process-hiragana-input (input)
  "ひらがなモードでの入力を処理する。
INPUTは入力文字。"
  ;; Core Layerの変換エンジンを呼び出す
  (nskk-application--delegate-to-core :convert-romaji input))

(defun nskk-application--process-katakana-input (input)
  "カタカナモードでの入力を処理する。
INPUTは入力文字。"
  ;; Core Layerの変換エンジンを呼び出し、ひらがな→カタカナ変換
  (let ((hiragana (nskk-application--delegate-to-core :convert-romaji input)))
    (nskk-application--hiragana-to-katakana hiragana)))

(defun nskk-application--process-latin-input (input)
  "英数モードでの入力を処理する。
INPUTは入力文字。"
  input)

(defun nskk-application--process-zenkaku-latin-input (input)
  "全角英数モードでの入力を処理する。
INPUTは入力文字。"
  (nskk-application--hankaku-to-zenkaku input))

(defun nskk-application--process-default-input (input)
  "デフォルトの入力処理。
INPUTは入力文字。"
  input)

;;; 変換処理

(defun nskk-application-start-conversion (query)
  "変換を開始する。
QUERYは変換クエリ文字列。"
  ;; 拡張ポイント: 変換前処理
  (nskk-application--run-extension-point :before-conversion query)

  ;; 変換状態を作成
  (setq nskk-application--conversion-state
        (make-nskk-application-conversion-state
         :query query
         :candidates nil
         :index 0
         :context (nskk-application--create-context)
         :timestamp (float-time)))

  ;; Data Layerから候補を取得
  (let ((candidates (nskk-application--fetch-candidates query)))
    (setf (nskk-application-conversion-state-candidates
           nskk-application--conversion-state)
          candidates)
    (setq nskk-application--candidate-list candidates)
    (setq nskk-application--candidate-index 0)

    ;; 拡張ポイント: 変換後処理
    (nskk-application--run-extension-point :after-conversion candidates)

    ;; イベント発行
    (nskk-application--emit-event :conversion-started
                                  :query query
                                  :candidates candidates)
    candidates))

(defun nskk-application--fetch-candidates (query)
  "候補を取得する。
QUERYは検索クエリ。"
  ;; Data Layerに問い合わせ
  (let ((results (nskk-application--delegate-to-data :search query)))
    ;; 候補数を制限
    (seq-take results nskk-application-candidate-limit)))

(defun nskk-application-select-candidate (index)
  "候補を選択する。
INDEXは候補のインデックス。"
  (when (and nskk-application--candidate-list
             (< index (length nskk-application--candidate-list)))
    (let ((candidate (nth index nskk-application--candidate-list)))
      ;; 拡張ポイント: 候補選択前処理
      (nskk-application--run-extension-point :before-candidate-selection candidate)

      ;; 候補を確定
      (nskk-application-commit-candidate candidate)

      ;; 学習
      (when nskk-application-enable-learning
        (nskk-application--learn-selection
         (nskk-application-conversion-state-query nskk-application--conversion-state)
         candidate
         index))

      ;; 拡張ポイント: 候補選択後処理
      (nskk-application--run-extension-point :after-candidate-selection candidate)

      ;; イベント発行
      (nskk-application--emit-event :candidate-selected
                                    :candidate candidate
                                    :index index)
      candidate)))

(defun nskk-application-commit-candidate (candidate)
  "候補を確定する。
CANDIDATEは確定する候補。"
  ;; 変換状態をクリア
  (setq nskk-application--conversion-state nil)
  (setq nskk-application--candidate-list nil)
  (setq nskk-application--candidate-index 0)

  ;; イベント発行
  (nskk-application--emit-event :conversion-committed :candidate candidate)

  candidate)

(defun nskk-application-next-candidate ()
  "次の候補を選択する。"
  (interactive)
  (when nskk-application--candidate-list
    (setq nskk-application--candidate-index
          (mod (1+ nskk-application--candidate-index)
               (length nskk-application--candidate-list)))
    (nskk-application--emit-event :candidate-changed
                                  :index nskk-application--candidate-index)))

(defun nskk-application-previous-candidate ()
  "前の候補を選択する。"
  (interactive)
  (when nskk-application--candidate-list
    (setq nskk-application--candidate-index
          (mod (1- nskk-application--candidate-index)
               (length nskk-application--candidate-list)))
    (nskk-application--emit-event :candidate-changed
                                  :index nskk-application--candidate-index)))

;;; モード管理

(defun nskk-application-switch-mode (new-mode)
  "入力モードを切り替える。
NEW-MODEは新しいモード。"
  (let ((old-mode nskk-application--current-mode))
    ;; 拡張ポイント: モード変更前処理
    (when (nskk-application--run-extension-point :before-mode-change
                                                  old-mode new-mode)
      (setq nskk-application--current-mode new-mode)

      ;; 拡張ポイント: モード変更後処理
      (nskk-application--run-extension-point :after-mode-change old-mode new-mode)

      ;; イベント発行
      (nskk-application--emit-event :mode-changed
                                    :from old-mode
                                    :to new-mode)

      new-mode)))

(defun nskk-application-current-mode ()
  "現在の入力モードを返す。"
  nskk-application--current-mode)

(defun nskk-application-toggle-kana ()
  "ひらがな/カタカナを切り替える。"
  (interactive)
  (pcase nskk-application--current-mode
    ('hiragana (nskk-application-switch-mode 'katakana))
    ('katakana (nskk-application-switch-mode 'hiragana))
    (_ (nskk-application-switch-mode 'hiragana))))

;;; 学習機能

(defun nskk-application--learn-selection (query candidate index)
  "候補選択を学習する。
QUERYは検索クエリ、CANDIDATEは選択された候補、INDEXは候補インデックス。"
  ;; 拡張ポイント: 学習前処理
  (nskk-application--run-extension-point :before-learning
                                         query candidate index)

  ;; Data Layerに学習データを保存
  (nskk-application--delegate-to-data :learn
                                      :query query
                                      :candidate candidate
                                      :index index
                                      :timestamp (float-time))

  ;; 拡張ポイント: 学習後処理
  (nskk-application--run-extension-point :after-learning
                                         query candidate index))

;;; コンテキスト管理

(defun nskk-application--create-context ()
  "変換コンテキストを作成する。"
  (list :buffer (current-buffer)
        :point (point)
        :mode nskk-application--current-mode
        :session-id nskk-application--session-id))

;;; 文字変換ユーティリティ

(defun nskk-application--hiragana-to-katakana (hiragana)
  "ひらがなをカタカナに変換する。
HIRAGANAはひらがな文字列。"
  ;; Core Layerに委譲
  (nskk-application--delegate-to-core :hiragana-to-katakana hiragana))

(defun nskk-application--hankaku-to-zenkaku (hankaku)
  "半角文字を全角文字に変換する。
HANKAKUは半角文字列。"
  ;; Core Layerに委譲
  (nskk-application--delegate-to-core :hankaku-to-zenkaku hankaku))

;;; レイヤー間通信

(defun nskk-application--delegate-to-core (action &rest args)
  "Core Engine Layerに処理を委譲する。
ACTIONは実行するアクション、ARGSは引数。"
  ;; Extension Layer経由でCore Layerに委譲
  ;; 実装は統合時に完成
  (nskk-application--log "Delegating to core: %s with %S" action args)
  nil)

(defun nskk-application--delegate-to-data (action &rest args)
  "Data Access Layerに処理を委譲する。
ACTIONは実行するアクション、ARGSは引数。"
  ;; Extension Layer経由でData Layerに委譲
  ;; 実装は統合時に完成
  (nskk-application--log "Delegating to data: %s with %S" action args)
  nil)

(defun nskk-application--emit-event (event-type &rest data)
  "イベントを発行する。
EVENT-TYPEはイベントタイプ、DATAはイベントデータ。"
  ;; Extension Layerのイベントバスに発行
  ;; 実装は統合時に完成
  (nskk-application--log "Emitting event: %s with %S" event-type data))

(defun nskk-application--run-extension-point (point-name &rest args)
  "拡張ポイントを実行する。
POINT-NAMEは拡張ポイント名、ARGSは引数。"
  ;; Extension Layerのフックを実行
  ;; 実装は統合時に完成
  (nskk-application--log "Running extension point: %s with %S" point-name args)
  t)

;;; デバッグ・ロギング

(defvar nskk-application--debug-enabled nil
  "デバッグモードが有効かどうか。")

(defun nskk-application-enable-debug ()
  "デバッグモードを有効にする。"
  (interactive)
  (setq nskk-application--debug-enabled t)
  (message "NSKK Application Layer: Debug mode enabled"))

(defun nskk-application-disable-debug ()
  "デバッグモードを無効にする。"
  (interactive)
  (setq nskk-application--debug-enabled nil)
  (message "NSKK Application Layer: Debug mode disabled"))

(defun nskk-application--log (format-string &rest args)
  "デバッグログを出力する。
FORMAT-STRINGはフォーマット文字列、ARGSは引数。"
  (when nskk-application--debug-enabled
    (apply #'message (concat "[NSKK-Application] " format-string) args)))

;;; ヘルスチェック

(defun nskk-application-health-check ()
  "Application Layerのヘルスチェックを実行する。"
  (interactive)
  (let ((issues '()))
    ;; セッション状態チェック
    (unless nskk-application--session-id
      (push "No active session" issues))
    ;; モード状態チェック
    (unless nskk-application--current-mode
      (push "No current mode set" issues))
    ;; 結果表示
    (if issues
        (message "Application Layer issues: %s" (string-join issues ", "))
      (message "Application Layer: All systems operational"))))

;;; 統計情報

(defun nskk-application-get-statistics ()
  "Application Layerの統計情報を取得する。"
  (interactive)
  (list :session-id nskk-application--session-id
        :current-mode nskk-application--current-mode
        :input-buffer-size (length nskk-application--input-buffer)
        :has-conversion-state (not (null nskk-application--conversion-state))
        :candidate-count (length nskk-application--candidate-list)))

(provide 'nskk-layer-application)
;;; nskk-layer-application.el ends here
