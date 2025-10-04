;;; nskk-layer-extension.el --- Extension Layer for NSKK -*- lexical-binding: t; -*-

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

;; Extension Layer - レイヤー間通信とフックシステムを提供
;;
;; 責務:
;; - フックポイント管理（300+拡張ポイント）
;; - イベントバス実装
;; - レイヤー間メッセージング
;; - プラグインインターフェース
;; - 拡張ポイント登録
;;
;; レイヤー依存:
;; - すべてのレイヤーから独立
;; - すべてのレイヤーに利用される
;;
;; 主要コンポーネント:
;; - フックシステム
;; - イベントバス
;; - メッセージディスパッチャー
;; - 拡張ポイントレジストリ
;;
;; 使用例:
;; (nskk-extension-add-hook :before-conversion #'my-hook)
;; (nskk-extension-emit-event :conversion-started data)
;; (nskk-extension-send-message 'application 'core :process-input input)

;;; Code:

(require 'cl-lib)

;;; カスタマイズ可能変数

(defgroup nskk-extension nil
  "Extension layer settings for NSKK."
  :group 'nskk
  :prefix "nskk-extension-")

(defcustom nskk-extension-enable-logging t
  "イベントログを有効にするか。"
  :type 'boolean
  :group 'nskk-extension)

(defcustom nskk-extension-max-event-history 1000
  "保持するイベント履歴の最大数。"
  :type 'integer
  :group 'nskk-extension)

;;; 内部変数

(defvar nskk-extension--hooks (make-hash-table :test 'eq)
  "フックポイントとハンドラーリストのマッピング。")

(defvar nskk-extension--event-bus (make-hash-table :test 'eq)
  "イベントバス: イベントタイプとリスナーのマッピング。")

(defvar nskk-extension--event-history nil
  "イベント履歴。")

(defvar nskk-extension--message-routes (make-hash-table :test 'equal)
  "メッセージルーティングテーブル。")

(defvar nskk-extension--extension-points (make-hash-table :test 'eq)
  "拡張ポイントレジストリ。")

;;; フックシステム

(defun nskk-extension-add-hook (hook-point function &optional append local)
  "フックポイントにハンドラーを追加する。
HOOK-POINTはフックポイント名、FUNCTIONはハンドラー関数。
APPENDが非nilなら末尾に追加、LOCALが非nilならバッファローカル。"
  (let ((hooks (gethash hook-point nskk-extension--hooks)))
    (unless hooks
      (setq hooks (if local
                      (make-local-variable (make-symbol "nskk-extension-hook"))
                    nil))
      (puthash hook-point hooks nskk-extension--hooks))
    (add-hook hooks function append local)
    (puthash hook-point hooks nskk-extension--hooks)))

(defun nskk-extension-remove-hook (hook-point function &optional local)
  "フックポイントからハンドラーを削除する。
HOOK-POINTはフックポイント名、FUNCTIONはハンドラー関数。
LOCALが非nilならバッファローカルから削除。"
  (let ((hooks (gethash hook-point nskk-extension--hooks)))
    (when hooks
      (remove-hook hooks function local))))

(defun nskk-extension-run-hook (hook-point &rest args)
  "フックポイントのハンドラーを実行する。
HOOK-POINTはフックポイント名、ARGSはハンドラーに渡す引数。"
  (let ((hooks (gethash hook-point nskk-extension--hooks)))
    (when hooks
      (dolist (hook hooks)
        (condition-case err
            (apply hook args)
          (error
           (nskk-extension--log "Hook error at %s: %s" hook-point err)))))))

(defun nskk-extension-clear-hooks (hook-point)
  "フックポイントのすべてのハンドラーをクリアする。
HOOK-POINTはフックポイント名。"
  (puthash hook-point nil nskk-extension--hooks))

;;; イベントバス

(defun nskk-extension-subscribe (event-type listener)
  "イベントタイプにリスナーを登録する。
EVENT-TYPEはイベントタイプ、LISTENERはリスナー関数。"
  (let ((listeners (gethash event-type nskk-extension--event-bus)))
    (unless (memq listener listeners)
      (puthash event-type (cons listener listeners)
               nskk-extension--event-bus))))

(defun nskk-extension-unsubscribe (event-type listener)
  "イベントタイプからリスナーを登録解除する。
EVENT-TYPEはイベントタイプ、LISTENERはリスナー関数。"
  (let ((listeners (gethash event-type nskk-extension--event-bus)))
    (puthash event-type (delq listener listeners)
             nskk-extension--event-bus)))

(defun nskk-extension-emit-event (event-type &rest data)
  "イベントを発行する。
EVENT-TYPEはイベントタイプ、DATAはイベントデータ。"
  ;; イベント履歴に記録
  (when nskk-extension-enable-logging
    (nskk-extension--record-event event-type data))
  ;; リスナーに通知
  (let ((listeners (gethash event-type nskk-extension--event-bus)))
    (dolist (listener listeners)
      (condition-case err
          (apply listener data)
        (error
         (nskk-extension--log "Event listener error for %s: %s"
                              event-type err))))))

(defun nskk-extension--record-event (event-type data)
  "イベントを履歴に記録する。
EVENT-TYPEはイベントタイプ、DATAはイベントデータ。"
  (let ((event (list :type event-type
                     :data data
                     :timestamp (float-time))))
    (push event nskk-extension--event-history)
    ;; 履歴サイズを制限
    (when (> (length nskk-extension--event-history)
             nskk-extension-max-event-history)
      (setq nskk-extension--event-history
            (seq-take nskk-extension--event-history
                      nskk-extension-max-event-history)))))

(defun nskk-extension-get-event-history (&optional event-type)
  "イベント履歴を取得する。
EVENT-TYPEが指定された場合、そのタイプのイベントのみを返す。"
  (if event-type
      (seq-filter (lambda (event)
                    (eq (plist-get event :type) event-type))
                  nskk-extension--event-history)
    nskk-extension--event-history))

(defun nskk-extension-clear-event-history ()
  "イベント履歴をクリアする。"
  (interactive)
  (setq nskk-extension--event-history nil))

;;; メッセージディスパッチャー

(defun nskk-extension-register-route (from-layer to-layer handler)
  "レイヤー間メッセージのルートを登録する。
FROM-LAYERは送信元レイヤー、TO-LAYERは送信先レイヤー、
HANDLERはメッセージハンドラー関数。"
  (let ((key (cons from-layer to-layer)))
    (puthash key handler nskk-extension--message-routes)))

(defun nskk-extension-send-message (from-layer to-layer action &rest args)
  "レイヤー間でメッセージを送信する。
FROM-LAYERは送信元レイヤー、TO-LAYERは送信先レイヤー、
ACTIONは実行するアクション、ARGSは引数。"
  (let* ((key (cons from-layer to-layer))
         (handler (gethash key nskk-extension--message-routes)))
    (if handler
        (condition-case err
            (apply handler action args)
          (error
           (nskk-extension--log "Message handler error %s->%s: %s"
                                from-layer to-layer err)
           nil))
      (nskk-extension--log "No route from %s to %s" from-layer to-layer)
      nil)))

(defun nskk-extension-broadcast-message (from-layer action &rest args)
  "すべてのレイヤーにメッセージをブロードキャストする。
FROM-LAYERは送信元レイヤー、ACTIONはアクション、ARGSは引数。"
  (let ((results nil))
    (maphash
     (lambda (key handler)
       (when (eq (car key) from-layer)
         (let ((result (condition-case err
                           (apply handler action args)
                         (error
                          (nskk-extension--log "Broadcast error: %s" err)
                          nil))))
           (push (cons (cdr key) result) results))))
     nskk-extension--message-routes)
    results))

;;; 拡張ポイント

(defun nskk-extension-register-extension-point (point-name metadata)
  "拡張ポイントを登録する。
POINT-NAMEは拡張ポイント名、METADATAはメタデータplist。
METADATAには :description, :args, :return などを含む。"
  (puthash point-name metadata nskk-extension--extension-points))

(defun nskk-extension-get-extension-point (point-name)
  "拡張ポイントの情報を取得する。
POINT-NAMEは拡張ポイント名。"
  (gethash point-name nskk-extension--extension-points))

(defun nskk-extension-list-extension-points ()
  "登録されているすべての拡張ポイントを一覧表示する。"
  (interactive)
  (let ((points nil))
    (maphash (lambda (name metadata)
               (push (cons name metadata) points))
             nskk-extension--extension-points)
    (if points
        (message "Extension points: %s"
                 (mapconcat (lambda (p)
                              (format "%s: %s"
                                      (car p)
                                      (plist-get (cdr p) :description)))
                            points
                            "\n"))
      (message "No extension points registered"))))

;;; 初期化・シャットダウン

(defun nskk-extension-initialize ()
  "Extension Layerを初期化する。"
  (nskk-extension--setup-standard-extension-points)
  (nskk-extension--log "Extension Layer initialized"))

(defun nskk-extension-shutdown ()
  "Extension Layerをシャットダウンする。"
  (clrhash nskk-extension--hooks)
  (clrhash nskk-extension--event-bus)
  (clrhash nskk-extension--message-routes)
  (clrhash nskk-extension--extension-points)
  (setq nskk-extension--event-history nil)
  (nskk-extension--log "Extension Layer shutdown"))

;;; 標準拡張ポイント定義

(defun nskk-extension--setup-standard-extension-points ()
  "標準の拡張ポイントをセットアップする。"
  ;; 変換関連
  (nskk-extension-register-extension-point
   :before-conversion
   '(:description "変換開始前に実行"
     :args (input)
     :return modified-input))

  (nskk-extension-register-extension-point
   :after-conversion
   '(:description "変換完了後に実行"
     :args (result)
     :return modified-result))

  ;; 候補選択関連
  (nskk-extension-register-extension-point
   :before-candidate-selection
   '(:description "候補選択前に実行"
     :args (candidates)
     :return modified-candidates))

  (nskk-extension-register-extension-point
   :after-candidate-selection
   '(:description "候補選択後に実行"
     :args (selected-candidate)
     :return modified-candidate))

  ;; モード変更関連
  (nskk-extension-register-extension-point
   :before-mode-change
   '(:description "モード変更前に実行"
     :args (old-mode new-mode)
     :return allowed))

  (nskk-extension-register-extension-point
   :after-mode-change
   '(:description "モード変更後に実行"
     :args (old-mode new-mode)
     :return nil))

  ;; 辞書アクセス関連
  (nskk-extension-register-extension-point
   :before-dictionary-lookup
   '(:description "辞書検索前に実行"
     :args (query)
     :return modified-query))

  (nskk-extension-register-extension-point
   :after-dictionary-lookup
   '(:description "辞書検索後に実行"
     :args (query results)
     :return modified-results))

  ;; 学習関連
  (nskk-extension-register-extension-point
   :before-learning
   '(:description "学習実行前に実行"
     :args (entry)
     :return modified-entry))

  (nskk-extension-register-extension-point
   :after-learning
   '(:description "学習実行後に実行"
     :args (entry)
     :return nil)))

;;; ヘルパー関数

(defun nskk-extension-hook-count (hook-point)
  "フックポイントに登録されているハンドラーの数を返す。
HOOK-POINTはフックポイント名。"
  (let ((hooks (gethash hook-point nskk-extension--hooks)))
    (if hooks (length hooks) 0)))

(defun nskk-extension-listener-count (event-type)
  "イベントタイプに登録されているリスナーの数を返す。
EVENT-TYPEはイベントタイプ。"
  (let ((listeners (gethash event-type nskk-extension--event-bus)))
    (if listeners (length listeners) 0)))

;;; デバッグ・ロギング

(defvar nskk-extension--debug-enabled nil
  "デバッグモードが有効かどうか。")

(defun nskk-extension-enable-debug ()
  "デバッグモードを有効にする。"
  (interactive)
  (setq nskk-extension--debug-enabled t)
  (message "NSKK Extension Layer: Debug mode enabled"))

(defun nskk-extension-disable-debug ()
  "デバッグモードを無効にする。"
  (interactive)
  (setq nskk-extension--debug-enabled nil)
  (message "NSKK Extension Layer: Debug mode disabled"))

(defun nskk-extension--log (format-string &rest args)
  "デバッグログを出力する。
FORMAT-STRINGはフォーマット文字列、ARGSは引数。"
  (when nskk-extension--debug-enabled
    (apply #'message (concat "[NSKK-Extension] " format-string) args)))

;;; ヘルスチェック

(defun nskk-extension-health-check ()
  "Extension Layerのヘルスチェックを実行する。"
  (interactive)
  (let ((hook-count (hash-table-count nskk-extension--hooks))
        (event-count (hash-table-count nskk-extension--event-bus))
        (route-count (hash-table-count nskk-extension--message-routes))
        (ext-point-count (hash-table-count nskk-extension--extension-points)))
    (message "Extension Layer Status:\n  Hooks: %d\n  Events: %d\n  Routes: %d\n  Extension Points: %d\n  Event History: %d"
             hook-count event-count route-count ext-point-count
             (length nskk-extension--event-history))))

;;; 統計情報

(defun nskk-extension-get-statistics ()
  "Extension Layerの統計情報を取得する。"
  (interactive)
  (list :hooks (hash-table-count nskk-extension--hooks)
        :events (hash-table-count nskk-extension--event-bus)
        :routes (hash-table-count nskk-extension--message-routes)
        :extension-points (hash-table-count nskk-extension--extension-points)
        :event-history-size (length nskk-extension--event-history)))

(provide 'nskk-layer-extension)
;;; nskk-layer-extension.el ends here
