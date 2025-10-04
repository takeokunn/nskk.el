;;; nskk-input-hybrid.el --- Hybrid input method for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, hybrid
;; Version: 0.1.0
;; Package-Requires: ((emacs "31.0"))

;; This file is part of NSKK.

;;; Commentary:

;; ハイブリッド入力方式の実装。
;;
;; 複数の入力方式を組み合わせて使用できるハイブリッドシステム。
;; 状況に応じて最適な入力方式を自動選択、または手動で切り替え可能。
;;
;; 主な機能:
;; - 複数入力方式の同時使用
;; - 優先順位による自動選択
;; - 動的な入力方式切り替え
;; - コンテキストベースの入力方式選択
;;
;; 使用例:
;; (require 'nskk-input-hybrid)
;;
;; ;; ハイブリッド入力方式の設定
;; (nskk-input-hybrid-configure
;;  :methods '(azik qwerty)
;;  :priority-order '(azik qwerty)
;;  :fallback 'qwerty)
;;
;; ;; ハイブリッド入力方式の使用
;; (nskk-register-input-method 'hybrid)

;;; Code:

(require 'cl-lib)
(require 'nskk-input-azik)
(require 'nskk-input-act)
(require 'nskk-input-qwerty)

;;; 設定変数

(defvar nskk-input-hybrid-methods '(azik qwerty)
  "ハイブリッド入力方式で使用する入力方式のリスト。
優先順位順に並べる。")

(defvar nskk-input-hybrid-fallback 'qwerty
  "変換に失敗した場合のフォールバック入力方式。")

(defvar nskk-input-hybrid-current-method nil
  "現在アクティブな入力方式。
nilの場合は自動選択モード。")

(defvar nskk-input-hybrid-auto-select t
  "自動選択モードを有効にするかどうか。")

;;; ハイブリッド入力方式の設定

(defun nskk-input-hybrid-configure (&rest options)
  "ハイブリッド入力方式を設定する。

OPTIONS はキーワード引数:
  :methods LIST          - 使用する入力方式のリスト
  :priority-order LIST   - 優先順位順のリスト
  :fallback SYMBOL       - フォールバック入力方式
  :auto-select BOOL      - 自動選択モードの有効化

使用例:
  (nskk-input-hybrid-configure
   :methods '(azik act qwerty)
   :priority-order '(azik act qwerty)
   :fallback 'qwerty
   :auto-select t)"
  (when-let ((methods (plist-get options :methods)))
    (setq nskk-input-hybrid-methods methods))

  (when-let ((fallback (plist-get options :fallback)))
    (setq nskk-input-hybrid-fallback fallback))

  (when (plist-member options :auto-select)
    (setq nskk-input-hybrid-auto-select (plist-get options :auto-select))))

;;; 入力方式の検索

(defun nskk-input-hybrid--get-lookup-function (method-name)
  "METHOD-NAMEに対応する検索関数を返す。"
  (cond
   ((eq method-name 'azik)
    #'nskk-input-azik-lookup)
   ((eq method-name 'act)
    #'nskk-input-act-lookup)
   ((eq method-name 'tutcode)
    (require 'nskk-input-tutcode)
    #'nskk-input-tutcode-lookup)
   ((eq method-name 'qwerty)
    #'nskk-input-qwerty-lookup)
   ((eq method-name 'nicola)
    (require 'nskk-input-nicola)
    #'nskk-input-nicola-lookup)
   ((eq method-name 'kana)
    (require 'nskk-input-kana)
    #'nskk-input-kana-lookup)
   ((eq method-name 'dvorak)
    (require 'nskk-input-dvorak)
    #'nskk-input-dvorak-lookup)
   ((eq method-name 'colemak)
    (require 'nskk-input-colemak)
    #'nskk-input-colemak-lookup)
   (t
    (warn "Unknown input method: %s" method-name)
    nil)))

;;; 検索関数

(defun nskk-input-hybrid-lookup (key)
  "ハイブリッド入力方式でKEYを変換する。

自動選択モードの場合:
  優先順位に従って各入力方式を試し、最初に成功したものを返す。

手動選択モードの場合:
  指定された入力方式のみを使用する。

使用例:
  (nskk-input-hybrid-lookup \"kj\")   ;; AZIKで \"きゃ\"
  (nskk-input-hybrid-lookup \"ka\")   ;; AZIKまたはQWERTYで \"か\""
  (cond
   ;; 手動選択モード
   ((and nskk-input-hybrid-current-method
         (not nskk-input-hybrid-auto-select))
    (when-let ((lookup-fn (nskk-input-hybrid--get-lookup-function
                           nskk-input-hybrid-current-method)))
      (funcall lookup-fn key)))

   ;; 自動選択モード
   (t
    (let ((result nil))
      ;; 優先順位に従って各入力方式を試す
      (cl-dolist (method nskk-input-hybrid-methods)
        (when-let ((lookup-fn (nskk-input-hybrid--get-lookup-function method)))
          (when-let ((converted (funcall lookup-fn key)))
            (setq result converted)
            (cl-return))))

      ;; 結果が見つからない場合、フォールバックを使用
      (unless result
        (when-let ((fallback-fn (nskk-input-hybrid--get-lookup-function
                                 nskk-input-hybrid-fallback)))
          (setq result (funcall fallback-fn key))))

      result))))

(defun nskk-input-hybrid-get-candidates (prefix)
  "PREFIX で始まる全ての候補を取得する。
全ての入力方式から候補を収集し、重複を除去して返す。"
  (let ((candidates nil)
        (seen (make-hash-table :test 'equal)))

    ;; 各入力方式から候補を取得
    (dolist (method nskk-input-hybrid-methods)
      (let ((get-candidates-fn
             (cond
              ((eq method 'azik) #'nskk-input-azik-get-candidates)
              ((eq method 'act) #'nskk-input-act-get-candidates)
              ((eq method 'qwerty) #'nskk-input-qwerty-get-candidates)
              (t nil))))
        (when get-candidates-fn
          (dolist (candidate (funcall get-candidates-fn prefix))
            (unless (gethash candidate seen)
              (puthash candidate t seen)
              (push candidate candidates))))))

    (nreverse candidates)))

;;; 入力方式の切り替え

(defun nskk-input-hybrid-switch-method (method)
  "現在の入力方式をMETHODに切り替える。
nilを指定すると自動選択モードに戻る。"
  (interactive
   (list (intern-soft
          (completing-read
           "Switch to input method (empty for auto): "
           (cons "auto" (mapcar #'symbol-name nskk-input-hybrid-methods))
           nil t))))
  (setq nskk-input-hybrid-current-method
        (if (eq method 'auto) nil method))
  (message "Hybrid input method: %s"
           (or method "auto")))

(defun nskk-input-hybrid-toggle-auto-select ()
  "自動選択モードのオン/オフを切り替える。"
  (interactive)
  (setq nskk-input-hybrid-auto-select
        (not nskk-input-hybrid-auto-select))
  (message "Hybrid auto-select: %s"
           (if nskk-input-hybrid-auto-select "ON" "OFF")))

;;; 入力方式登録

(defun nskk-input-hybrid-register ()
  "ハイブリッド入力方式をNSKKに登録する。"
  ;; 各入力方式を初期化
  (dolist (method nskk-input-hybrid-methods)
    (let ((register-fn
           (cond
            ((eq method 'azik) #'nskk-input-azik-register)
            ((eq method 'act) #'nskk-input-act-register)
            ((eq method 'qwerty) #'nskk-input-qwerty-register)
            ((eq method 'tutcode)
             (require 'nskk-input-tutcode)
             #'nskk-input-tutcode-register)
            ((eq method 'nicola)
             (require 'nskk-input-nicola)
             #'nskk-input-nicola-register)
            ((eq method 'kana)
             (require 'nskk-input-kana)
             #'nskk-input-kana-register)
            ((eq method 'dvorak)
             (require 'nskk-input-dvorak)
             #'nskk-input-dvorak-register)
            ((eq method 'colemak)
             (require 'nskk-input-colemak)
             #'nskk-input-colemak-register)
            (t nil))))
      (when register-fn
        (funcall register-fn)))))

;;; 統計情報

(defun nskk-input-hybrid-stats ()
  "ハイブリッド入力方式の統計情報を返す。"
  (list :methods nskk-input-hybrid-methods
        :current-method nskk-input-hybrid-current-method
        :fallback nskk-input-hybrid-fallback
        :auto-select nskk-input-hybrid-auto-select))

;;; パフォーマンス最適化

(defsubst nskk-input-hybrid-lookup-fast (key)
  "ハイブリッド入力方式でKEYを高速変換する。"
  (nskk-input-hybrid-lookup key))

(provide 'nskk-input-hybrid)

;;; nskk-input-hybrid.el ends here
