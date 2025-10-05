;;; nskk-input-switcher.el --- Input method switcher for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, switcher
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0"))

;; This file is part of NSKK.

;;; Commentary:

;; 入力方式切り替え機構の実装。
;;
;; 複数の入力方式を動的に切り替えるための機能を提供。
;; 状態保存・復元、UI統合、モードライン表示などを含む。
;;
;; 主な機能:
;; - 入力方式の動的切り替え
;; - 入力方式の状態保存・復元
;; - モードライン表示
;; - キーボードショートカット
;; - 入力方式の自動検出
;;
;; 使用例:
;; (require 'nskk-input-switcher)
;;
;; ;; 入力方式の切り替え
;; (nskk-input-switcher-switch 'azik)
;;
;; ;; インタラクティブな切り替え
;; (nskk-input-switcher-select)

;;; Code:

(require 'cl-lib)

;;; 設定変数

(defvar nskk-input-switcher-current-method 'qwerty
  "現在選択されている入力方式。")

(defvar nskk-input-switcher-available-methods
  '(qwerty azik act tutcode nicola kana dvorak colemak hybrid)
  "利用可能な入力方式のリスト。")

(defvar nskk-input-switcher-method-names
  '((qwerty . "QWERTY")
    (azik . "AZIK")
    (act . "ACT")
    (tutcode . "TUT-code")
    (nicola . "NICOLA")
    (kana . "かな")
    (dvorak . "Dvorak")
    (colemak . "Colemak")
    (hybrid . "Hybrid"))
  "入力方式の表示名の連想リスト。")

(defvar nskk-input-switcher-history nil
  "入力方式切り替えの履歴。")

(defvar nskk-input-switcher-max-history 10
  "保持する履歴の最大数。")

(defvar nskk-input-switcher-state-alist nil
  "各入力方式の状態を保存する連想リスト。")

;;; カスタマイズ可能な変数

(defgroup nskk-input-switcher nil
  "NSKK input method switcher settings."
  :group 'nskk
  :prefix "nskk-input-switcher-")

(defcustom nskk-input-switcher-show-in-modeline t
  "モードラインに現在の入力方式を表示するかどうか。"
  :type 'boolean
  :group 'nskk-input-switcher)

(defcustom nskk-input-switcher-save-state t
  "入力方式切り替え時に状態を保存するかどうか。"
  :type 'boolean
  :group 'nskk-input-switcher)

(defcustom nskk-input-switcher-restore-state t
  "入力方式切り替え時に以前の状態を復元するかどうか。"
  :type 'boolean
  :group 'nskk-input-switcher)

;;; 入力方式の登録と初期化

(defvar nskk-input-switcher-method-registry nil
  "登録された入力方式の情報。
各要素は (METHOD-NAME . PLIST) の形式。
PLISTには以下のキーを含む:
  :lookup-fn     - 検索関数
  :candidates-fn - 候補取得関数
  :register-fn   - 登録関数
  :stats-fn      - 統計情報関数
  :description   - 説明文字列")

(defun nskk-input-switcher-register-method (method-name plist)
  "METHOD-NAMEを入力方式として登録する。
PLISTには関数やメタデータを含む。"
  (setf (alist-get method-name nskk-input-switcher-method-registry) plist))

(defun nskk-input-switcher-get-method-info (method-name)
  "METHOD-NAMEの情報を取得する。"
  (alist-get method-name nskk-input-switcher-method-registry))

;;; 入力方式の切り替え

(defun nskk-input-switcher-switch (method-name)
  "入力方式をMETHOD-NAMEに切り替える。

METHOD-NAMEは利用可能な入力方式のシンボル。
状態の保存と復元を自動的に行う。"
  (unless (memq method-name nskk-input-switcher-available-methods)
    (error "Unknown input method: %s" method-name))

  ;; 現在の状態を保存
  (when (and nskk-input-switcher-save-state
             nskk-input-switcher-current-method)
    (nskk-input-switcher--save-state nskk-input-switcher-current-method))

  ;; 履歴に追加
  (push nskk-input-switcher-current-method nskk-input-switcher-history)
  (when (> (length nskk-input-switcher-history)
           nskk-input-switcher-max-history)
    (setq nskk-input-switcher-history
          (cl-subseq nskk-input-switcher-history 0
                     nskk-input-switcher-max-history)))

  ;; 入力方式を切り替え
  (setq nskk-input-switcher-current-method method-name)

  ;; 新しい入力方式を初期化
  (nskk-input-switcher--init-method method-name)

  ;; 状態を復元
  (when nskk-input-switcher-restore-state
    (nskk-input-switcher--restore-state method-name))

  ;; モードラインを更新
  (when nskk-input-switcher-show-in-modeline
    (nskk-input-switcher--update-modeline))

  ;; メッセージ表示
  (message "Input method: %s"
           (nskk-input-switcher--get-method-name method-name))

  method-name)

(defun nskk-input-switcher-select ()
  "インタラクティブに入力方式を選択する。"
  (interactive)
  (let* ((choices (mapcar (lambda (m)
                            (cons (nskk-input-switcher--get-method-name m) m))
                          nskk-input-switcher-available-methods))
         (choice (completing-read "Select input method: " choices nil t))
         (method (cdr (assoc choice choices))))
    (when method
      (nskk-input-switcher-switch method))))

(defun nskk-input-switcher-cycle ()
  "次の入力方式に循環的に切り替える。"
  (interactive)
  (let* ((current-pos (cl-position nskk-input-switcher-current-method
                                   nskk-input-switcher-available-methods))
         (next-pos (mod (1+ current-pos)
                        (length nskk-input-switcher-available-methods)))
         (next-method (nth next-pos nskk-input-switcher-available-methods)))
    (nskk-input-switcher-switch next-method)))

(defun nskk-input-switcher-previous ()
  "履歴から前の入力方式に戻る。"
  (interactive)
  (if nskk-input-switcher-history
      (let ((prev-method (pop nskk-input-switcher-history)))
        (nskk-input-switcher-switch prev-method))
    (message "No previous input method in history")))

;;; 状態管理

(defun nskk-input-switcher--save-state (method-name)
  "METHOD-NAMEの現在の状態を保存する。"
  (let ((state (nskk-input-switcher--get-current-state method-name)))
    (setf (alist-get method-name nskk-input-switcher-state-alist) state)))

(defun nskk-input-switcher--restore-state (method-name)
  "METHOD-NAMEの以前の状態を復元する。"
  (when-let ((state (alist-get method-name nskk-input-switcher-state-alist)))
    (nskk-input-switcher--apply-state method-name state)))

(defun nskk-input-switcher--get-current-state (_method-name)
  "現在の入力方式の状態を取得する。
将来的には各入力方式固有の状態を保存可能。"
  ;; 現在は基本的な状態のみ
  (list :timestamp (current-time)))

(defun nskk-input-switcher--apply-state (_method-name _state)
  "入力方式に状態を適用する。
将来的には各入力方式固有の状態を復元可能。"
  ;; 現在は何もしない
  nil)

;;; 入力方式の初期化

(defun nskk-input-switcher--init-method (method-name)
  "METHOD-NAMEの入力方式を初期化する。"
  (let ((init-fn (nskk-input-switcher--get-init-function method-name)))
    (when (functionp init-fn)
      (funcall init-fn))))

(defun nskk-input-switcher--get-init-function (method-name)
  "METHOD-NAMEの初期化関数を取得する。"
  (cond
   ((eq method-name 'qwerty)
    (require 'nskk-input-qwerty)
    #'nskk-input-qwerty-register)
   ((eq method-name 'azik)
    (require 'nskk-input-azik)
    #'nskk-input-azik-register)
   ((eq method-name 'act)
    (require 'nskk-input-act)
    #'nskk-input-act-register)
   ((eq method-name 'tutcode)
    (require 'nskk-input-tutcode)
    #'nskk-input-tutcode-register)
   ((eq method-name 'nicola)
    (require 'nskk-input-nicola)
    #'nskk-input-nicola-register)
   ((eq method-name 'kana)
    (require 'nskk-input-kana)
    #'nskk-input-kana-register)
   ((eq method-name 'dvorak)
    (require 'nskk-input-dvorak)
    #'nskk-input-dvorak-register)
   ((eq method-name 'colemak)
    (require 'nskk-input-colemak)
    #'nskk-input-colemak-register)
   ((eq method-name 'hybrid)
    (require 'nskk-input-hybrid)
    #'nskk-input-hybrid-register)
   (t nil)))

;;; ユーティリティ関数

(defun nskk-input-switcher--get-method-name (method-symbol)
  "METHOD-SYMBOLの表示名を取得する。"
  (or (cdr (assq method-symbol nskk-input-switcher-method-names))
      (symbol-name method-symbol)))

(defun nskk-input-switcher-current-method ()
  "現在の入力方式を返す。"
  nskk-input-switcher-current-method)

(defun nskk-input-switcher-available-methods ()
  "利用可能な入力方式のリストを返す。"
  nskk-input-switcher-available-methods)

;;; モードライン表示

(defvar nskk-input-switcher-modeline-string ""
  "モードラインに表示する文字列。")

(defun nskk-input-switcher--update-modeline ()
  "モードライン表示を更新する。"
  (setq nskk-input-switcher-modeline-string
        (format " [%s]"
                (nskk-input-switcher--get-method-name
                 nskk-input-switcher-current-method)))
  (force-mode-line-update t))

;; モードラインに追加
(when nskk-input-switcher-show-in-modeline
  (unless (memq 'nskk-input-switcher-modeline-string mode-line-format)
    (setq-default mode-line-format
                  (append mode-line-format
                          '(nskk-input-switcher-modeline-string)))))

;;; 統計情報

(defun nskk-input-switcher-stats ()
  "入力方式切り替え機構の統計情報を返す。"
  (list :current-method nskk-input-switcher-current-method
        :available-methods (length nskk-input-switcher-available-methods)
        :history-size (length nskk-input-switcher-history)
        :saved-states (length nskk-input-switcher-state-alist)))

(defun nskk-input-switcher-describe ()
  "現在の入力方式切り替え機構の状態を表示する。"
  (interactive)
  (let ((stats (nskk-input-switcher-stats)))
    (message "Current: %s, Available: %d, History: %d, Saved states: %d"
             (nskk-input-switcher--get-method-name
              (plist-get stats :current-method))
             (plist-get stats :available-methods)
             (plist-get stats :history-size)
             (plist-get stats :saved-states))))

;;; 初期化

(defun nskk-input-switcher-init ()
  "入力方式切り替え機構を初期化する。"
  (interactive)
  ;; デフォルトの入力方式を初期化
  (nskk-input-switcher-switch nskk-input-switcher-current-method))

(provide 'nskk-input-switcher)

;;; nskk-input-switcher.el ends here
