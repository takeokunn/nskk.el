;;; nskk-input-loader.el --- Dynamic loader for input methods in NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, loader
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0"))

;; This file is part of NSKK.

;;; Commentary:

;; 入力方式の動的ロード機構の実装。
;;
;; 入力方式を必要に応じて遅延ロードし、起動時間とメモリ使用量を
;; 最適化する。autoload設定とパフォーマンス最適化を提供。
;;
;; 主な機能:
;; - 遅延ロード（Lazy Loading）
;; - autoload設定の自動生成
;; - ロードキャッシュ
;; - 依存関係の自動解決
;; - パフォーマンス測定
;;
;; 使用例:
;; (require 'nskk-input-loader)
;;
;; ;; 自動ロード設定
;; (nskk-input-loader-setup-autoloads)
;;
;; ;; 入力方式のロード
;; (nskk-input-loader-load 'azik)

;;; Code:

(require 'cl-lib)

;;; 設定変数

(defvar nskk-input-loader-loaded-methods nil
  "既にロード済みの入力方式のリスト。")

(defvar nskk-input-loader-load-times nil
  "各入力方式のロード時間を記録する連想リスト。")

(defvar nskk-input-loader-autoload-alist
  '((azik . nskk-input-azik)
    (act . nskk-input-act)
    (tutcode . nskk-input-tutcode)
    (nicola . nskk-input-nicola)
    (kana . nskk-input-kana)
    (qwerty . nskk-input-qwerty)
    (dvorak . nskk-input-dvorak)
    (colemak . nskk-input-colemak)
    (custom . nskk-input-custom)
    (hybrid . nskk-input-hybrid))
  "入力方式名とライブラリ名の対応表。")

(defvar nskk-input-loader-dependencies
  '((hybrid . (azik act qwerty))
    (custom . (azik)))
  "入力方式の依存関係。")

;;; カスタマイズ可能な変数

(defgroup nskk-input-loader nil
  "NSKK input method loader settings."
  :group 'nskk
  :prefix "nskk-input-loader-")

(defcustom nskk-input-loader-lazy-load t
  "遅延ロードを有効にするかどうか。"
  :type 'boolean
  :group 'nskk-input-loader)

(defcustom nskk-input-loader-measure-time t
  "ロード時間を測定するかどうか。"
  :type 'boolean
  :group 'nskk-input-loader)

(defcustom nskk-input-loader-cache-enabled t
  "ロードキャッシュを有効にするかどうか。"
  :type 'boolean
  :group 'nskk-input-loader)

(defcustom nskk-input-loader-preload-methods nil
  "起動時にプリロードする入力方式のリスト。"
  :type '(repeat symbol)
  :group 'nskk-input-loader)

;;; 入力方式のロード

(defun nskk-input-loader-load (method-name &optional force)
  "METHOD-NAMEの入力方式をロードする。
FORCEが非nilの場合、既にロード済みでも再ロードする。"
  (when (or force
            (not (nskk-input-loader-loaded-p method-name)))
    (let ((start-time (when nskk-input-loader-measure-time
                        (current-time))))

      ;; 依存関係を先にロード
      (nskk-input-loader--load-dependencies method-name)

      ;; ライブラリをロード
      (let ((lib-name (nskk-input-loader--get-library-name method-name)))
        (when lib-name
          (require lib-name)

          ;; 登録関数を呼び出し
          (when-let ((register-fn (nskk-input-loader--get-register-function method-name)))
            (funcall register-fn))

          ;; ロード済みとしてマーク
          (cl-pushnew method-name nskk-input-loader-loaded-methods)

          ;; ロード時間を記録
          (when nskk-input-loader-measure-time
            (let ((elapsed-time (float-time (time-subtract (current-time) start-time))))
              (setf (alist-get method-name nskk-input-loader-load-times) elapsed-time)
              (message "Loaded %s in %.3f ms" method-name (* elapsed-time 1000))))

          t)))))

(defun nskk-input-loader-loaded-p (method-name)
  "METHOD-NAMEが既にロード済みかどうかを返す。"
  (memq method-name nskk-input-loader-loaded-methods))

(defun nskk-input-loader-unload (method-name)
  "METHOD-NAMEの入力方式をアンロードする。"
  (when (nskk-input-loader-loaded-p method-name)
    (let ((lib-name (nskk-input-loader--get-library-name method-name)))
      (when lib-name
        (unload-feature lib-name t)
        (setq nskk-input-loader-loaded-methods
              (delq method-name nskk-input-loader-loaded-methods))
        (message "Unloaded %s" method-name)))))

;;; 依存関係の解決

(defun nskk-input-loader--load-dependencies (method-name)
  "METHOD-NAMEの依存関係をロードする。"
  (when-let ((deps (alist-get method-name nskk-input-loader-dependencies)))
    (dolist (dep deps)
      (unless (nskk-input-loader-loaded-p dep)
        (nskk-input-loader-load dep)))))

;;; ヘルパー関数

(defun nskk-input-loader--get-library-name (method-name)
  "METHOD-NAMEに対応するライブラリ名を取得する。"
  (alist-get method-name nskk-input-loader-autoload-alist))

(defun nskk-input-loader--get-register-function (method-name)
  "METHOD-NAMEの登録関数を取得する。"
  (let ((fn-name (intern (format "nskk-input-%s-register" method-name))))
    (when (fboundp fn-name)
      fn-name)))

;;; autoload設定

;;;###autoload
(defun nskk-input-loader-setup-autoloads ()
  "全ての入力方式のautoload設定を行う。"
  (interactive)
  (dolist (entry nskk-input-loader-autoload-alist)
    (let* ((method-name (car entry))
           (lib-name (cdr entry))
           (lookup-fn (intern (format "nskk-input-%s-lookup" method-name)))
           (register-fn (intern (format "nskk-input-%s-register" method-name))))

      ;; 検索関数のautoload
      (autoload lookup-fn (symbol-name lib-name)
        (format "Lookup function for %s input method." method-name)
        nil nil)

      ;; 登録関数のautoload
      (autoload register-fn (symbol-name lib-name)
        (format "Register function for %s input method." method-name)
        t nil))))

;;; プリロード

(defun nskk-input-loader-preload-methods ()
  "設定されたプリロード対象の入力方式をロードする。"
  (interactive)
  (dolist (method nskk-input-loader-preload-methods)
    (nskk-input-loader-load method)))

;;; 一括操作

(defun nskk-input-loader-load-all ()
  "全ての入力方式をロードする。"
  (interactive)
  (dolist (entry nskk-input-loader-autoload-alist)
    (nskk-input-loader-load (car entry))))

(defun nskk-input-loader-unload-all ()
  "全ての入力方式をアンロードする。"
  (interactive)
  (dolist (method (copy-sequence nskk-input-loader-loaded-methods))
    (nskk-input-loader-unload method)))

;;; 統計情報

(defun nskk-input-loader-stats ()
  "ロード機構の統計情報を返す。"
  (list :loaded-methods (length nskk-input-loader-loaded-methods)
        :available-methods (length nskk-input-loader-autoload-alist)
        :total-load-time (cl-reduce #'+ (mapcar #'cdr nskk-input-loader-load-times)
                                     :initial-value 0.0)
        :load-times nskk-input-loader-load-times))

(defun nskk-input-loader-describe ()
  "ロード機構の状態を表示する。"
  (interactive)
  (let ((stats (nskk-input-loader-stats)))
    (with-output-to-temp-buffer "*NSKK Input Loader*"
      (princ "NSKK Input Method Loader Statistics\n")
      (princ "====================================\n\n")
      (princ (format "Loaded methods: %d / %d\n"
                     (plist-get stats :loaded-methods)
                     (plist-get stats :available-methods)))
      (princ (format "Total load time: %.3f ms\n\n"
                     (* (plist-get stats :total-load-time) 1000)))

      (princ "Loaded methods:\n")
      (dolist (method nskk-input-loader-loaded-methods)
        (let ((load-time (alist-get method nskk-input-loader-load-times)))
          (princ (format "  - %-10s : %.3f ms\n"
                         method
                         (if load-time (* load-time 1000) 0.0)))))

      (princ "\nAvailable methods:\n")
      (dolist (entry nskk-input-loader-autoload-alist)
        (princ (format "  - %-10s : %s\n"
                       (car entry)
                       (if (memq (car entry) nskk-input-loader-loaded-methods)
                           "loaded"
                         "not loaded")))))))

;;; パフォーマンス最適化

(defun nskk-input-loader-benchmark ()
  "全ての入力方式のロード時間をベンチマークする。"
  (interactive)
  (let ((results nil))
    ;; 全てアンロード
    (nskk-input-loader-unload-all)

    ;; 各入力方式をロードして測定
    (dolist (entry nskk-input-loader-autoload-alist)
      (let ((method (car entry)))
        (garbage-collect)  ; GCの影響を最小化
        (nskk-input-loader-load method)
        (push (cons method (alist-get method nskk-input-loader-load-times))
              results)))

    ;; 結果を表示
    (with-output-to-temp-buffer "*NSKK Loader Benchmark*"
      (princ "NSKK Input Method Load Time Benchmark\n")
      (princ "=====================================\n\n")
      (dolist (result (nreverse results))
        (princ (format "%-10s : %.3f ms\n"
                       (car result)
                       (* (cdr result) 1000))))
      (princ (format "\nTotal: %.3f ms\n"
                     (* (cl-reduce #'+ (mapcar #'cdr results)) 1000))))))

;;; 初期化

;;;###autoload
(defun nskk-input-loader-init ()
  "ロード機構を初期化する。"
  (interactive)
  ;; autoload設定
  (nskk-input-loader-setup-autoloads)

  ;; プリロード
  (when nskk-input-loader-preload-methods
    (nskk-input-loader-preload-methods))

  (message "NSKK input loader initialized"))

(provide 'nskk-input-loader)

;;; nskk-input-loader.el ends here
