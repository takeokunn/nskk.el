;;; nskk-scenario-dsl.el --- DSL for scenario tests -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, testing, scenario, dsl
;; Version: 0.1.0
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

;; シナリオテスト記述用DSL
;;
;; 使用例:
;; (nskk-defscenario basic-hiragana-input
;;   "基本的なひらがな入力のシナリオ"
;;   :tags '(:basic :beginner)
;;   :initial-mode 'hiragana
;;
;;   (step "ローマ字入力"
;;     (type "aiueo")
;;     (expect-pending "あいうえお"))
;;
;;   (step "確定"
;;     (press 'return)
;;     (expect-buffer-contains "あいうえお")))

;;; Code:

(require 'nskk-scenario-framework)

;;; DSLヘルパー変数

(defvar nskk-scenario--current-steps nil
  "DSL展開中に収集されるステップのリスト。")

;;; DSLヘルパーマクロ

(defmacro step (description &rest body)
  "シナリオステップを定義する。
DESCRIPTION はステップの説明、BODY はステップ内容。"
  (declare (indent 1) (debug (stringp body)))
  (let ((actions nil)
        (validators nil))

    ;; BODYを解析してactionとvalidatorに分類
    (dolist (form body)
      (let ((fn (car-safe form)))
        (cond
         ;; type, press はアクション
         ((memq fn '(type press))
          (push `(lambda () ,form) actions))

         ;; expect-* はバリデーション
         ((and (symbolp fn)
               (string-prefix-p "expect-" (symbol-name fn)))
          (let ((full-fn-name (intern (concat "nskk-scenario-" (symbol-name fn))))
                (args (cdr form)))
            (push `(lambda () (,full-fn-name ,@args)) validators)))

         ;; その他はアクション扱い
         (t
          (push `(lambda () ,form) actions)))))

    ;; リストを逆順に戻す（元の順序を保持）
    (setq actions (nreverse actions))
    (setq validators (nreverse validators))

    ;; nskk-scenario-step構造体を生成
    `(push (make-nskk-scenario-step
            :description ,description
            :action (lambda ()
                      ,@(mapcar (lambda (a) `(funcall ,a)) actions))
            :validators (list ,@validators))
           nskk-scenario--current-steps)))

;;; メインマクロ

(defmacro nskk-defscenario (name description &rest body)
  "シナリオテストを定義する。

NAME はシンボル、DESCRIPTION は文字列。
BODY の先頭で :tags と :initial-mode を指定可能。
その後に複数の (step ...) を記述。

使用例:
  (nskk-defscenario my-scenario
    \"My scenario description\"
    :tags \\='(:basic)
    :initial-mode \\='hiragana

    (step \"First step\"
      (type \"a\")
      (expect-pending \"あ\"))

    (step \"Second step\"
      (press \\='return)
      (expect-no-pending)))"

  (declare (indent 2) (debug (symbolp stringp &rest form)))

  (let ((tags nil)
        (initial-mode 'hiragana)
        (steps-body body))

    ;; :tags キーワードを抽出
    (when (eq (car steps-body) :tags)
      (setq tags (cadr steps-body))
      (setq steps-body (cddr steps-body)))

    ;; :initial-mode キーワードを抽出
    (when (eq (car steps-body) :initial-mode)
      (setq initial-mode (cadr steps-body))
      (setq steps-body (cddr steps-body)))

    ;; nskk-deftestに展開
    `(nskk-deftest ,(intern (format "%s-scenario" name))
       ,description
       :tags (append '(:scenario) ,tags)

       ;; ステップ収集用の動的変数をバインド
       (let ((nskk-scenario--current-steps nil))

         ;; ステップ定義を評価してnskk-scenario--current-stepsに収集
         ,@steps-body

         ;; シナリオ構造体を生成
         (let ((scenario (make-nskk-scenario
                          :name ',name
                          :description ,description
                          :tags ,tags
                          :initial-mode ,initial-mode
                          :steps (nreverse nskk-scenario--current-steps))))

           ;; シナリオ実行
           (nskk-scenario-with-setup ,initial-mode
             (nskk-scenario-run scenario)))))))

;;; DSL関数 (アクション)

(defun type (keys)
  "KEYS をタイプする。"
  (nskk-scenario-type keys))

(defun press (key)
  "KEY を押す。"
  (nskk-scenario-press key))

(provide 'nskk-scenario-dsl)

;;; nskk-scenario-dsl.el ends here
