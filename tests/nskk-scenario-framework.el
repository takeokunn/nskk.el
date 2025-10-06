;;; nskk-scenario-framework.el --- Scenario test framework for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, testing, scenario
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

;; シナリオベーステストフレームワーク
;;
;; 特徴:
;; - execute-kbd-macroベースのキー入力シミュレーション
;; - ステップバイステップの検証
;; - シナリオ専用アサーションマクロ
;; - ERT完全統合

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nskk-test-framework)
(require 'nskk)
(require 'nskk-state)
(require 'nskk-buffer)
(require 'nskk-events)
(require 'nskk-mode-switch)
(require 'nskk-converter)

;;; データ構造定義

(cl-defstruct nskk-scenario-step
  "シナリオの1ステップを表す構造体。"
  (description "" :type string)
  (action nil :type function)
  (validators nil :type list))

(cl-defstruct nskk-scenario
  "シナリオ全体を表す構造体。"
  (name nil :type symbol)
  (description "" :type string)
  (tags nil :type list)
  (initial-mode 'hiragana :type symbol)
  (steps nil :type list)
  (cleanup-fn nil :type (or null function)))

;;; テスト用モック辞書

(defvar nskk-scenario--mock-dictionary
  '(("かんじ" . ("漢字" "幹事" "感じ"))
    ("あい" . ("愛" "哀" "藍"))
    ("かんじへんかん" . ("漢字変換"))
    ("ひらがな" . ("平仮名"))
    ("かたかな" . ("片仮名" "カタカナ")))
  "シナリオテスト用のモック辞書。")

(defun nskk-scenario--lookup-candidates (reading)
  "READING に対する変換候補をモック辞書から取得する。"
  (cdr (assoc reading nskk-scenario--mock-dictionary)))

(defun nskk-scenario--get-current-candidate ()
  "現在選択中の候補を取得する。"
  (when-let* ((candidates (nskk-state-candidates nskk-current-state))
              (index (nskk-state-candidate-index nskk-current-state))
              (valid-index (and (>= index 0) (< index (length candidates)))))
    (nth index candidates)))

;;; キー入力シミュレーション関数

(defun nskk-scenario-type (keys)
  "KEYS をシミュレートして入力する。
KEYS は文字列またはキーシーケンス。
現在のモードに応じてローマ字→かな変換を行い、バッファに挿入する。"
  (unless (stringp keys)
    (error "Invalid keys: %s (must be a string)" keys))

  ;; 現在のモードを取得
  (let ((mode (nskk-state-mode nskk-current-state)))
    (cond
     ;; ひらがなモード
     ((eq mode 'hiragana)
      (let ((kana (nskk-convert-romaji-simple keys)))
        (when (and kana (not (string-empty-p kana)))
          (nskk-buffer-insert kana))))

     ;; カタカナモード
     ((eq mode 'katakana)
      (let* ((hiragana (nskk-convert-romaji-simple keys))
             (katakana (when (and hiragana (not (string-empty-p hiragana)))
                         (japanese-katakana hiragana))))
        (when katakana
          (nskk-buffer-insert katakana))))

     ;; その他のモードはキーボードマクロで処理
     (t
      (execute-kbd-macro (kbd keys))))))

(defun nskk-scenario-press (key)
  "単一キー KEY を押す。
KEY はシンボル('return, 'space等)または文字。"
  (pcase key
    ('return
     ;; Enterキー: 候補選択中なら確定、そうでなければ未確定入力を確定
     (if (nskk-state-candidates nskk-current-state)
         ;; 候補選択中: 選択中の候補を確定
         (when-let ((candidate (nskk-scenario--get-current-candidate)))
           (nskk-buffer-commit candidate)
           (setf (nskk-state-candidates nskk-current-state) nil)
           (setf (nskk-state-candidate-index nskk-current-state) 0))
       ;; 通常の未確定入力確定
       (when (nskk-buffer-has-pending-input-p)
         (let ((text (nskk-buffer-pending-text)))
           (nskk-buffer-commit text)))))

    ('space
     ;; スペースキー: 変換開始または次候補
     (cond
      ;; 既に候補選択中: 次候補へ
      ((nskk-state-candidates nskk-current-state)
       (let* ((candidates (nskk-state-candidates nskk-current-state))
              (index (nskk-state-candidate-index nskk-current-state))
              (next-index (mod (1+ index) (length candidates))))
         (setf (nskk-state-candidate-index nskk-current-state) next-index)))

      ;; 未確定入力あり: 変換開始
      ((nskk-buffer-has-pending-input-p)
       (let* ((reading (nskk-buffer-pending-text))
              (candidates (nskk-scenario--lookup-candidates reading)))
         (when candidates
           (setf (nskk-state-candidates nskk-current-state) candidates)
           (setf (nskk-state-candidate-index nskk-current-state) 0))))

      ;; その他: 通常のスペース入力
      (t
       (nskk-buffer-insert " "))))

    ("q"
     ;; qキー: モード切り替え
     (nskk-mode-toggle-kana))

    ("x"
     ;; xキー: 前候補
     (when (nskk-state-candidates nskk-current-state)
       (let* ((candidates (nskk-state-candidates nskk-current-state))
              (index (nskk-state-candidate-index nskk-current-state))
              (prev-index (mod (1- index) (length candidates))))
         (setf (nskk-state-candidate-index nskk-current-state) prev-index))))

    ("C-g"
     ;; C-g: 変換キャンセル
     (when (or (nskk-state-candidates nskk-current-state)
               (nskk-buffer-has-pending-input-p))
       (setf (nskk-state-candidates nskk-current-state) nil)
       (setf (nskk-state-candidate-index nskk-current-state) 0)
       (when (nskk-buffer-has-pending-input-p)
         (nskk-buffer-clear))))

    ('tab
     (execute-kbd-macro (kbd "TAB")))

    ('backspace
     (execute-kbd-macro (kbd "DEL")))

    ((pred characterp)
     (execute-kbd-macro (kbd (char-to-string key))))

    ((pred stringp)
     (execute-kbd-macro (kbd key)))

    (_
     (execute-kbd-macro (kbd (symbol-name key))))))

;;; アサーションマクロ

(defmacro nskk-scenario-expect-mode (expected-mode)
  "現在のモードが EXPECTED-MODE であることを検証。"
  `(should (eq (nskk-state-mode nskk-current-state) ,expected-mode)))

(defmacro nskk-scenario-expect-pending (expected-text)
  "未確定入力が EXPECTED-TEXT であることを検証。"
  `(should (equal (nskk-buffer-pending-text) ,expected-text)))

(defmacro nskk-scenario-expect-no-pending ()
  "未確定入力が空であることを検証。"
  `(should-not (nskk-buffer-has-pending-input-p)))

(defmacro nskk-scenario-expect-buffer-contains (expected-text)
  "バッファ内容に EXPECTED-TEXT が含まれることを検証。"
  `(should (string-match-p (regexp-quote ,expected-text) (buffer-string))))

(defmacro nskk-scenario-expect-candidates (expected-list)
  "変換候補が EXPECTED-LIST であることを検証。"
  `(should (equal (nskk-state-candidates nskk-current-state) ,expected-list)))

(defmacro nskk-scenario-expect-candidate-selected (expected-candidate)
  "選択中の候補が EXPECTED-CANDIDATE であることを検証。"
  `(should (equal (nskk-scenario--get-current-candidate) ,expected-candidate)))

;;; シナリオ実行エンジン

(defun nskk-scenario-run (scenario)
  "SCENARIO を実行する。"
  (let ((steps (nskk-scenario-steps scenario))
        (step-index 0))
    (dolist (step steps)
      (when nskk-test-verbose
        (message "Step %d: %s" step-index (nskk-scenario-step-description step)))

      ;; アクション実行
      (when-let ((action (nskk-scenario-step-action step)))
        (funcall action))

      ;; バリデーション実行
      (dolist (validator (nskk-scenario-step-validators step))
        (funcall validator))

      (setq step-index (1+ step-index)))))

(defun nskk-scenario-setup (initial-mode)
  "シナリオ実行前のセットアップ。
INITIAL-MODE は初期入力モード。"
  ;; 既存E2Eテストと同じパターン
  (nskk-state-init)
  (nskk-events-clear-listeners nil nil)
  (when (nskk-buffer-has-pending-input-p)
    (nskk-buffer-clear))
  ;; モード切り替え
  (when initial-mode
    (nskk-mode-switch nskk-current-state initial-mode)))

(defun nskk-scenario-teardown ()
  "シナリオ実行後のクリーンアップ。"
  (nskk-state-cleanup)
  (nskk-events-cleanup))

;;; ヘルパーマクロ

(defmacro nskk-scenario-with-setup (initial-mode &rest body)
  "INITIAL-MODE でシナリオ環境をセットアップして BODY を実行。"
  (declare (indent 1) (debug (form body)))
  `(nskk-test-with-temp-buffer
     (nskk-scenario-setup ,initial-mode)
     (unwind-protect
         (progn ,@body)
       (nskk-scenario-teardown))))

(provide 'nskk-scenario-framework)

;;; nskk-scenario-framework.el ends here
