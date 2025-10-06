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
(require 'subr-x)
(require 'nskk-test-framework)
(require 'nskk)
(require 'nskk-state)
(require 'nskk-buffer)
(require 'nskk-events)
(require 'nskk-mode-switch)
(require 'nskk-converter)
(require 'nskk-learning-frequency)

(declare-function nskk-annotation-display-get-current "nskk-annotation-display")
(declare-function nskk-annotation-parser-get-description "nskk-annotation-parser" (annotation))
(declare-function nskk-get-frequency-score "nskk-learning-frequency" (midashi candidate))
(declare-function nskk-update-frequency "nskk-learning-frequency" (midashi candidate))
(declare-function nskk-application--hankaku-to-zenkaku "nskk-layer-application" (hankaku))

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

(defconst nskk-scenario--default-mock-dictionary
  '(("かんじ" . ("漢字" "幹事" "感じ"))
    ("あい" . ("愛" "哀" "藍"))
    ("かんじへんかん" . ("漢字変換"))
    ("ひらがな" . ("平仮名"))
    ("かたかな" . ("片仮名" "カタカナ"))
    ("たべる" . ("食べる" "食べた"))
    ("たべもの" . ("食べ物"))
    ("うつくしい" . ("美しい" "美しかった"))
    ("かいぎ" . ("会議" "懇親会"))
    ("かんり" . ("管理" "監理" "官吏"))
    ("かんりしょく" . ("管理職"))
    ("がくしゅう" . ("学習"))
    ("じしょ" . ("辞書"))
    ("しんかんせん" . ("新幹線"))
    ("ながもじ" . ("長文字入力テスト")))
  "シナリオテスト用のモック辞書初期値。")

(defvar nskk-scenario--mock-dictionary nil
  "シナリオテスト用に利用するモック辞書。")

(defvar nskk-scenario--last-reading nil
  "直近で変換対象となった読み。")

(defun nskk-scenario--lookup-candidates (reading)
  "READING に対する変換候補をモック辞書から取得する。"
  (cdr (assoc reading nskk-scenario--mock-dictionary)))

(defun nskk-scenario--get-current-candidate ()
  "現在選択中の候補を取得する。"
  (when-let* ((candidates (nskk-state-candidates nskk-current-state))
              (index (nskk-state-candidate-index nskk-current-state))
              (valid-index (and (>= index 0) (< index (length candidates)))))
    (nth index candidates)))

(defun nskk-scenario-register-dictionary-entry (reading candidates &optional at-end)
  "モック辞書に新しい見出し語READINGと候補CANDIDATESを登録する。
AT-ENDが非nilなら末尾に追加する。"
  (let* ((existing (assoc reading nskk-scenario--mock-dictionary))
         (filtered (cl-remove-if (lambda (entry)
                                   (string= (car entry) reading))
                                 nskk-scenario--mock-dictionary)))
    (setq nskk-scenario--mock-dictionary
          (if at-end
              (append filtered (list (cons reading candidates)))
            (cons (cons reading candidates) filtered)))))

(defun nskk-scenario--matching-readings (prefix)
  "PREFIXを先頭に持つ読みをモック辞書から抽出する。"
  (cl-loop for (reading . _) in nskk-scenario--mock-dictionary
           when (string-prefix-p prefix reading)
           collect reading))

(defun nskk-scenario--common-prefix (a b)
  "文字列AとBの共通接頭辞を返す。"
  (let* ((len (min (length a) (length b)))
         (idx 0))
    (while (and (< idx len)
                (= (aref a idx) (aref b idx)))
      (setq idx (1+ idx)))
    (substring a 0 idx)))

(defun nskk-scenario--longest-common-prefix (strings)
  "STRINGSの最長共通接頭辞を求める。"
  (when strings
    (let ((prefix (car strings)))
      (dolist (str (cdr strings))
        (setq prefix (nskk-scenario--common-prefix prefix str))
        (when (string-empty-p prefix)
          (cl-return nil)))
      prefix)))

(defun nskk-scenario--complete-pending ()
  "未確定入力を辞書候補で補完する。"
  (when (nskk-buffer-has-pending-input-p)
    (let* ((pending (nskk-buffer-pending-text))
           (matches (nskk-scenario--matching-readings pending))
           (prefix (nskk-scenario--longest-common-prefix matches)))
      (when (and prefix (> (length prefix) (length pending)))
        (let ((append-text (substring prefix (length pending)))
              (candidates (nskk-scenario--lookup-candidates prefix)))
          (nskk-buffer-insert append-text)
          (setq nskk-scenario--last-reading prefix)
          (when candidates
            (setf (nskk-state-candidates nskk-current-state) candidates)
            (setf (nskk-state-candidate-index nskk-current-state) 0)))))))

(defun nskk-scenario-begin-conversion (&optional reading)
  "READING を基に候補リストを生成する。READING が省略された場合は直近の入力を利用する。"
    (let* ((target (or reading
                      (nskk-buffer-pending-text)
                      nskk-scenario--last-reading))
           (candidates (and target
                          (nskk-scenario--lookup-candidates target))))
    
    (when (and target candidates)
      (setq nskk-scenario--last-reading target)
      (setf (nskk-state-candidates nskk-current-state) candidates)
      (setf (nskk-state-candidate-index nskk-current-state) 0))
    candidates))

;;; キー入力シミュレーション関数

(defun nskk-scenario-press (key)
  "単一キー KEY を押す。
KEY はシンボル('return, 'space等)または文字。"
  (pcase key
    ('return
     ;; Enterキー: 候補選択中なら確定、そうでなければ未確定入力を確定
     (if (nskk-state-candidates nskk-current-state)
         ;; 候補選択中: 選択中の候補を確定
         (when-let* ((candidate (nskk-scenario--get-current-candidate))
                     (reading (or nskk-scenario--last-reading
                                  (nskk-buffer-pending-text))))
           (when (and (fboundp 'nskk-update-frequency)
                      reading)
             ;; 候補確定時に学習情報を反映
             (nskk-update-frequency reading candidate))
           (nskk-buffer-commit candidate)
           (setf (nskk-state-candidates nskk-current-state) nil)
           (setf (nskk-state-candidate-index nskk-current-state) 0)
           (setq nskk-scenario--last-reading nil))
       ;; 通常の未確定入力確定
       (when (nskk-buffer-has-pending-input-p)
         (let ((text (nskk-buffer-pending-text)))
           (nskk-buffer-commit text)
           (setq nskk-scenario--last-reading nil)))))

    ('space
     ;; スペースキー: 変換開始または次候補
     (cond
      ;; 既に候補選択中: 次候補へ
      ((nskk-state-candidates nskk-current-state)
       (let* ((candidates (nskk-state-candidates nskk-current-state))
              (index (nskk-state-candidate-index nskk-current-state))
              (next-index (mod (1+ index) (length candidates))))
         (setf (nskk-state-candidate-index nskk-current-state) next-index)))

      ;; 未確定入力または直近の読みを利用して変換開始
      ((nskk-scenario-begin-conversion))

      ;; 候補が見つからない場合はスペースを入力
      (t
       (nskk-buffer-insert " "))))

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
       (setq nskk-scenario--last-reading nil)
       (when (nskk-buffer-has-pending-input-p)
         (nskk-buffer-clear))))

    ('tab
     (nskk-scenario--complete-pending))

    ("TAB"
     (nskk-scenario--complete-pending))

    ("q"
     (setq nskk-scenario--last-reading nil)
     (nskk-mode-toggle-kana))

    ("Q"
     (setq nskk-scenario--last-reading nil)
     (nskk-mode-switch 'katakana))

    ("C-j"
     (setq nskk-scenario--last-reading nil)
     (nskk-mode-switch 'hiragana))

    ("l"
     (setq nskk-scenario--last-reading nil)
     (nskk-mode-toggle-latin))

    ("L"
     (setq nskk-scenario--last-reading nil)
     (nskk-mode-switch 'latin))

    ("\\"
     (setq nskk-scenario--last-reading nil)
     (nskk-mode-toggle-zenkaku-latin))

    ("/"
     (setq nskk-scenario--last-reading nil)
     (nskk-mode-switch 'abbrev))

    ('backspace
     (execute-kbd-macro (kbd "DEL")))

    ((and (pred stringp)
          (guard (string-match-p "^[1-9]$" key)))
     ;; 数字キーによる候補選択
     (when (nskk-state-candidates nskk-current-state)
       (let* ((candidates (nskk-state-candidates nskk-current-state))
              (index (1- (string-to-number key))))
         (when (and (>= index 0) (< index (length candidates)))
           (setf (nskk-state-candidate-index nskk-current-state) index)))))

    ((and (pred stringp)
          (guard (member key '("A" "S" "D" "F"))))
     ;; ホームポジションキーでの候補直接選択
     (let* ((mapping '(("A" . 0) ("S" . 1) ("D" . 2) ("F" . 3)))
            (index (cdr (assoc key mapping))))
       (when (and index (nskk-state-candidates nskk-current-state)
                  (< index (length (nskk-state-candidates nskk-current-state))))
         (setf (nskk-state-candidate-index nskk-current-state) index))))

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
  `(let ((pending (or (nskk-buffer-pending-text)
                      nskk-scenario--last-reading)))
     (should (equal pending ,expected-text))))

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

(defmacro nskk-scenario-expect-annotation (expected-text)
  "現在表示中の注釈がEXPECTED-TEXTを含むことを検証する。"
  `(let* ((current (when (fboundp 'nskk-annotation-display-get-current)
                     (nskk-annotation-display-get-current)))
          (description (when current
                         (nskk-annotation-parser-get-description current))))
     (should (and description
                  (string-match-p (regexp-quote ,expected-text) description)))))

(defmacro nskk-scenario-expect-frequency (reading candidate min-score)
  "READINGとCANDIDATEの学習スコアがMIN-SCORE以上であることを検証する。"
  `(let ((score (when (fboundp 'nskk-get-frequency-score)
                  (nskk-get-frequency-score ,reading ,candidate))))
     (should (and score (>= score ,min-score)))))

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
  (setq nskk-scenario--mock-dictionary (copy-tree nskk-scenario--default-mock-dictionary))
  (setq nskk-scenario--last-reading nil)
  (when (boundp 'nskk-frequency-table)
    (clrhash nskk-frequency-table))
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

(defun nskk-scenario-type (keys)
  "KEYS をシミュレートして入力する。
KEYS は文字列またはキーシーケンス。
現在のモードに応じてローマ字→かな変換を行い、バッファに挿入する。"
  (unless (stringp keys)
    (error "Invalid keys: %s (must be a string)" keys))

  (let* ((result (nskk-convert-romaji keys))
         (converted (nskk-converter-result-converted result))
         (pending (nskk-converter-result-pending result)))
    (pcase (nskk-state-mode nskk-current-state)
      ('hiragana
       (when (and converted (not (string-empty-p converted)))
         (nskk-buffer-insert converted))
       (when (and pending (not (string-empty-p pending)))
         (nskk-buffer-insert pending))
       (let ((reading (concat (or converted "") (or pending ""))))
         (unless (string-empty-p reading)
           (setq nskk-scenario--last-reading reading))))

      ('katakana
       (let ((katakana (when (and converted (not (string-empty-p converted)))
                         (japanese-katakana converted))))
         (when katakana
           (nskk-buffer-insert katakana)))
       (when (and pending (not (string-empty-p pending)))
         (nskk-buffer-insert pending)))

      ('latin
       ;; 英数モードでは入力文字をそのまま挿入
        (cl-loop for ch across keys
                 do (nskk-buffer-insert (string ch))))

      ('zenkaku-latin
       ;; 全角英数モードでは可能なら全角変換を利用
        (let ((zenkaku (when (fboundp 'nskk-application--hankaku-to-zenkaku)
                         (nskk-application--hankaku-to-zenkaku keys))))
          (if (and zenkaku (not (string-empty-p zenkaku)))
              (nskk-buffer-insert zenkaku)
            (cl-loop for ch across keys
                     do (nskk-buffer-insert (string ch))))))

      (_
       (execute-kbd-macro (kbd keys))))))

(provide 'nskk-scenario-framework)

;;; nskk-scenario-framework.el ends here
