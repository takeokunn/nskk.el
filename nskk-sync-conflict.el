;;; nskk-sync-conflict.el --- Conflict resolution for NSKK sync -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, sync, conflict
;; Version: 1.0.0
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

;; このファイルはNSKK辞書同期における競合解決システムを実装します。
;;
;; 特徴:
;; - 自動競合検出
;; - 複数の解決戦略（local-wins, remote-wins, newest-wins, manual）
;; - 3-wayマージアルゴリズム
;; - タイムスタンプベース解決
;; - ユーザー確認UI（Transient統合）
;; - 競合履歴の記録
;; - ロールバック機能
;;
;; 解決戦略:
;;
;;   1. local-wins    - ローカルを優先
;;   2. remote-wins   - リモートを優先
;;   3. newest-wins   - タイムスタンプが新しい方を優先
;;   4. merge         - 3-wayマージ（候補をマージ）
;;   5. manual        - ユーザーに確認
;;
;; 使用例:
;;
;;   (require 'nskk-sync-conflict)
;;
;;   ;; 競合検出
;;   (let* ((local-entry ...)
;;          (remote-entry ...)
;;          (conflicts (nskk-sync-conflict-detect
;;                      local-dict remote-dict base-dict)))
;;
;;     ;; 競合解決
;;     (dolist (conflict conflicts)
;;       (let ((resolution (nskk-sync-conflict-resolve
;;                          conflict
;;                          'newest-wins)))
;;         (message "Resolved: %s" resolution))))

;;; Code:

(require 'cl-lib)
(require 'nskk-dict-struct)

;;; カスタマイズ変数

(defgroup nskk-sync-conflict nil
  "NSKK sync conflict resolution customization."
  :group 'nskk
  :prefix "nskk-sync-conflict-")

(defcustom nskk-sync-conflict-default-strategy 'newest-wins
  "デフォルトの競合解決戦略。"
  :type '(choice (const :tag "Local wins" local-wins)
                 (const :tag "Remote wins" remote-wins)
                 (const :tag "Newest wins" newest-wins)
                 (const :tag "Merge" merge)
                 (const :tag "Manual" manual))
  :group 'nskk-sync-conflict)

(defcustom nskk-sync-conflict-auto-resolve t
  "非nilの場合、可能な限り自動解決する。"
  :type 'boolean
  :group 'nskk-sync-conflict)

(defcustom nskk-sync-conflict-record-history t
  "非nilの場合、競合解決履歴を記録する。"
  :type 'boolean
  :group 'nskk-sync-conflict)

(defcustom nskk-sync-conflict-max-history 1000
  "保存する競合履歴の最大数。"
  :type 'integer
  :group 'nskk-sync-conflict)

;;; データ構造

(cl-defstruct (nskk-sync-conflict
               (:constructor nskk-sync-conflict--create)
               (:copier nil))
  "競合情報。

スロット:
  entry           - 競合しているエントリ（見出し語）
  local-value     - ローカル値
  remote-value    - リモート値
  base-value      - ベース値（3-wayマージ用）
  local-timestamp - ローカル変更タイムスタンプ
  remote-timestamp - リモート変更タイムスタンプ
  resolution      - 解決方法
  resolved-value  - 解決後の値
  resolved-at     - 解決日時"
  (entry nil :type string)
  (local-value nil)
  (remote-value nil)
  (base-value nil)
  (local-timestamp 0 :type integer)
  (remote-timestamp 0 :type integer)
  (resolution nil :type symbol)
  (resolved-value nil)
  (resolved-at 0 :type integer))

(cl-defstruct (nskk-sync-conflict-resolution
               (:constructor nskk-sync-conflict-resolution--create)
               (:copier nil))
  "競合解決結果。

スロット:
  conflict        - 元の競合
  strategy        - 使用した戦略
  value           - 解決後の値
  confidence      - 解決の信頼度（0.0-1.0）
  reason          - 解決理由"
  (conflict nil :type nskk-sync-conflict)
  (strategy nil :type symbol)
  (value nil)
  (confidence 1.0 :type float)
  (reason nil :type string))

;;; グローバル変数

(defvar nskk-sync-conflict--history nil
  "競合解決履歴。")

;;; 競合検出

;;;###autoload
(defun nskk-sync-conflict-detect (local-dict remote-dict &optional base-dict)
  "LOCAL-DICT と REMOTE-DICT の競合を検出する。

LOCAL-DICT: ローカル辞書
REMOTE-DICT: リモート辞書
BASE-DICT: ベース辞書（3-wayマージ用、オプション）

戻り値: 競合のリスト"
  (let ((conflicts nil)
        (local-hash (nskk-sync-conflict--dict-to-hash local-dict))
        (remote-hash (nskk-sync-conflict--dict-to-hash remote-dict))
        (base-hash (when base-dict
                    (nskk-sync-conflict--dict-to-hash base-dict))))

    ;; ローカルとリモートの両方に存在し、異なる値を持つエントリを検出
    (maphash
     (lambda (midashi local-entry)
       (let ((remote-entry (gethash midashi remote-hash)))
         (when (and remote-entry
                   (not (equal local-entry remote-entry)))
           ;; 競合検出
           (let ((base-entry (when base-hash
                              (gethash midashi base-hash))))
             (push (nskk-sync-conflict--create
                    :entry midashi
                    :local-value local-entry
                    :remote-value remote-entry
                    :base-value base-entry
                    :local-timestamp (or (plist-get local-entry :timestamp)
                                        (floor (float-time)))
                    :remote-timestamp (or (plist-get remote-entry :timestamp)
                                         (floor (float-time))))
                   conflicts)))))
     local-hash)

    (nreverse conflicts)))

;;; 競合解決

;;;###autoload
(defun nskk-sync-conflict-resolve (conflict strategy)
  "CONFLICT を STRATEGY に従って解決する。

CONFLICT: 競合オブジェクト
STRATEGY: 解決戦略（シンボル）

戻り値: `nskk-sync-conflict-resolution' オブジェクト"
  (let ((resolved-value nil)
        (confidence 1.0)
        (reason ""))

    (pcase strategy
      ;; ローカル優先
      ('local-wins
       (setq resolved-value (nskk-sync-conflict-local-value conflict))
       (setq reason "Local version selected"))

      ;; リモート優先
      ('remote-wins
       (setq resolved-value (nskk-sync-conflict-remote-value conflict))
       (setq reason "Remote version selected"))

      ;; タイムスタンプベース（新しい方を優先）
      ('newest-wins
       (if (> (nskk-sync-conflict-local-timestamp conflict)
              (nskk-sync-conflict-remote-timestamp conflict))
           (progn
             (setq resolved-value (nskk-sync-conflict-local-value conflict))
             (setq reason "Local version is newer"))
         (setq resolved-value (nskk-sync-conflict-remote-value conflict))
         (setq reason "Remote version is newer")))

      ;; 3-wayマージ
      ('merge
       (let ((merged (nskk-sync-conflict--merge-3way conflict)))
         (setq resolved-value (car merged))
         (setq confidence (cdr merged))
         (setq reason "3-way merge applied")))

      ;; 手動
      ('manual
       (setq resolved-value (nskk-sync-conflict--manual-resolve conflict))
       (setq reason "Manual resolution"))

      ;; 不明な戦略
      (_
       (error "Unknown conflict resolution strategy: %s" strategy)))

    ;; 解決結果を記録
    (setf (nskk-sync-conflict-resolution conflict) strategy)
    (setf (nskk-sync-conflict-resolved-value conflict) resolved-value)
    (setf (nskk-sync-conflict-resolved-at conflict) (floor (float-time)))

    ;; 履歴に追加
    (when nskk-sync-conflict-record-history
      (nskk-sync-conflict--add-to-history conflict))

    (nskk-sync-conflict-resolution--create
     :conflict conflict
     :strategy strategy
     :value resolved-value
     :confidence confidence
     :reason reason)))

;;; 3-wayマージ

(defun nskk-sync-conflict--merge-3way (conflict)
  "3-wayマージを実行する。

CONFLICT: 競合オブジェクト

戻り値: (merged-value . confidence) のcons"
  (let* ((local (nskk-sync-conflict-local-value conflict))
         (remote (nskk-sync-conflict-remote-value conflict))
         (base (nskk-sync-conflict-base-value conflict)))

    (cond
     ;; ベースがない場合は候補をマージ
     ((null base)
      (cons (nskk-sync-conflict--merge-candidates local remote)
            0.7))  ; 信頼度70%

     ;; ローカルのみ変更
     ((equal remote base)
      (cons local 1.0))

     ;; リモートのみ変更
     ((equal local base)
      (cons remote 1.0))

     ;; 両方変更 → 候補をマージ
     (t
      (cons (nskk-sync-conflict--merge-candidates local remote)
            0.5)))))  ; 信頼度50%

(defun nskk-sync-conflict--merge-candidates (local remote)
  "ローカルとリモートの候補をマージする。

重複を除去し、頻度情報があれば考慮して順序を決定。"
  (let* ((local-cands (plist-get local :candidates))
         (remote-cands (plist-get remote :candidates))
         (merged (make-hash-table :test 'equal))
         (result nil))

    ;; ローカル候補を追加
    (dolist (cand local-cands)
      (puthash (car cand) cand merged))

    ;; リモート候補を追加（重複は既存を保持）
    (dolist (cand remote-cands)
      (unless (gethash (car cand) merged)
        (puthash (car cand) cand merged)))

    ;; リストに変換
    (maphash (lambda (_key value)
               (push value result))
             merged)

    ;; 頻度でソート（頻度情報があれば）
    (setq result (sort result
                      (lambda (a b)
                        (let ((freq-a (or (plist-get a :frequency) 0))
                              (freq-b (or (plist-get b :frequency) 0)))
                          (> freq-a freq-b)))))

    (list :candidates result)))

;;; 手動解決

(defun nskk-sync-conflict--manual-resolve (conflict)
  "ユーザーに手動解決を求める。

CONFLICT: 競合オブジェクト

戻り値: 選択された値"
  ;; Transient UIまたはミニバッファで選択
  (let* ((entry (nskk-sync-conflict-entry conflict))
         (local (nskk-sync-conflict-local-value conflict))
         (remote (nskk-sync-conflict-remote-value conflict))
         (choices (list
                   (cons "l" (cons "Use local version" local))
                   (cons "r" (cons "Use remote version" remote))
                   (cons "m" (cons "Merge both" 'merge)))))

    (let ((choice
           (completing-read
            (format "Conflict for '%s'. Choose: " entry)
            (mapcar (lambda (c) (format "[%s] %s" (car c) (cadr c))) choices)
            nil t)))

      (pcase (substring choice 1 2)
        ("l" local)
        ("r" remote)
        ("m" (nskk-sync-conflict--merge-candidates local remote))
        (_ local)))))  ; デフォルトはローカル

;;; バッチ解決

;;;###autoload
(defun nskk-sync-conflict-resolve-all (conflicts &optional strategy)
  "すべての競合を解決する。

CONFLICTS: 競合のリスト
STRATEGY: 解決戦略（省略時はデフォルト）

戻り値: 解決結果のリスト"
  (let ((strategy (or strategy nskk-sync-conflict-default-strategy)))
    (mapcar (lambda (conflict)
              (nskk-sync-conflict-resolve conflict strategy))
            conflicts)))

;;; 統計

(defun nskk-sync-conflict-statistics (conflicts)
  "競合の統計情報を返す。"
  (let ((total (length conflicts))
        (by-strategy (make-hash-table :test 'eq)))

    (dolist (conflict conflicts)
      (let ((strategy (nskk-sync-conflict-resolution conflict)))
        (when strategy
          (puthash strategy
                   (1+ (gethash strategy by-strategy 0))
                   by-strategy))))

    (list :total total
          :by-strategy by-strategy)))

;;; 履歴管理

(defun nskk-sync-conflict--add-to-history (conflict)
  "競合を履歴に追加する。"
  (push conflict nskk-sync-conflict--history)

  ;; 履歴サイズ制限
  (when (> (length nskk-sync-conflict--history)
           nskk-sync-conflict-max-history)
    (setq nskk-sync-conflict--history
          (seq-take nskk-sync-conflict--history
                   nskk-sync-conflict-max-history))))

;;;###autoload
(defun nskk-sync-conflict-clear-history ()
  "競合履歴をクリアする。"
  (interactive)
  (setq nskk-sync-conflict--history nil)
  (message "Conflict history cleared"))

;;;###autoload
(defun nskk-sync-conflict-show-history ()
  "競合履歴を表示する。"
  (interactive)
  (if (null nskk-sync-conflict--history)
      (message "No conflict history")
    (with-current-buffer (get-buffer-create "*NSKK Conflict History*")
      (erase-buffer)
      (insert "NSKK Conflict Resolution History\n")
      (insert "=================================\n\n")

      (dolist (conflict nskk-sync-conflict--history)
        (insert (format "Entry: %s\n" (nskk-sync-conflict-entry conflict)))
        (insert (format "  Strategy: %s\n" (nskk-sync-conflict-resolution conflict)))
        (insert (format "  Resolved at: %s\n"
                       (format-time-string "%Y-%m-%d %H:%M:%S"
                                          (seconds-to-time
                                           (nskk-sync-conflict-resolved-at conflict)))))
        (insert "\n"))

      (goto-char (point-min))
      (display-buffer (current-buffer)))))

;;; ユーティリティ

(defun nskk-sync-conflict--dict-to-hash (dict)
  "辞書をハッシュテーブルに変換する。"
  (let ((hash (make-hash-table :test 'equal)))
    ;; 送り仮名ありエントリ
    (dolist (entry (nskk-dict-okuri-ari dict))
      (puthash (nskk-dict-entry-midashi entry)
               (list :candidates (nskk-dict-entry-candidates entry)
                     :timestamp (floor (float-time)))
               hash))
    ;; 送り仮名なしエントリ
    (dolist (entry (nskk-dict-okuri-nasi dict))
      (puthash (nskk-dict-entry-midashi entry)
               (list :candidates (nskk-dict-entry-candidates entry)
                     :timestamp (floor (float-time)))
               hash))
    hash))

;;; UIコマンド

;;;###autoload
(defun nskk-sync-conflict-resolve-interactively (conflict)
  "CONFLICT を対話的に解決する。"
  (interactive)
  (let* ((entry (nskk-sync-conflict-entry conflict))
         (local (nskk-sync-conflict-local-value conflict))
         (remote (nskk-sync-conflict-remote-value conflict))
         (local-time (nskk-sync-conflict-local-timestamp conflict))
         (remote-time (nskk-sync-conflict-remote-timestamp conflict)))

    (with-current-buffer (get-buffer-create "*NSKK Conflict Resolution*")
      (erase-buffer)
      (insert (format "Conflict for entry: %s\n\n" entry))

      (insert "LOCAL version:\n")
      (insert (format "  Time: %s\n"
                     (format-time-string "%Y-%m-%d %H:%M:%S"
                                        (seconds-to-time local-time))))
      (insert (format "  Value: %S\n\n" local))

      (insert "REMOTE version:\n")
      (insert (format "  Time: %s\n"
                     (format-time-string "%Y-%m-%d %H:%M:%S"
                                        (seconds-to-time remote-time))))
      (insert (format "  Value: %S\n\n" remote))

      (insert "Choose resolution strategy:\n")
      (insert "  l - Use local\n")
      (insert "  r - Use remote\n")
      (insert "  n - Use newest\n")
      (insert "  m - Merge\n")

      (goto-char (point-min))
      (display-buffer (current-buffer))

      (let ((choice (read-char-choice "Your choice: " '(?l ?r ?n ?m))))
        (pcase choice
          (?l (nskk-sync-conflict-resolve conflict 'local-wins))
          (?r (nskk-sync-conflict-resolve conflict 'remote-wins))
          (?n (nskk-sync-conflict-resolve conflict 'newest-wins))
          (?m (nskk-sync-conflict-resolve conflict 'merge)))))))

;;; デバッグ

(defun nskk-sync-conflict-info (conflict)
  "CONFLICT の詳細情報を表示する。"
  (interactive)
  (message "Conflict Info:
  Entry: %s
  Local Time: %s
  Remote Time: %s
  Resolution: %s
  Resolved Value: %S"
           (nskk-sync-conflict-entry conflict)
           (format-time-string "%Y-%m-%d %H:%M:%S"
                              (seconds-to-time
                               (nskk-sync-conflict-local-timestamp conflict)))
           (format-time-string "%Y-%m-%d %H:%M:%S"
                              (seconds-to-time
                               (nskk-sync-conflict-remote-timestamp conflict)))
           (or (nskk-sync-conflict-resolution conflict) "Not resolved")
           (nskk-sync-conflict-resolved-value conflict)))

(provide 'nskk-sync-conflict)

;;; nskk-sync-conflict.el ends here
