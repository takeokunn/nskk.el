;;; nskk-learning-frequency.el --- Frequency learning engine for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, learning
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

;; このファイルは頻度学習エンジンを実装します。
;;
;; 特徴:
;; - 使用頻度追跡（LRU/LFUアルゴリズム）
;; - 個人辞書への自動反映
;; - 頻度減衰機能（時間経過による重み調整）
;; - メモリ効率的な実装
;;
;; アルゴリズム:
;; - LRU (Least Recently Used): 最近使用された順に優先度付け
;; - LFU (Least Frequently Used): 使用頻度に基づいて優先度付け
;; - ハイブリッドモード: LRUとLFUの組み合わせ
;;
;; 頻度減衰:
;; - 時間経過とともに頻度カウントを減衰
;; - 最近の使用パターンを重視
;;
;; 使用例:
;; (nskk-update-frequency "かんじ" "漢字")
;; (nskk-get-frequency-score "かんじ" "漢字")

;;; Code:

(require 'cl-lib)

;;; カスタマイズ変数

(defgroup nskk-learning-frequency nil
  "Frequency learning engine for NSKK."
  :group 'nskk
  :prefix "nskk-frequency-")

(defcustom nskk-frequency-algorithm 'hybrid
  "使用する頻度アルゴリズム。
- 'lru: 最近使用された順
- 'lfu: 使用頻度順
- 'hybrid: LRUとLFUの組み合わせ"
  :type '(choice (const :tag "LRU (Least Recently Used)" lru)
                 (const :tag "LFU (Least Frequently Used)" lfu)
                 (const :tag "Hybrid (LRU + LFU)" hybrid))
  :group 'nskk-learning-frequency)

(defcustom nskk-frequency-decay-enabled t
  "非nilの場合、頻度減衰を有効にする。"
  :type 'boolean
  :group 'nskk-learning-frequency)

(defcustom nskk-frequency-decay-interval 86400
  "頻度減衰を適用する間隔（秒）。
デフォルトは86400秒（1日）。"
  :type 'integer
  :group 'nskk-learning-frequency)

(defcustom nskk-frequency-decay-rate 0.95
  "頻度減衰率（0.0 - 1.0）。
1.0に近いほど減衰が緩やか。0.95なら1日で5%減衰。"
  :type 'float
  :group 'nskk-learning-frequency)

(defcustom nskk-frequency-lru-weight 0.3
  "ハイブリッドモードでのLRUの重み（0.0 - 1.0）。"
  :type 'float
  :group 'nskk-learning-frequency)

(defcustom nskk-frequency-lfu-weight 0.7
  "ハイブリッドモードでのLFUの重み（0.0 - 1.0）。"
  :type 'float
  :group 'nskk-learning-frequency)

(defcustom nskk-frequency-max-entries 10000
  "頻度データの最大エントリ数。
この数を超えた場合、古いエントリを削除する。"
  :type 'integer
  :group 'nskk-learning-frequency)

;;; データ構造

(cl-defstruct (nskk-frequency-entry
               (:constructor nskk-frequency-entry--create)
               (:copier nil))
  "頻度エントリ。

スロット:
  midashi        - 見出し語
  candidate      - 変換候補
  count          - 使用回数
  last-used      - 最終使用時刻（Emacs time形式）
  created        - 作成時刻
  score          - 計算されたスコア（キャッシュ用）"
  (midashi nil :type string)
  (candidate nil :type string)
  (count 0 :type integer)
  (last-used nil :type list)
  (created nil :type list)
  (score 0.0 :type float))

;;; グローバル変数

(defvar nskk-frequency-table (make-hash-table :test 'equal)
  "頻度データを保持するハッシュテーブル。
キー: (midashi . candidate) のcons
値: nskk-frequency-entry構造体")

(defvar nskk-frequency-last-decay-time (current-time)
  "最後に頻度減衰を実行した時刻。")

;;; 頻度更新

;;;###autoload
(defun nskk-update-frequency (midashi candidate)
  "見出し語 MIDASHI と候補 CANDIDATE の使用頻度を更新する。"
  (unless (and (stringp midashi) (stringp candidate))
    (error "Invalid arguments: midashi and candidate must be strings"))

  (let* ((key (cons midashi candidate))
         (entry (gethash key nskk-frequency-table)))

    (if entry
        ;; 既存エントリを更新
        (progn
          (cl-incf (nskk-frequency-entry-count entry))
          (setf (nskk-frequency-entry-last-used entry) (current-time))
          (nskk-frequency--update-score entry))

      ;; 新規エントリを作成
      (setq entry (nskk-frequency-entry--create
                   :midashi midashi
                   :candidate candidate
                   :count 1
                   :last-used (current-time)
                   :created (current-time)))
      (nskk-frequency--update-score entry)
      (puthash key entry nskk-frequency-table))

    ;; 定期的な減衰処理
    (when nskk-frequency-decay-enabled
      (nskk-frequency--maybe-decay))

    ;; エントリ数制限チェック
    (nskk-frequency--check-max-entries)

    entry))

;;;###autoload
(defun nskk-get-frequency-score (midashi candidate)
  "見出し語 MIDASHI と候補 CANDIDATE の頻度スコアを取得する。
エントリが存在しない場合は 0.0 を返す。"
  (let* ((key (cons midashi candidate))
         (entry (gethash key nskk-frequency-table)))
    (if entry
        (nskk-frequency-entry-score entry)
      0.0)))

;;;###autoload
(defun nskk-get-sorted-candidates (midashi candidates)
  "見出し語 MIDASHI の候補リスト CANDIDATES を頻度順にソートする。
CANDIDATES は文字列のリスト。
戻り値: 頻度順にソートされた候補リスト"
  (sort (copy-sequence candidates)
        (lambda (a b)
          (> (nskk-get-frequency-score midashi a)
             (nskk-get-frequency-score midashi b)))))

;;; スコア計算

(defun nskk-frequency--update-score (entry)
  "ENTRY のスコアを計算して更新する。"
  (let ((score (pcase nskk-frequency-algorithm
                 ('lru (nskk-frequency--calc-lru-score entry))
                 ('lfu (nskk-frequency--calc-lfu-score entry))
                 ('hybrid (nskk-frequency--calc-hybrid-score entry))
                 (_ (error "Unknown frequency algorithm: %s" nskk-frequency-algorithm)))))
    (setf (nskk-frequency-entry-score entry) score)))

(defun nskk-frequency--calc-lru-score (entry)
  "LRUスコアを計算する。
最近使用されたものほど高スコア。"
  (let* ((last-used (nskk-frequency-entry-last-used entry))
         (elapsed (float-time (time-since last-used))))
    ;; 経過時間が短いほど高スコア（指数関数的に減衰）
    (exp (- (/ elapsed 86400.0)))))

(defun nskk-frequency--calc-lfu-score (entry)
  "LFUスコアを計算する。
使用回数が多いものほど高スコア。"
  (float (nskk-frequency-entry-count entry)))

(defun nskk-frequency--calc-hybrid-score (entry)
  "ハイブリッドスコア（LRU + LFU）を計算する。"
  (+ (* nskk-frequency-lru-weight (nskk-frequency--calc-lru-score entry))
     (* nskk-frequency-lfu-weight (nskk-frequency--calc-lfu-score entry))))

;;; 頻度減衰

(defconst nskk-frequency--decay-min-interval 1.0
  "頻度減衰を実行する際に要求される最小経過秒数。")

(defun nskk-frequency--maybe-decay ()
  "必要に応じて頻度減衰を実行する。"
  (let* ((elapsed (float-time (time-since nskk-frequency-last-decay-time)))
         (threshold (max nskk-frequency-decay-interval
                          nskk-frequency--decay-min-interval)))
    (when (> elapsed threshold)
      (nskk-frequency--apply-decay)
      (setq nskk-frequency-last-decay-time (current-time)))))

(defun nskk-frequency--apply-decay ()
  "全エントリに頻度減衰を適用する。"
  (maphash
   (lambda (_key entry)
     (let ((new-count (* (nskk-frequency-entry-count entry)
                        nskk-frequency-decay-rate)))
       ;; カウントが1未満になったら削除対象
       (if (< new-count 1.0)
           (remhash (cons (nskk-frequency-entry-midashi entry)
                         (nskk-frequency-entry-candidate entry))
                   nskk-frequency-table)
         (setf (nskk-frequency-entry-count entry) (floor new-count))
         (nskk-frequency--update-score entry))))
   nskk-frequency-table))

;;; エントリ数制限

(defun nskk-frequency--check-max-entries ()
  "エントリ数が上限を超えた場合、低スコアのエントリを削除する。"
  (when (> (hash-table-count nskk-frequency-table)
           nskk-frequency-max-entries)
    (nskk-frequency--prune-entries)))

(defun nskk-frequency--prune-entries ()
  "低スコアのエントリを削除してエントリ数を制限内に収める。"
  (let ((entries nil))
    ;; 全エントリをリストに変換
    (maphash
     (lambda (key entry)
       (push (cons key (nskk-frequency-entry-score entry)) entries))
     nskk-frequency-table)

    ;; スコアでソート（降順）
    (setq entries (sort entries (lambda (a b) (> (cdr a) (cdr b)))))

    ;; 上位 nskk-frequency-max-entries * 0.9 個を残す
    (let ((keep-count (floor (* nskk-frequency-max-entries 0.9)))
          (new-table (make-hash-table :test 'equal)))
      (dolist (entry (seq-take entries keep-count))
        (puthash (car entry)
                (gethash (car entry) nskk-frequency-table)
                new-table))
      (setq nskk-frequency-table new-table))))

;;; 統計情報

;;;###autoload
(defun nskk-frequency-statistics ()
  "頻度学習の統計情報を返す。
戻り値: plist形式の統計情報"
  (let ((total-entries (hash-table-count nskk-frequency-table))
        (total-count 0)
        (avg-count 0.0)
        (max-count 0)
        (min-count most-positive-fixnum))

    (maphash
     (lambda (_key entry)
       (let ((count (nskk-frequency-entry-count entry)))
         (setq total-count (+ total-count count))
         (setq max-count (max max-count count))
         (setq min-count (min min-count count))))
     nskk-frequency-table)

    (when (> total-entries 0)
      (setq avg-count (/ (float total-count) total-entries)))

    (list :total-entries total-entries
          :total-count total-count
          :average-count avg-count
          :max-count max-count
          :min-count (if (= min-count most-positive-fixnum) 0 min-count)
          :algorithm nskk-frequency-algorithm
          :decay-enabled nskk-frequency-decay-enabled)))

;;;###autoload
(defun nskk-frequency-print-statistics ()
  "頻度学習の統計情報を表示する。"
  (interactive)
  (let ((stats (nskk-frequency-statistics)))
    (message "NSKK Frequency Learning Statistics:
  Total Entries: %d
  Total Count: %d
  Average Count: %.2f
  Max Count: %d
  Min Count: %d
  Algorithm: %s
  Decay Enabled: %s"
             (plist-get stats :total-entries)
             (plist-get stats :total-count)
             (plist-get stats :average-count)
             (plist-get stats :max-count)
             (plist-get stats :min-count)
             (plist-get stats :algorithm)
             (plist-get stats :decay-enabled))))

;;; クリーンアップ

;;;###autoload
(defun nskk-frequency-clear ()
  "全ての頻度データをクリアする。"
  (interactive)
  (clrhash nskk-frequency-table)
  (setq nskk-frequency-last-decay-time (current-time)))

;;;###autoload
(defun nskk-frequency-remove-entry (midashi candidate)
  "特定のエントリを削除する。"
  (remhash (cons midashi candidate) nskk-frequency-table))

;;; デバッグ関数

(defun nskk-frequency-get-top-entries (n)
  "上位 N 件の頻度エントリを取得する。
戻り値: ((midashi candidate score) ...) のリスト"
  (let ((entries nil))
    (maphash
     (lambda (key entry)
       (push (list (car key)
                  (cdr key)
                  (nskk-frequency-entry-score entry)
                  (nskk-frequency-entry-count entry))
             entries))
     nskk-frequency-table)

    (seq-take (sort entries (lambda (a b) (> (nth 2 a) (nth 2 b)))) n)))

(defun nskk-frequency-print-top-entries (&optional n)
  "上位 N 件（デフォルト10件）の頻度エントリを表示する。"
  (interactive "p")
  (let ((top-entries (nskk-frequency-get-top-entries (or n 10))))
    (message "Top %d Frequency Entries:" (length top-entries))
    (dolist (entry top-entries)
      (message "  %s -> %s (score: %.4f, count: %d)"
               (nth 0 entry)
               (nth 1 entry)
               (nth 2 entry)
               (nth 3 entry)))))

(provide 'nskk-learning-frequency)

;;; nskk-learning-frequency.el ends here
