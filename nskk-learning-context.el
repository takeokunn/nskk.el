;;; nskk-learning-context.el --- Context learning engine for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, learning, context
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

;; このファイルは文脈学習エンジンを実装します。
;;
;; 特徴:
;; - バイグラム学習（2-gram: 前1語との関係）
;; - トライグラム学習（3-gram: 前2語との関係）
;; - 文脈スコアリング
;; - パターン認識
;;
;; アルゴリズム:
;; - N-gram統計モデル
;; - 条件付き確率の計算
;; - スムージング（未知パターンへの対応）
;;
;; データ構造:
;; - バイグラム: (prev-word . current-word) -> count
;; - トライグラム: ((prev-prev-word . prev-word) . current-word) -> count
;;
;; 使用例:
;; (nskk-learn-context "私" "は")
;; (nskk-predict-next-word "私")

;;; Code:

(require 'cl-lib)

;;; カスタマイズ変数

(defgroup nskk-learning-context nil
  "Context learning engine for NSKK."
  :group 'nskk
  :prefix "nskk-context-")

(defcustom nskk-context-enable-bigram t
  "非nilの場合、バイグラム学習を有効にする。"
  :type 'boolean
  :group 'nskk-learning-context)

(defcustom nskk-context-enable-trigram t
  "非nilの場合、トライグラム学習を有効にする。"
  :type 'boolean
  :group 'nskk-learning-context)

(defcustom nskk-context-max-bigrams 50000
  "バイグラムの最大保存数。"
  :type 'integer
  :group 'nskk-learning-context)

(defcustom nskk-context-max-trigrams 50000
  "トライグラムの最大保存数。"
  :type 'integer
  :group 'nskk-learning-context)

(defcustom nskk-context-smoothing-factor 0.1
  "スムージング係数（0.0 - 1.0）。
未知のパターンに対する確率の調整。"
  :type 'float
  :group 'nskk-learning-context)

(defcustom nskk-context-bigram-weight 0.4
  "文脈スコア計算時のバイグラムの重み。"
  :type 'float
  :group 'nskk-learning-context)

(defcustom nskk-context-trigram-weight 0.6
  "文脈スコア計算時のトライグラムの重み。"
  :type 'float
  :group 'nskk-learning-context)

(defcustom nskk-context-min-count 2
  "パターン認識に使用する最小出現回数。"
  :type 'integer
  :group 'nskk-learning-context)

;;; データ構造

(cl-defstruct (nskk-context-entry
               (:constructor nskk-context-entry--create)
               (:copier nil))
  "文脈エントリ。

スロット:
  pattern    - パターン（文字列またはcons）
  count      - 出現回数
  probability - 計算された確率（キャッシュ用）"
  (pattern nil)
  (count 0 :type integer)
  (probability 0.0 :type float))

;;; グローバル変数

(defvar nskk-context-bigram-table (make-hash-table :test 'equal)
  "バイグラムデータを保持するハッシュテーブル。
キー: (prev-word . current-word)
値: count")

(defvar nskk-context-trigram-table (make-hash-table :test 'equal)
  "トライグラムデータを保持するハッシュテーブル。
キー: ((prev-prev-word . prev-word) . current-word)
値: count")

(defvar nskk-context-unigram-table (make-hash-table :test 'equal)
  "ユニグラム（単語出現頻度）データを保持するハッシュテーブル。
キー: word
値: count")

(defvar nskk-context-total-bigrams 0
  "バイグラムの総出現回数。")

(defvar nskk-context-total-trigrams 0
  "トライグラムの総出現回数。")

(defvar nskk-context-total-unigrams 0
  "ユニグラムの総出現回数。")

(defvar nskk-context-history nil
  "最近の変換履歴（文脈学習用）。
形式: (word1 word2 word3 ...) 最大3単語を保持")

;;; 文脈学習

;;;###autoload
(defun nskk-learn-context (prev current)
  "前の単語 PREV と現在の単語 CURRENT の文脈を学習する。
PREV が nil の場合は、CURRENT のみをユニグラムとして学習。"
  (unless (stringp current)
    (error "Current word must be a string"))

  ;; ユニグラム学習
  (nskk-context--update-unigram current)

  ;; バイグラム学習
  (when (and prev (stringp prev) nskk-context-enable-bigram)
    (nskk-context--update-bigram prev current))

  ;; トライグラム学習
  (when (and nskk-context-enable-trigram
             (>= (length nskk-context-history) 2))
    (let ((prev-prev (nth 1 nskk-context-history))
          (prev-word (nth 0 nskk-context-history)))
      (when (and prev-prev prev-word)
        (nskk-context--update-trigram prev-prev prev-word current))))

  ;; 履歴更新（最大3単語）
  (push current nskk-context-history)
  (when (> (length nskk-context-history) 3)
    (setq nskk-context-history (seq-take nskk-context-history 3)))

  ;; エントリ数制限チェック
  (nskk-context--check-max-entries))

(defun nskk-context--update-unigram (word)
  "ユニグラムを更新する。"
  (let ((count (gethash word nskk-context-unigram-table 0)))
    (puthash word (1+ count) nskk-context-unigram-table)
    (cl-incf nskk-context-total-unigrams)))

(defun nskk-context--update-bigram (prev current)
  "バイグラムを更新する。"
  (let* ((key (cons prev current))
         (count (gethash key nskk-context-bigram-table 0)))
    (puthash key (1+ count) nskk-context-bigram-table)
    (cl-incf nskk-context-total-bigrams)))

(defun nskk-context--update-trigram (prev-prev prev current)
  "トライグラムを更新する。"
  (let* ((key (cons (cons prev-prev prev) current))
         (count (gethash key nskk-context-trigram-table 0)))
    (puthash key (1+ count) nskk-context-trigram-table)
    (cl-incf nskk-context-total-trigrams)))

;;; 文脈スコアリング

;;;###autoload
(defun nskk-context-score (midashi candidate)
  "見出し語 MIDASHI と候補 CANDIDATE の文脈スコアを計算する。
現在の文脈（履歴）に基づいてスコアを返す。"
  (let ((bigram-score 0.0)
        (trigram-score 0.0))

    ;; バイグラムスコア
    (when (and nskk-context-enable-bigram
               (>= (length nskk-context-history) 1))
      (let ((prev (nth 0 nskk-context-history)))
        (setq bigram-score (nskk-context--bigram-probability prev candidate))))

    ;; トライグラムスコア
    (when (and nskk-context-enable-trigram
               (>= (length nskk-context-history) 2))
      (let ((prev-prev (nth 1 nskk-context-history))
            (prev (nth 0 nskk-context-history)))
        (setq trigram-score (nskk-context--trigram-probability prev-prev prev candidate))))

    ;; 重み付き合計
    (+ (* nskk-context-bigram-weight bigram-score)
       (* nskk-context-trigram-weight trigram-score))))

(defun nskk-context--bigram-probability (prev current)
  "バイグラム (PREV . CURRENT) の条件付き確率を計算する。
P(current | prev)"
  (let* ((key (cons prev current))
         (bigram-count (gethash key nskk-context-bigram-table 0))
         (prev-count (gethash prev nskk-context-unigram-table 0)))
    (if (> prev-count 0)
        ;; スムージング適用
        (/ (+ bigram-count nskk-context-smoothing-factor)
           (+ prev-count (* nskk-context-smoothing-factor
                           (hash-table-count nskk-context-unigram-table))))
      ;; 前の単語が未知の場合
      nskk-context-smoothing-factor)))

(defun nskk-context--trigram-probability (prev-prev prev current)
  "トライグラム ((PREV-PREV . PREV) . CURRENT) の条件付き確率を計算する。
P(current | prev-prev, prev)"
  (let* ((key (cons (cons prev-prev prev) current))
         (trigram-count (gethash key nskk-context-trigram-table 0))
         (bigram-key (cons prev-prev prev))
         (bigram-count (gethash bigram-key nskk-context-bigram-table 0)))
    (if (> bigram-count 0)
        ;; スムージング適用
        (/ (+ trigram-count nskk-context-smoothing-factor)
           (+ bigram-count (* nskk-context-smoothing-factor
                             (hash-table-count nskk-context-unigram-table))))
      ;; バイグラムが未知の場合
      nskk-context-smoothing-factor)))

;;; 予測機能

;;;###autoload
(defun nskk-predict-next-words (&optional n)
  "現在の文脈から次の単語を予測する。
N 件（デフォルト5件）の候補を返す。
戻り値: ((word . probability) ...) のリスト"
  (let ((candidates (make-hash-table :test 'equal)))

    ;; バイグラムから予測
    (when (and nskk-context-enable-bigram
               (>= (length nskk-context-history) 1))
      (let ((prev (nth 0 nskk-context-history)))
        (maphash
         (lambda (key count)
           (when (and (equal (car key) prev)
                     (>= count nskk-context-min-count))
             (let ((word (cdr key)))
               (puthash word
                       (+ (gethash word candidates 0.0)
                          (* nskk-context-bigram-weight
                             (nskk-context--bigram-probability prev word)))
                       candidates))))
         nskk-context-bigram-table)))

    ;; トライグラムから予測
    (when (and nskk-context-enable-trigram
               (>= (length nskk-context-history) 2))
      (let ((prev-prev (nth 1 nskk-context-history))
            (prev (nth 0 nskk-context-history)))
        (maphash
         (lambda (key count)
           (when (and (equal (caar key) prev-prev)
                     (equal (cdar key) prev)
                     (>= count nskk-context-min-count))
             (let ((word (cdr key)))
               (puthash word
                       (+ (gethash word candidates 0.0)
                          (* nskk-context-trigram-weight
                             (nskk-context--trigram-probability prev-prev prev word)))
                       candidates))))
         nskk-context-trigram-table)))

    ;; スコア順にソート
    (let ((result nil))
      (maphash (lambda (word prob) (push (cons word prob) result)) candidates)
      (seq-take (sort result (lambda (a b) (> (cdr a) (cdr b))))
                (or n 5)))))

;;; パターン認識

;;;###autoload
(defun nskk-context-find-patterns (word &optional min-count)
  "WORD に関連するパターンを検出する。
MIN-COUNT 回以上出現したパターンのみを返す。
戻り値: ((pattern . count) ...) のリスト"
  (let ((patterns nil)
        (threshold (or min-count nskk-context-min-count)))

    ;; バイグラムパターン（WORD が後続）
    (maphash
     (lambda (key count)
       (when (and (equal (cdr key) word)
                 (>= count threshold))
         (push (cons (format "%s -> %s" (car key) word) count) patterns)))
     nskk-context-bigram-table)

    ;; バイグラムパターン（WORD が先行）
    (maphash
     (lambda (key count)
       (when (and (equal (car key) word)
                 (>= count threshold))
         (push (cons (format "%s -> %s" word (cdr key)) count) patterns)))
     nskk-context-bigram-table)

    ;; カウント順にソート
    (sort patterns (lambda (a b) (> (cdr a) (cdr b))))))

;;; エントリ数制限

(defun nskk-context--check-max-entries ()
  "エントリ数が上限を超えた場合、低頻度エントリを削除する。"
  (when (> (hash-table-count nskk-context-bigram-table)
           nskk-context-max-bigrams)
    (nskk-context--prune-table nskk-context-bigram-table
                               (floor (* nskk-context-max-bigrams 0.9))))

  (when (> (hash-table-count nskk-context-trigram-table)
           nskk-context-max-trigrams)
    (nskk-context--prune-table nskk-context-trigram-table
                               (floor (* nskk-context-max-trigrams 0.9)))))

(defun nskk-context--prune-table (table keep-count)
  "TABLE から低頻度エントリを削除して KEEP-COUNT 個に制限する。"
  (let ((entries nil))
    ;; 全エントリをリストに変換
    (maphash (lambda (key count) (push (cons key count) entries)) table)

    ;; カウントでソート（降順）
    (setq entries (sort entries (lambda (a b) (> (cdr a) (cdr b)))))

    ;; 新しいテーブルを作成
    (clrhash table)
    (dolist (entry (seq-take entries keep-count))
      (puthash (car entry) (cdr entry) table))))

;;; 統計情報

;;;###autoload
(defun nskk-context-statistics ()
  "文脈学習の統計情報を返す。
戻り値: plist形式の統計情報"
  (list :bigram-entries (hash-table-count nskk-context-bigram-table)
        :trigram-entries (hash-table-count nskk-context-trigram-table)
        :unigram-entries (hash-table-count nskk-context-unigram-table)
        :total-bigrams nskk-context-total-bigrams
        :total-trigrams nskk-context-total-trigrams
        :total-unigrams nskk-context-total-unigrams
        :history-length (length nskk-context-history)
        :bigram-enabled nskk-context-enable-bigram
        :trigram-enabled nskk-context-enable-trigram))

;;;###autoload
(defun nskk-context-print-statistics ()
  "文脈学習の統計情報を表示する。"
  (interactive)
  (let ((stats (nskk-context-statistics)))
    (message "NSKK Context Learning Statistics:
  Unigram Entries: %d (Total: %d)
  Bigram Entries: %d (Total: %d)
  Trigram Entries: %d (Total: %d)
  History Length: %d
  Bigram Enabled: %s
  Trigram Enabled: %s"
             (plist-get stats :unigram-entries)
             (plist-get stats :total-unigrams)
             (plist-get stats :bigram-entries)
             (plist-get stats :total-bigrams)
             (plist-get stats :trigram-entries)
             (plist-get stats :total-trigrams)
             (plist-get stats :history-length)
             (plist-get stats :bigram-enabled)
             (plist-get stats :trigram-enabled))
    nil))

;;; クリーンアップ

;;;###autoload
(defun nskk-context-clear ()
  "全ての文脈データをクリアする。"
  (interactive)
  (clrhash nskk-context-bigram-table)
  (clrhash nskk-context-trigram-table)
  (clrhash nskk-context-unigram-table)
  (setq nskk-context-total-bigrams 0)
  (setq nskk-context-total-trigrams 0)
  (setq nskk-context-total-unigrams 0)
  (setq nskk-context-history nil))

;;;###autoload
(defun nskk-context-clear-history ()
  "文脈履歴のみをクリアする。"
  (interactive)
  (setq nskk-context-history nil))

(provide 'nskk-learning-context)

;;; nskk-learning-context.el ends here
