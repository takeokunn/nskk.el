;;; nskk-completion-frequency.el --- Frequency-based completion for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, completion, frequency
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

;; このファイルは学習データに基づく頻度ベース補完を実装します。
;;
;; 特徴:
;; - 使用頻度統計に基づくランキング
;; - 時間減衰による最新性の反映
;; - 複数の頻度計算手法
;; - 学習データとの統合
;;
;; パフォーマンス目標:
;; - 頻度計算: < 1ms
;; - ランキング: < 5ms
;; - 統計更新: < 2ms
;;
;; 使用例:
;;
;;   (require 'nskk-completion-frequency)
;;
;;   ;; 頻度ベース補完
;;   (nskk-completion-frequency-search index "か")
;;   ;; => 頻度の高い候補から順に返す
;;
;;   ;; 時間減衰を考慮
;;   (nskk-completion-frequency-search index "か" :decay t)
;;   ;; => 最近使用した候補を優先
;;
;;   ;; 頻度統計の更新
;;   (nskk-completion-frequency-update-stats index "かんじ")

;;; Code:

(require 'cl-lib)
(require 'nskk-dict-struct)

;;; カスタマイズ変数

(defgroup nskk-completion-frequency nil
  "Frequency-based completion for NSKK."
  :group 'nskk
  :prefix "nskk-completion-frequency-")

(defcustom nskk-completion-frequency-default-limit 30
  "デフォルトの補完候補最大数。"
  :type 'integer
  :group 'nskk-completion-frequency)

(defcustom nskk-completion-frequency-decay-enabled t
  "非nilの場合、時間減衰を考慮する。"
  :type 'boolean
  :group 'nskk-completion-frequency)

(defcustom nskk-completion-frequency-decay-factor 0.95
  "時間減衰係数（0.0-1.0）。
値が小さいほど減衰が強い（古いデータの影響が小さくなる）。"
  :type 'float
  :group 'nskk-completion-frequency)

(defcustom nskk-completion-frequency-decay-unit 86400
  "時間減衰の単位（秒）。
デフォルトは86400秒（1日）。"
  :type 'integer
  :group 'nskk-completion-frequency)

(defcustom nskk-completion-frequency-method 'weighted
  "頻度計算方法。
  - 'simple: 単純な使用回数
  - 'weighted: 時間減衰を考慮した重み付け
  - 'lfu: LFU (Least Frequently Used)
  - 'lru: LRU (Least Recently Used) 風"
  :type '(choice (const :tag "Simple count" simple)
                 (const :tag "Weighted with decay" weighted)
                 (const :tag "LFU" lfu)
                 (const :tag "LRU-like" lru))
  :group 'nskk-completion-frequency)

;;; データ構造

(cl-defstruct (nskk-completion-frequency-result
               (:constructor nskk-completion-frequency-result--create)
               (:copier nil))
  "頻度ベース補完結果。

スロット:
  midashi        - 見出し語
  entry          - 辞書エントリ
  frequency      - 生の頻度
  adjusted-freq  - 調整後頻度（時間減衰等を考慮）
  rank           - ランク
  last-used      - 最終使用時刻"
  (midashi nil :type string)
  (entry nil :type nskk-dict-entry)
  (frequency 0 :type integer)
  (adjusted-freq 0.0 :type float)
  (rank 0 :type integer)
  (last-used nil :type (or null number)))

;;; メイン検索関数

;;;###autoload
(defun nskk-completion-frequency-search (index query &rest args)
  "頻度ベース補完検索を実行する。

引数:
  INDEX - nskk-dict-index構造体
  QUERY - 検索クエリ（部分一致または前方一致）

キーワード引数:
  :limit      - 最大結果数（デフォルト: nskk-completion-frequency-default-limit）
  :okuri-type - 送り仮名タイプ（'okuri-ari/'okuri-nasi/nil）
  :method     - 頻度計算方法（デフォルト: nskk-completion-frequency-method）
  :decay      - 時間減衰を有効化（デフォルト: nskk-completion-frequency-decay-enabled）
  :match-type - マッチタイプ（'prefix/'partial、デフォルト: 'prefix）

戻り値:
  nskk-completion-frequency-result構造体のリスト（頻度順）"
  (unless (stringp query)
    (error "Query must be a string: %s" query))

  (let* ((limit (or (plist-get args :limit)
                   nskk-completion-frequency-default-limit))
         (okuri-type (plist-get args :okuri-type))
         (method (or (plist-get args :method)
                    nskk-completion-frequency-method))
         (decay (if (plist-member args :decay)
                   (plist-get args :decay)
                 nskk-completion-frequency-decay-enabled))
         (match-type (or (plist-get args :match-type) 'prefix))
         (results nil)
         (tables (cond
                  ((eq okuri-type 'okuri-ari)
                   (list (nskk-dict-index-okuri-ari-table index)))
                  ((eq okuri-type 'okuri-nasi)
                   (list (nskk-dict-index-okuri-nasi-table index)))
                  (t
                   (list (nskk-dict-index-okuri-nasi-table index)
                         (nskk-dict-index-okuri-ari-table index))))))

    ;; 各テーブルから候補を収集
    (dolist (table tables)
      (maphash
       (lambda (midashi entry)
         (when (nskk-completion-frequency--match-p query midashi match-type)
           (let* ((freq (nskk-dict-entry-frequency entry))
                  (last-used (nskk-dict-entry-last-used entry))
                  (adjusted (nskk-completion-frequency--calculate-adjusted-frequency
                            freq last-used method decay)))
             (push (nskk-completion-frequency-result--create
                    :midashi midashi
                    :entry entry
                    :frequency freq
                    :adjusted-freq adjusted
                    :last-used last-used)
                   results))))
       table))

    ;; 調整後頻度でソート
    (setq results (sort results
                       (lambda (a b)
                         (> (nskk-completion-frequency-result-adjusted-freq a)
                            (nskk-completion-frequency-result-adjusted-freq b)))))

    ;; ランクを設定
    (let ((rank 1))
      (dolist (result results)
        (setf (nskk-completion-frequency-result-rank result) rank)
        (setq rank (1+ rank))))

    ;; limit適用
    (when (and limit (> (length results) limit))
      (setq results (seq-take results limit)))

    results))

;;; マッチング

(defun nskk-completion-frequency--match-p (query midashi match-type)
  "クエリと見出し語がマッチするか判定する（内部関数）。

引数:
  QUERY      - 検索クエリ
  MIDASHI    - 見出し語
  MATCH-TYPE - マッチタイプ（'prefix/'partial）

戻り値:
  マッチする場合t、しない場合nil"
  (pcase match-type
    ('prefix
     (string-prefix-p query midashi))
    ('partial
     (string-match-p (regexp-quote query) midashi))
    (_
     (string-prefix-p query midashi))))

;;; 頻度計算

(defun nskk-completion-frequency--calculate-adjusted-frequency (freq last-used method decay)
  "調整後頻度を計算する（内部関数）。

引数:
  FREQ      - 生の頻度
  LAST-USED - 最終使用時刻（float-time形式、nilの場合は未使用）
  METHOD    - 頻度計算方法
  DECAY     - 時間減衰を有効化するか

戻り値:
  調整後頻度（浮動小数点数）"
  (pcase method
    ('simple
     ;; 単純な使用回数
     (float freq))

    ('weighted
     ;; 時間減衰を考慮した重み付け
     (if (and decay last-used)
         (nskk-completion-frequency--apply-time-decay freq last-used)
       (float freq)))

    ('lfu
     ;; LFU: 頻度のみを考慮（時間は無視）
     (float freq))

    ('lru
     ;; LRU風: 最終使用時刻を重視
     (if last-used
         (let ((elapsed (- (float-time) last-used)))
           ;; 最近使用したほど高スコア
           (+ (* freq 0.3)
              (* (/ 1.0 (+ 1.0 (/ elapsed 86400.0))) 100.0)))
       (* freq 0.3)))

    (_
     (float freq))))

(defun nskk-completion-frequency--apply-time-decay (freq last-used)
  "時間減衰を適用する（内部関数）。

引数:
  FREQ      - 生の頻度
  LAST-USED - 最終使用時刻（float-time形式）

処理:
  経過時間に基づいて頻度を減衰させる
  adjusted = freq * (decay-factor ^ (elapsed / decay-unit))

戻り値:
  減衰後頻度（浮動小数点数）"
  (if (null last-used)
      (float freq)
    (let* ((now (float-time))
           (elapsed (- now last-used))
           (units (/ elapsed nskk-completion-frequency-decay-unit))
           (decay-multiplier (expt nskk-completion-frequency-decay-factor units)))
      (* freq decay-multiplier))))

;;; 統計更新

;;;###autoload
(defun nskk-completion-frequency-update-stats (index midashi &optional okuri-type)
  "見出し語の使用統計を更新する。

引数:
  INDEX      - nskk-dict-index構造体
  MIDASHI    - 見出し語
  OKURI-TYPE - 送り仮名タイプ（'okuri-ari/'okuri-nasi、省略時は自動判定）

処理:
  1. エントリを検索
  2. 頻度をインクリメント
  3. 最終使用時刻を更新

戻り値:
  更新成功時t、エントリが見つからない場合nil"
  (let* ((okuri (or okuri-type
                   (nskk-completion-frequency--infer-okuri-type midashi)))
         (table (if (eq okuri 'okuri-ari)
                   (nskk-dict-index-okuri-ari-table index)
                 (nskk-dict-index-okuri-nasi-table index)))
         (entry (gethash midashi table)))
    (when entry
      ;; 頻度をインクリメント
      (cl-incf (nskk-dict-entry-frequency entry))
      ;; 最終使用時刻を更新
      (setf (nskk-dict-entry-last-used entry) (float-time))
      t)))

(defun nskk-completion-frequency--infer-okuri-type (midashi)
  "見出し語から送り仮名タイプを推測する（内部関数）。

引数:
  MIDASHI - 見出し語

戻り値:
  'okuri-ari または 'okuri-nasi"
  (if (and (> (length midashi) 0)
           (let ((last-char (aref midashi (1- (length midashi)))))
             (and (>= last-char ?a) (<= last-char ?z))))
      'okuri-ari
    'okuri-nasi))

;;; 頻度統計分析

;;;###autoload
(defun nskk-completion-frequency-top-entries (index n &optional okuri-type)
  "頻度の高いエントリをN件取得する。

引数:
  INDEX      - nskk-dict-index構造体
  N          - 取得件数
  OKURI-TYPE - 送り仮名タイプ（'okuri-ari/'okuri-nasi/nil）

戻り値:
  ((midashi . frequency) ...) のリスト"
  (let ((entries nil)
        (tables (cond
                 ((eq okuri-type 'okuri-ari)
                  (list (nskk-dict-index-okuri-ari-table index)))
                 ((eq okuri-type 'okuri-nasi)
                  (list (nskk-dict-index-okuri-nasi-table index)))
                 (t
                  (list (nskk-dict-index-okuri-nasi-table index)
                        (nskk-dict-index-okuri-ari-table index))))))

    ;; 全エントリを収集
    (dolist (table tables)
      (maphash (lambda (midashi entry)
                (push (cons midashi (nskk-dict-entry-frequency entry)) entries))
              table))

    ;; 頻度でソート
    (setq entries (sort entries (lambda (a b) (> (cdr a) (cdr b)))))

    ;; 上位N件を返す
    (seq-take entries n)))

;;;###autoload
(defun nskk-completion-frequency-statistics (index &optional okuri-type)
  "頻度統計情報を取得する。

引数:
  INDEX      - nskk-dict-index構造体
  OKURI-TYPE - 送り仮名タイプ（'okuri-ari/'okuri-nasi/nil）

戻り値:
  plist形式の統計情報
    :total-entries     - 総エントリ数
    :used-entries      - 使用されたエントリ数
    :total-frequency   - 総使用頻度
    :avg-frequency     - 平均頻度
    :max-frequency     - 最大頻度
    :median-frequency  - 中央値頻度"
  (let ((frequencies nil)
        (used-count 0)
        (total-freq 0)
        (tables (cond
                 ((eq okuri-type 'okuri-ari)
                  (list (nskk-dict-index-okuri-ari-table index)))
                 ((eq okuri-type 'okuri-nasi)
                  (list (nskk-dict-index-okuri-nasi-table index)))
                 (t
                  (list (nskk-dict-index-okuri-nasi-table index)
                        (nskk-dict-index-okuri-ari-table index))))))

    ;; 頻度を収集
    (dolist (table tables)
      (maphash (lambda (_ entry)
                (let ((freq (nskk-dict-entry-frequency entry)))
                  (push freq frequencies)
                  (setq total-freq (+ total-freq freq))
                  (when (> freq 0)
                    (setq used-count (1+ used-count)))))
              table))

    (let* ((total-entries (length frequencies))
           (avg-freq (if (> total-entries 0)
                        (/ (float total-freq) total-entries)
                      0.0))
           (sorted-freqs (sort frequencies #'>))
           (max-freq (if sorted-freqs (car sorted-freqs) 0))
           (median-freq (if sorted-freqs
                           (nth (/ (length sorted-freqs) 2) sorted-freqs)
                         0)))

      (list :total-entries total-entries
            :used-entries used-count
            :total-frequency total-freq
            :avg-frequency avg-freq
            :max-frequency max-freq
            :median-frequency median-freq))))

;;; ユーティリティ関数

(defun nskk-completion-frequency-to-simple-list (results)
  "頻度ベース補完結果を単純なリストに変換する。

引数:
  RESULTS - nskk-completion-frequency-result構造体のリスト

戻り値:
  ((midashi . entry) ...) のリスト"
  (mapcar (lambda (result)
           (cons (nskk-completion-frequency-result-midashi result)
                 (nskk-completion-frequency-result-entry result)))
          results))

(defun nskk-completion-frequency-to-string-list (results)
  "頻度ベース補完結果を文字列リストに変換する。

引数:
  RESULTS - nskk-completion-frequency-result構造体のリスト

戻り値:
  見出し語の文字列リスト"
  (mapcar #'nskk-completion-frequency-result-midashi results))

;;; デバッグ用関数

(defun nskk-completion-frequency-debug-info (index query)
  "頻度ベース補完検索のデバッグ情報を表示する。

引数:
  INDEX - nskk-dict-index構造体
  QUERY - 検索クエリ

戻り値:
  plist形式のデバッグ情報"
  (let* ((start-time (float-time))
         (results (nskk-completion-frequency-search index query))
         (elapsed-time (- (float-time) start-time)))
    (list :query query
          :result-count (length results)
          :elapsed-time elapsed-time
          :method nskk-completion-frequency-method
          :decay-enabled nskk-completion-frequency-decay-enabled
          :results (mapcar (lambda (r)
                            (list :midashi (nskk-completion-frequency-result-midashi r)
                                  :frequency (nskk-completion-frequency-result-frequency r)
                                  :adjusted-freq (nskk-completion-frequency-result-adjusted-freq r)
                                  :rank (nskk-completion-frequency-result-rank r)))
                          results))))

(provide 'nskk-completion-frequency)

;;; nskk-completion-frequency.el ends here
