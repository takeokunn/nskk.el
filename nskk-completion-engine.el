;;; nskk-completion-engine.el --- Unified completion engine for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, completion, engine
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

;; このファイルは複数の補完アルゴリズムを統合する補完エンジンを実装します。
;;
;; 統合アルゴリズム:
;; - 前方一致補完 (nskk-completion-prefix)
;; - 曖昧補完 (nskk-completion-fuzzy)
;; - 頻度ベース補完 (nskk-completion-frequency)
;; - 文脈補完 (nskk-completion-context)
;; - 予測補完 (nskk-completion-predictive)
;;
;; 特徴:
;; - 複数アルゴリズムの並列実行
;; - スコア集約と正規化
;; - ランキング最適化
;; - アルゴリズムの重み付け
;;
;; パフォーマンス目標:
;; - 統合補完応答: < 20ms
;; - スコア集約: < 5ms
;; - メモリ効率: 結果の重複除去
;;
;; 使用例:
;;
;;   (require 'nskk-completion-engine)
;;
;;   ;; 統合補完検索
;;   (nskk-completion-engine-search index "かん" :context '("これは"))
;;   ;; => 全アルゴリズムの結果を統合
;;
;;   ;; アルゴリズム選択
;;   (nskk-completion-engine-search index "か"
;;     :algorithms '(prefix frequency context))
;;
;;   ;; 重み付け調整
;;   (nskk-completion-engine-search index "か"
;;     :weights '((prefix . 0.3) (frequency . 0.4) (context . 0.3)))

;;; Code:

(require 'cl-lib)
(require 'nskk-dict-struct)
(require 'nskk-completion-prefix)
(require 'nskk-completion-fuzzy)
(require 'nskk-completion-frequency)
(require 'nskk-completion-context)
(require 'nskk-completion-predictive)

;;; カスタマイズ変数

(defgroup nskk-completion-engine nil
  "Unified completion engine for NSKK."
  :group 'nskk
  :prefix "nskk-completion-engine-")

(defcustom nskk-completion-engine-default-limit 30
  "デフォルトの補完候補最大数。"
  :type 'integer
  :group 'nskk-completion-engine)

(defcustom nskk-completion-engine-algorithms '(prefix frequency context)
  "使用する補完アルゴリズムのリスト。
  - prefix: 前方一致補完
  - fuzzy: 曖昧補完
  - frequency: 頻度ベース補完
  - context: 文脈補完
  - predictive: 予測補完"
  :type '(set (const :tag "Prefix matching" prefix)
              (const :tag "Fuzzy matching" fuzzy)
              (const :tag "Frequency-based" frequency)
              (const :tag "Context-aware" context)
              (const :tag "Predictive" predictive))
  :group 'nskk-completion-engine)

(defcustom nskk-completion-engine-default-weights
  '((prefix . 0.25)
    (fuzzy . 0.15)
    (frequency . 0.25)
    (context . 0.25)
    (predictive . 0.10))
  "各アルゴリズムのデフォルト重み（合計1.0）。"
  :type '(alist :key-type symbol :value-type float)
  :group 'nskk-completion-engine)

(defcustom nskk-completion-engine-score-method 'weighted-sum
  "スコア集約方法。
  - 'weighted-sum: 重み付き合計
  - 'max: 最大スコア
  - 'voting: 投票方式（出現回数）"
  :type '(choice (const :tag "Weighted sum" weighted-sum)
                 (const :tag "Maximum score" max)
                 (const :tag "Voting" voting))
  :group 'nskk-completion-engine)

;;; データ構造

(cl-defstruct (nskk-completion-engine-result
               (:constructor nskk-completion-engine-result--create)
               (:copier nil))
  "統合補完結果。

スロット:
  midashi       - 見出し語
  entry         - 辞書エントリ
  total-score   - 総合スコア
  algorithm-scores - 各アルゴリズムのスコア（alist）
  rank          - ランク"
  (midashi nil :type string)
  (entry nil :type nskk-dict-entry)
  (total-score 0.0 :type float)
  (algorithm-scores nil :type list)
  (rank 0 :type integer))

;;; メイン検索関数

;;;###autoload
(defun nskk-completion-engine-search (index query &rest args)
  "統合補完検索を実行する。

引数:
  INDEX - nskk-dict-index構造体
  QUERY - 検索クエリ

キーワード引数:
  :context       - 文脈（直前の単語のリスト）
  :limit         - 最大結果数
  :okuri-type    - 送り仮名タイプ
  :algorithms    - 使用するアルゴリズム（省略時はnskk-completion-engine-algorithms）
  :weights       - 各アルゴリズムの重み（省略時はdefault-weights）
  :score-method  - スコア集約方法
  :context-model - 文脈モデル（文脈補完用）
  :pred-model    - 予測モデル（予測補完用）

戻り値:
  nskk-completion-engine-result構造体のリスト（スコア順）

パフォーマンス: < 20ms（目標）"
  (unless (stringp query)
    (error "Query must be a string: %s" query))

  (let* ((context (plist-get args :context))
         (limit (or (plist-get args :limit)
                   nskk-completion-engine-default-limit))
         (okuri-type (plist-get args :okuri-type))
         (algorithms (or (plist-get args :algorithms)
                        nskk-completion-engine-algorithms))
         (weights (or (plist-get args :weights)
                     nskk-completion-engine-default-weights))
         (score-method (or (plist-get args :score-method)
                          nskk-completion-engine-score-method))
         (context-model (plist-get args :context-model))
         (pred-model (plist-get args :pred-model))
         (all-results (make-hash-table :test 'equal)))

    ;; 各アルゴリズムを実行
    (dolist (algo algorithms)
      (let ((algo-results (nskk-completion-engine--run-algorithm
                          algo index query
                          :context context
                          :okuri-type okuri-type
                          :context-model context-model
                          :pred-model pred-model)))
        ;; 結果を統合
        (nskk-completion-engine--merge-results
         all-results algo algo-results weights)))

    ;; スコアを集約してソート
    (let ((final-results (nskk-completion-engine--aggregate-scores
                         all-results weights score-method)))

      ;; ランクを設定
      (let ((rank 1))
        (dolist (result final-results)
          (setf (nskk-completion-engine-result-rank result) rank)
          (setq rank (1+ rank))))

      ;; limit適用
      (when (and limit (> (length final-results) limit))
        (setq final-results (seq-take final-results limit)))

      final-results)))

;;; アルゴリズム実行

(defun nskk-completion-engine--run-algorithm (algo index query &rest args)
  "指定されたアルゴリズムを実行する（内部関数）。

引数:
  ALGO  - アルゴリズム名（シンボル）
  INDEX - 辞書インデックス
  QUERY - 検索クエリ

キーワード引数:
  :context       - 文脈
  :okuri-type    - 送り仮名タイプ
  :context-model - 文脈モデル
  :pred-model    - 予測モデル

戻り値:
  ((midashi . score) ...) のリスト"
  (let ((context (plist-get args :context))
        (okuri-type (plist-get args :okuri-type))
        (context-model (plist-get args :context-model))
        (pred-model (plist-get args :pred-model)))

    (pcase algo
      ('prefix
       (let ((results (nskk-completion-prefix-search
                      index query
                      :okuri-type okuri-type
                      :limit nil)))
         (mapcar (lambda (r)
                  (cons (nskk-completion-prefix-result-midashi r)
                        ;; スコアを正規化（0.0-1.0）
                        (/ (float (nskk-completion-prefix-result-score r))
                           (+ (nskk-completion-prefix-result-score r) 10.0))))
                results)))

      ('fuzzy
       (let ((results (nskk-completion-fuzzy-search
                      index query
                      :okuri-type okuri-type
                      :limit nil)))
         (mapcar (lambda (r)
                  (cons (nskk-completion-fuzzy-result-midashi r)
                        ;; 距離を類似度に変換（距離が小さいほど高スコア）
                        (/ 1.0 (+ (nskk-completion-fuzzy-result-distance r) 1.0))))
                results)))

      ('frequency
       (let ((results (nskk-completion-frequency-search
                      index query
                      :okuri-type okuri-type
                      :limit nil)))
         (mapcar (lambda (r)
                  (cons (nskk-completion-frequency-result-midashi r)
                        ;; 調整後頻度を正規化
                        (min 1.0 (/ (nskk-completion-frequency-result-adjusted-freq r)
                                   100.0))))
                results)))

      ('context
       (if context-model
           (let ((results (nskk-completion-context-search
                          index query
                          :context context
                          :model context-model
                          :okuri-type okuri-type
                          :limit nil)))
             (mapcar (lambda (r)
                      (cons (nskk-completion-context-result-midashi r)
                            (nskk-completion-context-result-context-score r)))
                    results))
         nil))

      ('predictive
       (if (and pred-model context)
           (let ((results (nskk-completion-predictive-search
                          index pred-model
                          :context context
                          :okuri-type okuri-type
                          :limit nil)))
             (mapcar (lambda (r)
                      (cons (nskk-completion-predictive-result-midashi r)
                            (nskk-completion-predictive-result-probability r)))
                    results))
         nil))

      (_
       nil))))

;;; 結果統合

(defun nskk-completion-engine--merge-results (all-results algo algo-results weights)
  "アルゴリズムの結果を統合する（内部関数）。

引数:
  ALL-RESULTS  - 統合結果（ハッシュテーブル、見出し語→スコアalist）
  ALGO         - アルゴリズム名
  ALGO-RESULTS - アルゴリズムの結果
  WEIGHTS      - 重みテーブル"
  (dolist (result algo-results)
    (let* ((midashi (car result))
           (score (cdr result))
           (current (gethash midashi all-results)))
      (if current
          ;; 既存のエントリに追加
          (puthash midashi
                  (cons (cons algo score) current)
                  all-results)
        ;; 新規エントリ
        (puthash midashi
                (list (cons algo score))
                all-results)))))

;;; スコア集約

(defun nskk-completion-engine--aggregate-scores (all-results weights score-method)
  "スコアを集約してソートする（内部関数）。

引数:
  ALL-RESULTS  - 統合結果（ハッシュテーブル）
  WEIGHTS      - 重みテーブル
  SCORE-METHOD - スコア集約方法

戻り値:
  nskk-completion-engine-result構造体のリスト（スコア順）"
  (let ((results nil))
    (maphash
     (lambda (midashi algo-scores)
       (let ((total-score (nskk-completion-engine--calculate-total-score
                          algo-scores weights score-method)))
         (push (nskk-completion-engine-result--create
                :midashi midashi
                :entry nil  ; エントリは後で取得
                :total-score total-score
                :algorithm-scores algo-scores)
               results)))
     all-results)

    ;; スコアでソート
    (sort results
          (lambda (a b)
            (> (nskk-completion-engine-result-total-score a)
               (nskk-completion-engine-result-total-score b))))))

(defun nskk-completion-engine--calculate-total-score (algo-scores weights score-method)
  "総合スコアを計算する（内部関数）。

引数:
  ALGO-SCORES  - 各アルゴリズムのスコア（alist）
  WEIGHTS      - 重みテーブル
  SCORE-METHOD - スコア集約方法

戻り値:
  総合スコア（浮動小数点数）"
  (pcase score-method
    ('weighted-sum
     ;; 重み付き合計
     (let ((total 0.0)
           (total-weight 0.0))
       (dolist (pair algo-scores)
         (let* ((algo (car pair))
                (score (cdr pair))
                (weight (or (cdr (assq algo weights)) 0.0)))
           (setq total (+ total (* score weight)))
           (setq total-weight (+ total-weight weight))))
       (if (> total-weight 0)
           (/ total total-weight)
         0.0)))

    ('max
     ;; 最大スコア
     (let ((max-score 0.0))
       (dolist (pair algo-scores)
         (let ((score (cdr pair)))
           (when (> score max-score)
             (setq max-score score))))
       max-score))

    ('voting
     ;; 投票方式（アルゴリズム数に基づく）
     (/ (float (length algo-scores)) (length weights)))

    (_
     0.0)))

;;; 辞書エントリの取得

;;;###autoload
(defun nskk-completion-engine-populate-entries (index results &optional okuri-type)
  "補完結果に辞書エントリを設定する。

引数:
  INDEX      - 辞書インデックス
  RESULTS    - nskk-completion-engine-result構造体のリスト
  OKURI-TYPE - 送り仮名タイプ

戻り値:
  エントリを設定した結果リスト"
  (dolist (result results)
    (let* ((midashi (nskk-completion-engine-result-midashi result))
           (entry (nskk-dict-struct-lookup index midashi okuri-type)))
      (setf (nskk-completion-engine-result-entry result) entry)))
  results)

;;; ユーティリティ関数

(defun nskk-completion-engine-to-simple-list (results)
  "統合補完結果を単純なリストに変換する。

引数:
  RESULTS - nskk-completion-engine-result構造体のリスト

戻り値:
  ((midashi . entry) ...) のリスト"
  (mapcar (lambda (result)
           (cons (nskk-completion-engine-result-midashi result)
                 (nskk-completion-engine-result-entry result)))
          results))

(defun nskk-completion-engine-to-string-list (results)
  "統合補完結果を文字列リストに変換する。

引数:
  RESULTS - nskk-completion-engine-result構造体のリスト

戻り値:
  見出し語の文字列リスト"
  (mapcar #'nskk-completion-engine-result-midashi results))

(defun nskk-completion-engine-filter-by-score (results min-score)
  "指定スコア以上の結果のみフィルタする。

引数:
  RESULTS   - nskk-completion-engine-result構造体のリスト
  MIN-SCORE - 最小スコア

戻り値:
  フィルタ後のリスト"
  (seq-filter (lambda (result)
               (>= (nskk-completion-engine-result-total-score result)
                   min-score))
              results))

;;; デバッグ用関数

(defun nskk-completion-engine-debug-info (index query &rest args)
  "統合補完検索のデバッグ情報を表示する。

引数:
  INDEX - 辞書インデックス
  QUERY - 検索クエリ

キーワード引数:
  :context - 文脈

戻り値:
  plist形式のデバッグ情報"
  (let* ((start-time (float-time))
         (results (apply #'nskk-completion-engine-search index query args))
         (elapsed-time (- (float-time) start-time)))
    (list :query query
          :result-count (length results)
          :elapsed-time elapsed-time
          :algorithms nskk-completion-engine-algorithms
          :score-method nskk-completion-engine-score-method
          :top-results (mapcar (lambda (r)
                                (list :midashi (nskk-completion-engine-result-midashi r)
                                      :score (nskk-completion-engine-result-total-score r)
                                      :rank (nskk-completion-engine-result-rank r)
                                      :algo-scores (nskk-completion-engine-result-algorithm-scores r)))
                              (seq-take results 10)))))

(provide 'nskk-completion-engine)

;;; nskk-completion-engine.el ends here
