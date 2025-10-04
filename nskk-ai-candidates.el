;;; nskk-ai-candidates.el --- AI-powered smart candidate generation for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, ai, candidates
;; Version: 1.0.0
;; Package-Requires: ((emacs "31.0"))

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

;; このファイルはAI駆動のスマート候補生成システムを実装します。
;;
;; 特徴:
;; - AI候補ランキング（AI-powered ranking）
;; - 文脈適応型候補（context-adaptive candidates）
;; - ハイブリッド候補統合（hybrid candidate integration）
;; - リアルタイム学習（real-time learning）
;;
;; 依存関係:
;; - nskk-ai-context.el（文脈理解）
;; - nskk-ai-pattern.el（パターン認識）
;; - nskk-learning-frequency.el（頻度学習）
;; - nskk-learning-context.el（文脈学習）
;;
;; パフォーマンス目標:
;; - 候補生成: < 20ms
;; - ランキング: < 10ms
;; - 精度: 90%+（トップ3候補内での正答率）

;;; Code:

(require 'cl-lib)
(require 'nskk-ai-context nil t)
(require 'nskk-ai-pattern nil t)
(require 'nskk-learning-frequency nil t)
(require 'nskk-learning-context nil t)

;;; カスタマイズ変数

(defgroup nskk-ai-candidates nil
  "AI-powered smart candidate generation for NSKK."
  :group 'nskk
  :prefix "nskk-ai-candidates-")

(defcustom nskk-ai-candidates-enable-ai-ranking t
  "非nilの場合、AIランキングを有効にする。"
  :type 'boolean
  :group 'nskk-ai-candidates)

(defcustom nskk-ai-candidates-enable-context-adaptation t
  "非nilの場合、文脈適応を有効にする。"
  :type 'boolean
  :group 'nskk-ai-candidates)

(defcustom nskk-ai-candidates-enable-hybrid-mode t
  "非nilの場合、ハイブリッドモード（複数手法の統合）を有効にする。"
  :type 'boolean
  :group 'nskk-ai-candidates)

(defcustom nskk-ai-candidates-max-candidates 10
  "生成する候補の最大数。"
  :type 'integer
  :group 'nskk-ai-candidates)

(defcustom nskk-ai-candidates-min-confidence 0.01
  "候補として採用する最小信頼度（0.0 - 1.0）。"
  :type 'float
  :group 'nskk-ai-candidates)

(defcustom nskk-ai-candidates-weight-frequency 0.3
  "頻度スコアの重み（0.0 - 1.0）。"
  :type 'float
  :group 'nskk-ai-candidates)

(defcustom nskk-ai-candidates-weight-context 0.4
  "文脈スコアの重み（0.0 - 1.0）。"
  :type 'float
  :group 'nskk-ai-candidates)

(defcustom nskk-ai-candidates-weight-pattern 0.3
  "パターンスコアの重み（0.0 - 1.0）。"
  :type 'float
  :group 'nskk-ai-candidates)

(defcustom nskk-ai-candidates-cache-size 1000
  "候補キャッシュのサイズ。"
  :type 'integer
  :group 'nskk-ai-candidates)

(defcustom nskk-ai-candidates-enable-learning t
  "非nilの場合、リアルタイム学習を有効にする。"
  :type 'boolean
  :group 'nskk-ai-candidates)

;;; データ構造

(cl-defstruct (nskk-ai-candidate
               (:constructor nskk-ai-candidate--create)
               (:copier nil))
  "AI候補。

スロット:
  text       - 候補テキスト
  score      - 総合スコア
  scores     - 詳細スコア（plist）
  source     - 候補のソース（:dict/:pattern/:ai）
  confidence - 信頼度（0.0 - 1.0）
  metadata   - メタデータ"
  (text "" :type string)
  (score 0.0 :type float)
  (scores nil :type list)
  (source :unknown :type symbol)
  (confidence 0.0 :type float)
  (metadata nil :type list))

(cl-defstruct (nskk-ai-ranking-model
               (:constructor nskk-ai-ranking-model--create)
               (:copier nil))
  "ランキングモデル。

スロット:
  weights       - 特徴の重み
  feature-names - 特徴名のリスト
  accuracy      - 現在の精度"
  (weights nil :type vector)
  (feature-names nil :type list)
  (accuracy 0.0 :type float))

;;; グローバル変数

(defvar nskk-ai-candidates-cache (make-hash-table :test 'equal)
  "候補キャッシュ。キー: (midashi . context-hash)、値: 候補リスト。")

(defvar nskk-ai-candidates-ranking-model
  (nskk-ai-ranking-model--create
   :weights (vector 0.3 0.4 0.3)  ; [frequency, context, pattern]
   :feature-names '(:frequency :context :pattern)
   :accuracy 0.0)
  "ランキングモデル。")

(defvar nskk-ai-candidates-statistics
  (list :total-queries 0
        :cache-hits 0
        :cache-misses 0
        :avg-candidates 0.0
        :top1-accuracy 0.0
        :top3-accuracy 0.0)
  "候補生成の統計情報。")

(defvar nskk-ai-candidates-feedback-history nil
  "フィードバック履歴（学習用）。")

;;; スマート候補生成

;;;###autoload
(defun nskk-ai-candidates-generate (midashi &optional base-candidates)
  "見出し語 MIDASHI に対してスマート候補を生成する。
BASE-CANDIDATES は辞書からの基本候補リスト。
戻り値: ランキングされた候補リスト（文字列のリスト）"
  (when (stringp midashi)
    (cl-incf (plist-get nskk-ai-candidates-statistics :total-queries))

    ;; キャッシュチェック
    (let* ((context (when (and (featurep 'nskk-ai-context)
                              nskk-ai-candidates-enable-context-adaptation)
                     (nskk-ai-context-extract-context)))
           (context-hash (sxhash-equal context))
           (cache-key (cons midashi context-hash))
           (cached (gethash cache-key nskk-ai-candidates-cache)))

      (if cached
          (progn
            (cl-incf (plist-get nskk-ai-candidates-statistics :cache-hits))
            cached)

        ;; キャッシュミス - 候補を生成
        (cl-incf (plist-get nskk-ai-candidates-statistics :cache-misses))

        (let ((candidates (nskk-ai-candidates--generate-internal midashi base-candidates context)))
          ;; キャッシュに保存
          (puthash cache-key candidates nskk-ai-candidates-cache)

          ;; キャッシュサイズ制限
          (when (> (hash-table-count nskk-ai-candidates-cache)
                  nskk-ai-candidates-cache-size)
            (nskk-ai-candidates--prune-cache))

          candidates)))))

(defun nskk-ai-candidates--generate-internal (midashi base-candidates context)
  "内部的な候補生成処理。
MIDASHI: 見出し語
BASE-CANDIDATES: 辞書からの基本候補
CONTEXT: 文脈テキスト
戻り値: 候補リスト（文字列）"
  (let ((ai-candidates (make-hash-table :test 'equal))
        (context-words (when context
                        (and (featurep 'nskk-ai-context)
                             (nskk-ai-context-parse-sentence context)))))

    ;; 1. 辞書候補をAI候補に変換
    (dolist (cand base-candidates)
      (let ((ai-cand (nskk-ai-candidates--create-candidate
                     midashi cand :dict context-words)))
        (puthash (nskk-ai-candidate-text ai-cand) ai-cand ai-candidates)))

    ;; 2. パターンベース候補を追加
    (when (and (featurep 'nskk-ai-pattern)
              nskk-ai-candidates-enable-hybrid-mode)
      (let ((pattern-candidates (nskk-ai-pattern-predict-candidate midashi context-words)))
        (dolist (pred pattern-candidates)
          (let* ((cand-text (car pred))
                 (confidence (cdr pred))
                 (existing (gethash cand-text ai-candidates)))
            (if existing
                ;; 既存候補のスコアを更新
                (setf (nskk-ai-candidate-confidence existing)
                      (max (nskk-ai-candidate-confidence existing) confidence))
              ;; 新規候補を追加
              (let ((ai-cand (nskk-ai-candidates--create-candidate
                             midashi cand-text :pattern context-words)))
                (setf (nskk-ai-candidate-confidence ai-cand) confidence)
                (puthash cand-text ai-cand ai-candidates)))))))

    ;; 3. 候補をスコアリング
    (let ((scored-candidates nil))
      (maphash
       (lambda (_key ai-cand)
         (nskk-ai-candidates--score-candidate ai-cand midashi context-words)
         (when (>= (nskk-ai-candidate-confidence ai-cand)
                  nskk-ai-candidates-min-confidence)
           (push ai-cand scored-candidates)))
       ai-candidates)

      ;; 4. スコア順にソート
      (setq scored-candidates
            (sort scored-candidates
                  (lambda (a b)
                    (> (nskk-ai-candidate-score a)
                       (nskk-ai-candidate-score b)))))

      ;; 5. 上位N件を返す（文字列のリスト）
      (mapcar #'nskk-ai-candidate-text
              (seq-take scored-candidates nskk-ai-candidates-max-candidates)))))

(defun nskk-ai-candidates--create-candidate (midashi text source context-words)
  "AI候補を作成する。
MIDASHI: 見出し語
TEXT: 候補テキスト
SOURCE: ソース（:dict/:pattern/:ai）
CONTEXT-WORDS: 文脈の単語リスト
戻り値: nskk-ai-candidate構造体"
  (nskk-ai-candidate--create
   :text text
   :source source
   :confidence 0.5  ; 初期値
   :metadata (list :midashi midashi
                  :context-length (length context-words))))

;;; スコアリング

(defun nskk-ai-candidates--score-candidate (candidate midashi context-words)
  "候補 CANDIDATE をスコアリングする。
MIDASHI: 見出し語
CONTEXT-WORDS: 文脈の単語リスト"
  (let* ((text (nskk-ai-candidate-text candidate))
         (freq-score (nskk-ai-candidates--calc-frequency-score midashi text))
         (context-score (nskk-ai-candidates--calc-context-score midashi text context-words))
         (pattern-score (nskk-ai-candidates--calc-pattern-score midashi text context-words))
         (weights (nskk-ai-ranking-model-weights nskk-ai-candidates-ranking-model))
         (total-score 0.0))

    ;; 重み付き合算
    (setq total-score
          (+ (* (aref weights 0) freq-score)
             (* (aref weights 1) context-score)
             (* (aref weights 2) pattern-score)))

    ;; スコアを設定
    (setf (nskk-ai-candidate-score candidate) total-score)
    (setf (nskk-ai-candidate-scores candidate)
          (list :frequency freq-score
                :context context-score
                :pattern pattern-score
                :total total-score))

    ;; 信頼度を更新（スコアベース）
    (setf (nskk-ai-candidate-confidence candidate)
          (min 1.0 (max 0.0 total-score)))))

(defsubst nskk-ai-candidates--calc-frequency-score (midashi candidate)
  "頻度スコアを計算する。"
  (if (featurep 'nskk-learning-frequency)
      (let ((score (nskk-get-frequency-score midashi candidate)))
        ;; スコアを0-1に正規化（仮定: 最大100）
        (min 1.0 (/ score 100.0)))
    0.5))  ; デフォルト

(defsubst nskk-ai-candidates--calc-context-score (midashi candidate context-words)
  "文脈スコアを計算する。"
  (if (and (featurep 'nskk-ai-context)
          nskk-ai-candidates-enable-context-adaptation
          context-words)
      (nskk-ai-context-score-candidate midashi candidate)
    ;; 文脈学習のフォールバック
    (if (featurep 'nskk-learning-context)
        (let ((score (nskk-context-score midashi candidate)))
          (min 1.0 (max 0.0 score)))
      0.5)))  ; デフォルト

(defsubst nskk-ai-candidates--calc-pattern-score (midashi candidate context-words)
  "パターンスコアを計算する。"
  (if (and (featurep 'nskk-ai-pattern)
          nskk-ai-candidates-enable-hybrid-mode)
      (let ((predictions (nskk-ai-pattern-predict-candidate midashi context-words)))
        (or (cdr (assoc candidate predictions))
            0.1))  ; 予測に含まれない場合は低スコア
    0.5))  ; デフォルト

;;; ハイブリッドモード

;;;###autoload
(defun nskk-ai-candidates-merge-sources (dict-candidates ai-candidates)
  "辞書候補 DICT-CANDIDATES とAI候補 AI-CANDIDATES を統合する。
戻り値: 統合された候補リスト"
  (when nskk-ai-candidates-enable-hybrid-mode
    (let ((merged (make-hash-table :test 'equal)))
      ;; 辞書候補を追加
      (dolist (cand dict-candidates)
        (puthash cand (list :source :dict :score 1.0) merged))

      ;; AI候補を追加/マージ
      (dolist (cand ai-candidates)
        (let ((existing (gethash cand merged)))
          (if existing
              ;; 既存候補のスコアを更新
              (plist-put existing :score
                        (max (plist-get existing :score) 0.8))
            ;; 新規候補を追加
            (puthash cand (list :source :ai :score 0.8) merged))))

      ;; スコア順にソート
      (let ((result nil))
        (maphash
         (lambda (cand info)
           (push (cons cand (plist-get info :score)) result))
         merged)
        (mapcar #'car
                (sort result (lambda (a b) (> (cdr a) (cdr b)))))))))

;;; リアルタイム学習

;;;###autoload
(defun nskk-ai-candidates-learn-selection (midashi selected-candidate all-candidates)
  "ユーザーが選択した候補から学習する。
MIDASHI: 見出し語
SELECTED-CANDIDATE: 選択された候補
ALL-CANDIDATES: 全候補リスト"
  (when (and nskk-ai-candidates-enable-learning
            (stringp midashi)
            (stringp selected-candidate))

    ;; フィードバックを記録
    (let* ((context (when (featurep 'nskk-ai-context)
                     (nskk-ai-context-extract-context)))
           (context-words (when context
                           (nskk-ai-context-parse-sentence context)))
           (rank (cl-position selected-candidate all-candidates :test #'equal))
           (feedback (list :midashi midashi
                          :selected selected-candidate
                          :rank (or rank -1)
                          :context context-words
                          :timestamp (current-time))))

      (push feedback nskk-ai-candidates-feedback-history)

      ;; 精度統計を更新
      (when rank
        (when (= rank 0)
          (cl-incf (plist-get nskk-ai-candidates-statistics :top1-accuracy)))
        (when (<= rank 2)
          (cl-incf (plist-get nskk-ai-candidates-statistics :top3-accuracy))))

      ;; 各学習システムに通知
      (when (featurep 'nskk-learning-frequency)
        (nskk-update-frequency midashi selected-candidate))

      (when (and (featurep 'nskk-learning-context) context-words)
        (let ((prev (car (last context-words))))
          (when prev
            (nskk-learn-context prev selected-candidate))))

      (when (and (featurep 'nskk-ai-pattern) context-words)
        (nskk-ai-pattern-learn midashi selected-candidate context-words))

      (when (featurep 'nskk-ai-context)
        (nskk-ai-context-learn-text (concat context selected-candidate)))

      ;; キャッシュをクリア（学習により古いランキングは無効）
      (nskk-ai-candidates--clear-cache-for-midashi midashi)

      ;; 定期的にモデルを更新（100回ごと）
      (when (zerop (mod (length nskk-ai-candidates-feedback-history) 100))
        (nskk-ai-candidates--update-ranking-model)))))

(defun nskk-ai-candidates--update-ranking-model ()
  "ランキングモデルを更新する（簡易版）。
フィードバック履歴から最適な重みを学習。"
  (when (> (length nskk-ai-candidates-feedback-history) 10)
    ;; 簡易的な重み調整（正答率ベース）
    (let* ((recent-feedback (seq-take nskk-ai-candidates-feedback-history 100))
           (success-count 0)
           (total-count (length recent-feedback)))

      (dolist (fb recent-feedback)
        (when (<= (plist-get fb :rank) 2)
          (cl-incf success-count)))

      (let ((accuracy (/ (float success-count) total-count)))
        (setf (nskk-ai-ranking-model-accuracy nskk-ai-candidates-ranking-model)
              accuracy)

        ;; 精度が低い場合、重みを調整（ヒューリスティック）
        (when (< accuracy 0.7)
          (let ((weights (nskk-ai-ranking-model-weights nskk-ai-candidates-ranking-model)))
            ;; 文脈の重みを増やす
            (aset weights 1 (min 1.0 (* (aref weights 1) 1.1)))
            ;; 正規化
            (let ((sum (+ (aref weights 0) (aref weights 1) (aref weights 2))))
              (dotimes (i 3)
                (aset weights i (/ (aref weights i) sum))))))))))

;;; キャッシュ管理

(defun nskk-ai-candidates--prune-cache ()
  "キャッシュから古いエントリを削除する。"
  (let ((keys nil))
    (maphash (lambda (k _v) (push k keys)) nskk-ai-candidates-cache)
    ;; ランダムに半分削除（簡易的なLRU代替）
    (dolist (key (seq-take (shuffle keys)
                          (/ (length keys) 2)))
      (remhash key nskk-ai-candidates-cache))))

(defun nskk-ai-candidates--clear-cache-for-midashi (midashi)
  "見出し語 MIDASHI に関連するキャッシュをクリアする。"
  (let ((keys-to-remove nil))
    (maphash
     (lambda (key _value)
       (when (equal (car key) midashi)
         (push key keys-to-remove)))
     nskk-ai-candidates-cache)
    (dolist (key keys-to-remove)
      (remhash key nskk-ai-candidates-cache))))

;;; 精度測定

;;;###autoload
(defun nskk-ai-candidates-measure-accuracy (&optional n)
  "最近 N 回（デフォルト100回）の候補生成の精度を測定する。
戻り値: plist形式の精度データ"
  (let* ((recent-feedback (seq-take nskk-ai-candidates-feedback-history (or n 100)))
         (top1-count 0)
         (top3-count 0)
         (total (length recent-feedback)))

    (when (> total 0)
      (dolist (fb recent-feedback)
        (let ((rank (plist-get fb :rank)))
          (when (and (numberp rank) (>= rank 0))
            (when (= rank 0) (cl-incf top1-count))
            (when (<= rank 2) (cl-incf top3-count)))))

      (list :top1-accuracy (/ (float top1-count) total)
            :top3-accuracy (/ (float top3-count) total)
            :sample-size total
            :top1-count top1-count
            :top3-count top3-count))))

;;; 統計情報

;;;###autoload
(defun nskk-ai-candidates-statistics ()
  "候補生成システムの統計情報を返す。
戻り値: plist形式の統計情報"
  (let ((stats (copy-sequence nskk-ai-candidates-statistics))
        (accuracy-data (nskk-ai-candidates-measure-accuracy)))

    ;; 精度データを追加
    (plist-put stats :recent-top1-accuracy
               (or (plist-get accuracy-data :top1-accuracy) 0.0))
    (plist-put stats :recent-top3-accuracy
               (or (plist-get accuracy-data :top3-accuracy) 0.0))
    (plist-put stats :cache-size (hash-table-count nskk-ai-candidates-cache))
    (plist-put stats :feedback-count (length nskk-ai-candidates-feedback-history))
    (plist-put stats :model-accuracy
               (nskk-ai-ranking-model-accuracy nskk-ai-candidates-ranking-model))

    ;; キャッシュヒット率
    (let ((total-queries (plist-get stats :total-queries)))
      (when (> total-queries 0)
        (plist-put stats :cache-hit-rate
                  (/ (float (plist-get stats :cache-hits)) total-queries))))

    stats))

;;;###autoload
(defun nskk-ai-candidates-print-statistics ()
  "候補生成システムの統計情報を表示する。"
  (interactive)
  (let ((stats (nskk-ai-candidates-statistics)))
    (message "NSKK AI Candidates Statistics:
  Total Queries: %d
  Cache Hit Rate: %.2f%%
  Recent Top-1 Accuracy: %.2f%%
  Recent Top-3 Accuracy: %.2f%%
  Model Accuracy: %.2f%%
  Cache Size: %d
  Feedback History: %d"
             (plist-get stats :total-queries)
             (* 100 (or (plist-get stats :cache-hit-rate) 0.0))
             (* 100 (plist-get stats :recent-top1-accuracy))
             (* 100 (plist-get stats :recent-top3-accuracy))
             (* 100 (plist-get stats :model-accuracy))
             (plist-get stats :cache-size)
             (plist-get stats :feedback-count))))

;;; クリーンアップ

;;;###autoload
(defun nskk-ai-candidates-clear ()
  "全ての候補データをクリアする。"
  (interactive)
  (clrhash nskk-ai-candidates-cache)
  (setq nskk-ai-candidates-feedback-history nil)
  (setq nskk-ai-candidates-statistics
        (list :total-queries 0
              :cache-hits 0
              :cache-misses 0
              :avg-candidates 0.0
              :top1-accuracy 0.0
              :top3-accuracy 0.0))
  (setq nskk-ai-candidates-ranking-model
        (nskk-ai-ranking-model--create
         :weights (vector 0.3 0.4 0.3)
         :feature-names '(:frequency :context :pattern)
         :accuracy 0.0)))

;;;###autoload
(defun nskk-ai-candidates-clear-cache ()
  "候補キャッシュのみをクリアする。"
  (interactive)
  (clrhash nskk-ai-candidates-cache))

(provide 'nskk-ai-candidates)

;;; nskk-ai-candidates.el ends here
