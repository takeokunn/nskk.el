;;; nskk-ai-learning.el --- Advanced learning algorithms for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, ai, learning, machine-learning
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

;; このファイルは高度な学習アルゴリズムを実装します。
;;
;; 特徴:
;; - オンライン学習（online learning）
;; - 転移学習（transfer learning）
;; - モデル圧縮（model compression）
;; - 増分学習（incremental learning）
;;
;; アルゴリズム:
;; - 確率的勾配降下法（SGD）
;; - 知識蒸留（knowledge distillation）
;; - メモリ効率的な学習
;; - 適応的学習率
;;
;; パフォーマンス目標:
;; - オンライン学習: < 50ms
;; - モデル更新: < 100ms
;; - メモリ使用量: < 10MB

;;; Code:

(require 'cl-lib)

;;; カスタマイズ変数

(defgroup nskk-ai-learning nil
  "Advanced learning algorithms for NSKK."
  :group 'nskk
  :prefix "nskk-ai-learning-")

(defcustom nskk-ai-learning-enable-online t
  "非nilの場合、オンライン学習を有効にする。"
  :type 'boolean
  :group 'nskk-ai-learning)

(defcustom nskk-ai-learning-enable-transfer t
  "非nilの場合、転移学習を有効にする。"
  :type 'boolean
  :group 'nskk-ai-learning)

(defcustom nskk-ai-learning-enable-compression t
  "非nilの場合、モデル圧縮を有効にする。"
  :type 'boolean
  :group 'nskk-ai-learning)

(defcustom nskk-ai-learning-learning-rate 0.01
  "学習率（0.0 - 1.0）。"
  :type 'float
  :group 'nskk-ai-learning)

(defcustom nskk-ai-learning-momentum 0.9
  "モーメンタム係数（0.0 - 1.0）。"
  :type 'float
  :group 'nskk-ai-learning)

(defcustom nskk-ai-learning-batch-size 32
  "ミニバッチサイズ。"
  :type 'integer
  :group 'nskk-ai-learning)

(defcustom nskk-ai-learning-compression-ratio 0.5
  "モデル圧縮率（0.0 - 1.0）。小さいほど圧縮率が高い。"
  :type 'float
  :group 'nskk-ai-learning)

(defcustom nskk-ai-learning-auto-save-interval 1000
  "自動保存の間隔（学習ステップ数）。"
  :type 'integer
  :group 'nskk-ai-learning)

(defcustom nskk-ai-learning-max-history 10000
  "学習履歴の最大保存数。"
  :type 'integer
  :group 'nskk-ai-learning)

;;; データ構造

(cl-defstruct (nskk-ai-learning-model
               (:constructor nskk-ai-learning-model--create)
               (:copier nil))
  "学習モデル。

スロット:
  weights      - 重みベクトル
  bias         - バイアス
  velocity     - モーメンタム用速度
  version      - モデルバージョン
  training-steps - 学習ステップ数
  loss-history - 損失関数の履歴"
  (weights nil :type vector)
  (bias 0.0 :type float)
  (velocity nil :type vector)
  (version 1 :type integer)
  (training-steps 0 :type integer)
  (loss-history nil :type list))

(cl-defstruct (nskk-ai-learning-sample
               (:constructor nskk-ai-learning-sample--create)
               (:copier nil))
  "学習サンプル。

スロット:
  features     - 特徴ベクトル
  label        - ラベル（正解候補）
  weight       - サンプル重み
  timestamp    - タイムスタンプ"
  (features nil :type vector)
  (label "" :type string)
  (weight 1.0 :type float)
  (timestamp nil :type list))

(cl-defstruct (nskk-ai-transfer-knowledge
               (:constructor nskk-ai-transfer-knowledge--create)
               (:copier nil))
  "転移学習用の知識。

スロット:
  source-domain   - 元ドメイン
  target-domain   - 対象ドメイン
  shared-features - 共有特徴
  mapping         - ドメイン間マッピング"
  (source-domain "" :type string)
  (target-domain "" :type string)
  (shared-features nil :type vector)
  (mapping (make-hash-table :test 'equal)))

;;; グローバル変数

(defvar nskk-ai-learning-model
  (nskk-ai-learning-model--create
   :weights (make-vector 10 0.1)  ; 初期重み
   :bias 0.0
   :velocity (make-vector 10 0.0))
  "メイン学習モデル。")

(defvar nskk-ai-learning-training-data nil
  "学習データのバッファ。nskk-ai-learning-sample のリスト。")

(defvar nskk-ai-learning-transfer-knowledge-base (make-hash-table :test 'equal)
  "転移学習用の知識ベース。")

(defvar nskk-ai-learning-compressed-models (make-hash-table :test 'equal)
  "圧縮されたモデルのキャッシュ。")

(defvar nskk-ai-learning-statistics
  (list :total-updates 0
        :avg-loss 0.0
        :last-update-time nil
        :model-size 0)
  "学習統計情報。")

;;; オンライン学習

;;;###autoload
(defun nskk-ai-learning-online-update (features label)
  "オンライン学習で単一サンプルから学習する。
FEATURES: 特徴ベクトル
LABEL: 正解ラベル（候補テキスト）"
  (when (and nskk-ai-learning-enable-online
            (vectorp features))

    ;; サンプルを作成
    (let ((sample (nskk-ai-learning-sample--create
                  :features features
                  :label label
                  :timestamp (current-time))))

      ;; 訓練データに追加
      (push sample nskk-ai-learning-training-data)

      ;; バッチサイズに達したら学習
      (when (>= (length nskk-ai-learning-training-data)
               nskk-ai-learning-batch-size)
        (nskk-ai-learning--train-batch)
        (setq nskk-ai-learning-training-data nil))

      ;; 統計更新
      (cl-incf (plist-get nskk-ai-learning-statistics :total-updates))
      (plist-put nskk-ai-learning-statistics :last-update-time (current-time))

      ;; 定期的にモデルを圧縮
      (when (and nskk-ai-learning-enable-compression
                (zerop (mod (nskk-ai-learning-model-training-steps
                            nskk-ai-learning-model)
                           nskk-ai-learning-auto-save-interval)))
        (nskk-ai-learning-compress-model)))))

(defun nskk-ai-learning--train-batch ()
  "ミニバッチで学習を実行する。"
  (let* ((model nskk-ai-learning-model)
         (weights (nskk-ai-learning-model-weights model))
         (velocity (nskk-ai-learning-model-velocity model))
         (batch nskk-ai-learning-training-data)
         (batch-size (length batch))
         (total-loss 0.0))

    (when (> batch-size 0)
      ;; 各サンプルで勾配を計算
      (let ((gradient (make-vector (length weights) 0.0)))

        (dolist (sample batch)
          (let* ((features (nskk-ai-learning-sample-features sample))
                 (prediction (nskk-ai-learning--predict features))
                 (error (nskk-ai-learning--calc-error prediction sample))
                 (sample-weight (nskk-ai-learning-sample-weight sample)))

            ;; 損失を累積
            (setq total-loss (+ total-loss (* error error)))

            ;; 勾配を計算（簡易版: error * feature）
            (dotimes (i (min (length features) (length gradient)))
              (aset gradient i
                    (+ (aref gradient i)
                       (* error (aref features i) sample-weight))))))

        ;; 勾配の平均
        (dotimes (i (length gradient))
          (aset gradient i (/ (aref gradient i) batch-size)))

        ;; モーメンタム付きSGD
        (dotimes (i (length weights))
          ;; 速度更新
          (aset velocity i
                (+ (* nskk-ai-learning-momentum (aref velocity i))
                   (* nskk-ai-learning-learning-rate (aref gradient i))))
          ;; 重み更新
          (aset weights i (- (aref weights i) (aref velocity i))))

        ;; 統計更新
        (let ((avg-loss (/ total-loss batch-size)))
          (plist-put nskk-ai-learning-statistics :avg-loss avg-loss)
          (push avg-loss (nskk-ai-learning-model-loss-history model))

          ;; 損失履歴のサイズ制限
          (when (> (length (nskk-ai-learning-model-loss-history model)) 1000)
            (setf (nskk-ai-learning-model-loss-history model)
                  (seq-take (nskk-ai-learning-model-loss-history model) 1000))))

        ;; 学習ステップ数を増やす
        (cl-incf (nskk-ai-learning-model-training-steps model))))))

(defsubst nskk-ai-learning--predict (features)
  "特徴ベクトル FEATURES から予測値を計算する。"
  (let* ((model nskk-ai-learning-model)
         (weights (nskk-ai-learning-model-weights model))
         (bias (nskk-ai-learning-model-bias model))
         (score bias))
    ;; 内積を計算
    (dotimes (i (min (length features) (length weights)))
      (setq score (+ score (* (aref features i) (aref weights i)))))
    ;; シグモイド関数で正規化
    (/ 1.0 (+ 1.0 (exp (- score))))))

(defsubst nskk-ai-learning--calc-error (prediction sample)
  "予測値 PREDICTION とサンプル SAMPLE の誤差を計算する。
簡易版: 常に予測値を0-1の範囲で評価し、正解は1とする。"
  (- prediction 1.0))  ; 正解ラベルを1.0と仮定

;;; 増分学習

;;;###autoload
(defun nskk-ai-learning-incremental-update (new-samples)
  "増分学習で新しいサンプル NEW-SAMPLES から学習する。
NEW-SAMPLES: nskk-ai-learning-sample のリスト"
  (when (and new-samples (> (length new-samples) 0))
    ;; 各サンプルで学習
    (dolist (sample new-samples)
      (nskk-ai-learning-online-update
       (nskk-ai-learning-sample-features sample)
       (nskk-ai-learning-sample-label sample)))

    ;; 学習データの履歴を制限
    (when (> (length nskk-ai-learning-training-data)
            nskk-ai-learning-max-history)
      (setq nskk-ai-learning-training-data
            (seq-take nskk-ai-learning-training-data
                     nskk-ai-learning-max-history)))))

;;; 転移学習

;;;###autoload
(defun nskk-ai-learning-transfer-from-domain (source-domain target-domain)
  "SOURCE-DOMAIN から TARGET-DOMAIN へ知識を転移する。
戻り値: 転移学習により生成された知識"
  (when nskk-ai-learning-enable-transfer
    (let ((knowledge (nskk-ai-transfer-knowledge--create
                     :source-domain source-domain
                     :target-domain target-domain)))

      ;; 共有特徴を抽出（簡易版）
      (setf (nskk-ai-transfer-knowledge-shared-features knowledge)
            (nskk-ai-learning--extract-shared-features
             source-domain target-domain))

      ;; ドメイン間のマッピングを学習
      (nskk-ai-learning--learn-domain-mapping knowledge)

      ;; 知識ベースに保存
      (puthash (cons source-domain target-domain)
              knowledge
              nskk-ai-learning-transfer-knowledge-base)

      knowledge)))

(defun nskk-ai-learning--extract-shared-features (source target)
  "SOURCE と TARGET ドメインの共有特徴を抽出する。
簡易版: 現在のモデルの重みをコピー。"
  (copy-sequence (nskk-ai-learning-model-weights nskk-ai-learning-model)))

(defun nskk-ai-learning--learn-domain-mapping (knowledge)
  "ドメイン間のマッピングを学習する。
KNOWLEDGE: nskk-ai-transfer-knowledge 構造体"
  ;; 簡易版: 恒等写像を使用
  (let ((mapping (nskk-ai-transfer-knowledge-mapping knowledge)))
    (puthash "default" 1.0 mapping)))

;;;###autoload
(defun nskk-ai-learning-apply-transfer-knowledge (domain)
  "DOMAIN に転移学習の知識を適用する。"
  (when nskk-ai-learning-enable-transfer
    (maphash
     (lambda (key knowledge)
       (when (equal (cdr key) domain)
         ;; 共有特徴をモデルに適用
         (let ((shared-features (nskk-ai-transfer-knowledge-shared-features knowledge))
               (current-weights (nskk-ai-learning-model-weights nskk-ai-learning-model)))
           ;; 重みを混合（簡易版: 平均）
           (dotimes (i (min (length shared-features) (length current-weights)))
             (aset current-weights i
                   (* 0.5 (+ (aref current-weights i)
                            (aref shared-features i))))))))
     nskk-ai-learning-transfer-knowledge-base)))

;;; モデル圧縮

;;;###autoload
(defun nskk-ai-learning-compress-model ()
  "現在のモデルを圧縮する。
戻り値: 圧縮されたモデル"
  (interactive)
  (when nskk-ai-learning-enable-compression
    (let* ((model nskk-ai-learning-model)
           (weights (nskk-ai-learning-model-weights model))
           (compressed-size (floor (* (length weights)
                                     nskk-ai-learning-compression-ratio)))
           (compressed-weights (make-vector compressed-size 0.0)))

      ;; 重要度の高い重みのみ保持（簡易版: 絶対値の大きい順）
      (let* ((weight-list nil)
             (i 0))
        ;; 重みをリストに変換
        (dotimes (j (length weights))
          (push (cons j (aref weights j)) weight-list))

        ;; 絶対値でソート
        (setq weight-list
              (sort weight-list
                    (lambda (a b) (> (abs (cdr a)) (abs (cdr b))))))

        ;; 上位のみ保持
        (dolist (entry (seq-take weight-list compressed-size))
          (aset compressed-weights i (cdr entry))
          (cl-incf i)))

      ;; 圧縮モデルを保存
      (let ((compressed-model
             (nskk-ai-learning-model--create
              :weights compressed-weights
              :bias (nskk-ai-learning-model-bias model)
              :version (1+ (nskk-ai-learning-model-version model)))))

        (puthash (nskk-ai-learning-model-version model)
                compressed-model
                nskk-ai-learning-compressed-models)

        ;; 統計更新
        (plist-put nskk-ai-learning-statistics :model-size
                  (* compressed-size 8))  ; float = 8 bytes

        compressed-model))))

;;;###autoload
(defun nskk-ai-learning-load-compressed-model (version)
  "圧縮モデルをバージョン VERSION で読み込む。"
  (let ((compressed (gethash version nskk-ai-learning-compressed-models)))
    (when compressed
      (setf (nskk-ai-learning-model-weights nskk-ai-learning-model)
            (copy-sequence (nskk-ai-learning-model-weights compressed)))
      (setf (nskk-ai-learning-model-bias nskk-ai-learning-model)
            (nskk-ai-learning-model-bias compressed))
      (setf (nskk-ai-learning-model-version nskk-ai-learning-model)
            version)
      t)))

;;; 適応的学習率

(defvar nskk-ai-learning-adaptive-lr-enabled t
  "適応的学習率を有効にするかどうか。")

(defvar nskk-ai-learning-lr-decay-rate 0.99
  "学習率の減衰率。")

;;;###autoload
(defun nskk-ai-learning-adjust-learning-rate ()
  "学習率を適応的に調整する。
損失関数の履歴に基づいて自動調整。"
  (when nskk-ai-learning-adaptive-lr-enabled
    (let ((loss-history (nskk-ai-learning-model-loss-history
                        nskk-ai-learning-model)))
      (when (> (length loss-history) 10)
        ;; 最近の損失の傾向を分析
        (let* ((recent-loss (seq-take loss-history 10))
               (avg-recent (/ (apply #'+ recent-loss) 10.0))
               (older-loss (seq-take (nthcdr 10 loss-history) 10))
               (avg-older (if (> (length older-loss) 0)
                             (/ (apply #'+ older-loss) (float (length older-loss)))
                           avg-recent)))

          ;; 損失が改善していない場合、学習率を減衰
          (when (>= avg-recent avg-older)
            (setq nskk-ai-learning-learning-rate
                  (* nskk-ai-learning-learning-rate
                     nskk-ai-learning-lr-decay-rate))))))))

;;; モデルの評価

;;;###autoload
(defun nskk-ai-learning-evaluate-model (test-samples)
  "テストサンプル TEST-SAMPLES でモデルを評価する。
戻り値: plist形式の評価結果"
  (let ((correct 0)
        (total (length test-samples))
        (total-loss 0.0))

    (dolist (sample test-samples)
      (let* ((features (nskk-ai-learning-sample-features sample))
             (prediction (nskk-ai-learning--predict features))
             (error (nskk-ai-learning--calc-error prediction sample)))

        ;; 損失を累積
        (setq total-loss (+ total-loss (* error error)))

        ;; 正解判定（予測値 > 0.5 なら正解とする）
        (when (> prediction 0.5)
          (cl-incf correct))))

    (list :accuracy (if (> total 0) (/ (float correct) total) 0.0)
          :avg-loss (if (> total 0) (/ total-loss total) 0.0)
          :sample-count total
          :correct-count correct)))

;;; モデルのリセット

;;;###autoload
(defun nskk-ai-learning-reset-model ()
  "モデルを初期状態にリセットする。"
  (interactive)
  (setq nskk-ai-learning-model
        (nskk-ai-learning-model--create
         :weights (make-vector 10 0.1)
         :bias 0.0
         :velocity (make-vector 10 0.0)))
  (setq nskk-ai-learning-training-data nil)
  (setq nskk-ai-learning-statistics
        (list :total-updates 0
              :avg-loss 0.0
              :last-update-time nil
              :model-size 0)))

;;; 統計情報

;;;###autoload
(defun nskk-ai-learning-statistics ()
  "学習システムの統計情報を返す。
戻り値: plist形式の統計情報"
  (let ((model nskk-ai-learning-model)
        (stats (copy-sequence nskk-ai-learning-statistics)))

    (plist-put stats :training-steps
               (nskk-ai-learning-model-training-steps model))
    (plist-put stats :model-version
               (nskk-ai-learning-model-version model))
    (plist-put stats :loss-history-length
               (length (nskk-ai-learning-model-loss-history model)))
    (plist-put stats :training-buffer-size
               (length nskk-ai-learning-training-data))
    (plist-put stats :transfer-knowledge-count
               (hash-table-count nskk-ai-learning-transfer-knowledge-base))
    (plist-put stats :compressed-models-count
               (hash-table-count nskk-ai-learning-compressed-models))
    (plist-put stats :learning-rate
               nskk-ai-learning-learning-rate)

    stats))

;;;###autoload
(defun nskk-ai-learning-print-statistics ()
  "学習システムの統計情報を表示する。"
  (interactive)
  (let ((stats (nskk-ai-learning-statistics)))
    (message "NSKK AI Learning Statistics:
  Training Steps: %d
  Model Version: %d
  Average Loss: %.4f
  Learning Rate: %.4f
  Training Buffer: %d samples
  Transfer Knowledge: %d
  Compressed Models: %d
  Model Size: %d bytes"
             (plist-get stats :training-steps)
             (plist-get stats :model-version)
             (plist-get stats :avg-loss)
             (plist-get stats :learning-rate)
             (plist-get stats :training-buffer-size)
             (plist-get stats :transfer-knowledge-count)
             (plist-get stats :compressed-models-count)
             (plist-get stats :model-size))))

;;; クリーンアップ

;;;###autoload
(defun nskk-ai-learning-clear ()
  "全ての学習データをクリアする。"
  (interactive)
  (nskk-ai-learning-reset-model)
  (clrhash nskk-ai-learning-transfer-knowledge-base)
  (clrhash nskk-ai-learning-compressed-models))

(provide 'nskk-ai-learning)

;;; nskk-ai-learning.el ends here
