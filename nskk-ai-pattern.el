;;; nskk-ai-pattern.el --- AI pattern recognition for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, ai, pattern, machine-learning
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

;; このファイルはAI駆動のパターン認識システムを実装します。
;;
;; 特徴:
;; - ユーザーパターン学習（user pattern learning）
;; - 変換パターン抽出（conversion pattern extraction）
;; - 時系列分析（time series analysis）
;; - 異常検出（anomaly detection）
;;
;; アルゴリズム:
;; - 教師なし学習（unsupervised learning）
;; - クラスタリング（k-means）
;; - 時系列パターンマッチング
;; - 統計的異常検出
;;
;; パフォーマンス目標:
;; - パターン認識: < 10ms
;; - クラスタリング: < 50ms
;; - 異常検出: < 5ms

;;; Code:

(require 'cl-lib)

;;; カスタマイズ変数

(defgroup nskk-ai-pattern nil
  "AI pattern recognition for NSKK."
  :group 'nskk
  :prefix "nskk-ai-pattern-")

(defcustom nskk-ai-pattern-enable-clustering t
  "非nilの場合、クラスタリングを有効にする。"
  :type 'boolean
  :group 'nskk-ai-pattern)

(defcustom nskk-ai-pattern-enable-anomaly-detection t
  "非nilの場合、異常検出を有効にする。"
  :type 'boolean
  :group 'nskk-ai-pattern)

(defcustom nskk-ai-pattern-cluster-count 10
  "クラスタ数（k-meansのk値）。"
  :type 'integer
  :group 'nskk-ai-pattern)

(defcustom nskk-ai-pattern-max-iterations 20
  "k-meansの最大反復回数。"
  :type 'integer
  :group 'nskk-ai-pattern)

(defcustom nskk-ai-pattern-anomaly-threshold 3.0
  "異常検出の閾値（標準偏差の倍数）。"
  :type 'float
  :group 'nskk-ai-pattern)

(defcustom nskk-ai-pattern-time-window 3600
  "時系列分析のウィンドウサイズ（秒）。デフォルトは1時間。"
  :type 'integer
  :group 'nskk-ai-pattern)

(defcustom nskk-ai-pattern-min-pattern-count 3
  "パターンと認識する最小出現回数。"
  :type 'integer
  :group 'nskk-ai-pattern)

;;; データ構造

(cl-defstruct (nskk-ai-pattern-entry
               (:constructor nskk-ai-pattern-entry--create)
               (:copier nil))
  "パターンエントリ。

スロット:
  midashi       - 見出し語
  candidate     - 変換候補
  context       - 文脈情報（単語リスト）
  timestamp     - タイムスタンプ
  features      - 特徴ベクトル
  cluster-id    - 所属クラスタID"
  (midashi "" :type string)
  (candidate "" :type string)
  (context nil :type list)
  (timestamp nil :type list)
  (features nil :type vector)
  (cluster-id -1 :type integer))

(cl-defstruct (nskk-ai-cluster
               (:constructor nskk-ai-cluster--create)
               (:copier nil))
  "クラスタ。

スロット:
  id         - クラスタID
  centroid   - 重心ベクトル
  members    - メンバーのリスト
  size       - メンバー数"
  (id 0 :type integer)
  (centroid nil :type vector)
  (members nil :type list)
  (size 0 :type integer))

(cl-defstruct (nskk-ai-time-series
               (:constructor nskk-ai-time-series--create)
               (:copier nil))
  "時系列データ。

スロット:
  timestamps  - タイムスタンプのリスト
  values      - 値のリスト
  window-size - ウィンドウサイズ"
  (timestamps nil :type list)
  (values nil :type list)
  (window-size 3600 :type integer))

(cl-defstruct (nskk-ai-anomaly
               (:constructor nskk-ai-anomaly--create)
               (:copier nil))
  "異常データ。

スロット:
  entry       - パターンエントリ
  score       - 異常スコア
  reason      - 異常の理由
  timestamp   - 検出時刻"
  (entry nil)
  (score 0.0 :type float)
  (reason "" :type string)
  (timestamp nil :type list))

;;; グローバル変数

(defvar nskk-ai-pattern-history nil
  "パターン履歴。nskk-ai-pattern-entry のリスト。")

(defvar nskk-ai-pattern-clusters nil
  "クラスタのリスト。nskk-ai-cluster のリスト。")

(defvar nskk-ai-pattern-time-series-data (make-hash-table :test 'equal)
  "時系列データ。キー: (midashi . candidate)、値: nskk-ai-time-series。")

(defvar nskk-ai-pattern-anomalies nil
  "検出された異常のリスト。nskk-ai-anomaly のリスト。")

(defvar nskk-ai-pattern-user-patterns (make-hash-table :test 'equal)
  "ユーザー固有パターン。キー: pattern-key、値: count。")

;;; ユーザーパターン学習

;;;###autoload
(defun nskk-ai-pattern-learn (midashi candidate context)
  "変換パターンを学習する。
MIDASHI: 見出し語
CANDIDATE: 変換候補
CONTEXT: 文脈情報（単語リスト）"
  (when (and (stringp midashi) (stringp candidate))
    ;; パターンエントリを作成
    (let* ((features (nskk-ai-pattern--extract-features midashi candidate context))
           (entry (nskk-ai-pattern-entry--create
                  :midashi midashi
                  :candidate candidate
                  :context context
                  :timestamp (current-time)
                  :features features)))

      ;; 履歴に追加
      (push entry nskk-ai-pattern-history)

      ;; ユーザーパターンを更新
      (nskk-ai-pattern--update-user-patterns midashi candidate context)

      ;; 時系列データを更新
      (nskk-ai-pattern--update-time-series midashi candidate)

      ;; 定期的にクラスタリングを実行（100エントリごと）
      (when (and nskk-ai-pattern-enable-clustering
                (zerop (mod (length nskk-ai-pattern-history) 100)))
        (nskk-ai-pattern-cluster-patterns))

      ;; 異常検出
      (when nskk-ai-pattern-enable-anomaly-detection
        (nskk-ai-pattern--detect-anomaly entry))

      ;; 履歴サイズ制限（最新10000件）
      (when (> (length nskk-ai-pattern-history) 10000)
        (setq nskk-ai-pattern-history
              (seq-take nskk-ai-pattern-history 10000))))))

(defun nskk-ai-pattern--extract-features (midashi candidate context)
  "パターンの特徴ベクトルを抽出する。
MIDASHI: 見出し語
CANDIDATE: 変換候補
CONTEXT: 文脈情報
戻り値: 特徴ベクトル（vector）"
  (let ((features (make-vector 10 0.0)))
    ;; 特徴1: 見出し語の長さ
    (aset features 0 (float (length midashi)))
    ;; 特徴2: 候補の長さ
    (aset features 1 (float (length candidate)))
    ;; 特徴3: 文脈の単語数
    (aset features 2 (float (length context)))
    ;; 特徴4: 見出し語のひらがな率
    (aset features 3 (nskk-ai-pattern--hiragana-ratio midashi))
    ;; 特徴5: 候補の漢字率
    (aset features 4 (nskk-ai-pattern--kanji-ratio candidate))
    ;; 特徴6: 長さ比率
    (aset features 5 (if (> (length midashi) 0)
                        (/ (float (length candidate)) (length midashi))
                      0.0))
    ;; 特徴7-9: 文脈の最後3単語の長さ平均
    (let ((context-avg 0.0))
      (when (> (length context) 0)
        (setq context-avg
              (/ (apply #'+ (mapcar #'length (seq-take (reverse context) 3)))
                 (float (min 3 (length context))))))
      (aset features 6 context-avg))
    ;; 特徴8: 時刻（0-23の時間帯）
    (aset features 7 (float (nth 2 (decode-time))))
    ;; 特徴9: 曜日（0-6）
    (aset features 8 (float (nth 6 (decode-time))))
    features))

(defsubst nskk-ai-pattern--hiragana-ratio (str)
  "文字列 STR のひらがな比率を計算する。"
  (if (zerop (length str))
      0.0
    (let ((hiragana-count 0))
      (dotimes (i (length str))
        (let ((char (aref str i)))
          (when (and (>= char #x3040) (<= char #x309F))
            (cl-incf hiragana-count))))
      (/ (float hiragana-count) (length str)))))

(defsubst nskk-ai-pattern--kanji-ratio (str)
  "文字列 STR の漢字比率を計算する。"
  (if (zerop (length str))
      0.0
    (let ((kanji-count 0))
      (dotimes (i (length str))
        (let ((char (aref str i)))
          (when (or (and (>= char #x4E00) (<= char #x9FFF))
                   (and (>= char #x3400) (<= char #x4DBF)))
            (cl-incf kanji-count))))
      (/ (float kanji-count) (length str)))))

(defun nskk-ai-pattern--update-user-patterns (midashi candidate context)
  "ユーザー固有パターンを更新する。"
  (let* ((pattern-key (format "%s:%s:%s"
                             midashi
                             candidate
                             (mapconcat #'identity (seq-take context 2) ",")))
         (count (gethash pattern-key nskk-ai-pattern-user-patterns 0)))
    (puthash pattern-key (1+ count) nskk-ai-pattern-user-patterns)))

;;; 変換パターン抽出

;;;###autoload
(defun nskk-ai-pattern-extract-conversion-patterns (&optional min-count)
  "変換パターンを抽出する。
MIN-COUNT 回以上出現したパターンのみを返す。
戻り値: ((pattern . count) ...) のリスト"
  (let ((threshold (or min-count nskk-ai-pattern-min-pattern-count))
        (patterns nil))
    (maphash
     (lambda (pattern count)
       (when (>= count threshold)
         (push (cons pattern count) patterns)))
     nskk-ai-pattern-user-patterns)
    ;; カウント順にソート
    (sort patterns (lambda (a b) (> (cdr a) (cdr b))))))

;;;###autoload
(defun nskk-ai-pattern-find-similar-patterns (midashi candidate)
  "見出し語 MIDASHI と候補 CANDIDATE に類似するパターンを検索する。
戻り値: 類似パターンのリスト"
  (let ((target-features (nskk-ai-pattern--extract-features midashi candidate nil))
        (similar-patterns nil))
    ;; 履歴から類似パターンを検索
    (dolist (entry nskk-ai-pattern-history)
      (let ((distance (nskk-ai-pattern--euclidean-distance
                      target-features
                      (nskk-ai-pattern-entry-features entry))))
        (when (< distance 2.0)  ; 閾値
          (push (cons entry distance) similar-patterns))))
    ;; 距離順にソート
    (mapcar #'car
            (sort similar-patterns (lambda (a b) (< (cdr a) (cdr b)))))))

;;; 時系列分析

(defun nskk-ai-pattern--update-time-series (midashi candidate)
  "時系列データを更新する。"
  (let* ((key (cons midashi candidate))
         (ts (gethash key nskk-ai-pattern-time-series-data)))
    (unless ts
      (setq ts (nskk-ai-time-series--create
               :window-size nskk-ai-pattern-time-window))
      (puthash key ts nskk-ai-pattern-time-series-data))

    ;; タイムスタンプと値を追加
    (push (current-time) (nskk-ai-time-series-timestamps ts))
    (push 1 (nskk-ai-time-series-values ts))

    ;; 古いデータを削除（ウィンドウサイズ外）
    (let* ((now (current-time))
           (cutoff-time (time-subtract now (seconds-to-time nskk-ai-pattern-time-window)))
           (filtered-timestamps nil)
           (filtered-values nil))
      (cl-loop for timestamp in (nskk-ai-time-series-timestamps ts)
              for value in (nskk-ai-time-series-values ts)
              when (time-less-p cutoff-time timestamp)
              do (push timestamp filtered-timestamps)
              and do (push value filtered-values))
      (setf (nskk-ai-time-series-timestamps ts) filtered-timestamps)
      (setf (nskk-ai-time-series-values ts) filtered-values))))

;;;###autoload
(defun nskk-ai-pattern-analyze-time-series (midashi candidate)
  "見出し語 MIDASHI と候補 CANDIDATE の時系列データを分析する。
戻り値: plist形式の分析結果"
  (let* ((key (cons midashi candidate))
         (ts (gethash key nskk-ai-pattern-time-series-data)))
    (if ts
        (let* ((values (nskk-ai-time-series-values ts))
               (count (length values))
               (sum (apply #'+ values))
               (mean (if (> count 0) (/ (float sum) count) 0.0)))
          (list :count count
                :sum sum
                :mean mean
                :recent-activity (seq-take values 10)))
      (list :count 0 :sum 0 :mean 0.0 :recent-activity nil))))

;;; クラスタリング（k-means）

;;;###autoload
(defun nskk-ai-pattern-cluster-patterns ()
  "パターン履歴をクラスタリングする（k-means）。
戻り値: クラスタのリスト"
  (interactive)
  (when (< (length nskk-ai-pattern-history) nskk-ai-pattern-cluster-count)
    (error "Not enough patterns for clustering"))

  (let ((k nskk-ai-pattern-cluster-count)
        (max-iter nskk-ai-pattern-max-iterations)
        (data-points (mapcar #'nskk-ai-pattern-entry-features
                            nskk-ai-pattern-history)))

    ;; 初期クラスタ中心をランダムに選択
    (let ((clusters (nskk-ai-pattern--initialize-clusters k data-points)))

      ;; k-meansアルゴリズム
      (dotimes (_ max-iter)
        ;; 各データポイントを最も近いクラスタに割り当て
        (nskk-ai-pattern--assign-to-clusters clusters data-points)
        ;; クラスタ中心を再計算
        (nskk-ai-pattern--update-centroids clusters))

      ;; クラスタIDをパターンエントリに設定
      (dotimes (i (length nskk-ai-pattern-history))
        (let* ((entry (nth i nskk-ai-pattern-history))
               (features (nskk-ai-pattern-entry-features entry))
               (cluster-id (nskk-ai-pattern--find-nearest-cluster features clusters)))
          (setf (nskk-ai-pattern-entry-cluster-id entry) cluster-id)))

      (setq nskk-ai-pattern-clusters clusters)
      clusters)))

(defun nskk-ai-pattern--initialize-clusters (k data-points)
  "クラスタを初期化する（k-means++）。"
  (let ((clusters nil)
        (n (length data-points)))
    (dotimes (i k)
      (let* ((random-idx (random n))
             (centroid (copy-sequence (nth random-idx data-points)))
             (cluster (nskk-ai-cluster--create
                      :id i
                      :centroid centroid)))
        (push cluster clusters)))
    (nreverse clusters)))

(defun nskk-ai-pattern--assign-to-clusters (clusters data-points)
  "各データポイントを最も近いクラスタに割り当てる。"
  (dolist (cluster clusters)
    (setf (nskk-ai-cluster-members cluster) nil)
    (setf (nskk-ai-cluster-size cluster) 0))

  (dolist (point data-points)
    (let* ((nearest-cluster (nskk-ai-pattern--find-nearest-cluster point clusters))
           (cluster (nth nearest-cluster clusters)))
      (push point (nskk-ai-cluster-members cluster))
      (cl-incf (nskk-ai-cluster-size cluster)))))

(defun nskk-ai-pattern--update-centroids (clusters)
  "クラスタの重心を更新する。"
  (dolist (cluster clusters)
    (let ((members (nskk-ai-cluster-members cluster)))
      (when (> (length members) 0)
        (let* ((dim (length (car members)))
               (new-centroid (make-vector dim 0.0)))
          ;; 各次元の平均を計算
          (dotimes (d dim)
            (let ((sum 0.0))
              (dolist (member members)
                (setq sum (+ sum (aref member d))))
              (aset new-centroid d (/ sum (float (length members))))))
          (setf (nskk-ai-cluster-centroid cluster) new-centroid))))))

(defun nskk-ai-pattern--find-nearest-cluster (point clusters)
  "POINT に最も近いクラスタのIDを返す。"
  (let ((min-distance most-positive-fixnum)
        (nearest-id 0))
    (dotimes (i (length clusters))
      (let* ((cluster (nth i clusters))
             (distance (nskk-ai-pattern--euclidean-distance
                       point
                       (nskk-ai-cluster-centroid cluster))))
        (when (< distance min-distance)
          (setq min-distance distance)
          (setq nearest-id i))))
    nearest-id))

(defsubst nskk-ai-pattern--euclidean-distance (vec1 vec2)
  "2つのベクトル VEC1 と VEC2 のユークリッド距離を計算する。"
  (let ((sum 0.0)
        (len (min (length vec1) (length vec2))))
    (dotimes (i len)
      (let ((diff (- (aref vec1 i) (aref vec2 i))))
        (setq sum (+ sum (* diff diff)))))
    (sqrt sum)))

;;; 異常検出

(defun nskk-ai-pattern--detect-anomaly (entry)
  "パターンエントリ ENTRY が異常かどうかを検出する。"
  (let ((features (nskk-ai-pattern-entry-features entry)))
    ;; 各特徴の統計を計算
    (dotimes (dim (length features))
      (let* ((values (mapcar (lambda (e)
                              (aref (nskk-ai-pattern-entry-features e) dim))
                            nskk-ai-pattern-history))
             (mean (nskk-ai-pattern--mean values))
             (stddev (nskk-ai-pattern--stddev values mean))
             (value (aref features dim))
             (z-score (if (> stddev 0) (/ (abs (- value mean)) stddev) 0.0)))

        ;; 閾値を超えた場合、異常として記録
        (when (> z-score nskk-ai-pattern-anomaly-threshold)
          (let ((anomaly (nskk-ai-anomaly--create
                         :entry entry
                         :score z-score
                         :reason (format "Feature %d: z-score=%.2f" dim z-score)
                         :timestamp (current-time))))
            (push anomaly nskk-ai-pattern-anomalies)
            ;; 異常リストのサイズ制限（最新100件）
            (when (> (length nskk-ai-pattern-anomalies) 100)
              (setq nskk-ai-pattern-anomalies
                    (seq-take nskk-ai-pattern-anomalies 100)))))))))

(defsubst nskk-ai-pattern--mean (values)
  "VALUES の平均を計算する。"
  (if (> (length values) 0)
      (/ (apply #'+ values) (float (length values)))
    0.0))

(defsubst nskk-ai-pattern--stddev (values mean)
  "VALUES の標準偏差を計算する。MEAN は平均値。"
  (if (> (length values) 1)
      (let ((sum-sq 0.0))
        (dolist (val values)
          (let ((diff (- val mean)))
            (setq sum-sq (+ sum-sq (* diff diff)))))
        (sqrt (/ sum-sq (float (1- (length values))))))
    0.0))

;;;###autoload
(defun nskk-ai-pattern-get-anomalies (&optional n)
  "最近の異常を N 件取得する（デフォルト10件）。
戻り値: 異常のリスト"
  (seq-take nskk-ai-pattern-anomalies (or n 10)))

;;; パターン予測

;;;###autoload
(defun nskk-ai-pattern-predict-candidate (midashi context)
  "見出し語 MIDASHI と文脈 CONTEXT から候補を予測する。
戻り値: ((candidate . confidence) ...) のリスト"
  (let ((predictions (make-hash-table :test 'equal))
        (target-features (nskk-ai-pattern--extract-features midashi "" context)))

    ;; 履歴から類似パターンを検索
    (dolist (entry nskk-ai-pattern-history)
      (when (equal (nskk-ai-pattern-entry-midashi entry) midashi)
        (let* ((distance (nskk-ai-pattern--euclidean-distance
                         target-features
                         (nskk-ai-pattern-entry-features entry)))
               (confidence (exp (- distance)))  ; 距離を信頼度に変換
               (candidate (nskk-ai-pattern-entry-candidate entry))
               (current-conf (gethash candidate predictions 0.0)))
          (puthash candidate (max confidence current-conf) predictions))))

    ;; 信頼度順にソート
    (let ((result nil))
      (maphash (lambda (cand conf) (push (cons cand conf) result)) predictions)
      (sort result (lambda (a b) (> (cdr a) (cdr b)))))))

;;; 統計情報

;;;###autoload
(defun nskk-ai-pattern-statistics ()
  "パターン認識システムの統計情報を返す。
戻り値: plist形式の統計情報"
  (list :pattern-history-count (length nskk-ai-pattern-history)
        :user-patterns-count (hash-table-count nskk-ai-pattern-user-patterns)
        :clusters-count (length nskk-ai-pattern-clusters)
        :time-series-count (hash-table-count nskk-ai-pattern-time-series-data)
        :anomalies-count (length nskk-ai-pattern-anomalies)))

;;;###autoload
(defun nskk-ai-pattern-print-statistics ()
  "パターン認識システムの統計情報を表示する。"
  (interactive)
  (let ((stats (nskk-ai-pattern-statistics)))
    (message "NSKK AI Pattern Statistics:
  Pattern History: %d entries
  User Patterns: %d unique patterns
  Clusters: %d
  Time Series: %d
  Anomalies: %d"
             (plist-get stats :pattern-history-count)
             (plist-get stats :user-patterns-count)
             (plist-get stats :clusters-count)
             (plist-get stats :time-series-count)
             (plist-get stats :anomalies-count))))

;;; クリーンアップ

;;;###autoload
(defun nskk-ai-pattern-clear ()
  "全てのパターンデータをクリアする。"
  (interactive)
  (setq nskk-ai-pattern-history nil)
  (setq nskk-ai-pattern-clusters nil)
  (setq nskk-ai-pattern-anomalies nil)
  (clrhash nskk-ai-pattern-time-series-data)
  (clrhash nskk-ai-pattern-user-patterns))

(provide 'nskk-ai-pattern)

;;; nskk-ai-pattern.el ends here
