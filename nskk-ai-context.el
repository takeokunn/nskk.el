;;; nskk-ai-context.el --- AI-powered context understanding for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, ai, context
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

;; このファイルはAI駆動の文脈理解エンジンを実装します。
;;
;; 特徴:
;; - 文章解析（sentence parsing）
;; - 文脈スコアリング（context scoring）
;; - 意味理解（semantic understanding）
;; - トピック抽出（topic extraction）
;;
;; アルゴリズム:
;; - N-gram統計モデル（2-gram, 3-gram, 4-gram）
;; - TF-IDF（単語重要度計算）
;; - 簡易的な単語埋め込み（word embeddings）
;; - コサイン類似度計算
;;
;; パフォーマンス目標:
;; - 文脈解析: < 5ms
;; - スコアリング: < 2ms
;; - トピック抽出: < 10ms

;;; Code:

(require 'cl-lib)

;;; カスタマイズ変数

(defgroup nskk-ai-context nil
  "AI-powered context understanding for NSKK."
  :group 'nskk
  :prefix "nskk-ai-context-")

(defcustom nskk-ai-context-enable-tfidf t
  "非nilの場合、TF-IDF計算を有効にする。"
  :type 'boolean
  :group 'nskk-ai-context)

(defcustom nskk-ai-context-enable-embeddings t
  "非nilの場合、単語埋め込みを有効にする。"
  :type 'boolean
  :group 'nskk-ai-context)

(defcustom nskk-ai-context-window-size 50
  "文脈分析のウィンドウサイズ（文字数）。"
  :type 'integer
  :group 'nskk-ai-context)

(defcustom nskk-ai-context-max-ngrams 100000
  "N-gramの最大保存数。"
  :type 'integer
  :group 'nskk-ai-context)

(defcustom nskk-ai-context-embedding-dimensions 50
  "単語埋め込みの次元数。"
  :type 'integer
  :group 'nskk-ai-context)

(defcustom nskk-ai-context-topic-count 5
  "抽出するトピックの最大数。"
  :type 'integer
  :group 'nskk-ai-context)

;;; データ構造

(cl-defstruct (nskk-ai-ngram-model
               (:constructor nskk-ai-ngram-model--create)
               (:copier nil))
  "N-gramモデル。

スロット:
  n           - N-gramのサイズ（2, 3, 4）
  model       - N-gramのハッシュテーブル
  frequencies - 頻度カウント
  total-count - 総N-gram数"
  (n 2 :type integer)
  (model (make-hash-table :test 'equal))
  (frequencies (make-hash-table :test 'equal))
  (total-count 0 :type integer))

(cl-defstruct (nskk-ai-tfidf-model
               (:constructor nskk-ai-tfidf-model--create)
               (:copier nil))
  "TF-IDFモデル。

スロット:
  term-freq    - 単語頻度（ハッシュテーブル）
  doc-freq     - 文書頻度（ハッシュテーブル）
  idf-cache    - IDF計算結果のキャッシュ
  num-docs     - 総文書数"
  (term-freq (make-hash-table :test 'equal))
  (doc-freq (make-hash-table :test 'equal))
  (idf-cache (make-hash-table :test 'equal))
  (num-docs 0 :type integer))

(cl-defstruct (nskk-ai-embedding-model
               (:constructor nskk-ai-embedding-model--create)
               (:copier nil))
  "単語埋め込みモデル（簡易版）。

スロット:
  vocab       - 語彙（単語 -> ID）
  vectors     - ベクトル表現（ID -> ベクトル）
  dimensions  - 埋め込み次元数
  co-occurrence - 共起行列"
  (vocab (make-hash-table :test 'equal))
  (vectors (make-hash-table :test 'eql))
  (dimensions 50 :type integer)
  (co-occurrence (make-hash-table :test 'equal)))

(cl-defstruct (nskk-ai-context-state
               (:constructor nskk-ai-context-state--create)
               (:copier nil))
  "文脈状態。

スロット:
  text          - 現在の文脈テキスト
  words         - 単語リスト
  topics        - 抽出されたトピック
  scores        - 単語スコア
  last-update   - 最終更新時刻"
  (text "" :type string)
  (words nil :type list)
  (topics nil :type list)
  (scores (make-hash-table :test 'equal))
  (last-update nil :type list))

;;; グローバル変数

(defvar nskk-ai-context-bigram-model
  (nskk-ai-ngram-model--create :n 2)
  "バイグラムモデル。")

(defvar nskk-ai-context-trigram-model
  (nskk-ai-ngram-model--create :n 3)
  "トライグラムモデル。")

(defvar nskk-ai-context-4gram-model
  (nskk-ai-ngram-model--create :n 4)
  "4-gramモデル。")

(defvar nskk-ai-context-tfidf-model
  (nskk-ai-tfidf-model--create)
  "TF-IDFモデル。")

(defvar nskk-ai-context-embedding-model
  (nskk-ai-embedding-model--create
   :dimensions nskk-ai-context-embedding-dimensions)
  "単語埋め込みモデル。")

(defvar nskk-ai-context-current-state
  (nskk-ai-context-state--create)
  "現在の文脈状態。")

;;; 文章解析

;;;###autoload
(defun nskk-ai-context-parse-sentence (text)
  "テキスト TEXT を解析して単語リストに変換する。
日本語の場合は文字単位で分割（簡易版）。
戻り値: 単語リスト"
  (when (stringp text)
    ;; 簡易的な単語分割（文字単位＋英数字まとまり）
    (let ((words nil)
          (i 0)
          (len (length text))
          (current-word ""))
      (while (< i len)
        (let ((char (aref text i)))
          (cond
           ;; 英数字はまとめる
           ((or (and (>= char ?a) (<= char ?z))
                (and (>= char ?A) (<= char ?Z))
                (and (>= char ?0) (<= char ?9)))
            (setq current-word (concat current-word (char-to-string char))))
           ;; スペース・句読点は区切り
           ((memq char '(?\s ?\t ?\n ?、 ?。 ?！ ?？ ?, ?. ?! ??))
            (when (> (length current-word) 0)
              (push current-word words)
              (setq current-word ""))
            (unless (memq char '(?\s ?\t ?\n))
              (push (char-to-string char) words)))
           ;; その他の文字は1文字ずつ
           (t
            (when (> (length current-word) 0)
              (push current-word words)
              (setq current-word ""))
            (push (char-to-string char) words))))
        (setq i (1+ i)))
      (when (> (length current-word) 0)
        (push current-word words))
      (nreverse words))))

;;;###autoload
(defun nskk-ai-context-extract-context (&optional window-size)
  "現在のバッファから文脈を抽出する。
WINDOW-SIZE 文字分の文脈を取得（デフォルト: `nskk-ai-context-window-size`）。
戻り値: 文脈テキスト"
  (let ((size (or window-size nskk-ai-context-window-size))
        (pos (point)))
    (buffer-substring-no-properties
     (max (point-min) (- pos size))
     pos)))

;;; N-gram学習

(defun nskk-ai-context--update-ngram (model words)
  "N-gramモデル MODEL を単語リスト WORDS で更新する。"
  (let ((n (nskk-ai-ngram-model-n model))
        (model-table (nskk-ai-ngram-model-model model))
        (freq-table (nskk-ai-ngram-model-frequencies model)))
    (when (>= (length words) n)
      (dotimes (i (- (length words) n -1))
        (let ((ngram (cl-subseq words i (+ i n))))
          ;; N-gram全体をキーとして保存
          (let ((key (mapconcat #'identity ngram " "))
                (prefix (mapconcat #'identity (butlast ngram) " "))
                (word (car (last ngram))))
            ;; prefix -> word のマッピング
            (let ((candidates (gethash prefix model-table nil)))
              (puthash prefix
                      (if (member word candidates)
                          candidates
                        (cons word candidates))
                      model-table))
            ;; 頻度カウント
            (puthash key (1+ (gethash key freq-table 0)) freq-table)
            (cl-incf (nskk-ai-ngram-model-total-count model))))))))

;;;###autoload
(defun nskk-ai-context-learn-text (text)
  "テキスト TEXT を学習する。
N-gram、TF-IDF、単語埋め込みを更新。"
  (when (and text (> (length text) 0))
    (let ((words (nskk-ai-context-parse-sentence text)))
      ;; N-gram学習
      (nskk-ai-context--update-ngram nskk-ai-context-bigram-model words)
      (nskk-ai-context--update-ngram nskk-ai-context-trigram-model words)
      (nskk-ai-context--update-ngram nskk-ai-context-4gram-model words)

      ;; TF-IDF学習
      (when nskk-ai-context-enable-tfidf
        (nskk-ai-context--update-tfidf text words))

      ;; 単語埋め込み学習（共起ベース）
      (when nskk-ai-context-enable-embeddings
        (nskk-ai-context--update-embeddings words))

      ;; 現在の状態を更新
      (setf (nskk-ai-context-state-text nskk-ai-context-current-state) text)
      (setf (nskk-ai-context-state-words nskk-ai-context-current-state) words)
      (setf (nskk-ai-context-state-last-update nskk-ai-context-current-state)
            (current-time))

      ;; エントリ数制限チェック
      (nskk-ai-context--check-limits))))

;;; TF-IDF計算

(defun nskk-ai-context--update-tfidf (text words)
  "TF-IDFモデルを更新する。TEXT は文書、WORDS は単語リスト。"
  (let ((model nskk-ai-context-tfidf-model)
        (word-set (make-hash-table :test 'equal)))
    ;; 文書内の単語をセットに追加
    (dolist (word words)
      (puthash word t word-set)
      ;; 単語頻度を更新
      (puthash word (1+ (gethash word (nskk-ai-tfidf-model-term-freq model) 0))
               (nskk-ai-tfidf-model-term-freq model)))

    ;; 文書頻度を更新（この文書に含まれる各単語）
    (maphash
     (lambda (word _)
       (puthash word (1+ (gethash word (nskk-ai-tfidf-model-doc-freq model) 0))
                (nskk-ai-tfidf-model-doc-freq model)))
     word-set)

    ;; 文書数を増やす
    (cl-incf (nskk-ai-tfidf-model-num-docs model))

    ;; IDFキャッシュをクリア
    (clrhash (nskk-ai-tfidf-model-idf-cache model))))

(defsubst nskk-ai-context--calc-idf (word)
  "単語 WORD のIDF（逆文書頻度）を計算する。"
  (let* ((model nskk-ai-context-tfidf-model)
         (cached (gethash word (nskk-ai-tfidf-model-idf-cache model))))
    (if cached
        cached
      (let* ((df (gethash word (nskk-ai-tfidf-model-doc-freq model) 0))
             (n (max 1 (nskk-ai-tfidf-model-num-docs model)))
             (idf (if (> df 0)
                      (log (/ (float n) df))
                    0.0)))
        (puthash word idf (nskk-ai-tfidf-model-idf-cache model))
        idf))))

(defsubst nskk-ai-context--calc-tf (word words)
  "単語リスト WORDS における単語 WORD のTF（単語頻度）を計算する。"
  (let ((count 0))
    (dolist (w words)
      (when (equal w word)
        (cl-incf count)))
    (/ (float count) (max 1 (length words)))))

;;;###autoload
(defun nskk-ai-context-calc-tfidf-score (word words)
  "単語 WORD の TF-IDF スコアを計算する。
WORDS は文脈の単語リスト。"
  (when nskk-ai-context-enable-tfidf
    (* (nskk-ai-context--calc-tf word words)
       (nskk-ai-context--calc-idf word))))

;;; 単語埋め込み（簡易版）

(defun nskk-ai-context--update-embeddings (words)
  "共起情報から単語埋め込みを更新する（簡易版）。
WORDS は単語リスト。"
  (let ((model nskk-ai-context-embedding-model)
        (window 2))  ; 共起ウィンドウサイズ
    ;; 語彙に追加
    (dolist (word words)
      (unless (gethash word (nskk-ai-embedding-model-vocab model))
        (let ((id (hash-table-count (nskk-ai-embedding-model-vocab model))))
          (puthash word id (nskk-ai-embedding-model-vocab model)))))

    ;; 共起行列を更新
    (dotimes (i (length words))
      (let ((center-word (nth i words)))
        (dotimes (j (1+ (* 2 window)))
          (let ((offset (- j window)))
            (when (and (/= offset 0)
                      (>= (+ i offset) 0)
                      (< (+ i offset) (length words)))
              (let* ((context-word (nth (+ i offset) words))
                     (key (cons center-word context-word)))
                (puthash key (1+ (gethash key (nskk-ai-embedding-model-co-occurrence model) 0))
                        (nskk-ai-embedding-model-co-occurrence model))))))))))

(defsubst nskk-ai-context--get-embedding-vector (word)
  "単語 WORD の埋め込みベクトルを取得する（簡易版）。
実際のベクトルではなく、共起情報ベースのスパースベクトル。"
  (let ((model nskk-ai-context-embedding-model)
        (vector (make-hash-table :test 'equal)))
    (maphash
     (lambda (key count)
       (when (equal (car key) word)
         (puthash (cdr key) (float count) vector)))
     (nskk-ai-embedding-model-co-occurrence model))
    vector))

(defsubst nskk-ai-context--cosine-similarity (vec1 vec2)
  "2つのスパースベクトル VEC1 と VEC2 のコサイン類似度を計算する。"
  (let ((dot-product 0.0)
        (norm1 0.0)
        (norm2 0.0))
    ;; VEC1のノルムと内積計算
    (maphash
     (lambda (word val1)
       (setq norm1 (+ norm1 (* val1 val1)))
       (let ((val2 (gethash word vec2 0.0)))
         (setq dot-product (+ dot-product (* val1 val2)))))
     vec1)
    ;; VEC2のノルム計算
    (maphash
     (lambda (_word val2)
       (setq norm2 (+ norm2 (* val2 val2))))
     vec2)

    (if (and (> norm1 0) (> norm2 0))
        (/ dot-product (* (sqrt norm1) (sqrt norm2)))
      0.0)))

;;;###autoload
(defun nskk-ai-context-calc-similarity (word1 word2)
  "2つの単語 WORD1 と WORD2 の意味的類似度を計算する。
戻り値: 0.0 - 1.0 の類似度スコア"
  (when nskk-ai-context-enable-embeddings
    (let ((vec1 (nskk-ai-context--get-embedding-vector word1))
          (vec2 (nskk-ai-context--get-embedding-vector word2)))
      (nskk-ai-context--cosine-similarity vec1 vec2))))

;;; 文脈スコアリング

;;;###autoload
(defun nskk-ai-context-score-candidate (midashi candidate)
  "見出し語 MIDASHI と候補 CANDIDATE の文脈スコアを計算する。
現在の文脈に基づいて、候補の適切さをスコアリング。
戻り値: 0.0 - 1.0 のスコア"
  (let ((context-words (nskk-ai-context-state-words nskk-ai-context-current-state))
        (score 0.0)
        (weight-count 0))

    (when (> (length context-words) 0)
      ;; N-gramスコア
      (let ((ngram-score (nskk-ai-context--calc-ngram-score candidate context-words)))
        (setq score (+ score (* 0.4 ngram-score)))
        (cl-incf weight-count))

      ;; TF-IDFスコア
      (when nskk-ai-context-enable-tfidf
        (let ((tfidf-score (or (nskk-ai-context-calc-tfidf-score candidate context-words) 0.0)))
          (setq score (+ score (* 0.3 (min 1.0 tfidf-score))))
          (cl-incf weight-count)))

      ;; 意味的類似度スコア
      (when (and nskk-ai-context-enable-embeddings (> (length context-words) 0))
        (let ((sim-score (nskk-ai-context--calc-semantic-score candidate context-words)))
          (setq score (+ score (* 0.3 sim-score)))
          (cl-incf weight-count))))

    (if (> weight-count 0)
        (/ score weight-count)
      0.0)))

(defun nskk-ai-context--calc-ngram-score (word context-words)
  "N-gramモデルに基づいて単語 WORD のスコアを計算する。
CONTEXT-WORDS は文脈の単語リスト。"
  (let ((score 0.0)
        (count 0))
    ;; 直前の単語とのバイグラムスコア
    (when (>= (length context-words) 1)
      (let ((prev (car (last context-words))))
        (let ((prob (nskk-ai-context--ngram-probability
                    nskk-ai-context-bigram-model
                    (list prev) word)))
          (setq score (+ score prob))
          (cl-incf count))))

    ;; 直前2単語とのトライグラムスコア
    (when (>= (length context-words) 2)
      (let ((prev-2 (nth (- (length context-words) 2) context-words))
            (prev-1 (car (last context-words))))
        (let ((prob (nskk-ai-context--ngram-probability
                    nskk-ai-context-trigram-model
                    (list prev-2 prev-1) word)))
          (setq score (+ score (* 2.0 prob)))  ; トライグラムに高重み
          (cl-incf count 2))))

    (if (> count 0)
        (/ score count)
      0.0)))

(defsubst nskk-ai-context--ngram-probability (model prefix-words word)
  "N-gramモデル MODEL において PREFIX-WORDS の後に WORD が続く確率。"
  (let* ((prefix (mapconcat #'identity prefix-words " "))
         (key (concat prefix " " word))
         (freq-table (nskk-ai-ngram-model-frequencies model))
         (ngram-count (gethash key freq-table 0))
         (prefix-count 0))
    ;; prefix の出現回数を計算
    (maphash
     (lambda (k v)
       (when (string-prefix-p prefix k)
         (setq prefix-count (+ prefix-count v))))
     freq-table)

    (if (> prefix-count 0)
        (/ (float ngram-count) prefix-count)
      0.0)))

(defun nskk-ai-context--calc-semantic-score (word context-words)
  "単語 WORD と文脈単語 CONTEXT-WORDS の意味的類似度スコアを計算する。"
  (let ((max-sim 0.0))
    ;; 直近5単語との類似度を計算
    (dolist (context-word (seq-take (reverse context-words) 5))
      (let ((sim (or (nskk-ai-context-calc-similarity word context-word) 0.0)))
        (setq max-sim (max max-sim sim))))
    max-sim))

;;; トピック抽出

;;;###autoload
(defun nskk-ai-context-extract-topics (&optional n)
  "現在の文脈からトピックを抽出する。
N 個のトピック単語を返す（デフォルト: `nskk-ai-context-topic-count`）。
戻り値: ((word . score) ...) のリスト"
  (let ((count (or n nskk-ai-context-topic-count))
        (words (nskk-ai-context-state-words nskk-ai-context-current-state))
        (topic-scores (make-hash-table :test 'equal)))

    ;; 各単語のTF-IDFスコアを計算
    (dolist (word words)
      (when (> (length word) 1)  ; 1文字単語は除外
        (let ((score (or (nskk-ai-context-calc-tfidf-score word words) 0.0)))
          (puthash word (max score (gethash word topic-scores 0.0)) topic-scores))))

    ;; スコア順にソート
    (let ((topics nil))
      (maphash (lambda (word score) (push (cons word score) topics)) topic-scores)
      (setq topics (sort topics (lambda (a b) (> (cdr a) (cdr b)))))

      ;; 上位N個を返す
      (let ((result (seq-take topics count)))
        (setf (nskk-ai-context-state-topics nskk-ai-context-current-state) result)
        result))))

;;; 意味理解

;;;###autoload
(defun nskk-ai-context-understand-meaning (text)
  "テキスト TEXT の意味を理解する（簡易版）。
トピック抽出と文脈分析を組み合わせて、テキストの要約を返す。
戻り値: plist形式の分析結果"
  (let ((words (nskk-ai-context-parse-sentence text)))
    (list :text text
          :word-count (length words)
          :unique-words (hash-table-count
                        (let ((ht (make-hash-table :test 'equal)))
                          (dolist (w words ht) (puthash w t ht))))
          :topics (let ((scores (make-hash-table :test 'equal)))
                   (dolist (word words)
                     (when (> (length word) 1)
                       (let ((score (or (nskk-ai-context-calc-tfidf-score word words) 0.0)))
                         (puthash word score scores))))
                   (let ((topics nil))
                     (maphash (lambda (w s) (push (cons w s) topics)) scores)
                     (seq-take (sort topics (lambda (a b) (> (cdr a) (cdr b)))) 3)))
          :avg-word-length (if (> (length words) 0)
                              (/ (apply #'+ (mapcar #'length words))
                                 (float (length words)))
                            0.0))))

;;; エントリ数制限

(defun nskk-ai-context--check-limits ()
  "モデルのエントリ数が上限を超えた場合、低頻度エントリを削除する。"
  ;; N-gramモデルの制限
  (dolist (model (list nskk-ai-context-bigram-model
                      nskk-ai-context-trigram-model
                      nskk-ai-context-4gram-model))
    (when (> (hash-table-count (nskk-ai-ngram-model-frequencies model))
            nskk-ai-context-max-ngrams)
      (nskk-ai-context--prune-ngram model)))

  ;; 共起行列の制限
  (when (> (hash-table-count (nskk-ai-embedding-model-co-occurrence
                             nskk-ai-context-embedding-model))
          (* 2 nskk-ai-context-max-ngrams))
    (nskk-ai-context--prune-cooccurrence)))

(defun nskk-ai-context--prune-ngram (model)
  "N-gramモデル MODEL から低頻度エントリを削除する。"
  (let* ((freq-table (nskk-ai-ngram-model-frequencies model))
         (entries nil)
         (keep-count (floor (* nskk-ai-context-max-ngrams 0.9))))
    ;; 全エントリをリストに変換
    (maphash (lambda (k v) (push (cons k v) entries)) freq-table)
    ;; 頻度でソート
    (setq entries (sort entries (lambda (a b) (> (cdr a) (cdr b)))))
    ;; 上位エントリのみ保持
    (clrhash freq-table)
    (dolist (entry (seq-take entries keep-count))
      (puthash (car entry) (cdr entry) freq-table))))

(defun nskk-ai-context--prune-cooccurrence ()
  "共起行列から低頻度エントリを削除する。"
  (let* ((co-occ (nskk-ai-embedding-model-co-occurrence nskk-ai-context-embedding-model))
         (entries nil)
         (keep-count (floor (* nskk-ai-context-max-ngrams 1.8))))
    (maphash (lambda (k v) (push (cons k v) entries)) co-occ)
    (setq entries (sort entries (lambda (a b) (> (cdr a) (cdr b)))))
    (clrhash co-occ)
    (dolist (entry (seq-take entries keep-count))
      (puthash (car entry) (cdr entry) co-occ))))

;;; 統計情報

;;;###autoload
(defun nskk-ai-context-statistics ()
  "文脈エンジンの統計情報を返す。
戻り値: plist形式の統計情報"
  (list :bigram-count (hash-table-count (nskk-ai-ngram-model-frequencies nskk-ai-context-bigram-model))
        :trigram-count (hash-table-count (nskk-ai-ngram-model-frequencies nskk-ai-context-trigram-model))
        :4gram-count (hash-table-count (nskk-ai-ngram-model-frequencies nskk-ai-context-4gram-model))
        :tfidf-terms (hash-table-count (nskk-ai-tfidf-model-term-freq nskk-ai-context-tfidf-model))
        :tfidf-docs (nskk-ai-tfidf-model-num-docs nskk-ai-context-tfidf-model)
        :vocab-size (hash-table-count (nskk-ai-embedding-model-vocab nskk-ai-context-embedding-model))
        :co-occurrence-count (hash-table-count (nskk-ai-embedding-model-co-occurrence nskk-ai-context-embedding-model))
        :current-words (length (nskk-ai-context-state-words nskk-ai-context-current-state))
        :current-topics (length (nskk-ai-context-state-topics nskk-ai-context-current-state))))

;;;###autoload
(defun nskk-ai-context-print-statistics ()
  "文脈エンジンの統計情報を表示する。"
  (interactive)
  (let ((stats (nskk-ai-context-statistics)))
    (message "NSKK AI Context Statistics:
  Bigrams: %d
  Trigrams: %d
  4-grams: %d
  TF-IDF Terms: %d (Docs: %d)
  Vocabulary: %d
  Co-occurrences: %d
  Current Context: %d words, %d topics"
             (plist-get stats :bigram-count)
             (plist-get stats :trigram-count)
             (plist-get stats :4gram-count)
             (plist-get stats :tfidf-terms)
             (plist-get stats :tfidf-docs)
             (plist-get stats :vocab-size)
             (plist-get stats :co-occurrence-count)
             (plist-get stats :current-words)
             (plist-get stats :current-topics))))

;;; クリーンアップ

;;;###autoload
(defun nskk-ai-context-clear ()
  "全ての文脈データをクリアする。"
  (interactive)
  (setq nskk-ai-context-bigram-model (nskk-ai-ngram-model--create :n 2))
  (setq nskk-ai-context-trigram-model (nskk-ai-ngram-model--create :n 3))
  (setq nskk-ai-context-4gram-model (nskk-ai-ngram-model--create :n 4))
  (setq nskk-ai-context-tfidf-model (nskk-ai-tfidf-model--create))
  (setq nskk-ai-context-embedding-model
        (nskk-ai-embedding-model--create :dimensions nskk-ai-context-embedding-dimensions))
  (setq nskk-ai-context-current-state (nskk-ai-context-state--create)))

(provide 'nskk-ai-context)

;;; nskk-ai-context.el ends here
