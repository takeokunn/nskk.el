;;; nskk-completion-context.el --- Context-aware completion for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, completion, context
;; Version: 0.1.0
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

;; このファイルはバイグラム/トライグラムを利用した文脈補完を実装します。
;;
;; 特徴:
;; - バイグラム（2単語）モデル
;; - トライグラム（3単語）モデル
;; - 文脈スコアリング
;; - 文脈履歴の学習と永続化
;;
;; パフォーマンス目標:
;; - 文脈検索: < 5ms
;; - スコア計算: < 2ms
;; - 学習更新: < 3ms
;;
;; 使用例:
;;
;;   (require 'nskk-completion-context)
;;
;;   ;; 文脈を考慮した補完
;;   (nskk-completion-context-search index "かん" :context '("これは"))
;;   ;; => 直前の単語を考慮した候補
;;
;;   ;; トライグラムモデル
;;   (nskk-completion-context-search index "です" :context '("便利" "とても") :model 'trigram)
;;
;;   ;; 文脈学習の更新
;;   (nskk-completion-context-learn ctx-model '("これは" "漢字"))

;;; Code:

(require 'cl-lib)
(require 'nskk-dict-struct)

;;; カスタマイズ変数

(defgroup nskk-completion-context nil
  "Context-aware completion for NSKK."
  :group 'nskk
  :prefix "nskk-completion-context-")

(defcustom nskk-completion-context-default-limit 20
  "デフォルトの補完候補最大数。"
  :type 'integer
  :group 'nskk-completion-context)

(defcustom nskk-completion-context-model-type 'bigram
  "使用する言語モデルのタイプ。
  - 'bigram: バイグラムモデル（2単語の連鎖）
  - 'trigram: トライグラムモデル（3単語の連鎖）"
  :type '(choice (const :tag "Bigram" bigram)
                 (const :tag "Trigram" trigram))
  :group 'nskk-completion-context)

(defcustom nskk-completion-context-window-size 50
  "文脈分析のウィンドウサイズ（文字数）。"
  :type 'integer
  :group 'nskk-completion-context)

(defcustom nskk-completion-context-smoothing-factor 0.1
  "スムージング係数（0.0-1.0）。
未知の組み合わせに対する確率の補正に使用。"
  :type 'float
  :group 'nskk-completion-context)

;;; データ構造

(cl-defstruct (nskk-completion-context-model
               (:constructor nskk-completion-context-model--create)
               (:copier nil))
  "文脈モデル構造。

スロット:
  bigram-table  - バイグラムテーブル（(word1 . word2) → count）
  trigram-table - トライグラムテーブル（(word1 word2 . word3) → count）
  unigram-table - ユニグラムテーブル（word → count）
  total-count   - 総観測数"
  (bigram-table nil :type hash-table)
  (trigram-table nil :type hash-table)
  (unigram-table nil :type hash-table)
  (total-count 0 :type integer))

(cl-defstruct (nskk-completion-context-result
               (:constructor nskk-completion-context-result--create)
               (:copier nil))
  "文脈補完結果。

スロット:
  midashi        - 見出し語
  entry          - 辞書エントリ
  context-score  - 文脈スコア（確率）
  frequency      - 頻度
  total-score    - 総合スコア"
  (midashi nil :type string)
  (entry nil :type nskk-dict-entry)
  (context-score 0.0 :type float)
  (frequency 0 :type integer)
  (total-score 0.0 :type float))

;;; 文脈モデルの初期化

;;;###autoload
(defun nskk-completion-context-model-create ()
  "空の文脈モデルを作成する。

戻り値:
  nskk-completion-context-model構造体"
  (nskk-completion-context-model--create
   :bigram-table (make-hash-table :test 'equal :size 10000)
   :trigram-table (make-hash-table :test 'equal :size 10000)
   :unigram-table (make-hash-table :test 'equal :size 10000)
   :total-count 0))

;;; メイン検索関数

;;;###autoload
(defun nskk-completion-context-search (index query &rest args)
  "文脈を考慮した補完検索を実行する。

引数:
  INDEX - nskk-dict-index構造体
  QUERY - 検索クエリ

キーワード引数:
  :context    - 文脈（直前の単語のリスト、新しい順）
  :model      - 文脈モデル（nskk-completion-context-model構造体）
  :limit      - 最大結果数
  :okuri-type - 送り仮名タイプ
  :model-type - モデルタイプ（'bigram/'trigram）

戻り値:
  nskk-completion-context-result構造体のリスト（スコア順）"
  (unless (stringp query)
    (error "Query must be a string: %s" query))

  (let* ((context (plist-get args :context))
         (ctx-model (plist-get args :model))
         (limit (or (plist-get args :limit)
                   nskk-completion-context-default-limit))
         (okuri-type (plist-get args :okuri-type))
         (model-type (or (plist-get args :model-type)
                        nskk-completion-context-model-type))
         (results nil)
         (tables (cond
                  ((eq okuri-type 'okuri-ari)
                   (list (nskk-dict-index-okuri-ari-table index)))
                  ((eq okuri-type 'okuri-nasi)
                   (list (nskk-dict-index-okuri-nasi-table index)))
                  (t
                   (list (nskk-dict-index-okuri-nasi-table index)
                         (nskk-dict-index-okuri-ari-table index))))))

    ;; クエリに前方一致する候補を収集
    (dolist (table tables)
      (maphash
       (lambda (midashi entry)
         (when (string-prefix-p query midashi)
           (let* ((context-score (if (and ctx-model context)
                                    (nskk-completion-context--calculate-score
                                     ctx-model context midashi model-type)
                                  0.5))
                  (freq (nskk-dict-entry-frequency entry))
                  (total-score (nskk-completion-context--combine-scores
                               context-score freq)))
             (push (nskk-completion-context-result--create
                    :midashi midashi
                    :entry entry
                    :context-score context-score
                    :frequency freq
                    :total-score total-score)
                   results))))
       table))

    ;; 総合スコアでソート
    (setq results (sort results
                       (lambda (a b)
                         (> (nskk-completion-context-result-total-score a)
                            (nskk-completion-context-result-total-score b)))))

    ;; limit適用
    (when (and limit (> (length results) limit))
      (setq results (seq-take results limit)))

    results))

;;; スコア計算

(defun nskk-completion-context--calculate-score (model context word model-type)
  "文脈スコアを計算する（内部関数）。

引数:
  MODEL      - 文脈モデル
  CONTEXT    - 文脈（直前の単語のリスト）
  WORD       - 候補単語
  MODEL-TYPE - モデルタイプ（'bigram/'trigram）

戻り値:
  文脈スコア（0.0-1.0の確率）"
  (pcase model-type
    ('bigram
     (nskk-completion-context--bigram-score model context word))
    ('trigram
     (nskk-completion-context--trigram-score model context word))
    (_
     (nskk-completion-context--bigram-score model context word))))

(defun nskk-completion-context--bigram-score (model context word)
  "バイグラムスコアを計算する（内部関数）。

引数:
  MODEL   - 文脈モデル
  CONTEXT - 文脈（直前の単語のリスト）
  WORD    - 候補単語

処理:
  P(word|prev) = count(prev, word) / count(prev)
  スムージングを適用

戻り値:
  バイグラムスコア"
  (if (null context)
      0.5  ; 文脈なしの場合は中立スコア
    (let* ((prev-word (car context))
           (bigram-key (cons prev-word word))
           (bigram-count (gethash bigram-key (nskk-completion-context-model-bigram-table model) 0))
           (prev-count (gethash prev-word (nskk-completion-context-model-unigram-table model) 0)))
      (if (zerop prev-count)
          nskk-completion-context-smoothing-factor
        ;; スムージング適用
        (/ (+ bigram-count nskk-completion-context-smoothing-factor)
           (+ prev-count 1.0))))))

(defun nskk-completion-context--trigram-score (model context word)
  "トライグラムスコアを計算する（内部関数）。

引数:
  MODEL   - 文脈モデル
  CONTEXT - 文脈（直前の単語のリスト）
  WORD    - 候補単語

処理:
  P(word|prev2,prev1) = count(prev2, prev1, word) / count(prev2, prev1)
  バイグラムスコアとのバックオフ

戻り値:
  トライグラムスコア"
  (if (< (length context) 2)
      ;; 文脈が不十分な場合はバイグラムにフォールバック
      (nskk-completion-context--bigram-score model context word)
    (let* ((prev1 (nth 0 context))
           (prev2 (nth 1 context))
           (trigram-key (cons prev2 (cons prev1 word)))
           (bigram-key (cons prev1 word))
           (trigram-count (gethash trigram-key (nskk-completion-context-model-trigram-table model) 0))
           (bigram-count-context (gethash (cons prev2 prev1)
                                         (nskk-completion-context-model-bigram-table model) 0)))
      (if (zerop bigram-count-context)
          ;; トライグラムデータがない場合はバイグラムにバックオフ
          (nskk-completion-context--bigram-score model (list prev1) word)
        ;; 補間法: トライグラム * 0.7 + バイグラム * 0.3
        (let ((trigram-prob (/ (+ trigram-count nskk-completion-context-smoothing-factor)
                              (+ bigram-count-context 1.0)))
              (bigram-prob (nskk-completion-context--bigram-score model (list prev1) word)))
          (+ (* 0.7 trigram-prob) (* 0.3 bigram-prob)))))))

(defun nskk-completion-context--combine-scores (context-score frequency)
  "文脈スコアと頻度を組み合わせて総合スコアを計算する（内部関数）。

引数:
  CONTEXT-SCORE - 文脈スコア（0.0-1.0）
  FREQUENCY     - 頻度

処理:
  総合スコア = context-score * 0.7 + frequency-score * 0.3

戻り値:
  総合スコア"
  (let ((freq-score (/ (float frequency) (+ frequency 10.0))))
    (+ (* context-score 0.7) (* freq-score 0.3))))

;;; 学習機能

;;;###autoload
(defun nskk-completion-context-learn (model word-sequence)
  "文脈モデルを更新する（学習）。

引数:
  MODEL         - 文脈モデル
  WORD-SEQUENCE - 単語シーケンス（リスト）

処理:
  1. ユニグラム、バイグラム、トライグラムを抽出
  2. 各カウントをインクリメント

戻り値:
  更新された文脈モデル"
  (unless (listp word-sequence)
    (error "Word sequence must be a list: %s" word-sequence))

  (let ((seq-len (length word-sequence)))
    ;; ユニグラム
    (dolist (word word-sequence)
      (let ((count (gethash word (nskk-completion-context-model-unigram-table model) 0)))
        (puthash word (1+ count) (nskk-completion-context-model-unigram-table model))))

    ;; バイグラム
    (when (>= seq-len 2)
      (dotimes (i (1- seq-len))
        (let* ((word1 (nth i word-sequence))
               (word2 (nth (1+ i) word-sequence))
               (key (cons word1 word2))
               (count (gethash key (nskk-completion-context-model-bigram-table model) 0)))
          (puthash key (1+ count) (nskk-completion-context-model-bigram-table model)))))

    ;; トライグラム
    (when (>= seq-len 3)
      (dotimes (i (- seq-len 2))
        (let* ((word1 (nth i word-sequence))
               (word2 (nth (1+ i) word-sequence))
               (word3 (nth (+ i 2) word-sequence))
               (key (cons word1 (cons word2 word3)))
               (count (gethash key (nskk-completion-context-model-trigram-table model) 0)))
          (puthash key (1+ count) (nskk-completion-context-model-trigram-table model)))))

    ;; 総カウント更新
    (cl-incf (nskk-completion-context-model-total-count model))

    model))

;;;###autoload
(defun nskk-completion-context-extract-context (&optional buffer)
  "バッファから文脈を抽出する。

引数:
  BUFFER - バッファ（省略時はカレントバッファ）

処理:
  カーソル位置の前N文字を分析し、単語リストを返す

戻り値:
  文脈（単語のリスト、新しい順）"
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (let* ((pos (point))
             (start (max (point-min) (- pos nskk-completion-context-window-size)))
             (text (buffer-substring-no-properties start pos))
             (words (nskk-completion-context--tokenize text)))
        ;; 新しい順に返す
        (nreverse words)))))

(defun nskk-completion-context--tokenize (text)
  "テキストを単語に分割する（内部関数）。

引数:
  TEXT - 分析対象テキスト

処理:
  簡易的な分かち書き（スペース、句読点で分割）

戻り値:
  単語のリスト"
  (let ((words nil)
        (current-word ""))
    (dotimes (i (length text))
      (let ((char (aref text i)))
        (cond
         ;; 区切り文字
         ((or (= char ?\s) (= char ?\t) (= char ?\n)
              (= char ?、) (= char ?。) (= char ?！) (= char ?？))
          (when (> (length current-word) 0)
            (push current-word words)
            (setq current-word "")))
         ;; 通常の文字
         (t
          (setq current-word (concat current-word (char-to-string char)))))))
    ;; 最後の単語
    (when (> (length current-word) 0)
      (push current-word words))
    (nreverse words)))

;;; 永続化

;;;###autoload
(defun nskk-completion-context-save-model (model file-path)
  "文脈モデルをファイルに保存する。

引数:
  MODEL     - 文脈モデル
  FILE-PATH - 保存先ファイルパス"
  (let ((data (list :version "1.0"
                   :total-count (nskk-completion-context-model-total-count model)
                   :unigrams (nskk-completion-context--hash-to-alist
                             (nskk-completion-context-model-unigram-table model))
                   :bigrams (nskk-completion-context--hash-to-alist
                            (nskk-completion-context-model-bigram-table model))
                   :trigrams (nskk-completion-context--hash-to-alist
                             (nskk-completion-context-model-trigram-table model)))))
    (with-temp-file file-path
      (insert ";; -*- mode: emacs-lisp; coding: utf-8 -*-\n")
      (insert ";; NSKK 文脈モデルデータ\n\n")
      (prin1 data (current-buffer)))))

;;;###autoload
(defun nskk-completion-context-load-model (file-path)
  "ファイルから文脈モデルを読み込む。

引数:
  FILE-PATH - 読み込むファイルパス

戻り値:
  文脈モデル"
  (with-temp-buffer
    (insert-file-contents file-path)
    (goto-char (point-min))
    (while (looking-at ";;")
      (forward-line 1))
    (let* ((data (read (current-buffer)))
           (version (plist-get data :version))
           (model (nskk-completion-context-model-create)))
      (unless (equal version "1.0")
        (error "Unsupported context model version: %s" version))

      ;; データを復元
      (setf (nskk-completion-context-model-total-count model)
            (plist-get data :total-count))
      (nskk-completion-context--alist-to-hash
       (plist-get data :unigrams)
       (nskk-completion-context-model-unigram-table model))
      (nskk-completion-context--alist-to-hash
       (plist-get data :bigrams)
       (nskk-completion-context-model-bigram-table model))
      (nskk-completion-context--alist-to-hash
       (plist-get data :trigrams)
       (nskk-completion-context-model-trigram-table model))

      model)))

(defun nskk-completion-context--hash-to-alist (hash-table)
  "ハッシュテーブルをalistに変換する（内部関数）。"
  (let ((alist nil))
    (maphash (lambda (k v) (push (cons k v) alist)) hash-table)
    alist))

(defun nskk-completion-context--alist-to-hash (alist hash-table)
  "alistをハッシュテーブルに変換する（内部関数）。"
  (dolist (pair alist)
    (puthash (car pair) (cdr pair) hash-table)))

;;; ユーティリティ関数

(defun nskk-completion-context-to-simple-list (results)
  "文脈補完結果を単純なリストに変換する。

引数:
  RESULTS - nskk-completion-context-result構造体のリスト

戻り値:
  ((midashi . entry) ...) のリスト"
  (mapcar (lambda (result)
           (cons (nskk-completion-context-result-midashi result)
                 (nskk-completion-context-result-entry result)))
          results))

(provide 'nskk-completion-context)

;;; nskk-completion-context.el ends here
