;;; nskk-completion-fuzzy.el --- Fuzzy completion for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, completion, fuzzy
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

;; このファイルはLevenshtein距離を利用した曖昧補完を実装します。
;;
;; 特徴:
;; - Levenshtein距離によるあいまいマッチング
;; - 編集距離に基づくスコアリング
;; - 複数の類似度計算アルゴリズム
;; - カスタマイズ可能な距離閾値
;;
;; パフォーマンス目標:
;; - 1000エントリ検索: < 50ms
;; - スコアリング: < 10ms
;; - メモリ効率: 動的計画法テーブルの再利用
;;
;; 使用例:
;;
;;   (require 'nskk-completion-fuzzy)
;;
;;   ;; 基本的な曖昧補完
;;   (nskk-completion-fuzzy-search index "かんじ")
;;   ;; => タイポや送り仮名違いも検索
;;
;;   ;; 閾値指定
;;   (nskk-completion-fuzzy-search index "かん" :threshold 2)
;;   ;; => 編集距離2以内の候補
;;
;;   ;; アルゴリズム選択
;;   (nskk-completion-fuzzy-search index "かん" :algorithm 'damerau-levenshtein)

;;; Code:

(require 'cl-lib)
(require 'nskk-dict-struct)

;;; カスタマイズ変数

(defgroup nskk-completion-fuzzy nil
  "Fuzzy completion for NSKK."
  :group 'nskk
  :prefix "nskk-completion-fuzzy-")

(defcustom nskk-completion-fuzzy-default-threshold 3
  "デフォルトの編集距離閾値。
この値以下の編集距離を持つエントリが候補となる。"
  :type 'integer
  :group 'nskk-completion-fuzzy)

(defcustom nskk-completion-fuzzy-default-limit 20
  "デフォルトの補完候補最大数。"
  :type 'integer
  :group 'nskk-completion-fuzzy)

(defcustom nskk-completion-fuzzy-algorithm 'levenshtein
  "使用する類似度計算アルゴリズム。
  - 'levenshtein: 標準Levenshtein距離
  - 'damerau-levenshtein: Damerau-Levenshtein距離（転置も考慮）
  - 'jaro-winkler: Jaro-Winkler距離（プレフィックス重視）"
  :type '(choice (const :tag "Levenshtein" levenshtein)
                 (const :tag "Damerau-Levenshtein" damerau-levenshtein)
                 (const :tag "Jaro-Winkler" jaro-winkler))
  :group 'nskk-completion-fuzzy)

(defcustom nskk-completion-fuzzy-case-sensitive nil
  "非nilの場合、大文字小文字を区別する。"
  :type 'boolean
  :group 'nskk-completion-fuzzy)

;;; データ構造

(cl-defstruct (nskk-completion-fuzzy-result
               (:constructor nskk-completion-fuzzy-result--create)
               (:copier nil))
  "曖昧補完結果。

スロット:
  midashi  - 見出し語
  entry    - 辞書エントリ
  distance - 編集距離
  score    - 総合スコア（距離 + 頻度）
  matches  - マッチした位置のリスト"
  (midashi nil :type string)
  (entry nil :type nskk-dict-entry)
  (distance 0 :type number)
  (score 0.0 :type float)
  (matches nil :type list))

;;; メイン検索関数

;;;###autoload
(defun nskk-completion-fuzzy-search (index query &rest args)
  "曖昧補完検索を実行する。

引数:
  INDEX - nskk-dict-index構造体
  QUERY - 検索クエリ

キーワード引数:
  :threshold  - 編集距離閾値（デフォルト: nskk-completion-fuzzy-default-threshold）
  :limit      - 最大結果数（デフォルト: nskk-completion-fuzzy-default-limit）
  :okuri-type - 送り仮名タイプ（'okuri-ari/'okuri-nasi/nil）
  :algorithm  - アルゴリズム（デフォルト: nskk-completion-fuzzy-algorithm）

戻り値:
  nskk-completion-fuzzy-result構造体のリスト（編集距離の小さい順）

パフォーマンス: O(n * m) (n=エントリ数, m=クエリ長)"
  (unless (stringp query)
    (error "Query must be a string: %s" query))

  (let* ((threshold (or (plist-get args :threshold)
                       nskk-completion-fuzzy-default-threshold))
         (limit (or (plist-get args :limit)
                   nskk-completion-fuzzy-default-limit))
         (okuri-type (plist-get args :okuri-type))
         (algorithm (or (plist-get args :algorithm)
                       nskk-completion-fuzzy-algorithm))
         (results nil)
         (tables (cond
                  ((eq okuri-type 'okuri-ari)
                   (list (nskk-dict-index-okuri-ari-table index)))
                  ((eq okuri-type 'okuri-nasi)
                   (list (nskk-dict-index-okuri-nasi-table index)))
                  (t
                   (list (nskk-dict-index-okuri-nasi-table index)
                         (nskk-dict-index-okuri-ari-table index))))))

    ;; 各テーブルから曖昧マッチング
    (dolist (table tables)
      (maphash
       (lambda (midashi entry)
         (let ((distance (nskk-completion-fuzzy--calculate-distance
                         query midashi algorithm)))
           (when (<= distance threshold)
             (push (nskk-completion-fuzzy-result--create
                    :midashi midashi
                    :entry entry
                    :distance distance
                    :score (nskk-completion-fuzzy--calculate-score
                           distance entry))
                   results))))
       table))

    ;; スコアでソート
    (setq results (sort results
                       (lambda (a b)
                         (< (nskk-completion-fuzzy-result-score a)
                            (nskk-completion-fuzzy-result-score b)))))

    ;; limit適用
    (when (and limit (> (length results) limit))
      (setq results (seq-take results limit)))

    results))

;;; 距離計算アルゴリズム

(defun nskk-completion-fuzzy--calculate-distance (s1 s2 algorithm)
  "2つの文字列の距離を計算する（内部関数）。

引数:
  S1        - 文字列1
  S2        - 文字列2
  ALGORITHM - アルゴリズム

戻り値:
  編集距離（整数または浮動小数点数）"
  (pcase algorithm
    ('levenshtein
     (nskk-completion-fuzzy--levenshtein-distance s1 s2))
    ('damerau-levenshtein
     (nskk-completion-fuzzy--damerau-levenshtein-distance s1 s2))
    ('jaro-winkler
     ;; Jaro-Winklerは類似度（0-1）なので、1から引いて距離に変換
     (* 10 (- 1.0 (nskk-completion-fuzzy--jaro-winkler-similarity s1 s2))))
    (_
     (nskk-completion-fuzzy--levenshtein-distance s1 s2))))

(defun nskk-completion-fuzzy--levenshtein-distance (s1 s2)
  "標準Levenshtein距離を計算する。

引数:
  S1 - 文字列1
  S2 - 文字列2

アルゴリズム: 動的計画法
計算量: O(|s1| * |s2|)

戻り値: Levenshtein距離（整数）"
  (let* ((len1 (length s1))
         (len2 (length s2))
         ;; 動的計画法のテーブル（前の行と現在の行のみ保持）
         (prev-row (make-vector (1+ len2) 0))
         (curr-row (make-vector (1+ len2) 0)))

    ;; 初期値設定
    (dotimes (j (1+ len2))
      (aset prev-row j j))

    ;; 動的計画法で距離を計算
    (dotimes (i len1)
      (aset curr-row 0 (1+ i))
      (dotimes (j len2)
        (let* ((char1 (aref s1 i))
               (char2 (aref s2 j))
               (cost (if (char-equal char1 char2) 0 1)))
          (aset curr-row (1+ j)
                (min
                 ;; 挿入
                 (1+ (aref prev-row (1+ j)))
                 ;; 削除
                 (1+ (aref curr-row j))
                 ;; 置換
                 (+ (aref prev-row j) cost)))))
      ;; 行を入れ替え
      (let ((tmp prev-row))
        (setq prev-row curr-row)
        (setq curr-row tmp)))

    ;; 結果を返す
    (aref prev-row len2)))

(defun nskk-completion-fuzzy--damerau-levenshtein-distance (s1 s2)
  "Damerau-Levenshtein距離を計算する（転置も考慮）。

引数:
  S1 - 文字列1
  S2 - 文字列2

アルゴリズム: 動的計画法（転置を含む）
計算量: O(|s1| * |s2|)

戻り値: Damerau-Levenshtein距離（整数）"
  (let* ((len1 (length s1))
         (len2 (length s2))
         (dp (make-vector (1+ len1) nil)))

    ;; テーブルを初期化
    (dotimes (i (1+ len1))
      (aset dp i (make-vector (1+ len2) 0))
      (aset (aref dp i) 0 i))
    (dotimes (j (1+ len2))
      (aset (aref dp 0) j j))

    ;; 動的計画法で距離を計算
    (dotimes (i len1)
      (dotimes (j len2)
        (let ((cost (if (char-equal (aref s1 i) (aref s2 j)) 0 1)))
          (aset (aref dp (1+ i)) (1+ j)
                (min
                 ;; 挿入
                 (1+ (aref (aref dp i) (1+ j)))
                 ;; 削除
                 (1+ (aref (aref dp (1+ i)) j))
                 ;; 置換
                 (+ (aref (aref dp i) j) cost)))

          ;; 転置（隣接する2文字の入れ替え）
          (when (and (> i 0) (> j 0)
                    (char-equal (aref s1 i) (aref s2 (1- j)))
                    (char-equal (aref s1 (1- i)) (aref s2 j)))
            (aset (aref dp (1+ i)) (1+ j)
                  (min (aref (aref dp (1+ i)) (1+ j))
                       (1+ (aref (aref dp (1- i)) (1- j)))))))))

    ;; 結果を返す
    (aref (aref dp len1) len2)))

(defun nskk-completion-fuzzy--jaro-winkler-similarity (s1 s2)
  "Jaro-Winkler類似度を計算する。

引数:
  S1 - 文字列1
  S2 - 文字列2

アルゴリズム: Jaro類似度 + プレフィックスボーナス
計算量: O(|s1| * |s2|)

戻り値: 類似度（0.0-1.0の浮動小数点数）"
  (let* ((len1 (length s1))
         (len2 (length s2)))

    (if (or (zerop len1) (zerop len2))
        0.0
      (let* ((match-distance (max 0 (1- (/ (max len1 len2) 2))))
             (s1-matches (make-vector len1 nil))
             (s2-matches (make-vector len2 nil))
             (matches 0)
             (transpositions 0))

        ;; マッチング
        (dotimes (i len1)
          (let ((start (max 0 (- i match-distance)))
                (end (min (1+ (+ i match-distance)) len2)))
            (catch 'matched
              (dotimes (j (- end start))
                (let ((j-idx (+ start j)))
                  (when (and (not (aref s2-matches j-idx))
                            (char-equal (aref s1 i) (aref s2 j-idx)))
                    (aset s1-matches i t)
                    (aset s2-matches j-idx t)
                    (setq matches (1+ matches))
                    (throw 'matched t)))))))

        (if (zerop matches)
            0.0
          ;; 転置数をカウント
          (let ((k 0))
            (dotimes (i len1)
              (when (aref s1-matches i)
                (while (not (aref s2-matches k))
                  (setq k (1+ k)))
                (unless (char-equal (aref s1 i) (aref s2 k))
                  (setq transpositions (1+ transpositions)))
                (setq k (1+ k))))

            (setq transpositions (/ transpositions 2))

            ;; Jaro類似度
            (let ((jaro (/ (+ (/ (float matches) len1)
                             (/ (float matches) len2)
                             (/ (float (- matches transpositions)) matches))
                          3.0)))

              ;; 共通プレフィックス長（最大4）
              (let ((prefix 0))
                (cl-loop for i from 0 below (min len1 len2 4)
                        while (char-equal (aref s1 i) (aref s2 i))
                        do (setq prefix (1+ prefix)))

                ;; Jaro-Winkler類似度
                (+ jaro (* prefix 0.1 (- 1.0 jaro)))))))))))

;;; スコアリング

(defun nskk-completion-fuzzy--calculate-score (distance entry)
  "総合スコアを計算する（内部関数）。

引数:
  DISTANCE - 編集距離
  ENTRY    - 辞書エントリ

処理:
  編集距離を基本スコアとし、頻度で調整

戻り値:
  スコア（浮動小数点数、小さいほど良い）"
  (let ((frequency (nskk-dict-entry-frequency entry)))
    ;; 距離を優先し、頻度で微調整
    ;; distance + (1.0 / (1 + frequency))
    (+ distance (/ 1.0 (+ 1.0 frequency)))))

;;; ユーティリティ関数

(defun nskk-completion-fuzzy-to-simple-list (results)
  "曖昧補完結果を単純なリストに変換する。

引数:
  RESULTS - nskk-completion-fuzzy-result構造体のリスト

戻り値:
  ((midashi . entry) ...) のリスト"
  (mapcar (lambda (result)
           (cons (nskk-completion-fuzzy-result-midashi result)
                 (nskk-completion-fuzzy-result-entry result)))
          results))

(defun nskk-completion-fuzzy-to-string-list (results)
  "曖昧補完結果を文字列リストに変換する。

引数:
  RESULTS - nskk-completion-fuzzy-result構造体のリスト

戻り値:
  見出し語の文字列リスト"
  (mapcar #'nskk-completion-fuzzy-result-midashi results))

(defun nskk-completion-fuzzy-filter-by-distance (results max-distance)
  "指定距離以下の結果のみフィルタする。

引数:
  RESULTS      - nskk-completion-fuzzy-result構造体のリスト
  MAX-DISTANCE - 最大距離

戻り値:
  フィルタ後のリスト"
  (seq-filter (lambda (result)
               (<= (nskk-completion-fuzzy-result-distance result)
                   max-distance))
              results))

;;; デバッグ用関数

(defun nskk-completion-fuzzy-debug-info (index query)
  "曖昧補完検索のデバッグ情報を表示する。

引数:
  INDEX - nskk-dict-index構造体
  QUERY - 検索クエリ

戻り値:
  plist形式のデバッグ情報"
  (let* ((start-time (float-time))
         (results (nskk-completion-fuzzy-search index query))
         (elapsed-time (- (float-time) start-time)))
    (list :query query
          :result-count (length results)
          :elapsed-time elapsed-time
          :algorithm nskk-completion-fuzzy-algorithm
          :threshold nskk-completion-fuzzy-default-threshold
          :results (mapcar (lambda (r)
                            (list :midashi (nskk-completion-fuzzy-result-midashi r)
                                  :distance (nskk-completion-fuzzy-result-distance r)
                                  :score (nskk-completion-fuzzy-result-score r)))
                          results))))

(provide 'nskk-completion-fuzzy)

;;; nskk-completion-fuzzy.el ends here
