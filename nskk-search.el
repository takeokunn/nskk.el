;;; nskk-search.el --- Dictionary search algorithms for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, dictionary, search
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

;; このファイルはSKK辞書の統合検索システムを実装します。
;;
;; サポートする検索タイプ:
;; - 完全一致検索 (exact): O(1) 平均
;; - 前方一致検索 (prefix): O(k + n) - トライ木を活用
;; - 部分一致検索 (partial): O(n)
;; - ファジー検索 (fuzzy): O(n * m) - Levenshtein距離
;;
;; 特徴:
;; - トライ木を活用した高速前方一致検索
;; - カスタマイズ可能なソート順
;; - 送り仮名タイプの柔軟な指定
;; - パフォーマンス要件を満たす実装
;;
;; パフォーマンス目標:
;; - 完全一致: < 0.1ms
;; - 前方一致（100件）: < 1ms
;; - 部分一致（1000件）: < 50ms
;; - ファジー検索（1000件）: < 100ms
;;
;; 使用例:
;;
;;   (require 'nskk-search)
;;
;;   ;; 完全一致検索
;;   (nskk-search index "かんじ" 'exact)
;;   ;; => nskk-dict-entry
;;
;;   ;; 前方一致検索
;;   (nskk-search index "かん" 'prefix nil 10)
;;   ;; => (("かん" . entry1) ("かんじ" . entry2) ...)
;;
;;   ;; ファジー検索
;;   (nskk-search index "かんじ" 'fuzzy nil 5)
;;   ;; => (("かんじ" . entry1 . 0) ("かんき" . entry2 . 1) ...)

;;; Code:

(require 'cl-lib)
(require 'nskk-dict-struct)
(require 'nskk-trie)
(require 'nskk-dict-errors)
(require 'nskk-cache)

(defvar nskk-parallel-search--sequential-cache nil)

;;; カスタマイズ変数

(defgroup nskk-search nil
  "SKK dictionary search customization."
  :group 'nskk
  :prefix "nskk-search-")

(defcustom nskk-search-sort-method 'frequency
  "検索結果のソート方法。
  - 'frequency: 使用頻度順
  - 'kana: 五十音順
  - 'none: ソートしない"
  :type '(choice (const :tag "Frequency order" frequency)
                 (const :tag "Kana order" kana)
                 (const :tag "No sorting" none))
  :group 'nskk-search)

(defcustom nskk-search-fuzzy-threshold 3
  "ファジー検索の距離閾値。
この値以下のLevenshtein距離を持つエントリが候補となる。"
  :type 'integer
  :group 'nskk-search)

(defcustom nskk-search-enable-cache t
  "非nilの場合、検索結果のキャッシュを有効にする。
Task 1.19で実装予定。"
  :type 'boolean
  :group 'nskk-search)

;;; エラー定義

(define-error 'nskk-dict-search-error
  "Dictionary search error"
  'nskk-dict-error)

(define-error 'nskk-dict-search-invalid-query
  "Invalid search query"
  'nskk-dict-search-error)

(define-error 'nskk-dict-search-invalid-index
  "Invalid dictionary index"
  'nskk-dict-search-error)

;;; 統合検索インターフェース

;;;###autoload
(defun nskk-search (index query &optional search-type okuri-type limit)
  "辞書INDEXからQUERYを検索する。

引数:
  INDEX       - nskk-dict-index構造体、または nskk-index構造体
  QUERY       - 検索クエリ文字列
  SEARCH-TYPE - 検索タイプ（'exact/'prefix/'partial/'fuzzy、デフォルト'exact）
  OKURI-TYPE  - 送り仮名タイプ（'okuri-ari/'okuri-nasi/nil=両方、デフォルトnil）
  LIMIT       - 結果の最大数（nilの場合無制限）

戻り値:
  検索タイプに応じた結果
  - exact: nskk-dict-entry または nil
  - prefix/partial/fuzzy: ((key . entry) ...) のリスト

エラー:
  - クエリが空文字列の場合: nskk-dict-search-invalid-query
  - インデックスが無効の場合: nskk-dict-search-invalid-index"
  (let ((dict-index (cond
                     ((nskk-dict-index-p index)
                      index)
                     ((and (fboundp 'nskk-index-p)
                           (nskk-index-p index))
                      (let ((dict (nskk-index-dict-struct index)))
                        (unless dict
                          (signal 'nskk-dict-search-invalid-index (list index)))
                        dict))
                     (t
                      (signal 'nskk-dict-search-invalid-index (list index))))))
    (unless (and (stringp query) (> (length query) 0))
      (signal 'nskk-dict-search-invalid-query (list query)))

    ;; 検索タイプのデフォルト値
    (setq search-type (or search-type 'exact))

    ;; 検索タイプに応じてディスパッチ
    (let ((result
           (pcase search-type
             ('exact
              (nskk-search-exact dict-index query okuri-type))
             ('prefix
              (nskk-search-prefix dict-index query okuri-type limit))
             ('partial
              (nskk-search-partial dict-index query okuri-type limit))
             ('fuzzy
              (nskk-search-fuzzy dict-index query okuri-type limit))
             (_
              (signal 'nskk-dict-search-invalid-query
                      (list (format "Unknown search type: %s" search-type)))))))
      ;; 並列検索のウォームアップ用に逐次結果をキャッシュ
      (cond
       ((and (eq search-type 'prefix)
             (not okuri-type)
             (fboundp 'nskk-index-p)
             (nskk-index-p index))
        (setq nskk-parallel-search--sequential-cache
              (list :index index
                    :query query
                    :search-type search-type
                    :result result)))
       (t
        (setq nskk-parallel-search--sequential-cache nil)))
      result)))

;;; 完全一致検索

(defun nskk-search-exact (index query okuri-type)
  "完全一致検索を実行する。

引数:
  INDEX      - nskk-dict-index構造体
  QUERY      - 検索クエリ文字列
  OKURI-TYPE - 送り仮名タイプ（'okuri-ari/'okuri-nasi/nil）

処理:
  1. 送り仮名タイプを判定（指定がない場合）
  2. 対応するハッシュテーブルから検索
  3. エントリを返す

計算量: O(1) 平均

戻り値: nskk-dict-entry または nil"
  (cond
   ((eq okuri-type 'okuri-ari)
    (gethash query (nskk-dict-index-okuri-ari-table index)))
   ((eq okuri-type 'okuri-nasi)
    (gethash query (nskk-dict-index-okuri-nasi-table index)))
   (t
    ;; 送り仮名タイプが指定されていない場合、両方を検索
    ;; まず送り仮名なしを優先
    (or (gethash query (nskk-dict-index-okuri-nasi-table index))
        (gethash query (nskk-dict-index-okuri-ari-table index))))))

;;; 前方一致検索

(defun nskk-search-prefix (index query okuri-type limit)
  "前方一致検索を実行する。

引数:
  INDEX      - nskk-dict-index構造体
  QUERY      - 検索クエリ文字列
  OKURI-TYPE - 送り仮名タイプ（'okuri-ari/'okuri-nasi/nil）
  LIMIT      - 結果の最大数（nilの場合無制限）

処理:
  1. トライ木を使用した高速検索
  2. 送り仮名タイプに応じたトライ木を選択
  3. nskk-trie-prefix-searchを呼び出す
  4. 結果をソート（頻度順、五十音順など）

計算量: O(k + n) (k=prefix長, n=結果数)

戻り値: ((key . entry) ...) のリスト"
  (let ((results nil)
        (tries (cond
                ((eq okuri-type 'okuri-ari)
                 (list (nskk-dict-index-trie-ari index)))
                ((eq okuri-type 'okuri-nasi)
                 (list (nskk-dict-index-trie-nasi index)))
                (t
                 ;; 両方から検索（送り仮名なしを優先）
                 (list (nskk-dict-index-trie-nasi index)
                       (nskk-dict-index-trie-ari index))))))

    ;; 各トライ木から検索
    (dolist (trie tries)
      (when trie
        (let ((trie-results (nskk-trie-prefix-search trie query limit)))
          (setq results (append results trie-results)))))

    ;; 重複を除去（同じキーが送り仮名ありとなしに存在する場合）
    (setq results (nskk-search--remove-duplicates results))

    ;; limit適用（複数トライ木から取得した場合）
    (when (and limit (> (length results) limit))
      (setq results (seq-take results limit)))

    ;; ソート
    (nskk-search--sort-results results)))

(defun nskk-search--remove-duplicates (results)
  "検索結果から重複を除去する（内部関数）。

引数:
  RESULTS - ((key . entry) ...) のリスト

戻り値:
  重複除去後のリスト（最初に出現したものを保持）"
  (let ((seen (make-hash-table :test 'equal))
        (unique nil))
    (dolist (result results)
      (let ((key (car result)))
        (unless (gethash key seen)
          (puthash key t seen)
          (push result unique))))
    (nreverse unique)))

;;; 部分一致検索

(defun nskk-search-partial (index query okuri-type limit)
  "部分一致検索を実行する。

引数:
  INDEX      - nskk-dict-index構造体
  QUERY      - 検索クエリ文字列
  OKURI-TYPE - 送り仮名タイプ（'okuri-ari/'okuri-nasi/nil）
  LIMIT      - 結果の最大数（nilの場合無制限）

処理:
  1. 全エントリを走査
  2. 見出し語にクエリが含まれるか判定
  3. マッチしたエントリを収集
  4. 頻度でソート

計算量: O(n) (n=エントリ数)

戻り値: ((key . entry) ...) のリスト

注意: 大規模辞書では遅い可能性があるため、limitの使用を推奨"
  (let ((results nil)
        (count 0)
        (tables (cond
                 ((eq okuri-type 'okuri-ari)
                  (list (nskk-dict-index-okuri-ari-table index)))
                 ((eq okuri-type 'okuri-nasi)
                  (list (nskk-dict-index-okuri-nasi-table index)))
                 (t
                  (list (nskk-dict-index-okuri-nasi-table index)
                        (nskk-dict-index-okuri-ari-table index))))))

    ;; 各テーブルを走査
    (catch 'limit-reached
      (dolist (table tables)
        (maphash (lambda (key entry)
                   (when (string-match-p (regexp-quote query) key)
                     (push (cons key entry) results)
                     (cl-incf count)
                     (when (and limit (>= count limit))
                       (throw 'limit-reached nil))))
                 table)))

    ;; ソート
    (nskk-search--sort-results results)))

;;; ファジー検索

(defun nskk-search-fuzzy (index query okuri-type limit)
  "ファジー検索を実行する（基礎実装）。

引数:
  INDEX      - nskk-dict-index構造体
  QUERY      - 検索クエリ文字列
  OKURI-TYPE - 送り仮名タイプ（'okuri-ari/'okuri-nasi/nil）
  LIMIT      - 結果の最大数（nilの場合無制限）

処理:
  1. Levenshtein距離を計算
  2. 距離が閾値以下のエントリを収集
  3. 距離でソート

計算量: O(n * m) (n=エントリ数, m=クエリ長)

戻り値: ((key . entry . distance) ...) のリスト"
  (let ((results nil)
        (tables (cond
                 ((eq okuri-type 'okuri-ari)
                  (list (nskk-dict-index-okuri-ari-table index)))
                 ((eq okuri-type 'okuri-nasi)
                  (list (nskk-dict-index-okuri-nasi-table index)))
                 (t
                  (list (nskk-dict-index-okuri-nasi-table index)
                        (nskk-dict-index-okuri-ari-table index))))))

    ;; 各テーブルを走査し、Levenshtein距離を計算
    (dolist (table tables)
      (maphash (lambda (key entry)
                 (let ((distance (nskk-search--levenshtein-distance query key)))
                   (when (<= distance nskk-search-fuzzy-threshold)
                     (push (cons key (cons entry distance)) results))))
               table))

    ;; 距離でソート（昇順）
    (setq results (sort results
                       (lambda (a b)
                         (< (cddr a) (cddr b)))))

    ;; limit適用
    (when (and limit (> (length results) limit))
      (setq results (seq-take results limit)))

    results))

(defun nskk-search--levenshtein-distance (s1 s2)
  "2つの文字列のLevenshtein距離を計算する（内部関数）。

引数:
  S1 - 文字列1
  S2 - 文字列2

アルゴリズム: 動的計画法
計算量: O(|s1| * |s2|)

戻り値: Levenshtein距離（整数）"
  (let* ((len1 (length s1))
         (len2 (length s2))
         ;; 動的計画法のテーブル
         (dp (make-vector (1+ len1) nil)))

    ;; テーブルを初期化
    (dotimes (i (1+ len1))
      (aset dp i (make-vector (1+ len2) 0)))

    ;; 初期値設定
    (dotimes (i (1+ len1))
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
                 (+ (aref (aref dp i) j) cost))))))

    ;; 結果を返す
    (aref (aref dp len1) len2)))

;;; ソート機能

(defun nskk-search--sort-results (results)
  "検索結果をソートする（内部関数）。

引数:
  RESULTS - ((key . entry) ...) のリスト

戻り値:
  ソート済みリスト"
  (pcase nskk-search-sort-method
    ('frequency
     (nskk-search-sort-by-frequency results))
    ('kana
     (nskk-search-sort-by-kana-order results))
    ('none
     results)
    (_
     results)))

(defun nskk-search-sort-by-frequency (results)
  "検索結果を使用頻度でソートする。

引数:
  RESULTS - ((key . entry) ...) のリスト

処理:
  エントリのfrequencyフィールドで降順ソート

戻り値: ソート済みリスト"
  (sort results
        (lambda (a b)
          (let ((freq-a (nskk-dict-entry-frequency (cdr a)))
                (freq-b (nskk-dict-entry-frequency (cdr b))))
            (> freq-a freq-b)))))

(defun nskk-search-sort-by-kana-order (results)
  "検索結果を五十音順でソートする。

引数:
  RESULTS - ((key . entry) ...) のリスト

処理:
  見出し語（キー）で辞書順ソート

戻り値: ソート済みリスト"
  (sort results
        (lambda (a b)
          (string< (car a) (car b)))))

;;; ユーティリティ関数

(defun nskk-search-entry-count (index &optional search-type query okuri-type)
  "検索結果のエントリ数を取得する（検索を実行せずカウントのみ）。

引数:
  INDEX       - nskk-dict-index構造体
  SEARCH-TYPE - 検索タイプ（省略時は全エントリ数）
  QUERY       - 検索クエリ（search-typeがexact以外の場合必須）
  OKURI-TYPE  - 送り仮名タイプ

戻り値:
  エントリ数（整数）"
  (if (null search-type)
      ;; 全エントリ数
      (nskk-dict-struct-entry-count index okuri-type)
    ;; 検索実行してカウント
    (let ((results (nskk-search index query search-type okuri-type nil)))
      (if (nskk-dict-entry-p results)
          1
        (length results)))))

;;; キャッシュ統合検索

(defun nskk-search--cache-key (query search-type okuri-type)
  "キャッシュキー文字列を生成する。

引数:
  QUERY       - 検索クエリ文字列
  SEARCH-TYPE - 検索タイプ ('exact/'prefix/'partial/'fuzzy)
  OKURI-TYPE  - 送り仮名タイプ ('okuri-ari/'okuri-nasi/nil)

戻り値:
  キャッシュキー文字列"
  (format "%s:%s:%s"
          query
          (or search-type 'exact)
          (or okuri-type 'none)))

;;;###autoload
(defun nskk-search-with-cache (cache index query &optional search-type okuri-type limit)
  "CACHE を使用して INDEX から QUERY を検索する。

キャッシュヒット時は即座に結果を返し、ミス時は通常検索を実行して
結果をキャッシュに保存する。

引数:
  CACHE       - nskk-cache構造体
  INDEX       - nskk-index構造体 or nskk-trie構造体 or nskk-dict-index構造体
  QUERY       - 検索クエリ文字列
  SEARCH-TYPE - 'exact/'prefix/'partial/'fuzzy (デフォルト: 'exact)
  OKURI-TYPE  - 'okuri-ari/'okuri-nasi/nil (デフォルト: nil)
  LIMIT       - 結果の最大数

戻り値:
  - exact: 候補リスト or nil
  - prefix/partial/fuzzy: ((key . entry) ...) のリスト

パフォーマンス:
  - キャッシュヒット時: < 0.5ms
  - キャッシュミス時: 通常のnskk-searchと同等"
  (unless (nskk-cache-p cache)
    (signal 'wrong-type-argument (list 'nskk-cache-p cache)))

  (let ((cache-key (nskk-search--cache-key query search-type okuri-type)))
    (or (nskk-cache-get cache cache-key)
        (let ((result
               ;; nskk-trieの場合は直接検索、それ以外はnskk-searchを使用
               (if (nskk-trie-p index)
                   (nskk-trie-lookup-values index query)
                 (nskk-search index query search-type okuri-type limit))))
          (when result
            (nskk-cache-put cache cache-key result))
          result))))

(provide 'nskk-search)

;;; nskk-search.el ends here
