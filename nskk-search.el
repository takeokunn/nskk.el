;;; nskk-search.el --- Dictionary search algorithms for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; URL: https://github.com/takeokunn/nskk.el
;; Keywords: i18n

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

;;; カスタマイズ変数

(defgroup nskk-search nil
  "SKK dictionary search customization."
  :group 'nskk
  :prefix "nskk-search-")

(defcustom nskk-search-sort-method 'frequency
  "Sort method for search results."
  :type '(choice (const :tag "Frequency order" frequency)
                 (const :tag "Kana order" kana)
                 (const :tag "No sorting" none))
  :group 'nskk-search)

(defcustom nskk-search-fuzzy-threshold 3
  "Maximum Levenshtein distance threshold for fuzzy search."
  :type 'integer
  :group 'nskk-search)

(defcustom nskk-search-enable-cache t
  "Enable search result caching when non-nil."
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
  "Search dictionary INDEX for QUERY.
SEARCH-TYPE is `exact', `prefix', `partial', or `fuzzy'.
OKURI-TYPE is `okuri-ari', `okuri-nasi', or nil.
LIMIT is the maximum number of results."
  (let ((dict-index (if (nskk-dict-index-p index)
                      index
                    (signal 'nskk-dict-search-invalid-index (list index)))))
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
      result)))

;;; 完全一致検索

(defun nskk-search-exact (index query _okuri-type)
  "Perform exact match search in INDEX for QUERY.
_OKURI-TYPE specifies okurigana filtering."
  (let ((entries (nskk-dict-index-entries index)))
    (when entries
      (if (hash-table-p entries)
          (gethash query entries)
        (assoc query entries)))))

;;; 前方一致検索

(defun nskk-search-prefix (index query _okuri-type limit)
  "Perform prefix match search in INDEX for QUERY.
_OKURI-TYPE specifies okurigana filtering.
LIMIT is the maximum number of results."
  (let ((results nil)
        (tries (list (nskk-dict-index-by-prefix index))))

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
  "Remove duplicate entries from RESULTS."
  (let ((seen (make-hash-table :test 'equal))
        (unique nil))
    (dolist (result results)
      (let ((key (car result)))
        (unless (gethash key seen)
          (puthash key t seen)
          (push result unique))))
    (nreverse unique)))

;;; 部分一致検索

(defun nskk-search-partial (index query _okuri-type limit)
  "Perform partial match search in INDEX for QUERY.
_OKURI-TYPE specifies okurigana filtering.
LIMIT is the maximum number of results."
  (let ((results nil)
        (count 0)
        (entries (nskk-dict-index-entries index)))

    (when (hash-table-p entries)
      ;; 各テーブルを走査
      (catch 'limit-reached
        (maphash (lambda (key entry)
                   (when (string-match-p (regexp-quote query) key)
                     (push (cons key entry) results)
                     (cl-incf count)
                     (when (and limit (>= count limit))
                       (throw 'limit-reached nil))))
                 entries)))

    ;; ソート
    (nskk-search--sort-results results)))

;;; ファジー検索

(defun nskk-search-fuzzy (index query _okuri-type limit)
  "Perform fuzzy search in INDEX for QUERY using Levenshtein distance.
_OKURI-TYPE specifies okurigana filtering.
LIMIT is the maximum number of results."
  (let ((results nil)
        (entries (nskk-dict-index-entries index)))

    ;; エントリを走査し、Levenshtein距離を計算
    (when (hash-table-p entries)
      (maphash (lambda (key entry)
                 (let ((distance (nskk-search--levenshtein-distance query key)))
                   (when (<= distance nskk-search-fuzzy-threshold)
                     (push (cons key (cons entry distance)) results))))
               entries))

    ;; 距離でソート（昇順）
    (setq results (sort results
                       (lambda (a b)
                         (< (cddr a) (cddr b)))))

    ;; limit適用
    (when (and limit (> (length results) limit))
      (setq results (seq-take results limit)))

    results))

(defun nskk-search--levenshtein-distance (s1 s2)
  "Compute Levenshtein distance between S1 and S2."
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
  "Sort search RESULTS according to `nskk-search-sort-method'."
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
  "Sort RESULTS by usage frequency in descending order.
;; TODO: Placeholder - currently returns results unsorted.
;; Implement frequency-based sorting using nskk-learning data."
  results)

(defun nskk-search-sort-by-kana-order (results)
  "Sort RESULTS in Japanese kana order."
  (sort results
        (lambda (a b)
          (string< (car a) (car b)))))

;;; ユーティリティ関数

(defun nskk-search-entry-count (index &optional search-type query okuri-type)
  "Count entries in INDEX matching SEARCH-TYPE and QUERY.
OKURI-TYPE specifies okurigana filtering."
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
  "Generate cache key string from QUERY, SEARCH-TYPE, and OKURI-TYPE."
  (format "%s:%s:%s"
          query
          (or search-type 'exact)
          (or okuri-type 'none)))

;;;###autoload
(defun nskk-search-with-cache (cache index query &optional search-type okuri-type limit)
  "Search INDEX for QUERY using CACHE for result caching.
SEARCH-TYPE, OKURI-TYPE, and LIMIT are passed to `nskk-search'."
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
