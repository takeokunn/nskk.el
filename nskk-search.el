;;; nskk-search.el --- Dictionary search algorithms for NSKK -*- lexical-binding: t; -*-

;; Author: takeokunn <bararararatty@gmail.com>
;; Maintainer: takeokunn <bararararatty@gmail.com>
;; URL: https://github.com/takeokunn/nskk.el
;; Keywords: i18n

;; Copyright (C) 2026 NSKK Contributors

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

;; Integrated dictionary search engine for NSKK (SKK Japanese input method).
;;
;; Supported search types:
;; - Exact match (exact): O(1) average via hash table
;; - Prefix match (prefix): O(k + n) leveraging the trie structure
;; - Partial match (partial): O(n)
;; - Fuzzy match (fuzzy): O(n * m) using Levenshtein distance
;;
;; Performance targets:
;; - Exact match: < 0.1ms
;; - Prefix match (100 results): < 1ms
;; - Partial match (1000 entries): < 50ms
;; - Fuzzy match (1000 entries): < 100ms
;;
;; The search engine supports okurigana type filtering and
;; customizable sort order for candidate ranking.
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
(require 'subr-x)
(require 'nskk-dictionary)
(require 'nskk-trie)
(require 'nskk-cache)
(require 'nskk-prolog)

(declare-function nskk-dict--struct-entry-count "nskk-dictionary")

(defgroup nskk-search nil
  "SKK dictionary search customization."
  :group 'nskk-dictionary
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

(defcustom nskk-search-learning-file "~/.emacs.d/nskk/learning.dat"
  "File path for persisting learning data."
  :type 'file
  :group 'nskk-search)

(defcustom nskk-search-auto-save t
  "When non-nil, automatically save learning data periodically."
  :type 'boolean
  :group 'nskk-search)

(defcustom nskk-search-auto-save-interval 300
  "Auto-save interval in seconds for learning data."
  :type 'integer
  :group 'nskk-search)

(defvar nskk-search-jisyo-hook nil
  "Hook run during dictionary search.
DDSKK equivalent: skk-search-jisyo-hook")

(defvar nskk-save-history-hook nil
  "Hook run when history is saved.
DDSKK equivalent: skk-save-history-hook")

;;; Internal variables for learning data

(defvar nskk-search--learning-data (make-hash-table :test 'equal)
  "Hash table storing learning data.
Keys are reading strings, values are hash tables mapping candidates to scores.")

(defvar nskk-search--dirty-flag nil
  "Non-nil when learning data has been modified since last save.")

(defvar nskk-search--auto-save-timer nil
  "Timer for periodic auto-save of learning data.")

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

;;;; Prolog Search Strategy Facts

;; Search type dispatch rules
(nskk-prolog-set-index 'search-strategy 1 :hash)
(nskk-prolog-<- (search-strategy exact))
(nskk-prolog-<- (search-strategy prefix))
(nskk-prolog-<- (search-strategy partial))
(nskk-prolog-<- (search-strategy fuzzy))

;; Sort method dispatch rules
(nskk-prolog-set-index 'sort-strategy 1 :hash)
(nskk-prolog-<- (sort-strategy frequency))
(nskk-prolog-<- (sort-strategy kana))
(nskk-prolog-<- (sort-strategy none))

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

    ;; Validate search type via Prolog
    (unless (nskk-prolog-query `(search-strategy ,search-type))
      (signal 'nskk-dict-search-invalid-query
              (list (format "Unknown search type: %s" search-type))))

    ;; 検索タイプに応じてディスパッチ
    (let ((result
           (pcase search-type
             ('exact (nskk-search-exact dict-index query okuri-type))
             ('prefix (nskk-search-prefix dict-index query okuri-type limit))
             ('partial (nskk-search-partial dict-index query okuri-type limit))
             ('fuzzy (nskk-search-fuzzy dict-index query okuri-type limit)))))
      result)))

;;; Okuri-type filtering

(defsubst nskk-search--match-okuri-type-p (okuri-type entry-okuri)
  "Return non-nil if ENTRY-OKURI matches OKURI-TYPE filter.
OKURI-TYPE is \\='okuri-ari, \\='okuri-nasi, or nil (match all)."
  (cond
   ((eq okuri-type 'okuri-ari) entry-okuri)
   ((eq okuri-type 'okuri-nasi) (or (null entry-okuri) (string= entry-okuri "")))
   (t t)))

;;; 完全一致検索

(defun nskk-search-exact (index query okuri-type)
  "Perform exact match search in INDEX for QUERY.
OKURI-TYPE specifies okurigana filtering: \\='okuri-ari, \\='okuri-nasi, or nil."
  (let ((entries (nskk-dict-index-entries index))
        (entry nil))
    (when entries
      (setq entry (if (hash-table-p entries)
                      (gethash query entries)
                    (assoc query entries)))
      ;; Filter by okuri-type if specified
      (when (and entry okuri-type)
        (unless (nskk-search--match-okuri-type-p okuri-type (nskk-dict-entry-okuri entry))
          (setq entry nil)))
      entry)))

;;; 前方一致検索

(defun nskk-search-prefix (index query okuri-type limit)
  "Perform prefix match search in INDEX for QUERY.
OKURI-TYPE specifies okurigana filtering: \\='okuri-ari, \\='okuri-nasi, or nil.
LIMIT is the maximum number of results."
  (let ((results nil)
        (tries (list (nskk-dict-index-by-prefix index))))

    ;; 各トライ木から検索
    (dolist (trie tries)
      (when trie
        (let ((trie-results (nskk-trie-prefix-search trie query limit)))
          (setq results (append results trie-results)))))

    ;; Filter by okuri-type if specified
    (when okuri-type
      (setq results
            (cl-remove-if-not
             (lambda (result)
               (nskk-search--match-okuri-type-p okuri-type (nskk-dict-entry-okuri (cdr result))))
             results)))

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

(defun nskk-search-partial (index query okuri-type limit)
  "Perform partial match search in INDEX for QUERY.
OKURI-TYPE specifies okurigana filtering: \\='okuri-ari, \\='okuri-nasi, or nil.
LIMIT is the maximum number of results."
  (let ((results nil)
        (count 0)
        (entries (nskk-dict-index-entries index)))

    (when (hash-table-p entries)
      ;; 各テーブルを走査
      (catch 'limit-reached
        (maphash (lambda (key entry)
                   (when (string-match-p (regexp-quote query) key)
                     ;; Filter by okuri-type if specified
                     (when (or (null okuri-type)
                               (nskk-search--match-okuri-type-p okuri-type (nskk-dict-entry-okuri entry)))
                       (push (cons key entry) results)
                       (cl-incf count)
                       (when (and limit (>= count limit))
                         (throw 'limit-reached nil)))))
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
  (unless (nskk-prolog-query `(sort-strategy ,nskk-search-sort-method))
    (setq nskk-search-sort-method 'none))
  (pcase nskk-search-sort-method
    ('frequency (nskk-search-sort-by-frequency results))
    ('kana (nskk-search-sort-by-kana-order results))
    (_ results)))

(defun nskk-search-sort-by-frequency (results)
  "Sort RESULTS by usage frequency based on learning data."
  (nskk-search--sort-prefix-results results))

(defun nskk-search-sort-by-kana-order (results)
  "Sort RESULTS in Japanese kana order."
  (sort results
        (lambda (a b)
          (string< (car a) (car b)))))

;;; Learning-based sorting

(defun nskk-search--sort-entry-by-learning (entry)
  "Sort candidates within ENTRY by learning scores."
  (when (nskk-dict-entry-p entry)
    (let* ((reading (nskk-dict-entry-key entry))
           (scores (gethash reading nskk-search--learning-data)))
      (when scores
        (setf (nskk-dict-entry-candidates entry)
              (sort (copy-sequence (nskk-dict-entry-candidates entry))
                    (lambda (a b)
                      (> (nskk-search--candidate-score reading a scores)
                         (nskk-search--candidate-score reading b scores)))))))
    entry))

(defun nskk-search--sort-prefix-results (results)
  "Sort prefix search RESULTS by learning scores in descending order."
  (let ((scored (mapcar (lambda (item)
                          (cons (nskk-search--reading-score (car item) (cdr item))
                                item))
                        results)))
    (mapcar #'cdr
            (sort scored (lambda (a b)
                           (> (car a) (car b)))))))

(defun nskk-search--reading-score (reading entry)
  "Return the maximum learning score for READING and ENTRY."
  (let ((scores (gethash reading nskk-search--learning-data)))
    (if (and scores (nskk-dict-entry-p entry))
        (cl-loop for cand in (nskk-dict-entry-candidates entry)
                 maximize (nskk-search--candidate-score reading cand scores))
      0)))

(defun nskk-search--candidate-score (_reading candidate scores)
  "Return learning score for CANDIDATE from SCORES hash table.
_READING is unused."
  (let ((word (nskk-search--candidate-word candidate)))
    (or (and word (gethash word scores)) 0)))

(defun nskk-search--candidate-word (candidate)
  "Extract the word string from CANDIDATE."
  (cond
   ((stringp candidate) candidate)
   ((and (consp candidate)
         (stringp (car candidate)))
    (car candidate))
   (t nil)))

;;; Deduplication

(defun nskk-search--dedupe-fuzzy (results)
  "Remove duplicate entries from fuzzy search RESULTS, keeping closest match."
  (let ((seen (make-hash-table :test 'equal))
        (acc '()))
    (dolist (item results)
      (let* ((key (car item))
             (distance (cddr item))
             (existing (gethash key seen)))
        (cond
         ((null existing)
          (puthash key item seen)
          (push item acc))
         ((< distance (cddr existing))
          (puthash key item seen)
          (setq acc (cons item (delete existing acc)))))))
    (nreverse acc)))

;;; Learning data management

(defun nskk-search--load-learning-data ()
  "Load learning data from `nskk-search-learning-file'."
  (when (file-readable-p nskk-search-learning-file)
    (condition-case _err
        (with-temp-buffer
          (insert-file-contents nskk-search-learning-file)
          (let ((data (read (current-buffer))))
            (if (hash-table-p data)
                (setq nskk-search--learning-data data)
              (setq nskk-search--learning-data (make-hash-table :test 'equal)))))
      (error
       (setq nskk-search--learning-data (make-hash-table :test 'equal))))))

;;;###autoload
(defun nskk-search-save-learning-data ()
  "Save learning data to `nskk-search-learning-file'."
  (interactive)
  (let ((dir (file-name-directory nskk-search-learning-file)))
    (unless (file-directory-p dir)
      (make-directory dir t)))
  (condition-case err
      (with-temp-file nskk-search-learning-file
        (prin1 nskk-search--learning-data (current-buffer))
        (setq nskk-search--dirty-flag nil)
        (message "Learning data saved"))
    (error
     (message "Failed to save learning data: %s" err))))

;;;###autoload
(defun nskk-search-learn (query candidate &optional _context)
  "Record that CANDIDATE was selected for QUERY.
_CONTEXT is reserved for future use."
  (let ((scores (or (gethash query nskk-search--learning-data)
                    (make-hash-table :test 'equal))))
    (puthash candidate
             (1+ (or (gethash candidate scores) 0))
             scores)
    (puthash query scores nskk-search--learning-data)
    (setq nskk-search--dirty-flag t)))

;;; Auto-save

(defun nskk-search--start-auto-save ()
  "Start the auto-save timer for learning data."
  (when nskk-search--auto-save-timer
    (cancel-timer nskk-search--auto-save-timer))
  (setq nskk-search--auto-save-timer
        (run-with-timer nskk-search-auto-save-interval
                        nskk-search-auto-save-interval
                        #'nskk-search--auto-save-handler)))

(defun nskk-search--stop-auto-save ()
  "Stop the auto-save timer for learning data."
  (when nskk-search--auto-save-timer
    (cancel-timer nskk-search--auto-save-timer)
    (setq nskk-search--auto-save-timer nil)))

(defun nskk-search--auto-save-handler ()
  "Handler called by the auto-save timer."
  (when nskk-search--dirty-flag
    (nskk-search-save-learning-data)))

;;; ユーティリティ関数

(defun nskk-search-entry-count (index &optional search-type query okuri-type)
  "Count entries in INDEX matching SEARCH-TYPE and QUERY.
OKURI-TYPE specifies okurigana filtering."
  (if (null search-type)
      ;; 全エントリ数
      (nskk-dict--struct-entry-count index okuri-type)
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
