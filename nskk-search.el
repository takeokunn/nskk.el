;;; nskk-search.el --- Dictionary search algorithms for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

;; Author: takeokunn <bararararatty@gmail.com>
;; Maintainer: takeokunn <bararararatty@gmail.com>
;; URL: https://github.com/takeokunn/nskk.el
;; Keywords: i18n

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Dictionary search algorithms for NSKK (Layer 2: Domain).
;;
;; Layer position: L2 (Domain) -- depends on nskk-dictionary, nskk-cache,
;;   nskk-prolog, and nskk-custom.
;;
;; Provides an integrated search engine over the Prolog-backed dictionary
;; with four search modes, learning-based candidate ranking, and optional
;; result caching.
;;
;; Supported search types:
;; - Exact match (exact):   O(1) average via Prolog hash index
;; - Prefix match (prefix): O(k + n) via Prolog trie index
;; - Partial match (partial): O(n) substring scan
;; - Fuzzy match (fuzzy):   O(n * m) Levenshtein distance
;;
;; Performance targets:
;; - Exact match: < 0.1ms
;; - Prefix match (100 results): < 1ms
;; - Partial match (1000 entries): < 50ms
;; - Fuzzy match (1000 entries): < 100ms
;;
;; The search engine supports okurigana type filtering and customizable
;; sort order for candidate ranking.  Learning data is persisted via
;; Prolog `learning-score/3' facts and serialized to
;; `nskk-search-learning-file'.
;;
;; Prolog predicates maintained by this module:
;; - `search-strategy/1'  -- valid search type membership
;; - `learning-score/3'   -- (reading candidate score) usage frequency
;;
;; Key public API:
;; - `nskk-search'              -- unified search dispatcher
;; - `nskk-search-exact'        -- exact match search
;; - `nskk-search-prefix'       -- prefix match search
;; - `nskk-search-partial'      -- partial (substring) match search
;; - `nskk-search-fuzzy'        -- fuzzy (Levenshtein) match search
;; - `nskk-search-with-cache'   -- cache-backed search
;; - `nskk-search-learn'        -- record candidate selection for learning
;; - `nskk-search-save-learning-data' -- persist learning data to file
;;
;; Usage examples:
;;   (nskk-search index "かんじ" 'exact)
;;   ;; => nskk-dict-entry
;;
;;   (nskk-search index "かん" 'prefix nil 10)
;;   ;; => (("かん" . entry1) ("かんじ" . entry2) ...)
;;
;;   (nskk-search index "かんじ" 'fuzzy nil 5)
;;   ;; => (("かんじ" . entry1 . 0) ("かんき" . entry2 . 1) ...)

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'nskk-dictionary)
(require 'nskk-cache)
(require 'nskk-prolog)
(require 'nskk-custom)
(require 'nskk-debug nil t)

(declare-function nskk-dict--entry-count "nskk-dictionary")
(declare-function nskk-dict--struct-entry-count "nskk-dictionary")

(defvar nskk-search-jisyo-hook nil
  "Hook run during dictionary search.
DDSKK equivalent: skk-search-jisyo-hook")

(defvar nskk-save-history-hook nil
  "Hook run when history is saved.
DDSKK equivalent: skk-save-history-hook")

;;; Internal variables for learning data

(defvar nskk-search--dirty-flag nil
  "Non-nil means learning data has been modified since last save.")

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
(nskk-prolog-define-fact-table search-strategy (:arity 1 :index :hash)
  (exact)
  (prefix)
  (partial)
  (fuzzy))

;; Learning score facts: (learning-score reading candidate score)
;; Hash-indexed on first arg (reading) for O(1) lookup by reading
(nskk-prolog-set-index 'learning-score 3 :hash)

;;; 統合検索インターフェース

;;;###autoload
(defun nskk-search (index query &optional search-type okuri-type limit)
  "Search dictionary INDEX for QUERY.
SEARCH-TYPE is `exact', `prefix', `partial', or `fuzzy'.
OKURI-TYPE is `okuri-ari', `okuri-nasi', or nil.
LIMIT is the maximum number of results."
  (unless (nskk-dict-index-p index)
    (signal 'nskk-dict-search-invalid-index (list index)))
  (unless (and (stringp query) (> (length query) 0))
    (signal 'nskk-dict-search-invalid-query (list query)))

  ;; 検索タイプのデフォルト値
  (setq search-type (or search-type 'exact))
  (nskk-debug-log "[SEARCH] search: query=%s type=%s" query search-type)

  ;; Validate search type
  (unless (memq search-type '(exact prefix partial fuzzy))
    (signal 'nskk-dict-search-invalid-query
            (list (format "Unknown search type: %s" search-type))))

  ;; 検索タイプに応じてディスパッチ
  (let ((result
         (pcase search-type
           ('exact (nskk-search-exact index query okuri-type))
           ('prefix (nskk-search-prefix index query okuri-type limit))
           ('partial (nskk-search-partial index query okuri-type limit))
           ('fuzzy (nskk-search-fuzzy index query okuri-type limit)))))
    result))

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
  (let* ((pred (nskk-dict-index-predicate index))
         (candidates (when pred
                       (nskk-prolog-query-value
                        `(,pred ,query \?candidates) '\?candidates))))
    (nskk-debug-log "[SEARCH] exact: query=%s found=%s" query (if candidates t nil))
    (when candidates
      (let ((entry (make-nskk-dict-entry :key query :candidates candidates)))
        (when (nskk-search--match-okuri-type-p okuri-type (nskk-dict-entry-okuri entry))
          entry)))))

;;; 前方一致検索

(defsubst nskk-search--post-process-results (results okuri-type limit)
  "Apply standard post-processing to RESULTS.
Filters by OKURI-TYPE, removes duplicates, applies LIMIT, and sorts by
`nskk-search-sort-method'.  Returns the processed result list."
  (when okuri-type
    (setq results
          (cl-remove-if-not
           (lambda (result)
             (nskk-search--match-okuri-type-p
              okuri-type (nskk-dict-entry-okuri (cdr result))))
           results)))
  (setq results (nskk-search--remove-duplicates results))
  (when (and limit (> (length results) limit))
    (setq results (seq-take results limit)))
  (nskk-search--sort-results results))

(defun nskk-search-prefix (index query okuri-type limit)
  "Perform prefix match search in INDEX for QUERY.
OKURI-TYPE specifies okurigana filtering: \\='okuri-ari, \\='okuri-nasi, or nil.
LIMIT is the maximum number of results."
  (let* ((pred (nskk-dict-index-predicate index))
         (raw-results (when pred (nskk-prolog-trie-prefix-search pred 2 query)))
         ;; Convert raw (key . candidates) pairs to (key . nskk-dict-entry) pairs
         (results (mapcar (lambda (pair)
                            (cons (car pair)
                                  (make-nskk-dict-entry
                                   :key (car pair)
                                   :candidates (cdr pair))))
                          raw-results)))
    (nskk-debug-log "[SEARCH] prefix: query=%s results=%d" query (length results))
    (nskk-search--post-process-results results okuri-type limit)))

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
  (let* ((pred (nskk-dict-index-predicate index))
         (results nil)
         (count 0))
    (when pred
      (let ((all-solutions (nskk-prolog-query `(,pred \?k \?candidates))))
        (catch 'limit-reached
          (dolist (sol all-solutions)
            (let* ((key (nskk-prolog-walk '\?k sol))
                   (candidates (nskk-prolog-walk '\?candidates sol)))
              (when (string-match-p (regexp-quote query) key)
                (let ((entry (make-nskk-dict-entry :key key :candidates candidates)))
                  (when (nskk-search--match-okuri-type-p
                         okuri-type (nskk-dict-entry-okuri entry))
                    (push (cons key entry) results)
                    (cl-incf count)
                    (when (and limit (>= count limit))
                      (throw 'limit-reached nil))))))))))
    (nskk-search--sort-results results)))

;;; ファジー検索

(defun nskk-search-fuzzy (index query _okuri-type limit)
  "Perform fuzzy search in INDEX for QUERY using Levenshtein distance.
_OKURI-TYPE specifies okurigana filtering.
LIMIT is the maximum number of results."
  (let* ((pred (nskk-dict-index-predicate index))
         (results nil))
    (when pred
      (let ((all-solutions (nskk-prolog-query `(,pred \?k \?candidates))))
        (dolist (sol all-solutions)
          (let* ((key (nskk-prolog-walk '\?k sol))
                 (candidates (nskk-prolog-walk '\?candidates sol))
                 (distance (nskk-search--levenshtein-distance query key)))
            (when (<= distance nskk-search-fuzzy-threshold)
              (let ((entry (make-nskk-dict-entry :key key :candidates candidates)))
                (push (cons key (cons entry distance)) results)))))))
    ;; Sort by distance (ascending)
    (setq results (sort results (lambda (a b) (< (cddr a) (cddr b)))))
    ;; Apply limit
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
  (unless (memq nskk-search-sort-method '(frequency kana none))
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
  "Sort candidates within ENTRY by Prolog learning scores."
  (when (nskk-dict-entry-p entry)
    (let ((reading (nskk-dict-entry-key entry)))
      (setf (nskk-dict-entry-candidates entry)
            (sort (copy-sequence (nskk-dict-entry-candidates entry))
                  (lambda (a b)
                    (> (nskk-search--candidate-score reading a)
                       (nskk-search--candidate-score reading b))))))
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
  (if (nskk-dict-entry-p entry)
      (cl-loop for cand in (nskk-dict-entry-candidates entry)
               maximize (nskk-search--candidate-score reading cand))
    0))

(defun nskk-search--candidate-score (reading candidate)
  "Return learning score for CANDIDATE given READING from Prolog database."
  (let ((word (nskk-search--candidate-word candidate)))
    (or (and word (nskk-prolog-query-value
                   `(learning-score ,reading ,word \?s) '\?s))
        0)))

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
  "Load learning data from `nskk-search-learning-file' into Prolog facts."
  (nskk-prolog-retract-all 'learning-score 3)
  (nskk-prolog-set-index 'learning-score 3 :hash)
  (when (file-readable-p nskk-search-learning-file)
    (ignore-errors
      (with-temp-buffer
        (insert-file-contents nskk-search-learning-file)
        (let ((data (read (current-buffer))))
          ;; data is a list of (reading candidate score) tuples
          (when (listp data)
            (dolist (entry data)
              (when (= (length entry) 3)
                (nskk-prolog-assert
                 (list (cons 'learning-score entry)))))))))))

;;;###autoload
(defun nskk-search-save-learning-data ()
  "Save learning data from Prolog facts to `nskk-search-learning-file'."
  (interactive)
  (let ((dir (file-name-directory nskk-search-learning-file)))
    (unless (file-directory-p dir)
      (make-directory dir t)))
  (condition-case err
      (with-temp-file nskk-search-learning-file
        ;; Query all learning scores and serialize as (reading candidate score) tuples
        (let ((solutions (nskk-prolog-query '(learning-score \?r \?c \?s))))
          (prin1
           (mapcar (lambda (sol)
                     (list (nskk-prolog-walk '\?r sol)
                           (nskk-prolog-walk '\?c sol)
                           (nskk-prolog-walk '\?s sol)))
                   solutions)
           (current-buffer)))
        (setq nskk-search--dirty-flag nil)
        (message "Learning data saved"))
    (error
     (message "Failed to save learning data: %s" err))))

;;;###autoload
(defun nskk-search-learn (query candidate &optional _context)
  "Record that CANDIDATE was selected for QUERY.
_CONTEXT is reserved for future use.
Stores learning score as a Prolog learning-score/3 fact."
  (let* ((word (if (stringp candidate) candidate (car candidate)))
         (old-score (when word
                      (nskk-prolog-query-value
                       `(learning-score ,query ,word \?s) '\?s)))
         (new-score (1+ (or old-score 0))))
    (when word
      (nskk-debug-log "[SEARCH] learn: query=%s word=%s new-score=%d" query word new-score)
      (when old-score
        (nskk-prolog-retract `(learning-score ,query ,word ,old-score)))
      (nskk-prolog-assert (list `(learning-score ,query ,word ,new-score)))
      (setq nskk-search--dirty-flag t))))

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
    (let ((cached (nskk-cache-get cache cache-key)))
      (if cached
          (progn
            (nskk-debug-log "[SEARCH] cache-hit: key=%s" cache-key)
            cached)
        (nskk-debug-log "[SEARCH] cache-miss: key=%s" cache-key)
        (let ((result (nskk-search index query search-type okuri-type limit)))
          (when result
            (nskk-cache-put cache cache-key result))
          result)))))

(provide 'nskk-search)

;;; nskk-search.el ends here
