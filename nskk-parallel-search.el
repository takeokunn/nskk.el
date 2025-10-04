;;; nskk-parallel-search.el --- Parallel dictionary search for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Development Team

;; Author: NSKK Development Team
;; Maintainer: NSKK Development Team
;; Keywords: japanese, input-method, skk, threading, search
;; Package-Requires: ((emacs "31.0"))

;;; Commentary:

;; このファイルは、スレッドプールを活用した並列辞書検索を実装します。
;;
;; 主な機能:
;; - 辞書分割戦略（ハッシュベース/範囲ベース）
;; - 並列検索実行
;; - 結果マージ
;; - ロードバランシング
;;
;; パフォーマンス目標: 3倍以上の高速化
;;
;; 使用例:
;;   (let ((index (nskk-index-create)))
;;     ;; 並列検索
;;     (nskk-parallel-search index "かん" 'prefix)
;;     ;; => (("かん" . entry1) ("かんじ" . entry2) ...))

;;; Code:

(require 'cl-lib)
(require 'nskk-thread-pool)
(require 'nskk-search)
(require 'nskk-trie)

;;; カスタマイズ変数

(defgroup nskk-parallel-search nil
  "NSKK 並列検索設定。"
  :group 'nskk
  :prefix "nskk-parallel-search-")

(defcustom nskk-parallel-search-partition-strategy 'hash
  "辞書分割戦略。
  - 'hash: ハッシュベース分割
  - 'range: 範囲ベース分割"
  :type '(choice (const :tag "Hash-based" hash)
                 (const :tag "Range-based" range))
  :group 'nskk-parallel-search)

(defcustom nskk-parallel-search-min-entries 1000
  "並列検索を有効にする最小エントリ数。
これ以下の場合は通常の検索を使用する。"
  :type 'integer
  :group 'nskk-parallel-search)

(defcustom nskk-parallel-search-timeout 5.0
  "並列検索のタイムアウト秒数。"
  :type 'float
  :group 'nskk-parallel-search)

;;; グローバル変数

(defvar nskk-parallel-search--pool nil
  "並列検索用のグローバルスレッドプール。")

;;; 初期化

(defun nskk-parallel-search-initialize ()
  "並列検索システムを初期化する。"
  (unless nskk-parallel-search--pool
    (when (nskk-thread-pool-available-p)
      (setq nskk-parallel-search--pool
            (nskk-thread-pool-create)))))

(defun nskk-parallel-search-shutdown ()
  "並列検索システムをシャットダウンする。"
  (when nskk-parallel-search--pool
    (nskk-thread-pool-shutdown nskk-parallel-search--pool)
    (setq nskk-parallel-search--pool nil)))

;;; 辞書分割

(defun nskk-parallel-search--partition-hash (entries num-partitions)
  "ENTRIES をハッシュベースで NUM-PARTITIONS に分割する。"
  (let ((partitions (make-vector num-partitions nil)))
    (dolist (entry entries)
      (let* ((key (car entry))
             (hash (sxhash key))
             (partition-idx (mod hash num-partitions)))
        (push entry (aref partitions partition-idx))))
    (cl-coerce partitions 'list)))

(defun nskk-parallel-search--partition-range (entries num-partitions)
  "ENTRIES を範囲ベースで NUM-PARTITIONS に分割する。"
  (let* ((sorted-entries (sort (copy-sequence entries)
                               (lambda (a b)
                                 (string< (car a) (car b)))))
         (total (length sorted-entries))
         (partition-size (/ total num-partitions))
         (partitions nil))
    (dotimes (i num-partitions)
      (let* ((start (* i partition-size))
             (end (if (= i (1- num-partitions))
                      total
                    (* (1+ i) partition-size)))
             (partition (cl-subseq sorted-entries start end)))
        (push partition partitions)))
    (nreverse partitions)))

(defun nskk-parallel-search--partition (entries strategy num-partitions)
  "ENTRIES を STRATEGY に従って NUM-PARTITIONS に分割する。"
  (pcase strategy
    ('hash (nskk-parallel-search--partition-hash entries num-partitions))
    ('range (nskk-parallel-search--partition-range entries num-partitions))
    (_ (error "Unknown partition strategy: %s" strategy))))

;;; 並列検索コア

(defun nskk-parallel-search--search-partition (partition query search-type)
  "PARTITION 内で QUERY を SEARCH-TYPE で検索する。"
  (let ((results nil))
    (pcase search-type
      ('exact
       ;; 完全一致検索
       (let ((entry (assoc query partition)))
         (when entry
           (push entry results))))

      ('prefix
       ;; 前方一致検索
       (dolist (entry partition)
         (when (string-prefix-p query (car entry))
           (push entry results))))

      ('partial
       ;; 部分一致検索
       (dolist (entry partition)
         (when (string-match-p (regexp-quote query) (car entry))
           (push entry results))))

      ('fuzzy
       ;; ファジー検索
       (dolist (entry partition)
         (let ((distance (nskk-parallel-search--levenshtein-distance
                          query (car entry))))
           (when (<= distance nskk-search-fuzzy-threshold)
             (push (cons entry distance) results)))))

      (_ (error "Unknown search type: %s" search-type)))

    results))

;;; ヘルパー関数

(defun nskk-parallel-search--levenshtein-distance (s1 s2)
  "S1 と S2 の Levenshtein 距離を計算する。"
  (let* ((len1 (length s1))
         (len2 (length s2))
         (matrix (make-vector (1+ len1) nil)))

    ;; 行列初期化
    (dotimes (i (1+ len1))
      (aset matrix i (make-vector (1+ len2) 0))
      (aset (aref matrix i) 0 i))
    (dotimes (j (1+ len2))
      (aset (aref matrix 0) j j))

    ;; DP計算
    (dotimes (i len1)
      (dotimes (j len2)
        (let ((cost (if (= (aref s1 i) (aref s2 j)) 0 1)))
          (aset (aref matrix (1+ i)) (1+ j)
                (min (1+ (aref (aref matrix i) (1+ j)))
                     (1+ (aref (aref matrix (1+ i)) j))
                     (+ (aref (aref matrix i) j) cost))))))

    (aref (aref matrix len1) len2)))

(defun nskk-parallel-search--merge-results (results search-type)
  "複数の検索結果 RESULTS をマージする。"
  (let ((merged nil))
    (dolist (result results)
      (setq merged (append merged result)))

    ;; 重複除去
    (setq merged (delete-dups merged))

    ;; ソート
    (pcase search-type
      ('fuzzy
       ;; ファジー検索: 距離でソート
       (sort merged (lambda (a b) (< (cdr a) (cdr b)))))
      (_
       ;; その他: 辞書順
       (sort merged (lambda (a b) (string< (car a) (car b))))))))

;;; 公開API

;;;###autoload
(defun nskk-parallel-search (index query search-type &optional max-results)
  "INDEX 内で QUERY を並列検索する。
SEARCH-TYPE は検索タイプ（'exact, 'prefix, 'partial, 'fuzzy）。
MAX-RESULTS は最大結果数（オプション）。"
  ;; 初期化確認
  (unless nskk-parallel-search--pool
    (nskk-parallel-search-initialize))

  ;; 並列検索が利用不可能な場合は通常検索にフォールバック
  (unless (and nskk-parallel-search--pool
               (nskk-thread-pool-available-p))
    (return-from nskk-parallel-search
      (nskk-search index query search-type nil max-results)))

  ;; エントリ数チェック
  (let* ((entries (nskk-index-get-all-entries index))
         (entry-count (length entries)))

    ;; エントリ数が少ない場合は通常検索
    (when (< entry-count nskk-parallel-search-min-entries)
      (return-from nskk-parallel-search
        (nskk-search index query search-type nil max-results)))

    ;; 並列検索実行
    (let* ((num-workers (nskk-thread-pool-size nskk-parallel-search--pool))
           (partitions (nskk-parallel-search--partition
                        entries
                        nskk-parallel-search-partition-strategy
                        num-workers))
           (results-mutex (make-mutex))
           (all-results nil)
           (completed-count 0))

      ;; 各パーティションを並列検索
      (dolist (partition partitions)
        (nskk-thread-submit
         nskk-parallel-search--pool
         (lambda ()
           (nskk-parallel-search--search-partition
            partition query search-type))
         (lambda (partition-results)
           (with-mutex results-mutex
             (push partition-results all-results)
             (setq completed-count (1+ completed-count))))))

      ;; 全検索完了待機
      (let ((start-time (current-time)))
        (while (and (< completed-count num-workers)
                    (< (float-time (time-subtract (current-time) start-time))
                       nskk-parallel-search-timeout))
          (sleep-for 0.01)))

      ;; タイムアウトチェック
      (when (< completed-count num-workers)
        (message "Parallel search timeout, using partial results"))

      ;; 結果マージ
      (let ((merged (nskk-parallel-search--merge-results all-results search-type)))
        ;; 最大結果数制限
        (if max-results
            (cl-subseq merged 0 (min (length merged) max-results))
          merged)))))

;;;###autoload
(defun nskk-parallel-search-benchmark (index query search-type iterations)
  "並列検索と通常検索のベンチマーク。
INDEX 内で QUERY を SEARCH-TYPE で ITERATIONS 回検索し、
平均実行時間と高速化率を報告する。"
  (interactive)

  ;; 通常検索ベンチマーク
  (let ((sequential-times nil)
        (parallel-times nil))

    (message "Running sequential search benchmark...")
    (dotimes (_ iterations)
      (let ((start (current-time)))
        (nskk-search index query search-type)
        (push (float-time (time-subtract (current-time) start))
              sequential-times)))

    ;; 並列検索ベンチマーク
    (message "Running parallel search benchmark...")
    (dotimes (_ iterations)
      (let ((start (current-time)))
        (nskk-parallel-search index query search-type)
        (push (float-time (time-subtract (current-time) start))
              parallel-times)))

    ;; 結果分析
    (let* ((seq-avg (/ (apply #'+ sequential-times) iterations))
           (par-avg (/ (apply #'+ parallel-times) iterations))
           (speedup (/ seq-avg par-avg)))

      (message "Benchmark Results:")
      (message "  Sequential: %.3f ms" (* seq-avg 1000))
      (message "  Parallel:   %.3f ms" (* par-avg 1000))
      (message "  Speedup:    %.2fx" speedup)

      (list :sequential seq-avg
            :parallel par-avg
            :speedup speedup))))

;;; 統計情報

(defun nskk-parallel-search-statistics ()
  "並列検索の統計情報を表示する。"
  (interactive)
  (if nskk-parallel-search--pool
      (message "Parallel Search Statistics:
  Pool size: %d
  Partition strategy: %s
  Min entries for parallel: %d
  Timeout: %.1f seconds"
               (nskk-thread-pool-size nskk-parallel-search--pool)
               nskk-parallel-search-partition-strategy
               nskk-parallel-search-min-entries
               nskk-parallel-search-timeout)
    (message "Parallel search not initialized")))

(provide 'nskk-parallel-search)

;;; nskk-parallel-search.el ends here
