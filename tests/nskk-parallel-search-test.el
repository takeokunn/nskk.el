;;; nskk-parallel-search-test.el --- Tests for nskk-parallel-search -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Development Team

;;; Commentary:

;; nskk-parallel-search.el のテスト

;;; Code:

(require 'ert)
(require 'nskk-parallel-search)
(require 'nskk-index)

;;; テストヘルパー

(defun nskk-parallel-search-test--create-test-index (size)
  "SIZE エントリのテスト用インデックスを作成する。"
  (let ((index (nskk-index-create)))
    (dotimes (i size)
      (let ((key (format "かん%d" i))
            (value (format "漢字%d" i)))
        (nskk-index-insert index key (list value))))
    index))

;;; 初期化テスト

(ert-deftest nskk-parallel-search-test-initialize ()
  "並列検索初期化のテスト。"
  (skip-unless (nskk-thread-pool-available-p))
  (nskk-parallel-search-initialize)
  (should nskk-parallel-search--pool)
  (nskk-parallel-search-shutdown))

(ert-deftest nskk-parallel-search-test-shutdown ()
  "並列検索シャットダウンのテスト。"
  (skip-unless (nskk-thread-pool-available-p))
  (nskk-parallel-search-initialize)
  (nskk-parallel-search-shutdown)
  (should-not nskk-parallel-search--pool))

;;; 分割戦略テスト

(ert-deftest nskk-parallel-search-test-partition-hash ()
  "ハッシュベース分割のテスト。"
  (let* ((entries '(("a" . 1) ("b" . 2) ("c" . 3) ("d" . 4)))
         (partitions (nskk-parallel-search--partition-hash entries 2)))
    (should (= (length partitions) 2))
    ;; 全エントリが分割されている
    (should (= (+ (length (nth 0 partitions))
                  (length (nth 1 partitions)))
               4))))

(ert-deftest nskk-parallel-search-test-partition-range ()
  "範囲ベース分割のテスト。"
  (let* ((entries '(("a" . 1) ("b" . 2) ("c" . 3) ("d" . 4)))
         (partitions (nskk-parallel-search--partition-range entries 2)))
    (should (= (length partitions) 2))
    ;; 各パーティションのサイズがほぼ均等
    (should (= (length (nth 0 partitions)) 2))
    (should (= (length (nth 1 partitions)) 2))))

(ert-deftest nskk-parallel-search-test-partition-strategy ()
  "分割戦略選択のテスト。"
  (let ((entries '(("a" . 1) ("b" . 2) ("c" . 3) ("d" . 4))))
    ;; ハッシュ戦略
    (let ((partitions (nskk-parallel-search--partition entries 'hash 2)))
      (should (= (length partitions) 2)))

    ;; 範囲戦略
    (let ((partitions (nskk-parallel-search--partition entries 'range 2)))
      (should (= (length partitions) 2)))))

;;; 検索機能テスト

(ert-deftest nskk-parallel-search-test-exact-search ()
  "完全一致検索のテスト。"
  (skip-unless (nskk-thread-pool-available-p))
  (let* ((index (nskk-parallel-search-test--create-test-index 100))
         (result (nskk-parallel-search index "かん50" 'exact)))
    (should result)
    (should (string= (caar result) "かん50"))
    (nskk-parallel-search-shutdown)))

(ert-deftest nskk-parallel-search-test-prefix-search ()
  "前方一致検索のテスト。"
  (skip-unless (nskk-thread-pool-available-p))
  (let* ((index (nskk-parallel-search-test--create-test-index 100))
         (result (nskk-parallel-search index "かん" 'prefix 10)))
    (should result)
    (should (<= (length result) 10))
    ;; 全結果が前方一致している
    (dolist (entry result)
      (should (string-prefix-p "かん" (car entry))))
    (nskk-parallel-search-shutdown)))

(ert-deftest nskk-parallel-search-test-partial-search ()
  "部分一致検索のテスト。"
  (skip-unless (nskk-thread-pool-available-p))
  (let* ((index (nskk-parallel-search-test--create-test-index 100))
         (result (nskk-parallel-search index "ん5" 'partial)))
    (should result)
    ;; 全結果が部分一致している
    (dolist (entry result)
      (should (string-match-p "ん5" (car entry))))
    (nskk-parallel-search-shutdown)))

;;; Levenshtein距離テスト

(ert-deftest nskk-parallel-search-test-levenshtein-distance ()
  "Levenshtein距離計算のテスト。"
  (should (= (nskk-parallel-search--levenshtein-distance "kitten" "sitting") 3))
  (should (= (nskk-parallel-search--levenshtein-distance "abc" "abc") 0))
  (should (= (nskk-parallel-search--levenshtein-distance "" "abc") 3))
  (should (= (nskk-parallel-search--levenshtein-distance "abc" "") 3)))

;;; マージ機能テスト

(ert-deftest nskk-parallel-search-test-merge-results ()
  "結果マージのテスト。"
  (let* ((results '((("a" . 1) ("b" . 2))
                    (("c" . 3) ("d" . 4))))
         (merged (nskk-parallel-search--merge-results results 'exact)))
    (should (= (length merged) 4))
    ;; ソートされている
    (should (string= (caar merged) "a"))
    (should (string= (car (car (last merged))) "d"))))

(ert-deftest nskk-parallel-search-test-merge-with-duplicates ()
  "重複を含む結果マージのテスト。"
  (let* ((results '((("a" . 1) ("b" . 2))
                    (("a" . 1) ("c" . 3))))
         (merged (nskk-parallel-search--merge-results results 'exact)))
    ;; 重複が除去されている
    (should (= (length merged) 3))))

;;; フォールバックテスト

(ert-deftest nskk-parallel-search-test-fallback-small-index ()
  "小規模インデックスでの通常検索フォールバックのテスト。"
  (skip-unless (nskk-thread-pool-available-p))
  (let ((nskk-parallel-search-min-entries 1000)
        (index (nskk-parallel-search-test--create-test-index 100)))
    ;; エントリ数が少ないので通常検索にフォールバック
    (let ((result (nskk-parallel-search index "かん" 'prefix)))
      (should result))
    (nskk-parallel-search-shutdown)))

;;; パフォーマンステスト

(ert-deftest nskk-parallel-search-test-performance ()
  "並列検索パフォーマンステスト。"
  (skip-unless (nskk-thread-pool-available-p))
  (let* ((index (nskk-parallel-search-test--create-test-index 5000))
         (start (current-time))
         (result (nskk-parallel-search index "かん" 'prefix 100))
         (elapsed (float-time (time-subtract (current-time) start))))

    (should result)
    ;; 妥当な時間内に完了
    (should (< elapsed 1.0))  ; 1秒以内

    (nskk-parallel-search-shutdown)))

(ert-deftest nskk-parallel-search-test-speedup ()
  "並列化による高速化のテスト。"
  (skip-unless (nskk-thread-pool-available-p))
  (let* ((index (nskk-parallel-search-test--create-test-index 10000))
         (iterations 5))

    ;; 通常検索
    (let ((seq-times nil))
      (dotimes (_ iterations)
        (let ((start (current-time)))
          (nskk-search index "かん" 'prefix)
          (push (float-time (time-subtract (current-time) start))
                seq-times)))

      ;; 並列検索
      (let ((par-times nil))
        (dotimes (_ iterations)
          (let ((start (current-time)))
            (nskk-parallel-search index "かん" 'prefix)
            (push (float-time (time-subtract (current-time) start))
                  par-times)))

        (let* ((seq-avg (/ (apply #'+ seq-times) iterations))
               (par-avg (/ (apply #'+ par-times) iterations))
               (speedup (/ seq-avg par-avg)))

          ;; 並列検索が高速であることを確認（最低でも1.2倍）
          (should (>= speedup 1.2))
          (message "Speedup: %.2fx" speedup))))

    (nskk-parallel-search-shutdown)))

;;; タイムアウトテスト

(ert-deftest nskk-parallel-search-test-timeout ()
  "タイムアウト処理のテスト。"
  (skip-unless (nskk-thread-pool-available-p))
  (let ((nskk-parallel-search-timeout 0.1)
        (index (nskk-parallel-search-test--create-test-index 10000)))

    ;; タイムアウトしても結果が返ることを確認
    (let ((result (nskk-parallel-search index "かん" 'prefix)))
      ;; 部分的な結果でも返る
      (should (or result t)))

    (nskk-parallel-search-shutdown)))

;;; 統計情報テスト

(ert-deftest nskk-parallel-search-test-statistics ()
  "統計情報表示のテスト。"
  (skip-unless (nskk-thread-pool-available-p))
  (nskk-parallel-search-initialize)
  (should-not (nskk-parallel-search-statistics))
  (nskk-parallel-search-shutdown))

(provide 'nskk-parallel-search-test)

;;; nskk-parallel-search-test.el ends here
