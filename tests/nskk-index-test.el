;;; nskk-index-test.el --- Tests for nskk-index -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; This file is part of NSKK.

;;; Commentary:

;; このファイルはnskk-indexの包括的なテストを提供します。
;;
;; テスト項目:
;; 1. インデックス構築テスト（1件、100件、1000件）
;; 2. 検索性能テスト（< 10ms）
;; 3. 増分更新テスト
;; 4. マージテスト（複数辞書統合）
;; 5. キャッシュ統合テスト
;; 6. バッチ更新テスト
;; 7. パーティション分割テスト
;; 8. 統計情報テスト
;; 9. メモリ使用量テスト
;; 10. 最適化効果テスト

;;; Code:

(require 'ert)
(require 'nskk-index)
(require 'nskk-dict-struct)
(require 'nskk-dict-parser)

;;; テストフィクスチャ

(defun nskk-index-test--create-test-dict (size)
  "テスト用辞書を作成する。

引数:
  SIZE - エントリ数

戻り値:
  nskk-dict構造体"
  (let* ((okuri-ari nil)
         (okuri-nasi nil)
         (okuri-ari-count (if (>= size 10)
                              (max 1 (/ size 10))
                            0))
         (okuri-nasi-count (max 0 (- size okuri-ari-count))))
    ;; 送り仮名なしエントリ生成
    (dotimes (i okuri-nasi-count)
      (let* ((midashi (format "てすと%d" i))
             (candidates (list (cons (format "テスト%d" i) nil)))
             (entry (nskk-dict-entry--create
                    :midashi midashi
                    :candidates candidates)))
        (push entry okuri-nasi)))

    ;; 送り仮名ありエントリ生成（少数）
    (dotimes (i okuri-ari-count)
      (let* ((midashi (format "あるk%d" i))
             (candidates (list (cons (format "歩%d" i) nil)))
             (entry (nskk-dict-entry--create
                    :midashi midashi
                    :candidates candidates)))
        (push entry okuri-ari)))

    ;; nskk-dict構造体を作成（簡易版）
    ;; nskk-dict-parserのnskk-dict構造体を利用
    (require 'nskk-dict-parser)
    (nskk-dict--create
     :okuri-ari (nreverse okuri-ari)
     :okuri-nasi (nreverse okuri-nasi)
     :file-path "/tmp/test.dict"
     :encoding 'utf-8
     :header nil
     :errors nil)))

(defun nskk-index-test--create-test-index (size)
  "テスト用インデックスを作成する。

引数:
  SIZE - エントリ数

戻り値:
  nskk-index構造体"
  (let* ((test-dict (nskk-index-test--create-test-dict size))
         (dict-struct (nskk-dict-struct-from-parser test-dict))
         (index (nskk-index-create)))
    (nskk-index-build index dict-struct)
    index))

;;; Test 1: インデックス構築テスト

(ert-deftest nskk-index-test-build-small ()
  "小規模辞書（1件）のインデックス構築テスト。"
  (let ((index (nskk-index-test--create-test-index 1)))
    (should (nskk-index-p index))
    (should (= (nskk-index-total-entries index) 1))
    (should (> (nskk-index-build-time index) 0))))

(ert-deftest nskk-index-test-build-medium ()
  "中規模辞書（100件）のインデックス構築テスト。"
  (let ((index (nskk-index-test--create-test-index 100)))
    (should (nskk-index-p index))
    (should (>= (nskk-index-total-entries index) 100))
    (should (< (nskk-index-build-time index) 1.0))))  ; < 1秒

(ert-deftest nskk-index-test-build-large ()
  "大規模辞書（1000件）のインデックス構築テスト。"
  (let ((index (nskk-index-test--create-test-index 1000)))
    (should (nskk-index-p index))
    (should (>= (nskk-index-total-entries index) 1000))
    (should (< (nskk-index-build-time index) 5.0))))  ; < 5秒

;;; Test 2: 検索性能テスト

(ert-deftest nskk-index-test-search-exact ()
  "完全一致検索テスト。"
  (let* ((index (nskk-index-test--create-test-index 100))
         (start-time (float-time))
         (result (nskk-index-lookup index "てすと0" 'okuri-nasi))
         (elapsed-time (- (float-time) start-time)))
    (should (nskk-dict-entry-p result))
    (should (equal (nskk-dict-entry-midashi result) "てすと0"))
    (should (< elapsed-time 0.01))))  ; < 10ms

(ert-deftest nskk-index-test-search-prefix ()
  "前方一致検索テスト。"
  (let* ((index (nskk-index-test--create-test-index 100))
         (start-time (float-time))
         (results (nskk-index-search index "てすと" 'prefix 10))
         (elapsed-time (- (float-time) start-time)))
    (should (listp results))
    (should (> (length results) 0))
    (should (<= (length results) 10))
    (should (< elapsed-time 0.01))))  ; < 10ms

(ert-deftest nskk-index-test-search-performance-large ()
  "大規模辞書での検索性能テスト（10万エントリを想定して1000件で検証）。"
  (let* ((index (nskk-index-test--create-test-index 1000))
         (start-time (float-time))
         (result (nskk-index-lookup index "てすと500" 'okuri-nasi))
         (elapsed-time (- (float-time) start-time)))
    (should (nskk-dict-entry-p result))
    (should (< elapsed-time 0.01))))  ; < 10ms

;;; Test 3: 増分更新テスト

(ert-deftest nskk-index-test-update-single ()
  "単一エントリの増分更新テスト。"
  (let* ((index (nskk-index-test--create-test-index 10))
         (new-entry (nskk-dict-entry-create "あたらしい"
                                           (list (cons "新しい" nil))
                                           'okuri-nasi))
         (start-time (float-time)))
    (nskk-index-update index new-entry)
    (let ((elapsed-time (- (float-time) start-time))
          (result (nskk-index-lookup index "あたらしい" 'okuri-nasi)))
      (should (nskk-dict-entry-p result))
      (should (equal (nskk-dict-entry-midashi result) "あたらしい"))
      (should (< elapsed-time 0.001))  ; < 1ms
      (should (= (nskk-index-total-entries index) 11)))))

(ert-deftest nskk-index-test-update-existing ()
  "既存エントリの更新テスト。"
  (let* ((index (nskk-index-test--create-test-index 10))
         (updated-entry (nskk-dict-entry-create "てすと0"
                                               (list (cons "更新済み" nil))
                                               'okuri-nasi)))
    (nskk-index-update index updated-entry)
    (let ((result (nskk-index-lookup index "てすと0" 'okuri-nasi)))
      (should (nskk-dict-entry-p result))
      ;; エントリ数は変わらない
      (should (= (nskk-index-total-entries index) 10)))))

;;; Test 4: マージテスト

(ert-deftest nskk-index-test-merge ()
  "複数辞書のマージテスト。"
  (let* ((index1 (nskk-index-test--create-test-index 50))
         (index2 (nskk-index-test--create-test-index 50))
         (initial-count (nskk-index-total-entries index1)))
    (nskk-index-merge index1 index2)
    ;; マージ後のエントリ数（重複があるため正確な数は検証困難）
    (should (>= (nskk-index-total-entries index1) initial-count))))

;;; Test 5: キャッシュ統合テスト

(ert-deftest nskk-index-test-cache-hit ()
  "キャッシュヒットテスト。"
  (let ((index (nskk-index-test--create-test-index 10)))
    ;; 1回目の検索（キャッシュミス）
    (nskk-index-lookup index "てすと0" 'okuri-nasi)
    (should (= (nskk-index-cache-misses index) 1))

    ;; 2回目の検索（キャッシュヒット）
    (nskk-index-lookup index "てすと0" 'okuri-nasi)
    (should (= (nskk-index-cache-hits index) 1))))

(ert-deftest nskk-index-test-cache-invalidation ()
  "キャッシュ無効化テスト。"
  (let ((index (nskk-index-test--create-test-index 10)))
    ;; 1回目の検索
    (nskk-index-lookup index "てすと0" 'okuri-nasi)

    ;; エントリを更新
    (let ((updated-entry (nskk-dict-entry-create "てすと0"
                                                (list (cons "更新" nil))
                                                'okuri-nasi)))
      (nskk-index-update index updated-entry))

    ;; 2回目の検索（キャッシュが無効化されているため再検索）
    (nskk-index-lookup index "てすと0" 'okuri-nasi)
    ;; キャッシュミスが2回になる
    (should (= (nskk-index-cache-misses index) 2))))

;;; Test 6: バッチ更新テスト

(ert-deftest nskk-index-test-batch-update ()
  "バッチ更新テスト。"
  (let* ((index (nskk-index-test--create-test-index 10))
         (new-entries nil))
    ;; 10個の新規エントリを作成
    (dotimes (i 10)
      (push (nskk-dict-entry-create (format "ばっち%d" i)
                                   (list (cons (format "バッチ%d" i) nil))
                                   'okuri-nasi)
            new-entries))

    ;; バッチ更新
    (let ((start-time (float-time)))
      (nskk-index-update-batch index new-entries)
      (let ((elapsed-time (- (float-time) start-time)))
        ;; バッチ更新時間が妥当か（< 100ms）
        (should (< elapsed-time 0.1))))

    ;; エントリ数が増えている
    (should (= (nskk-index-total-entries index) 20))

    ;; 追加されたエントリを検索できる
    (let ((result (nskk-index-lookup index "ばっち0" 'okuri-nasi)))
      (should (nskk-dict-entry-p result)))))

;;; Test 7: パーティション分割テスト

(ert-deftest nskk-index-test-partition-hash ()
  "ハッシュベースのパーティション分割テスト。"
  (let ((index (nskk-index-test--create-test-index 100)))
    (nskk-index-partition index 4)
    (should (= (length (nskk-index-partitions index)) 4))
    (should (nskk-index-parallel-ready index))
    ;; 各パーティションが正しく生成されている
    (dolist (partition (nskk-index-partitions index))
      (should (nskk-index-partition-p partition)))))

(ert-deftest nskk-index-test-partition-range ()
  "範囲ベースのパーティション分割テスト。"
  (let* ((index (nskk-index-test--create-test-index 100)))
    (setf (nskk-index-partition-strategy index) 'range)
    (nskk-index-partition index 4)
    (should (= (length (nskk-index-partitions index)) 4))
    ;; 範囲情報が設定されている
    (dolist (partition (nskk-index-partitions index))
      (should (nskk-index-partition-range partition)))))

;;; Test 8: 統計情報テスト

(ert-deftest nskk-index-test-stats ()
  "統計情報取得テスト。"
  (let ((index (nskk-index-test--create-test-index 10)))
    ;; いくつか検索を実行
    (nskk-index-lookup index "てすと0" 'okuri-nasi)
    (nskk-index-lookup index "てすと1" 'okuri-nasi)
    (nskk-index-lookup index "てすと0" 'okuri-nasi)  ; キャッシュヒット

    (let ((stats (nskk-index-stats index)))
      (should (plist-get stats :version))
      (should (= (plist-get stats :total-entries) 10))
      (should (= (plist-get stats :total-searches) 3))
      (should (= (plist-get stats :cache-hits) 1))
      (should (= (plist-get stats :cache-misses) 2))
      (should (> (plist-get stats :cache-hit-rate) 0))
      (should (> (plist-get stats :build-time) 0)))))

(ert-deftest nskk-index-test-cache-hit-rate ()
  "キャッシュヒット率テスト。"
  (let ((index (nskk-index-test--create-test-index 10)))
    ;; 同じクエリを複数回実行
    (dotimes (_ 5)
      (nskk-index-lookup index "てすと0" 'okuri-nasi))

    (let ((stats (nskk-index-stats index)))
      ;; 1回目がミス、2-5回目がヒットなので80%
      (should (>= (plist-get stats :cache-hit-rate) 0.8)))))

;;; Test 9: メモリ使用量テスト

(ert-deftest nskk-index-test-memory-usage ()
  "メモリ使用量テスト（1000エントリで < 50MB目標を簡易検証）。"
  (let* ((index (nskk-index-test--create-test-index 1000))
         (stats (nskk-index-stats index))
         (dict-stats (plist-get stats :dict-stats)))
    ;; 辞書統計情報が取得できる
    (should dict-stats)
    ;; メモリ使用量の推定値が妥当な範囲（実際の測定は困難）
    (when (nskk-dict-statistics-p dict-stats)
      (let ((mem-usage (nskk-dict-statistics-memory-usage dict-stats)))
        (should (> mem-usage 0))))))

;;; Test 10: 最適化効果テスト

(ert-deftest nskk-index-test-optimize ()
  "インデックス最適化テスト。"
  (let ((index (nskk-index-test--create-test-index 10)))
    ;; 最適化前
    (should-not (nskk-index-optimized index))

    ;; 最適化実行
    (nskk-index-optimize index)

    ;; 最適化後
    (should (nskk-index-optimized index))))

(ert-deftest nskk-index-test-compact ()
  "インデックスコンパクト化テスト。"
  (let ((index (nskk-index-test--create-test-index 10)))
    ;; いくつか検索してキャッシュを溜める
    (nskk-index-lookup index "てすと0" 'okuri-nasi)
    (nskk-index-lookup index "てすと1" 'okuri-nasi)

    ;; コンパクト化
    (nskk-index-compact index)

    ;; キャッシュがクリアされている
    (let ((stats (nskk-index-stats index)))
      (should (= (plist-get (plist-get stats :cache-stats) :size) 0)))))

;;; Test 11: エラーハンドリングテスト

(ert-deftest nskk-index-test-search-before-build ()
  "構築前の検索でエラーが発生することを確認。"
  (let ((index (nskk-index-create)))
    (should-error (nskk-index-lookup index "test" 'okuri-nasi))))

(ert-deftest nskk-index-test-update-before-build ()
  "構築前の更新でエラーが発生することを確認。"
  (let ((index (nskk-index-create))
        (entry (nskk-dict-entry-create "test" (list (cons "テスト" nil)) 'okuri-nasi)))
    (should-error (nskk-index-update index entry))))

;;; Test 12: 整合性テスト

(ert-deftest nskk-index-test-validate ()
  "インデックス整合性検証テスト。"
  (let ((index (nskk-index-test--create-test-index 10)))
    (let ((validation-result (nskk-index-validate index)))
      (should (eq validation-result t)))))

(ert-deftest nskk-index-test-rebuild ()
  "インデックス再構築テスト。"
  (let ((index (nskk-index-test--create-test-index 10)))
    ;; いくつか検索してキャッシュとカウンタを増やす
    (nskk-index-lookup index "てすと0" 'okuri-nasi)
    (nskk-index-lookup index "てすと1" 'okuri-nasi)

    ;; 再構築
    (nskk-index-rebuild index)

    ;; カウンタがリセットされている
    (should (= (nskk-index-total-searches index) 0))
    (should (= (nskk-index-cache-hits index) 0))
    (should (= (nskk-index-cache-misses index) 0))
    (should-not (nskk-index-optimized index))))

;;; Test 13: 並列化準備テスト

(ert-deftest nskk-index-test-parallel-ready ()
  "並列化準備テスト。"
  (let* ((test-dict (nskk-index-test--create-test-dict 10))
         (dict-struct (nskk-dict-struct-from-parser test-dict))
         (index (nskk-index-create)))
    (nskk-index-build-parallel-ready index dict-struct)
    (should (nskk-index-parallel-ready index))))

(provide 'nskk-index-test)

;;; nskk-index-test.el ends here
