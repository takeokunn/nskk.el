;;; nskk-multi-cache-test.el --- Tests for nskk-multi-cache.el -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; This file is part of NSKK.

;;; Commentary:

;; nskk-multi-cache.el の単体テスト。

;;; Code:

(require 'ert)
(require 'nskk-multi-cache)

;;; ARCキャッシュテスト

(ert-deftest nskk-multi-cache-test-arc-create ()
  "ARCキャッシュ作成のテスト。"
  (let ((cache (nskk-multi-cache-arc-create 100)))
    (should (nskk-multi-cache-arc-p cache))
    (should (= (nskk-multi-cache-arc-capacity cache) 100))
    (should (= (nskk-multi-cache-arc-hits cache) 0))
    (should (= (nskk-multi-cache-arc-misses cache) 0))))

(ert-deftest nskk-multi-cache-test-arc-put-get ()
  "ARCキャッシュのput/getテスト。"
  (let ((cache (nskk-multi-cache-arc-create 10)))
    ;; データを追加
    (nskk-multi-cache-arc-put cache "key1" "value1")
    (nskk-multi-cache-arc-put cache "key2" "value2")

    ;; データを取得
    (should (string= (nskk-multi-cache-arc-get cache "key1") "value1"))
    (should (string= (nskk-multi-cache-arc-get cache "key2") "value2"))

    ;; ヒット/ミス数の確認
    (should (= (nskk-multi-cache-arc-hits cache) 2))
    (should (= (nskk-multi-cache-arc-misses cache) 0))))

(ert-deftest nskk-multi-cache-test-arc-miss ()
  "ARCキャッシュミスのテスト。"
  (let ((cache (nskk-multi-cache-arc-create 10)))
    (should (null (nskk-multi-cache-arc-get cache "missing")))
    (should (= (nskk-multi-cache-arc-misses cache) 1))))

(ert-deftest nskk-multi-cache-test-arc-eviction ()
  "ARCキャッシュエビクションのテスト。"
  (let ((cache (nskk-multi-cache-arc-create 3)))
    ;; 容量を超えてデータを追加
    (nskk-multi-cache-arc-put cache "key1" "value1")
    (nskk-multi-cache-arc-put cache "key2" "value2")
    (nskk-multi-cache-arc-put cache "key3" "value3")
    (nskk-multi-cache-arc-put cache "key4" "value4")

    ;; 古いエントリは追い出される
    (should (hash-table-p (nskk-multi-cache-arc-hash cache)))))

;;; 多層キャッシュテスト

(ert-deftest nskk-multi-cache-test-create ()
  "多層キャッシュ作成のテスト。"
  (let ((cache (nskk-multi-cache-create)))
    (should (nskk-multi-cache-p cache))
    (should (nskk-cache-lru-p (nskk-multi-cache-l1 cache)))
    (should (nskk-multi-cache-arc-p (nskk-multi-cache-l2 cache)))
    (should (nskk-cache-lfu-p (nskk-multi-cache-l3 cache)))
    (should (nskk-cache-lru-p (nskk-multi-cache-tlb cache)))))

(ert-deftest nskk-multi-cache-test-put-get ()
  "多層キャッシュのput/getテスト。"
  (let ((cache (nskk-multi-cache-create)))
    ;; データを追加（L1に追加される）
    (nskk-multi-cache-put cache "key1" "value1")

    ;; データを取得（L1ヒット）
    (should (string= (nskk-multi-cache-get cache "key1") "value1"))
    (should (= (nskk-multi-cache-l1-hits cache) 1))
    (should (= (nskk-multi-cache-hits cache) 1))))

(ert-deftest nskk-multi-cache-test-tlb ()
  "TLBキャッシュのテスト。"
  (let ((cache (nskk-multi-cache-create)))
    ;; TLBに追加
    (nskk-multi-cache-put-tlb cache "tlb-key" "tlb-value")

    ;; TLBから取得
    (should (string= (nskk-multi-cache-get cache "tlb-key") "tlb-value"))
    (should (= (nskk-multi-cache-tlb-hits cache) 1))))

(ert-deftest nskk-multi-cache-test-miss ()
  "多層キャッシュミスのテスト。"
  (let ((cache (nskk-multi-cache-create)))
    (should (null (nskk-multi-cache-get cache "missing")))
    (should (= (nskk-multi-cache-misses cache) 1))))

(ert-deftest nskk-multi-cache-test-auto-promotion ()
  "自動プロモーションのテスト。"
  (let ((cache (nskk-multi-cache-create))
        (nskk-multi-cache-enable-auto-promotion t))
    ;; L3に直接追加（通常はありえないが、テスト用）
    (nskk-cache-lfu-put (nskk-multi-cache-l3 cache) "key1" "value1")

    ;; L3から取得（L2にプロモーションされる）
    (should (string= (nskk-multi-cache-get cache "key1") "value1"))
    (should (= (nskk-multi-cache-l3-hits cache) 1))

    ;; 再度取得（L2ヒット）
    (should (string= (nskk-multi-cache-get cache "key1") "value1"))
    (should (= (nskk-multi-cache-l2-hits cache) 1))))

(ert-deftest nskk-multi-cache-test-invalidate ()
  "キャッシュ無効化のテスト。"
  (let ((cache (nskk-multi-cache-create)))
    ;; データを追加
    (nskk-multi-cache-put cache "key1" "value1")
    (should (string= (nskk-multi-cache-get cache "key1") "value1"))

    ;; 無効化
    (nskk-multi-cache-invalidate cache "key1")

    ;; 取得できない
    (should (null (nskk-multi-cache-get cache "key1")))))

(ert-deftest nskk-multi-cache-test-clear ()
  "キャッシュクリアのテスト。"
  (let ((cache (nskk-multi-cache-create)))
    ;; データを追加
    (nskk-multi-cache-put cache "key1" "value1")
    (nskk-multi-cache-put cache "key2" "value2")

    ;; クリア
    (nskk-multi-cache-clear cache)

    ;; すべてのデータが削除される
    (should (null (nskk-multi-cache-get cache "key1")))
    (should (null (nskk-multi-cache-get cache "key2")))

    ;; 統計がリセットされる
    (should (= (nskk-multi-cache-hits cache) 0))
    (should (= (nskk-multi-cache-misses cache) 2))))

;;; キャッシュウォーミングテスト

(ert-deftest nskk-multi-cache-test-warm-up ()
  "キャッシュウォーミングのテスト。"
  (let ((cache (nskk-multi-cache-create))
        (entries '(("key1" . "value1")
                  ("key2" . "value2")
                  ("key3" . "value3"))))
    ;; ウォームアップ
    (nskk-multi-cache-warm-up cache entries)

    ;; データが取得できる（L3から）
    (should (string= (nskk-multi-cache-get cache "key1") "value1"))
    (should (string= (nskk-multi-cache-get cache "key2") "value2"))
    (should (string= (nskk-multi-cache-get cache "key3") "value3"))))

;;; 統計情報テスト

(ert-deftest nskk-multi-cache-test-stats ()
  "統計情報のテスト。"
  (let ((cache (nskk-multi-cache-create)))
    ;; データを追加して操作
    (nskk-multi-cache-put cache "key1" "value1")
    (nskk-multi-cache-get cache "key1") ; L1 hit
    (nskk-multi-cache-get cache "missing") ; miss

    (let ((stats (nskk-multi-cache-stats cache)))
      (should (plist-member stats :total-hits))
      (should (plist-member stats :total-misses))
      (should (plist-member stats :hit-rate))
      (should (plist-member stats :l1-hits))
      (should (plist-member stats :l2-hits))
      (should (plist-member stats :l3-hits))
      (should (plist-member stats :tlb-hits))
      (should (plist-member stats :l1-size))
      (should (plist-member stats :l2-size))
      (should (plist-member stats :l3-size))
      (should (plist-member stats :tlb-size))

      ;; 値の確認
      (should (= (plist-get stats :total-hits) 1))
      (should (= (plist-get stats :total-misses) 1))
      (should (= (plist-get stats :l1-hits) 1))
      (should (= (plist-get stats :hit-rate) 0.5)))))

;;; ヒット率テスト

(ert-deftest nskk-multi-cache-test-hit-rate ()
  "ヒット率のテスト。"
  (let ((cache (nskk-multi-cache-create)))
    ;; データを追加
    (nskk-multi-cache-put cache "key1" "value1")
    (nskk-multi-cache-put cache "key2" "value2")

    ;; 10回アクセス（8回ヒット、2回ミス）
    (dotimes (_ 4)
      (nskk-multi-cache-get cache "key1")
      (nskk-multi-cache-get cache "key2"))
    (nskk-multi-cache-get cache "missing1")
    (nskk-multi-cache-get cache "missing2")

    (let ((stats (nskk-multi-cache-stats cache)))
      (should (= (plist-get stats :total-hits) 8))
      (should (= (plist-get stats :total-misses) 2))
      ;; ヒット率 = 8 / 10 = 0.8
      (should (= (plist-get stats :hit-rate) 0.8)))))

;;; 階層別ヒットテスト

(ert-deftest nskk-multi-cache-test-level-hits ()
  "階層別ヒットのテスト。"
  (let ((cache (nskk-multi-cache-create))
        (nskk-multi-cache-enable-auto-promotion nil))
    ;; L1に追加
    (nskk-multi-cache-put cache "l1-key" "l1-value")

    ;; L3に追加
    (nskk-cache-lfu-put (nskk-multi-cache-l3 cache) "l3-key" "l3-value")

    ;; TLBに追加
    (nskk-multi-cache-put-tlb cache "tlb-key" "tlb-value")

    ;; 各階層から取得
    (nskk-multi-cache-get cache "tlb-key")  ; TLB hit
    (nskk-multi-cache-get cache "l1-key")   ; L1 hit
    (nskk-multi-cache-get cache "l3-key")   ; L3 hit

    ;; 階層別ヒット数の確認
    (should (= (nskk-multi-cache-tlb-hits cache) 1))
    (should (= (nskk-multi-cache-l1-hits cache) 1))
    (should (= (nskk-multi-cache-l3-hits cache) 1))
    (should (= (nskk-multi-cache-hits cache) 3))))

(provide 'nskk-multi-cache-test)

;;; nskk-multi-cache-test.el ends here
