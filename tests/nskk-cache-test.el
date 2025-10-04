;;; nskk-cache-test.el --- Tests for nskk-cache.el -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, cache, testing

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

;; このファイルはnskk-cache.elのテストを実装します。

;;; Code:

(require 'ert)
(require 'nskk-cache)
(require 'nskk-test-framework)

;;; LRUキャッシュ基本動作テスト

(nskk-deftest nskk-cache-lru-create-test
  "LRUキャッシュが正しく作成される"
  :tags '(:unit :cache)
  (let ((cache (nskk-cache-lru-create 100)))
    (should (nskk-cache-lru-p cache))
    (should (= (nskk-cache-lru-capacity cache) 100))
    (should (= (nskk-cache-lru-size cache) 0))
    (should (= (nskk-cache-lru-hits cache) 0))
    (should (= (nskk-cache-lru-misses cache) 0))))

(nskk-deftest nskk-cache-lru-put-get-test
  "LRUキャッシュのput/get操作が正しく動作する"
  :tags '(:unit :cache)
  (let ((cache (nskk-cache-lru-create 100)))
    ;; データを追加
    (nskk-cache-lru-put cache "key1" "value1")
    (nskk-cache-lru-put cache "key2" "value2")
    (nskk-cache-lru-put cache "key3" "value3")

    ;; サイズチェック
    (should (= (nskk-cache-lru-size cache) 3))

    ;; データ取得
    (should (equal (nskk-cache-lru-get cache "key1") "value1"))
    (should (equal (nskk-cache-lru-get cache "key2") "value2"))
    (should (equal (nskk-cache-lru-get cache "key3") "value3"))

    ;; 存在しないキー
    (should (null (nskk-cache-lru-get cache "key4")))

    ;; 統計チェック
    (should (= (nskk-cache-lru-hits cache) 3))
    (should (= (nskk-cache-lru-misses cache) 1))))

(nskk-deftest nskk-cache-lru-update-test
  "LRUキャッシュの値更新が正しく動作する"
  :tags '(:unit :cache)
  (let ((cache (nskk-cache-lru-create 100)))
    ;; データを追加
    (nskk-cache-lru-put cache "key1" "value1")
    (should (equal (nskk-cache-lru-get cache "key1") "value1"))

    ;; 値を更新
    (nskk-cache-lru-put cache "key1" "updated-value1")
    (should (equal (nskk-cache-lru-get cache "key1") "updated-value1"))

    ;; サイズは変わらない
    (should (= (nskk-cache-lru-size cache) 1))))

(nskk-deftest nskk-cache-lru-eviction-test
  "LRUキャッシュの容量超過時のエビクションが正しく動作する"
  :tags '(:unit :cache)
  (let ((cache (nskk-cache-lru-create 3)))
    ;; 容量いっぱいまで追加
    (nskk-cache-lru-put cache "key1" "value1")
    (nskk-cache-lru-put cache "key2" "value2")
    (nskk-cache-lru-put cache "key3" "value3")
    (should (= (nskk-cache-lru-size cache) 3))

    ;; 容量を超える追加（key1が削除されるはず）
    (nskk-cache-lru-put cache "key4" "value4")
    (should (= (nskk-cache-lru-size cache) 3))
    (should (null (nskk-cache-lru-get cache "key1")))
    (should (equal (nskk-cache-lru-get cache "key2") "value2"))
    (should (equal (nskk-cache-lru-get cache "key3") "value3"))
    (should (equal (nskk-cache-lru-get cache "key4") "value4"))))

(nskk-deftest nskk-cache-lru-access-order-test
  "LRUキャッシュのアクセス順序が正しく管理される"
  :tags '(:unit :cache)
  (let ((cache (nskk-cache-lru-create 3)))
    ;; データを追加
    (nskk-cache-lru-put cache "key1" "value1")
    (nskk-cache-lru-put cache "key2" "value2")
    (nskk-cache-lru-put cache "key3" "value3")

    ;; key1をアクセス（最近使用済みにする）
    (nskk-cache-lru-get cache "key1")

    ;; 新しいエントリを追加（key2が削除されるはず）
    (nskk-cache-lru-put cache "key4" "value4")
    (should (equal (nskk-cache-lru-get cache "key1") "value1"))
    (should (null (nskk-cache-lru-get cache "key2")))
    (should (equal (nskk-cache-lru-get cache "key3") "value3"))
    (should (equal (nskk-cache-lru-get cache "key4") "value4"))))

(nskk-deftest nskk-cache-lru-invalidate-test
  "LRUキャッシュの無効化が正しく動作する"
  :tags '(:unit :cache)
  (let ((cache (nskk-cache-lru-create 100)))
    ;; データを追加
    (nskk-cache-lru-put cache "key1" "value1")
    (nskk-cache-lru-put cache "key2" "value2")
    (should (= (nskk-cache-lru-size cache) 2))

    ;; key1を無効化
    (should (nskk-cache-lru-invalidate cache "key1"))
    (should (= (nskk-cache-lru-size cache) 1))
    (should (null (nskk-cache-lru-get cache "key1")))
    (should (equal (nskk-cache-lru-get cache "key2") "value2"))

    ;; 存在しないキーの無効化
    (should (null (nskk-cache-lru-invalidate cache "key3")))))

(nskk-deftest nskk-cache-lru-clear-test
  "LRUキャッシュのクリアが正しく動作する"
  :tags '(:unit :cache)
  (let ((cache (nskk-cache-lru-create 100)))
    ;; データを追加
    (nskk-cache-lru-put cache "key1" "value1")
    (nskk-cache-lru-put cache "key2" "value2")
    (nskk-cache-lru-get cache "key1")
    (should (= (nskk-cache-lru-size cache) 2))
    (should (> (nskk-cache-lru-hits cache) 0))

    ;; クリア
    (nskk-cache-lru-clear cache)
    (should (= (nskk-cache-lru-size cache) 0))
    (should (= (nskk-cache-lru-hits cache) 0))
    (should (= (nskk-cache-lru-misses cache) 0))
    (should (null (nskk-cache-lru-get cache "key1")))
    (should (null (nskk-cache-lru-get cache "key2")))))

;;; LFUキャッシュ基本動作テスト

(nskk-deftest nskk-cache-lfu-create-test
  "LFUキャッシュが正しく作成される"
  :tags '(:unit :cache)
  (let ((cache (nskk-cache-lfu-create 100)))
    (should (nskk-cache-lfu-p cache))
    (should (= (nskk-cache-lfu-capacity cache) 100))
    (should (= (nskk-cache-lfu-size cache) 0))
    (should (= (nskk-cache-lfu-hits cache) 0))
    (should (= (nskk-cache-lfu-misses cache) 0))))

(nskk-deftest nskk-cache-lfu-put-get-test
  "LFUキャッシュのput/get操作が正しく動作する"
  :tags '(:unit :cache)
  (let ((cache (nskk-cache-lfu-create 100)))
    ;; データを追加
    (nskk-cache-lfu-put cache "key1" "value1")
    (nskk-cache-lfu-put cache "key2" "value2")
    (nskk-cache-lfu-put cache "key3" "value3")

    ;; サイズチェック
    (should (= (nskk-cache-lfu-size cache) 3))

    ;; データ取得
    (should (equal (nskk-cache-lfu-get cache "key1") "value1"))
    (should (equal (nskk-cache-lfu-get cache "key2") "value2"))
    (should (equal (nskk-cache-lfu-get cache "key3") "value3"))

    ;; 存在しないキー
    (should (null (nskk-cache-lfu-get cache "key4")))

    ;; 統計チェック
    (should (= (nskk-cache-lfu-hits cache) 3))
    (should (= (nskk-cache-lfu-misses cache) 1))))

(nskk-deftest nskk-cache-lfu-frequency-test
  "LFUキャッシュの頻度カウントが正しく動作する"
  :tags '(:unit :cache)
  (let ((cache (nskk-cache-lfu-create 100)))
    ;; データを追加
    (nskk-cache-lfu-put cache "key1" "value1")

    ;; 複数回アクセス
    (nskk-cache-lfu-get cache "key1")
    (nskk-cache-lfu-get cache "key1")
    (nskk-cache-lfu-get cache "key1")

    ;; 頻度が増加していることを確認（内部構造チェック）
    (let ((entry (gethash "key1" (nskk-cache-lfu-hash cache))))
      (should entry)
      (should (= (nskk-cache-lfu-entry-frequency entry) 4)))))

(nskk-deftest nskk-cache-lfu-eviction-test
  "LFUキャッシュの容量超過時のエビクションが正しく動作する"
  :tags '(:unit :cache)
  (let ((cache (nskk-cache-lfu-create 3)))
    ;; データを追加してアクセス頻度に差をつける
    (nskk-cache-lfu-put cache "key1" "value1")
    (nskk-cache-lfu-get cache "key1")  ; freq=2
    (nskk-cache-lfu-get cache "key1")  ; freq=3

    (nskk-cache-lfu-put cache "key2" "value2")
    (nskk-cache-lfu-get cache "key2")  ; freq=2

    (nskk-cache-lfu-put cache "key3" "value3")
    ;; key3のfreq=1

    (should (= (nskk-cache-lfu-size cache) 3))

    ;; 新しいエントリを追加（key3が削除されるはず）
    (nskk-cache-lfu-put cache "key4" "value4")
    (should (= (nskk-cache-lfu-size cache) 3))
    (should (equal (nskk-cache-lfu-get cache "key1") "value1"))
    (should (equal (nskk-cache-lfu-get cache "key2") "value2"))
    (should (null (nskk-cache-lfu-get cache "key3")))
    (should (equal (nskk-cache-lfu-get cache "key4") "value4"))))

(nskk-deftest nskk-cache-lfu-invalidate-test
  "LFUキャッシュの無効化が正しく動作する"
  :tags '(:unit :cache)
  (let ((cache (nskk-cache-lfu-create 100)))
    ;; データを追加
    (nskk-cache-lfu-put cache "key1" "value1")
    (nskk-cache-lfu-put cache "key2" "value2")
    (should (= (nskk-cache-lfu-size cache) 2))

    ;; key1を無効化
    (should (nskk-cache-lfu-invalidate cache "key1"))
    (should (= (nskk-cache-lfu-size cache) 1))
    (should (null (nskk-cache-lfu-get cache "key1")))
    (should (equal (nskk-cache-lfu-get cache "key2") "value2"))

    ;; 存在しないキーの無効化
    (should (null (nskk-cache-lfu-invalidate cache "key3")))))

(nskk-deftest nskk-cache-lfu-clear-test
  "LFUキャッシュのクリアが正しく動作する"
  :tags '(:unit :cache)
  (let ((cache (nskk-cache-lfu-create 100)))
    ;; データを追加
    (nskk-cache-lfu-put cache "key1" "value1")
    (nskk-cache-lfu-put cache "key2" "value2")
    (nskk-cache-lfu-get cache "key1")
    (should (= (nskk-cache-lfu-size cache) 2))
    (should (> (nskk-cache-lfu-hits cache) 0))

    ;; クリア
    (nskk-cache-lfu-clear cache)
    (should (= (nskk-cache-lfu-size cache) 0))
    (should (= (nskk-cache-lfu-hits cache) 0))
    (should (= (nskk-cache-lfu-misses cache) 0))
    (should (null (nskk-cache-lfu-get cache "key1")))
    (should (null (nskk-cache-lfu-get cache "key2")))))

;;; 統合インターフェーステスト

(nskk-deftest nskk-cache-create-test
  "統合インターフェースでキャッシュが正しく作成される"
  :tags '(:unit :cache)
  ;; LRUキャッシュ
  (let ((lru-cache (nskk-cache-create 'lru 100)))
    (should (nskk-cache-lru-p lru-cache))
    (should (= (nskk-cache-lru-capacity lru-cache) 100)))

  ;; LFUキャッシュ
  (let ((lfu-cache (nskk-cache-create 'lfu 200)))
    (should (nskk-cache-lfu-p lfu-cache))
    (should (= (nskk-cache-lfu-capacity lfu-cache) 200)))

  ;; デフォルト値
  (let ((default-cache (nskk-cache-create)))
    (should (nskk-cache-lru-p default-cache))
    (should (= (nskk-cache-lru-capacity default-cache) nskk-cache-default-capacity))))

(nskk-deftest nskk-cache-unified-interface-test
  "統合インターフェースが正しく動作する"
  :tags '(:unit :cache)
  (dolist (type '(lru lfu))
    (let ((cache (nskk-cache-create type 100)))
      ;; put/get
      (nskk-cache-put cache "key1" "value1")
      (nskk-cache-put cache "key2" "value2")
      (should (equal (nskk-cache-get cache "key1") "value1"))
      (should (equal (nskk-cache-get cache "key2") "value2"))

      ;; invalidate
      (should (nskk-cache-invalidate cache "key1"))
      (should (null (nskk-cache-get cache "key1")))

      ;; clear
      (nskk-cache-clear cache)
      (should (null (nskk-cache-get cache "key2"))))))

(nskk-deftest nskk-cache-stats-test
  "キャッシュ統計が正しく計算される"
  :tags '(:unit :cache)
  (let ((cache (nskk-cache-create 'lru 100)))
    ;; データを追加してアクセス
    (nskk-cache-put cache "key1" "value1")
    (nskk-cache-put cache "key2" "value2")
    (nskk-cache-get cache "key1")  ; hit
    (nskk-cache-get cache "key1")  ; hit
    (nskk-cache-get cache "key3")  ; miss

    ;; 統計取得
    (let ((stats (nskk-cache-stats cache)))
      (should (eq (plist-get stats :type) 'lru))
      (should (= (plist-get stats :capacity) 100))
      (should (= (plist-get stats :size) 2))
      (should (= (plist-get stats :hits) 2))
      (should (= (plist-get stats :misses) 1))
      (should (= (plist-get stats :hit-rate) (/ 2.0 3.0))))))

(nskk-deftest nskk-cache-invalidate-pattern-test
  "パターンマッチ無効化が正しく動作する"
  :tags '(:unit :cache)
  (let ((cache (nskk-cache-create 'lru 100)))
    ;; データを追加
    (nskk-cache-put cache "user:1" "alice")
    (nskk-cache-put cache "user:2" "bob")
    (nskk-cache-put cache "post:1" "hello")
    (nskk-cache-put cache "post:2" "world")

    ;; "user:"で始まるキーを削除
    (let ((deleted (nskk-cache-invalidate-pattern cache "^user:")))
      (should (= (length deleted) 2))
      (should (member "user:1" deleted))
      (should (member "user:2" deleted)))

    ;; user:*は削除されている
    (should (null (nskk-cache-get cache "user:1")))
    (should (null (nskk-cache-get cache "user:2")))

    ;; post:*は残っている
    (should (equal (nskk-cache-get cache "post:1") "hello"))
    (should (equal (nskk-cache-get cache "post:2") "world"))))

;;; パフォーマンステスト

(nskk-deftest nskk-cache-lru-performance-test
  "LRUキャッシュのパフォーマンスが目標を満たす"
  :tags '(:performance :cache)
  (let ((cache (nskk-cache-lru-create 10000))
        (iterations 1000)
        (start-time nil)
        (elapsed nil))

    ;; put操作のパフォーマンス
    (setq start-time (float-time))
    (dotimes (i iterations)
      (nskk-cache-lru-put cache (format "key%d" i) (format "value%d" i)))
    (setq elapsed (- (float-time) start-time))
    (should (< (/ elapsed iterations) 0.0001))  ; < 0.1ms per operation

    ;; get操作のパフォーマンス
    (setq start-time (float-time))
    (dotimes (i iterations)
      (nskk-cache-lru-get cache (format "key%d" i)))
    (setq elapsed (- (float-time) start-time))
    (should (< (/ elapsed iterations) 0.0001))))  ; < 0.1ms per operation

(nskk-deftest nskk-cache-lfu-performance-test
  "LFUキャッシュのパフォーマンスが目標を満たす"
  :tags '(:performance :cache)
  (let ((cache (nskk-cache-lfu-create 10000))
        (iterations 1000)
        (start-time nil)
        (elapsed nil))

    ;; put操作のパフォーマンス
    (setq start-time (float-time))
    (dotimes (i iterations)
      (nskk-cache-lfu-put cache (format "key%d" i) (format "value%d" i)))
    (setq elapsed (- (float-time) start-time))
    (should (< (/ elapsed iterations) 0.0001))  ; < 0.1ms per operation

    ;; get操作のパフォーマンス
    (setq start-time (float-time))
    (dotimes (i iterations)
      (nskk-cache-lfu-get cache (format "key%d" i)))
    (setq elapsed (- (float-time) start-time))
    (should (< (/ elapsed iterations) 0.0001))))  ; < 0.1ms per operation

(nskk-deftest nskk-cache-hit-rate-test
  "キャッシュヒット率が正しく計算される"
  :tags '(:unit :cache)
  (let ((cache (nskk-cache-create 'lru 100)))
    ;; データを追加
    (dotimes (i 10)
      (nskk-cache-put cache (format "key%d" i) (format "value%d" i)))

    ;; 20回アクセス（10回ヒット、10回ミス）
    (dotimes (i 20)
      (nskk-cache-get cache (format "key%d" i)))

    ;; ヒット率確認
    (let ((stats (nskk-cache-stats cache)))
      (should (= (plist-get stats :hits) 10))
      (should (= (plist-get stats :misses) 10))
      (should (= (plist-get stats :hit-rate) 0.5)))))

;;; メモリ使用量テスト

(nskk-deftest nskk-cache-memory-usage-test
  "大量データでのメモリ使用が適切"
  :tags '(:performance :cache :slow)
  (let ((cache (nskk-cache-create 'lru 10000)))
    ;; 10000エントリ追加
    (dotimes (i 10000)
      (nskk-cache-put cache (format "key%d" i) (format "value%d" i)))

    ;; サイズチェック
    (let ((stats (nskk-cache-stats cache)))
      (should (= (plist-get stats :size) 10000))
      ;; 容量を超えていない
      (should (<= (plist-get stats :size) (plist-get stats :capacity))))))

;;; エッジケーステスト

(nskk-deftest nskk-cache-capacity-one-test
  "容量1のキャッシュが正しく動作する"
  :tags '(:unit :cache)
  (let ((cache (nskk-cache-create 'lru 1)))
    (nskk-cache-put cache "key1" "value1")
    (should (equal (nskk-cache-get cache "key1") "value1"))

    ;; 2つ目を追加
    (nskk-cache-put cache "key2" "value2")
    (should (null (nskk-cache-get cache "key1")))
    (should (equal (nskk-cache-get cache "key2") "value2"))))

(nskk-deftest nskk-cache-empty-key-test
  "空文字列キーが正しく扱われる"
  :tags '(:unit :cache)
  (let ((cache (nskk-cache-create 'lru 100)))
    (nskk-cache-put cache "" "empty-key-value")
    (should (equal (nskk-cache-get cache "") "empty-key-value"))))

(nskk-deftest nskk-cache-nil-value-test
  "nil値が正しく扱われる"
  :tags '(:unit :cache)
  (let ((cache (nskk-cache-create 'lru 100)))
    (nskk-cache-put cache "key1" nil)
    ;; nil値が格納されているが、getはnilを返す
    ;; （存在チェックとnil値の区別はできない仕様）
    (should (null (nskk-cache-get cache "key1")))))

(nskk-deftest nskk-cache-complex-value-test
  "複雑なデータ構造が正しく扱われる"
  :tags '(:unit :cache)
  (let ((cache (nskk-cache-create 'lru 100))
        (complex-value '((a . 1) (b . 2) (c . (d e f)))))
    (nskk-cache-put cache "complex" complex-value)
    (should (equal (nskk-cache-get cache "complex") complex-value))))

(provide 'nskk-cache-test)

;;; nskk-cache-test.el ends here
