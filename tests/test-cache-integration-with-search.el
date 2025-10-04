;;; test-cache-integration-with-search.el --- Cache integration with search -*- lexical-binding: t; -*-

;; このファイルは、nskk-cache.elがnskk-search.elと統合される
;; 将来の実装を想定したデモンストレーションです。

(require 'nskk-cache)

(message "=== Cache Integration with Search Demo ===")
(message "")

;; 辞書検索結果のキャッシュデモ
(message "1. Dictionary Search Result Caching Demo")
(let ((search-cache (nskk-cache-create 'lru 1000)))

  ;; 模擬辞書検索関数（実際のnskk-searchとの統合を想定）
  (defun demo-search-with-cache (cache query)
    "キャッシュを使用した辞書検索のデモ。"
    (or (nskk-cache-get cache query)
        (let ((result (list (format "漢字%s" query)
                            (format "感じ%s" query)
                            (format "幹事%s" query))))
          (nskk-cache-put cache query result)
          result)))

  ;; 検索実行
  (message "   Searching for 'かんじ' (first time - cache miss)...")
  (let ((start (float-time)))
    (demo-search-with-cache search-cache "かんじ")
    (message "   Time: %.6f ms" (* (- (float-time) start) 1000)))

  (message "   Searching for 'かんじ' (second time - cache hit)...")
  (let ((start (float-time)))
    (demo-search-with-cache search-cache "かんじ")
    (message "   Time: %.6f ms" (* (- (float-time) start) 1000)))

  (let ((stats (nskk-cache-stats search-cache)))
    (message "   Cache Stats: Hits=%d, Misses=%d, Hit Rate=%.2f%%"
             (plist-get stats :hits)
             (plist-get stats :misses)
             (* (plist-get stats :hit-rate) 100))))

(message "")

;; 頻度ベースキャッシュデモ
(message "2. Frequency-based Cache Demo (LFU)")
(let ((freq-cache (nskk-cache-create 'lfu 5)))

  ;; よく使われる単語を頻繁に検索
  (message "   Simulating frequent searches...")
  (dotimes (_ 10) (nskk-cache-put freq-cache "かんじ" "漢字"))
  (dotimes (_ 10) (nskk-cache-get freq-cache "かんじ"))

  (dotimes (_ 5) (nskk-cache-put freq-cache "ひらがな" "平仮名"))
  (dotimes (_ 5) (nskk-cache-get freq-cache "ひらがな"))

  (dotimes (_ 2) (nskk-cache-put freq-cache "カタカナ" "片仮名"))
  (dotimes (_ 2) (nskk-cache-get freq-cache "カタカナ"))

  ;; 低頻度の単語
  (nskk-cache-put freq-cache "あ" "亜")
  (nskk-cache-put freq-cache "い" "以")
  (nskk-cache-put freq-cache "う" "宇")

  (message "   Cache size: %d" (plist-get (nskk-cache-stats freq-cache) :size))

  ;; 新しい単語を追加（低頻度の単語が削除されるはず）
  (nskk-cache-put freq-cache "え" "江")

  (message "   After adding new entry:")
  (message "     'かんじ' (high freq) exists: %s"
           (not (null (nskk-cache-get freq-cache "かんじ"))))
  (message "     'あ' (low freq) evicted: %s"
           (null (nskk-cache-get freq-cache "あ"))))

(message "")

;; キャッシュ無効化デモ
(message "3. Cache Invalidation Demo")
(let ((dict-cache (nskk-cache-create 'lru 100)))

  ;; 複数の辞書エントリをキャッシュ
  (nskk-cache-put dict-cache "かんじ:1" "漢字")
  (nskk-cache-put dict-cache "かんじ:2" "感じ")
  (nskk-cache-put dict-cache "ひらがな:1" "平仮名")
  (nskk-cache-put dict-cache "ひらがな:2" "平假名")
  (nskk-cache-put dict-cache "カタカナ:1" "片仮名")

  (message "   Total entries: %d" (plist-get (nskk-cache-stats dict-cache) :size))

  ;; パターンマッチングで特定のエントリを無効化
  (let ((deleted (nskk-cache-invalidate-pattern dict-cache "^かんじ:")))
    (message "   Invalidated %d entries matching '^かんじ:'" (length deleted)))

  (message "   Remaining entries: %d" (plist-get (nskk-cache-stats dict-cache) :size))
  (message "   'かんじ:1' exists: %s" (not (null (nskk-cache-get dict-cache "かんじ:1"))))
  (message "   'ひらがな:1' exists: %s" (not (null (nskk-cache-get dict-cache "ひらがな:1")))))

(message "")

;; パフォーマンス比較デモ
(message "4. Performance Comparison: Cache vs No Cache")
(let ((cache (nskk-cache-create 'lru 1000))
      (test-data '("かんじ" "ひらがな" "カタカナ" "えいご" "にほんご")))

  ;; キャッシュなしのシミュレーション（毎回計算）
  (message "   Without cache (100 searches):")
  (let ((start (float-time)))
    (dotimes (_ 100)
      (dolist (query test-data)
        ;; 重い処理をシミュレート
        (length (concat query query query))))
    (message "     Time: %.6f ms" (* (- (float-time) start) 1000)))

  ;; キャッシュありのシミュレーション
  (message "   With cache (100 searches):")
  ;; まずキャッシュに格納
  (dolist (query test-data)
    (nskk-cache-put cache query (concat query query query)))

  (let ((start (float-time)))
    (dotimes (_ 100)
      (dolist (query test-data)
        (nskk-cache-get cache query)))
    (message "     Time: %.6f ms" (* (- (float-time) start) 1000)))

  (let ((stats (nskk-cache-stats cache)))
    (message "   Cache Hit Rate: %.2f%%" (* (plist-get stats :hit-rate) 100))))

(message "")
(message "=== Integration Demo Completed Successfully ===")
(message "")
(message "Note: This demonstrates how nskk-cache.el will integrate with nskk-search.el")
(message "      in future implementations to provide significant performance improvements.")

;;; test-cache-integration-with-search.el ends here
