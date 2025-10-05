;;; nskk-conjugation-tables-test.el --- Tests for NSKK conjugation tables -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; This file is part of NSKK.

;;; Commentary:

;; 活用テーブル最適化のテスト

;;; Code:

(require 'ert)
(require 'nskk-conjugation-tables)
(require 'nskk-test-framework)

;;; テーブル検索のテスト

(ert-deftest nskk-conjugation-tables-test/lookup-verb ()
  "動詞の活用テーブル検索テスト。"
  ;; 五段活用
  (should (equal (nskk-conjugation-table-lookup "書" 'godan 'renyou) "き"))
  (should (equal (nskk-conjugation-table-lookup "書" 'godan 'mizen-nai) "か"))

  ;; 上一段活用
  (should (equal (nskk-conjugation-table-lookup "見" 'kami-ichidan 'renyou) "見"))
  (should (equal (nskk-conjugation-table-lookup "見" 'kami-ichidan 'shushi) "見る"))

  ;; 下一段活用
  (should (equal (nskk-conjugation-table-lookup "食べ" 'shimo-ichidan 'renyou) "食べ"))
  (should (equal (nskk-conjugation-table-lookup "食べ" 'shimo-ichidan 'shushi) "食べる"))

  ;; サ変活用
  (should (equal (nskk-conjugation-table-lookup "し" 'sa-hen 'shushi) "する"))

  ;; カ変活用
  (should (equal (nskk-conjugation-table-lookup "来" 'ka-hen 'shushi) "来る")))

(ert-deftest nskk-conjugation-tables-test/lookup-adjective ()
  "形容詞の活用テーブル検索テスト。"
  ;; イ形容詞
  (should (equal (nskk-conjugation-table-lookup "高" 'i-adjective 'renyou-ku) "高く"))
  (should (equal (nskk-conjugation-table-lookup "高" 'i-adjective 'renyou-katta) "高かっ"))

  ;; ナ形容詞
  (should (equal (nskk-conjugation-table-lookup "静か" 'na-adjective 'renyou-ni) "静かに"))
  (should (equal (nskk-conjugation-table-lookup "静か" 'na-adjective 'rentai-na) "静かな")))

;;; キャッシュ機能のテスト

(ert-deftest nskk-conjugation-tables-test/cache-basic ()
  "基本的なキャッシュ機能のテスト。"
  ;; キャッシュクリア
  (nskk-conjugation-clear-cache)

  ;; 最初のアクセス（キャッシュミス）
  (should (equal (nskk-conjugation-table-lookup "書" 'godan 'renyou) "き"))

  ;; 2回目のアクセス（キャッシュヒット）
  (should (equal (nskk-conjugation-table-lookup "書" 'godan 'renyou) "き"))

  ;; ヒット数を確認
  (should (> nskk-conjugation--cache-hits 0)))

(ert-deftest nskk-conjugation-tables-test/cache-eviction ()
  "キャッシュの削除機能のテスト。"
  ;; キャッシュサイズを小さく設定
  (let ((nskk-conjugation-cache-size 5))
    (nskk-conjugation-clear-cache)

    ;; 6個のエントリをキャッシュ（1個は削除されるはず）
    (dotimes (i 6)
      (nskk-conjugation-table-lookup
       (format "test%d" i) 'godan 'renyou))

    ;; キャッシュサイズが制限内であることを確認
    (should (<= (hash-table-count nskk-conjugation--cache)
                nskk-conjugation-cache-size))))

(ert-deftest nskk-conjugation-tables-test/cache-disabled ()
  "キャッシュ無効時の動作テスト。"
  (let ((nskk-conjugation-enable-cache nil))
    (nskk-conjugation-clear-cache)

    ;; キャッシュ無効でも正常に動作すること
    (should (equal (nskk-conjugation-table-lookup "書" 'godan 'renyou) "き"))
    (should (equal (nskk-conjugation-table-lookup "見" 'kami-ichidan 'renyou) "見"))))

;;; 最適化機能のテスト

(ert-deftest nskk-conjugation-tables-test/optimize ()
  "テーブル最適化機能のテスト。"
  ;; 最適化実行
  (should-not (nskk-conjugation-table-optimize))

  ;; 事前計算されたパターンがキャッシュに存在することを確認
  (should (> (hash-table-count nskk-conjugation--cache) 0))

  ;; 頻出動詞の活用が高速に取得できることを確認
  (should (equal (nskk-conjugation-table-lookup "書" 'godan 'renyou) "き"))
  (should (equal (nskk-conjugation-table-lookup "見" 'kami-ichidan 'renyou) "見")))

;;; 統計情報のテスト

(ert-deftest nskk-conjugation-tables-test/stats ()
  "キャッシュ統計情報のテスト。"
  (nskk-conjugation-clear-cache)

  ;; いくつかの活用を実行
  (dotimes (_ 10)
    (nskk-conjugation-table-lookup "書" 'godan 'renyou))

  ;; 統計情報が更新されていることを確認
  (should (> nskk-conjugation--cache-hits 0))

  ;; 統計表示がエラーなく実行できることを確認
  (should-not (nskk-conjugation-cache-stats)))

;;; メモリ使用量のテスト

(ert-deftest nskk-conjugation-tables-test/memory-usage ()
  "メモリ使用量推定のテスト。"
  ;; メモリ使用量が妥当な範囲内であることを確認
  (let ((memory (nskk-conjugation-memory-usage)))
    ;; 5MB以下であること
    (should (< memory (* 5 1024 1024)))))

;;; パフォーマンステスト

(ert-deftest nskk-conjugation-tables-test/performance-lookup ()
  "テーブル検索のパフォーマンステスト（目標: < 30ms）。"
  :tags '(:performance)
  (nskk-conjugation-table-optimize)

  (let ((start-time (current-time))
        (iterations 10000))
    ;; 10000回の検索（キャッシュありで高速化されるはず）
    (dotimes (_ iterations)
      (nskk-conjugation-table-lookup "書" 'godan 'renyou))

    (let* ((end-time (current-time))
           (elapsed-ms (* 1000.0 (float-time (time-subtract end-time start-time))))
           (per-call-ms (/ elapsed-ms iterations)))
      (message "テーブル検索: %.3f ms/call (%d回)" per-call-ms iterations)
      ;; 1回あたり0.03ms以下であること
      (should (< per-call-ms 0.03)))))

(ert-deftest nskk-conjugation-tables-test/performance-cache-hit ()
  "キャッシュヒット時のパフォーマンステスト。"
  :tags '(:performance)
  (nskk-conjugation-clear-cache)

  ;; 最初にキャッシュにデータを入れる
  (nskk-conjugation-table-lookup "書" 'godan 'renyou)

  (let ((start-time (current-time))
        (iterations 100000))
    ;; 100000回のキャッシュヒット
    (dotimes (_ iterations)
      (nskk-conjugation-table-lookup "書" 'godan 'renyou))

    (let* ((end-time (current-time))
           (elapsed-ms (* 1000.0 (float-time (time-subtract end-time start-time))))
           (per-call-ms (/ elapsed-ms iterations)))
      (message "キャッシュヒット: %.4f ms/call (%d回)" per-call-ms iterations)
      ;; キャッシュヒット時は非常に高速（0.002ms以下）
      ;; Note: カバレッジ測定時はインストルメンテーションのオーバーヘッドで遅くなる
      (should (< per-call-ms 0.002)))))

(ert-deftest nskk-conjugation-tables-test/performance-optimization ()
  "最適化処理自体のパフォーマンステスト。"
  :tags '(:performance)
  (let ((start-time (current-time)))
    (nskk-conjugation-table-optimize)
    (let* ((end-time (current-time))
           (elapsed-ms (* 1000.0 (float-time (time-subtract end-time start-time)))))
      (message "テーブル最適化: %.2f ms" elapsed-ms)
      ;; 最適化処理は30ms以内に完了すること
      (should (< elapsed-ms 30)))))

;;; 統合テスト

(ert-deftest nskk-conjugation-tables-test/integration ()
  "活用テーブルの統合テスト。"
  ;; 最適化実行
  (nskk-conjugation-table-optimize)

  ;; 様々な活用パターンをテスト
  (let ((test-cases '(;; 動詞
                     ("書" godan renyou "き")
                     ("読" godan mizen-nai "ま")
                     ("見" kami-ichidan shushi "見る")
                     ("食べ" shimo-ichidan renyou "食べ")
                     ("し" sa-hen shushi "する")
                     ("来" ka-hen shushi "来る")
                     ;; 形容詞
                     ("高" i-adjective renyou-ku "高く")
                     ("美しい" i-adjective renyou-katta "美しかっ")
                     ("静か" na-adjective renyou-ni "静かに")
                     ("元気" na-adjective rentai-na "元気な"))))
    (dolist (case test-cases)
      (let ((stem (nth 0 case))
            (type (nth 1 case))
            (form (nth 2 case))
            (expected (nth 3 case)))
        (should (equal (nskk-conjugation-table-lookup stem type form)
                       expected)))))

  ;; キャッシュヒット率を確認
  (let* ((total (+ nskk-conjugation--cache-hits nskk-conjugation--cache-misses))
         (hit-rate (if (> total 0)
                       (* 100.0 (/ (float nskk-conjugation--cache-hits) total))
                     0.0)))
    (message "キャッシュヒット率: %.1f%%" hit-rate)
    ;; 最適化後はヒット率が高いはず
    (should (> hit-rate 50.0))))

(provide 'nskk-conjugation-tables-test)

;;; nskk-conjugation-tables-test.el ends here
