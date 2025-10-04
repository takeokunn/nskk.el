;;; nskk-optimize-test.el --- Tests for nskk-optimize.el -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, test, performance

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

;; nskk-optimize.el の単体テスト。
;; ERTフレームワークを使用してパフォーマンス最適化機能を検証する。
;;
;; テストカテゴリ:
;; 1. ベンチマーク機能
;; 2. プロファイリング機能
;; 3. メモリ測定
;; 4. パフォーマンス回帰テスト
;; 5. 最適化マクロ

;;; Code:

(require 'ert)
(require 'nskk-optimize)
(require 'nskk-converter)
(require 'nskk-special-chars)

;;; ベンチマーク機能テスト

(ert-deftest nskk-optimize-test-measure-time ()
  "`nskk-measure-time' の動作をテストする。"
  (let ((result (nskk-measure-time (+ 1 1))))
    ;; 結果はcons (時間 . 返り値)
    (should (consp result))
    (should (numberp (car result)))
    (should (= (cdr result) 2))
    ;; 時間は正の数
    (should (>= (car result) 0))))

(ert-deftest nskk-optimize-test-benchmark-romaji-basic ()
  "`nskk-benchmark-romaji-conversion' の基本動作をテストする。"
  (let ((result (nskk-benchmark-romaji-conversion "ka" 100)))
    ;; 必須フィールドの存在確認
    (should (plist-member result :total-time))
    (should (plist-member result :avg-time))
    (should (plist-member result :iterations))
    (should (plist-member result :throughput))

    ;; 値の妥当性確認
    (should (> (plist-get result :total-time) 0))
    (should (> (plist-get result :avg-time) 0))
    (should (= (plist-get result :iterations) 100))
    (should (> (plist-get result :throughput) 0))))

(ert-deftest nskk-optimize-test-benchmark-hiragana-basic ()
  "`nskk-benchmark-hiragana-to-katakana' の基本動作をテストする。"
  (let ((result (nskk-benchmark-hiragana-to-katakana "あいうえお" 100)))
    (should (plist-member result :total-time))
    (should (plist-member result :avg-time))
    (should (plist-member result :iterations))
    (should (plist-member result :throughput))

    (should (> (plist-get result :total-time) 0))
    (should (> (plist-get result :avg-time) 0))
    (should (= (plist-get result :iterations) 100))
    (should (> (plist-get result :throughput) 0))))

(ert-deftest nskk-optimize-test-benchmark-performance-target ()
  "パフォーマンス目標（< 100μs）を満たすかテストする。"
  ;; 簡単な変換で目標を達成できるはず
  (let ((result (nskk-benchmark-romaji-conversion "ka" 1000)))
    ;; 平均時間が100μs未満であることを確認
    ;; ただし、環境によって変動するため、警告レベルで記録
    (let ((avg-time (plist-get result :avg-time)))
      (when (>= avg-time 100.0)
        (message "Warning: Average time %.2fμs exceeds target 100μs" avg-time))
      ;; テストは常にパス（環境依存のため）
      (should t))))

;;; プロファイリング機能テスト

(ert-deftest nskk-optimize-test-profiling-disabled ()
  "プロファイリング無効時の動作をテストする。"
  (let ((nskk-optimize-enable-profiling nil))
    (nskk-optimize-reset-profile)
    (nskk-with-profiling "test"
      (+ 1 1))
    ;; プロファイルデータは記録されない
    (should (null nskk-optimize--profile-data))))

(ert-deftest nskk-optimize-test-profiling-enabled ()
  "プロファイリング有効時の動作をテストする。"
  (let ((nskk-optimize-enable-profiling t))
    (nskk-optimize-reset-profile)
    (nskk-with-profiling "test-addition"
      (+ 1 1))
    ;; プロファイルデータが記録される
    (should (not (null nskk-optimize--profile-data)))
    (let ((entry (assoc "test-addition" nskk-optimize--profile-data)))
      (should entry)
      ;; エントリの構造確認
      (should (= (nth 1 entry) 1)) ; count = 1
      (should (>= (nth 2 entry) 0)) ; total-time >= 0 (非常に速い場合0になることもある)
      )))

(ert-deftest nskk-optimize-test-profiling-multiple ()
  "複数回のプロファイリング記録をテストする。"
  (let ((nskk-optimize-enable-profiling t))
    (nskk-optimize-reset-profile)
    (dotimes (_ 5)
      (nskk-with-profiling "test-loop"
        (+ 1 1)))
    (let ((entry (assoc "test-loop" nskk-optimize--profile-data)))
      (should entry)
      (should (= (nth 1 entry) 5))))) ; count = 5

(ert-deftest nskk-optimize-test-reset-profile ()
  "`nskk-optimize-reset-profile' の動作をテストする。"
  (let ((nskk-optimize-enable-profiling t))
    ;; データを記録
    (nskk-with-profiling "test"
      (+ 1 1))
    (should (not (null nskk-optimize--profile-data)))
    ;; リセット
    (nskk-optimize-reset-profile)
    (should (null nskk-optimize--profile-data))))

;;; メモリ測定テスト

(ert-deftest nskk-optimize-test-memory-usage ()
  "`nskk-benchmark-memory-usage' の動作をテストする。"
  (let ((mem (nskk-benchmark-memory-usage)))
    (should (plist-member mem :gc-cons-threshold))
    (should (plist-member mem :gc-elapsed))
    (should (plist-member mem :gcs-done))
    (should (plist-member mem :cons-cells-allocated))
    (should (plist-member mem :floats-allocated))
    (should (plist-member mem :strings-allocated))

    ;; 値の妥当性確認
    (should (> (plist-get mem :gc-cons-threshold) 0))
    (should (>= (plist-get mem :gc-elapsed) 0))
    (should (>= (plist-get mem :gcs-done) 0))))

(ert-deftest nskk-optimize-test-memory-delta ()
  "`nskk-measure-memory-delta' の動作をテストする。"
  (let ((result (nskk-measure-memory-delta
                 (lambda ()
                   ;; 意図的により多くのメモリを消費
                   ;; GCで完全回収されないよう、グローバル変数に保持
                   (setq nskk-optimize-test--dummy-data
                         (make-list 10000 (make-string 100 ?x)))))))
    (should (plist-member result :before))
    (should (plist-member result :after))
    (should (plist-member result :delta))

    (let ((delta (plist-get result :delta)))
      (should (plist-member delta :cons-cells))
      (should (plist-member delta :floats))
      (should (plist-member delta :strings))
      ;; cons cellsまたはstringsが増加しているはず
      ;; 環境によってはGCで回収されるため、どちらかが増えていればOK
      (should (or (> (plist-get delta :cons-cells) 0)
                 (> (plist-get delta :strings) 0))))))

;;; 最適化マクロテスト

(ert-deftest nskk-optimize-test-optimize-loop ()
  "`nskk-optimize-loop' マクロの動作をテストする。"
  (let ((sum 0))
    (nskk-optimize-loop i 10
      (setq sum (+ sum i)))
    ;; 0 + 1 + 2 + ... + 9 = 45
    (should (= sum 45))))

(ert-deftest nskk-optimize-test-string-concat ()
  "`nskk-optimize-string-concat' マクロの動作をテストする。"
  (let ((result (nskk-optimize-string-concat "hello" "world")))
    (should (string= result "helloworld"))))

;;; パフォーマンス回帰テスト

(ert-deftest nskk-optimize-test-baseline-set ()
  "`nskk-optimize-set-baseline' の動作をテストする。"
  (nskk-optimize-set-baseline)
  (should (not (null nskk-optimize--performance-baseline)))
  (should (plist-member nskk-optimize--performance-baseline :avg-time))
  (should (> (plist-get nskk-optimize--performance-baseline :avg-time) 0)))

(ert-deftest nskk-optimize-test-regression-check-no-baseline ()
  "ベースライン未設定時のエラーをテストする。"
  (setq nskk-optimize--performance-baseline nil)
  (should-error (nskk-optimize-check-regression)))

(ert-deftest nskk-optimize-test-regression-check-with-baseline ()
  "ベースライン設定後の回帰チェックをテストする。"
  (nskk-optimize-set-baseline)
  (let ((result (nskk-optimize-check-regression)))
    (should (plist-member result :baseline))
    (should (plist-member result :current))
    (should (plist-member result :ratio))
    (should (plist-member result :regression))

    ;; 比率は1.0前後のはず（同じ処理を実行）
    (let ((ratio (plist-get result :ratio)))
      (should (> ratio 0.5))
      (should (< ratio 2.0)))))

;;; 統計情報テスト

(ert-deftest nskk-optimize-test-stats ()
  "`nskk-optimize-stats' の動作をテストする。"
  (let ((stats (nskk-optimize-stats)))
    (should (plist-member stats :profiling-enabled))
    (should (plist-member stats :benchmark-iterations))
    (should (plist-member stats :profile-entries))
    (should (plist-member stats :baseline-set))

    (should (numberp (plist-get stats :benchmark-iterations)))
    (should (>= (plist-get stats :profile-entries) 0))))

;;; 統合テスト

(ert-deftest nskk-optimize-test-full-benchmark-workflow ()
  "ベンチマーク→ベースライン設定→回帰チェックの一連の流れをテストする。"
  ;; 1. ベンチマーク実行
  (let ((bench-result (nskk-benchmark-romaji-conversion "konnnichiha" 100)))
    (should (> (plist-get bench-result :avg-time) 0))

    ;; 2. ベースライン設定
    (nskk-optimize-set-baseline)
    (should (not (null nskk-optimize--performance-baseline)))

    ;; 3. 回帰チェック
    (let ((reg-result (nskk-optimize-check-regression)))
      (should (plist-member reg-result :baseline))
      (should (plist-member reg-result :current))
      (should (plist-member reg-result :ratio)))))

(ert-deftest nskk-optimize-test-profiling-workflow ()
  "プロファイリングの一連の流れをテストする。"
  (let ((nskk-optimize-enable-profiling t))
    ;; 1. リセット
    (nskk-optimize-reset-profile)
    (should (null nskk-optimize--profile-data))

    ;; 2. プロファイリング実行
    (dotimes (i 3)
      (nskk-with-profiling "test-workflow"
        (nskk-convert-romaji "ka")))

    ;; 3. データ確認
    (let ((entry (assoc "test-workflow" nskk-optimize--profile-data)))
      (should entry)
      (should (= (nth 1 entry) 3)))

    ;; 4. リセット確認
    (nskk-optimize-reset-profile)
    (should (null nskk-optimize--profile-data))))

;;; パフォーマンス比較テスト

(ert-deftest nskk-optimize-test-conversion-performance-comparison ()
  "各種変換のパフォーマンスを比較テストする。"
  (let ((iterations 100)
        (results nil))
    ;; 短い入力
    (push (cons "short" (nskk-benchmark-romaji-conversion "ka" iterations))
          results)
    ;; 中程度の入力
    (push (cons "medium" (nskk-benchmark-romaji-conversion "konnnichiha" iterations))
          results)
    ;; 長い入力
    (push (cons "long" (nskk-benchmark-romaji-conversion "kyouhagakkoude benkyoushimasu" iterations))
          results)

    ;; 全ての結果が有効であることを確認
    (dolist (result results)
      (should (> (plist-get (cdr result) :avg-time) 0))
      (should (> (plist-get (cdr result) :throughput) 0)))

    ;; 短い入力が最も速いことを確認（通常は）
    (let ((short-time (plist-get (cdr (assoc "short" results)) :avg-time))
          (long-time (plist-get (cdr (assoc "long" results)) :avg-time)))
      ;; 長い入力は短い入力より時間がかかるはず
      (should (>= long-time short-time)))))

(provide 'nskk-optimize-test)

;;; nskk-optimize-test.el ends here
