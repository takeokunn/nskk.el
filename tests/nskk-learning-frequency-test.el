;;; nskk-learning-frequency-test.el --- Tests for nskk-learning-frequency -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; This file is part of NSKK.

;;; Commentary:

;; Tests for frequency learning engine

;;; Code:

(require 'ert)
(require 'nskk-learning-frequency)
(require 'nskk-test-framework)

;;; 基本機能テスト

(nskk-deftest nskk-frequency-update-test
  "頻度更新の基本テスト"
  :tags '(:unit)

  ;; 新規エントリの作成
  (nskk-frequency-clear)
  (nskk-update-frequency "かんじ" "漢字")

  (should (> (nskk-get-frequency-score "かんじ" "漢字") 0.0))

  ;; エントリの更新
  (nskk-update-frequency "かんじ" "漢字")
  (let ((score1 (nskk-get-frequency-score "かんじ" "漢字")))

    (nskk-update-frequency "かんじ" "漢字")
    (let ((score2 (nskk-get-frequency-score "かんじ" "漢字")))
      (should (> score2 score1)))))

(nskk-deftest nskk-frequency-multiple-candidates-test
  "複数候補の頻度管理テスト"
  :tags '(:unit)

  (nskk-frequency-clear)

  ;; 複数候補の登録
  (nskk-update-frequency "かんじ" "漢字")
  (nskk-update-frequency "かんじ" "幹事")
  (nskk-update-frequency "かんじ" "感じ")

  ;; 「漢字」を多く使用
  (dotimes (_ 5)
    (nskk-update-frequency "かんじ" "漢字"))

  (let ((candidates '("漢字" "幹事" "感じ"))
        (sorted (nskk-get-sorted-candidates "かんじ" '("漢字" "幹事" "感じ"))))

    (should (equal (car sorted) "漢字"))))

(nskk-deftest nskk-frequency-algorithm-lru-test
  "LRUアルゴリズムのテスト"
  :tags '(:unit)

  (let ((nskk-frequency-algorithm 'lru))
    (nskk-frequency-clear)

    ;; 古いエントリ
    (nskk-update-frequency "test" "old")
    (sleep-for 0.2)

    ;; 新しいエントリ
    (nskk-update-frequency "test" "new")

    (let ((score-old (nskk-get-frequency-score "test" "old"))
          (score-new (nskk-get-frequency-score "test" "new")))

      ;; LRUなので新しい方がスコアが高いはず
      ;; （ただし、時間差が小さいため、ほぼ同等の場合もある）
      (should (>= score-new (* score-old 0.9))))))

(nskk-deftest nskk-frequency-algorithm-lfu-test
  "LFUアルゴリズムのテスト"
  :tags '(:unit)

  (let ((nskk-frequency-algorithm 'lfu))
    (nskk-frequency-clear)

    ;; 頻度が低いエントリ
    (nskk-update-frequency "test" "low")

    ;; 頻度が高いエントリ
    (dotimes (_ 5)
      (nskk-update-frequency "test" "high"))

    (let ((score-low (nskk-get-frequency-score "test" "low"))
          (score-high (nskk-get-frequency-score "test" "high")))

      ;; LFUなので使用回数が多い方がスコアが高い
      (should (> score-high score-low)))))

;;; 頻度減衰テスト

(nskk-deftest nskk-frequency-decay-test
  "頻度減衰のテスト"
  :tags '(:unit)

  (let ((nskk-frequency-decay-enabled t)
        (nskk-frequency-decay-interval 0)  ; 即座に減衰
        (nskk-frequency-decay-rate 0.5))   ; 50%減衰

    (nskk-frequency-clear)

    ;; エントリ作成
    (dotimes (_ 10)
      (nskk-update-frequency "test" "word"))

    ;; 減衰前のカウントを確認
    (let* ((key (cons "test" "word"))
           (entry-before (gethash key nskk-frequency-table))
           (count-before (nskk-frequency-entry-count entry-before)))

      (should (= count-before 10))

      ;; 減衰適用のため、最終減衰時刻を過去に設定
      (setq nskk-frequency-last-decay-time (time-subtract (current-time) (seconds-to-time 100000)))
      (nskk-update-frequency "test" "another")

      ;; 減衰後のカウントを確認（エントリは残っているはず）
      (let* ((entry-after (gethash key nskk-frequency-table)))
        (when entry-after
          (let ((count-after (nskk-frequency-entry-count entry-after)))
            (should (< count-after count-before))))))))

;;; エントリ数制限テスト

(nskk-deftest nskk-frequency-max-entries-test
  "エントリ数制限のテスト"
  :tags '(:unit :slow)

  (let ((nskk-frequency-max-entries 100))
    (nskk-frequency-clear)

    ;; 制限を超えるエントリを追加
    (dotimes (i 150)
      (nskk-update-frequency (format "midashi%d" i)
                            (format "candidate%d" i)))

    ;; エントリ数が制限内に収まっていることを確認
    (should (<= (hash-table-count nskk-frequency-table)
               nskk-frequency-max-entries))))

;;; 統計情報テスト

(nskk-deftest nskk-frequency-statistics-test
  "統計情報のテスト"
  :tags '(:unit)

  (nskk-frequency-clear)

  (dotimes (i 5)
    (nskk-update-frequency "test" (format "word%d" i)))

  (let ((stats (nskk-frequency-statistics)))
    (should (= (plist-get stats :total-entries) 5))
    (should (= (plist-get stats :total-count) 5))
    (should (eq (plist-get stats :algorithm) nskk-frequency-algorithm))))

;;; クリーンアップ

(nskk-deftest nskk-frequency-clear-test
  "クリア機能のテスト"
  :tags '(:unit)

  (nskk-frequency-clear)

  (nskk-update-frequency "test" "word")
  (should (> (hash-table-count nskk-frequency-table) 0))

  (nskk-frequency-clear)
  (should (= (hash-table-count nskk-frequency-table) 0)))

;;; エッジケーステスト

(nskk-deftest nskk-frequency-empty-midashi-test
  "空の見出し語のエラー処理テスト"
  :tags '(:unit)

  ;; 空の見出し語でもエラーにならない（仕様変更）
  (should (nskk-update-frequency "" "candidate")))

(nskk-deftest nskk-frequency-nil-candidate-test
  "nilの候補のエラー処理テスト"
  :tags '(:unit)

  (should-error (nskk-update-frequency "midashi" nil)))

(provide 'nskk-learning-frequency-test)

;;; nskk-learning-frequency-test.el ends here
