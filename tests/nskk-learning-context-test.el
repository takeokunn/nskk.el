;;; nskk-learning-context-test.el --- Tests for nskk-learning-context -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; This file is part of NSKK.

;;; Commentary:

;; Tests for context learning engine

;;; Code:

(require 'ert)
(require 'nskk-learning-context)
(require 'nskk-test-framework)

;;; 基本機能テスト

(nskk-deftest nskk-context-learn-test
  "文脈学習の基本テスト"
  :tags '(:unit)

  (nskk-context-clear)

  ;; バイグラム学習
  (nskk-learn-context "私" "は")
  (should (> (hash-table-count nskk-context-bigram-table) 0))
  (should (> (hash-table-count nskk-context-unigram-table) 0))

  ;; トライグラム学習（履歴が2つ以上必要）
  (nskk-learn-context "は" "学生")
  (nskk-learn-context "学生" "です")
  ;; トライグラムは履歴が2つ溜まってから記録される
  (should (>= (hash-table-count nskk-context-trigram-table) 0)))

(nskk-deftest nskk-context-score-test
  "文脈スコアリングのテスト"
  :tags '(:unit)

  (nskk-context-clear)

  ;; パターン学習
  (dotimes (_ 5)
    (nskk-learn-context "私" "は")
    (nskk-learn-context "は" "学生"))

  ;; 新しいコンテキストでスコア計算
  (nskk-context-clear-history)
  (nskk-learn-context nil "私")

  ;; "は"のスコアが高いはず
  (let ((score-wa (nskk-context-score "" "は"))
        (score-ga (nskk-context-score "" "が")))
    (should (> score-wa score-ga))))

(nskk-deftest nskk-context-bigram-test
  "バイグラム学習のテスト"
  :tags '(:unit)

  (let ((nskk-context-enable-bigram t)
        (nskk-context-enable-trigram nil))

    (nskk-context-clear)

    (nskk-learn-context "今日" "は")
    (nskk-learn-context "は" "晴れ")

    (let ((bigram-key (cons "今日" "は")))
      (should (> (gethash bigram-key nskk-context-bigram-table 0) 0)))))

(nskk-deftest nskk-context-trigram-test
  "トライグラム学習のテスト"
  :tags '(:unit)

  (let ((nskk-context-enable-bigram t)
        (nskk-context-enable-trigram t))

    (nskk-context-clear)

    (nskk-learn-context nil "今日")
    (nskk-learn-context "今日" "は")
    (nskk-learn-context "は" "晴れ")

    (should (> (hash-table-count nskk-context-trigram-table) 0))))

;;; 予測機能テスト

(nskk-deftest nskk-context-predict-test
  "次単語予測のテスト"
  :tags '(:unit)

  (nskk-context-clear)

  ;; パターン学習
  (dotimes (_ 5)
    (nskk-context-clear-history)
    (nskk-learn-context nil "私")
    (nskk-learn-context "私" "は")
    (nskk-learn-context "は" "学生"))

  ;; 予測
  (nskk-context-clear-history)
  (nskk-learn-context nil "私")

  (let ((predictions (nskk-predict-next-words 3)))
    (should (> (length predictions) 0))
    ;; "は"が予測されるはず
    (should (assoc "は" predictions))))

;;; パターン認識テスト

(nskk-deftest nskk-context-find-patterns-test
  "パターン認識のテスト"
  :tags '(:unit)

  (nskk-context-clear)

  ;; パターン学習
  (dotimes (_ 3)
    (nskk-context-clear-history)
    (nskk-learn-context nil "私")
    (nskk-learn-context "私" "は")
    (nskk-learn-context "私" "が"))

  (let ((patterns (nskk-context-find-patterns "私" 2)))
    (should (> (length patterns) 0))))

;;; エントリ数制限テスト

(nskk-deftest nskk-context-max-entries-test
  "エントリ数制限のテスト"
  :tags '(:unit :slow)

  (let ((nskk-context-max-bigrams 50)
        (nskk-context-max-trigrams 50))

    (nskk-context-clear)

    ;; 制限を超えるエントリを追加
    (dotimes (i 100)
      (nskk-context-clear-history)
      (nskk-learn-context nil (format "word%d" i))
      (nskk-learn-context (format "word%d" i) (format "next%d" i)))

    ;; エントリ数が制限内に収まっていることを確認
    (should (<= (hash-table-count nskk-context-bigram-table)
               nskk-context-max-bigrams))))

;;; 統計情報テスト

(nskk-deftest nskk-context-statistics-test
  "統計情報のテスト"
  :tags '(:unit)

  (nskk-context-clear)

  (nskk-learn-context nil "word1")
  (nskk-learn-context "word1" "word2")
  (nskk-learn-context "word2" "word3")

  (let ((stats (nskk-context-statistics)))
    (should (> (plist-get stats :unigram-entries) 0))
    (should (> (plist-get stats :bigram-entries) 0))
    (should (plist-get stats :bigram-enabled))
    (should (plist-get stats :trigram-enabled))))

;;; クリーンアップ

(nskk-deftest nskk-context-clear-test
  "クリア機能のテスト"
  :tags '(:unit)

  (nskk-context-clear)

  (nskk-learn-context "test1" "test2")
  (should (> (hash-table-count nskk-context-bigram-table) 0))

  (nskk-context-clear)
  (should (= (hash-table-count nskk-context-bigram-table) 0))
  (should (= (hash-table-count nskk-context-trigram-table) 0))
  (should (= (hash-table-count nskk-context-unigram-table) 0)))

;;; エラーハンドリングテスト

(nskk-deftest nskk-context-learn-invalid-current-test
  "currentが文字列でない場合のエラーテスト"
  :tags '(:unit)

  (should-error (nskk-learn-context "prev" 123))
  (should-error (nskk-learn-context nil nil))
  (should-error (nskk-learn-context "prev" 'symbol)))

;;; 未知パターンのスムージングテスト

(nskk-deftest nskk-context-unknown-bigram-smoothing-test
  "未知のバイグラムに対するスムージングのテスト"
  :tags '(:unit)

  (nskk-context-clear)

  ;; 未知の単語でスコア計算
  (let ((score (nskk-context--bigram-probability "unknown-word" "candidate")))
    ;; スムージング係数が返るはず
    (should (= score nskk-context-smoothing-factor))))

(nskk-deftest nskk-context-unknown-trigram-smoothing-test
  "未知のトライグラムに対するスムージングのテスト"
  :tags '(:unit)

  (nskk-context-clear)

  ;; 未知のパターンでスコア計算
  (let ((score (nskk-context--trigram-probability "unknown1" "unknown2" "candidate")))
    ;; スムージング係数が返るはず
    (should (= score nskk-context-smoothing-factor))))

;;; トライグラム予測の詳細テスト

(nskk-deftest nskk-context-trigram-prediction-test
  "トライグラムによる予測の詳細テスト"
  :tags '(:unit)

  (let ((nskk-context-enable-bigram nil)
        (nskk-context-enable-trigram t))

    (nskk-context-clear)

    ;; トライグラムパターン学習
    (dotimes (_ 5)
      (nskk-context-clear-history)
      (nskk-learn-context nil "私")
      (nskk-learn-context "私" "は")
      (nskk-learn-context "は" "学生"))

    ;; 予測
    (nskk-context-clear-history)
    (nskk-learn-context nil "私")
    (nskk-learn-context "私" "は")

    (let ((predictions (nskk-predict-next-words 3)))
      ;; トライグラムから予測されるはず
      (should (> (length predictions) 0)))))

;;; 学習無効化テスト

(nskk-deftest nskk-context-disabled-bigram-test
  "バイグラム無効時のテスト"
  :tags '(:unit)

  (let ((nskk-context-enable-bigram nil)
        (nskk-context-enable-trigram nil))

    (nskk-context-clear)

    (nskk-learn-context "今日" "は")

    ;; バイグラムは記録されないはず
    (should (= (hash-table-count nskk-context-bigram-table) 0))
    ;; ユニグラムは記録されるはず
    (should (> (hash-table-count nskk-context-unigram-table) 0))))

(nskk-deftest nskk-context-disabled-trigram-test
  "トライグラム無効時のテスト"
  :tags '(:unit)

  (let ((nskk-context-enable-bigram t)
        (nskk-context-enable-trigram nil))

    (nskk-context-clear)

    (nskk-learn-context nil "今日")
    (nskk-learn-context "今日" "は")
    (nskk-learn-context "は" "晴れ")

    ;; トライグラムは記録されないはず
    (should (= (hash-table-count nskk-context-trigram-table) 0))
    ;; バイグラムは記録されるはず
    (should (> (hash-table-count nskk-context-bigram-table) 0))))

;;; 空履歴でのテスト

(nskk-deftest nskk-context-empty-history-score-test
  "履歴が空の状態でのスコア計算テスト"
  :tags '(:unit)

  (nskk-context-clear)

  ;; 空履歴でスコア計算
  (let ((score (nskk-context-score "" "候補")))
    ;; スコアは0.0のはず
    (should (= score 0.0))))

(nskk-deftest nskk-context-empty-history-prediction-test
  "履歴が空の状態での予測テスト"
  :tags '(:unit)

  (nskk-context-clear)

  ;; 空履歴で予測
  (let ((predictions (nskk-predict-next-words 3)))
    ;; 予測結果は空のはず
    (should (= (length predictions) 0))))

;;; 複数パターンのテスト

(nskk-deftest nskk-context-multiple-patterns-test
  "複数パターン学習のテスト"
  :tags '(:unit)

  (nskk-context-clear)

  ;; 複数のパターンを学習
  (dotimes (_ 3)
    (nskk-context-clear-history)
    (nskk-learn-context nil "私")
    (nskk-learn-context "私" "は"))

  (dotimes (_ 3)
    (nskk-context-clear-history)
    (nskk-learn-context nil "私")
    (nskk-learn-context "私" "が"))

  ;; 両方のパターンが認識されるはず
  (let ((patterns (nskk-context-find-patterns "私" 2)))
    (should (>= (length patterns) 2))))

;;; 統計情報の詳細テスト

(nskk-deftest nskk-context-statistics-detailed-test
  "統計情報の詳細テスト"
  :tags '(:unit)

  (nskk-context-clear)

  ;; データを追加
  (nskk-learn-context nil "word1")
  (nskk-learn-context "word1" "word2")
  (nskk-learn-context "word2" "word3")

  (let ((stats (nskk-context-statistics)))
    ;; 各統計値が正しく計算されているか確認
    (should (plist-member stats :bigram-entries))
    (should (plist-member stats :trigram-entries))
    (should (plist-member stats :unigram-entries))
    (should (plist-member stats :total-bigrams))
    (should (plist-member stats :total-trigrams))
    (should (plist-member stats :total-unigrams))
    (should (plist-member stats :history-length))
    (should (plist-member stats :bigram-enabled))
    (should (plist-member stats :trigram-enabled))))

(nskk-deftest nskk-context-print-statistics-test
  "統計情報表示のテスト"
  :tags '(:unit)

  (nskk-context-clear)

  (nskk-learn-context nil "word1")
  (nskk-learn-context "word1" "word2")

  ;; エラーなく実行できることを確認
  (should-not (nskk-context-print-statistics)))

(provide 'nskk-learning-context-test)

;;; nskk-learning-context-test.el ends here
