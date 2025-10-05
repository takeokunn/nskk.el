;;; nskk-ai-test.el --- Comprehensive tests for NSKK AI modules -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, ai, testing
;; Version: 1.0.0
;; Package-Requires: ((emacs "30.0"))

;; This file is part of NSKK.

;;; Commentary:

;; このファイルはNSKK AIモジュールの包括的なテストスイートです。
;;
;; テスト対象:
;; - nskk-ai-context.el (文脈理解)
;; - nskk-ai-pattern.el (パターン認識)
;; - nskk-ai-candidates.el (スマート候補生成)
;; - nskk-ai-learning.el (高度な学習アルゴリズム)
;;
;; テストカバレッジ:
;; - 単体テスト (100+)
;; - 統合テスト
;; - パフォーマンステスト
;; - 精度テスト
;; - メモリリークテスト

;;; Code:

(require 'ert)
(require 'nskk-test-framework)
(require 'nskk-ai-context)
(require 'nskk-ai-pattern)
(require 'nskk-ai-candidates)
(require 'nskk-ai-learning)

;;; nskk-ai-context.el のテスト

(nskk-deftest nskk-ai-context-parse-sentence-test
  "文章解析のテスト"
  :tags '(:unit :ai :context)
  (let ((text "今日は良い天気です。"))
    (let ((words (nskk-ai-context-parse-sentence text)))
      (should (listp words))
      (should (> (length words) 0))
      (should (member "今" words))
      (should (member "日" words))
      (should (member "。" words))))

  ;; 英数字混在
  (let ((text "Hello123世界"))
    (let ((words (nskk-ai-context-parse-sentence text)))
      (should (member "Hello123" words))
      (should (member "世" words))
      (should (member "界" words))))

  ;; 空文字列
  (should (null (nskk-ai-context-parse-sentence "")))
  (should (null (nskk-ai-context-parse-sentence nil))))

(nskk-deftest nskk-ai-context-learn-text-test
  "テキスト学習のテスト"
  :tags '(:unit :ai :context)
  (nskk-ai-context-clear)

  (nskk-ai-context-learn-text "私は学生です")
  (let ((stats (nskk-ai-context-statistics)))
    (should (> (plist-get stats :bigram-count) 0))
    (should (> (plist-get stats :tfidf-terms) 0)))

  ;; 複数回学習
  (nskk-ai-context-learn-text "私は社会人です")
  (nskk-ai-context-learn-text "私は日本人です")
  (let ((stats (nskk-ai-context-statistics)))
    (should (> (plist-get stats :bigram-count) 3))
    (should (>= (plist-get stats :vocab-size) 5))))

(nskk-deftest nskk-ai-context-score-candidate-test
  "文脈スコアリングのテスト"
  :tags '(:unit :ai :context)
  (nskk-ai-context-clear)

  ;; 学習データを準備
  (nskk-ai-context-learn-text "今日は良い天気です")
  (nskk-ai-context-learn-text "明日は良い天気でしょう")

  ;; スコア計算
  (let ((score (nskk-ai-context-score-candidate "てんき" "天気")))
    (should (numberp score))
    (should (>= score 0.0))
    (should (<= score 1.0))))

(nskk-deftest nskk-ai-context-calc-tfidf-test
  "TF-IDFスコア計算のテスト"
  :tags '(:unit :ai :context)
  (nskk-ai-context-clear)

  ;; 複数文書を学習
  (nskk-ai-context-learn-text "機械学習は面白い")
  (nskk-ai-context-learn-text "深層学習も面白い")
  (nskk-ai-context-learn-text "自然言語処理は重要だ")

  (let* ((words '("学" "習"))
         (score1 (nskk-ai-context-calc-tfidf-score "学" words))
         (score2 (nskk-ai-context-calc-tfidf-score "習" words)))
    (should (numberp score1))
    (should (numberp score2))
    (should (>= score1 0.0))
    (should (>= score2 0.0))))

(nskk-deftest nskk-ai-context-similarity-test
  "意味的類似度計算のテスト"
  :tags '(:unit :ai :context)
  (nskk-ai-context-clear)

  ;; 共起データを学習
  (nskk-ai-context-learn-text "機械学習と深層学習")
  (nskk-ai-context-learn-text "機械学習と統計学習")
  (nskk-ai-context-learn-text "深層学習とニューラルネットワーク")

  (let ((sim (nskk-ai-context-calc-similarity "機" "深")))
    (should (numberp sim))
    (should (>= sim 0.0))
    (should (<= sim 1.0))))

(nskk-deftest nskk-ai-context-extract-topics-test
  "トピック抽出のテスト"
  :tags '(:unit :ai :context)
  (nskk-ai-context-clear)

  ;; 文脈を設定
  (nskk-ai-context-learn-text "自然言語処理は機械学習の重要な応用分野です")

  (let ((topics (nskk-ai-context-extract-topics 3)))
    (should (listp topics))
    (should (<= (length topics) 3))
    (dolist (topic topics)
      (should (consp topic))
      (should (stringp (car topic)))
      (should (numberp (cdr topic))))))

(nskk-deftest nskk-ai-context-understand-meaning-test
  "意味理解のテスト"
  :tags '(:unit :ai :context)
  (let ((result (nskk-ai-context-understand-meaning "今日は良い天気ですね")))
    (should (plistp result))
    (should (stringp (plist-get result :text)))
    (should (numberp (plist-get result :word-count)))
    (should (> (plist-get result :word-count) 0))
    (should (listp (plist-get result :topics)))))

;;; nskk-ai-pattern.el のテスト

(nskk-deftest nskk-ai-pattern-learn-test
  "パターン学習のテスト"
  :tags '(:unit :ai :pattern)
  (nskk-ai-pattern-clear)

  (nskk-ai-pattern-learn "かんじ" "漢字" '("これは"))
  (nskk-ai-pattern-learn "かんじ" "感じ" '("良い"))
  (nskk-ai-pattern-learn "かんじ" "漢字" '("この"))

  (let ((stats (nskk-ai-pattern-statistics)))
    (should (> (plist-get stats :pattern-history-count) 0))
    (should (> (plist-get stats :user-patterns-count) 0))))

(nskk-deftest nskk-ai-pattern-extract-conversion-patterns-test
  "変換パターン抽出のテスト"
  :tags '(:unit :ai :pattern)
  (nskk-ai-pattern-clear)

  ;; 複数回学習
  (dotimes (i 5)
    (nskk-ai-pattern-learn "てすと" "テスト" '("実行")))

  (let ((patterns (nskk-ai-pattern-extract-conversion-patterns 3)))
    (should (listp patterns))
    (should (> (length patterns) 0))
    (let ((pattern (car patterns)))
      (should (consp pattern))
      (should (stringp (car pattern)))
      (should (numberp (cdr pattern)))
      (should (>= (cdr pattern) 3)))))

(nskk-deftest nskk-ai-pattern-find-similar-patterns-test
  "類似パターン検索のテスト"
  :tags '(:unit :ai :pattern)
  (nskk-ai-pattern-clear)

  (nskk-ai-pattern-learn "がくせい" "学生" '("私は"))
  (nskk-ai-pattern-learn "せんせい" "先生" '("私の"))
  (nskk-ai-pattern-learn "かいしゃいん" "会社員" '("彼は"))

  (let ((similar (nskk-ai-pattern-find-similar-patterns "がくせい" "学生")))
    (should (listp similar))))

(nskk-deftest nskk-ai-pattern-time-series-test
  "時系列分析のテスト"
  :tags '(:unit :ai :pattern)
  (nskk-ai-pattern-clear)

  (nskk-ai-pattern-learn "てすと" "テスト" nil)
  (sleep-for 0.1)
  (nskk-ai-pattern-learn "てすと" "テスト" nil)

  (let ((analysis (nskk-ai-pattern-analyze-time-series "てすと" "テスト")))
    (should (plistp analysis))
    (should (numberp (plist-get analysis :count)))
    (should (>= (plist-get analysis :count) 2))))

(nskk-deftest nskk-ai-pattern-cluster-test
  "クラスタリングのテスト"
  :tags '(:unit :ai :pattern :slow)
  (nskk-ai-pattern-clear)

  ;; 十分なデータを学習（クラスタ数以上）
  (dotimes (i 15)
    (nskk-ai-pattern-learn (format "test%d" i)
                          (format "テスト%d" i)
                          (list (format "context%d" i))))

  ;; クラスタリングを実行（データが十分にある場合は成功する）
  (let ((clusters (nskk-ai-pattern-cluster-patterns)))
    (should (listp clusters))
    (should (> (length clusters) 0))))

(nskk-deftest nskk-ai-pattern-predict-test
  "候補予測のテスト"
  :tags '(:unit :ai :pattern)
  (nskk-ai-pattern-clear)

  (dotimes (i 3)
    (nskk-ai-pattern-learn "かんじ" "漢字" '("この")))

  (let ((predictions (nskk-ai-pattern-predict-candidate "かんじ" '("この"))))
    (should (listp predictions))))

;;; nskk-ai-candidates.el のテスト

(nskk-deftest nskk-ai-candidates-generate-test
  "候補生成のテスト"
  :tags '(:unit :ai :candidates)
  (nskk-ai-candidates-clear)

  (let ((candidates (nskk-ai-candidates-generate "かんじ" '("漢字" "感じ" "幹事"))))
    (should (listp candidates))
    (should (> (length candidates) 0))
    (should (<= (length candidates) nskk-ai-candidates-max-candidates))))

(nskk-deftest nskk-ai-candidates-learn-selection-test
  "選択学習のテスト"
  :tags '(:unit :ai :candidates)
  (nskk-ai-candidates-clear)

  (let ((all-candidates '("漢字" "感じ" "幹事")))
    (nskk-ai-candidates-learn-selection "かんじ" "漢字" all-candidates)

    (let ((stats (nskk-ai-candidates-statistics)))
      (should (> (plist-get stats :feedback-count) 0)))))

(nskk-deftest nskk-ai-candidates-merge-sources-test
  "候補統合のテスト"
  :tags '(:unit :ai :candidates)
  (let* ((dict-cands '("漢字" "感じ"))
         (ai-cands '("幹事" "感じ"))
         (merged (nskk-ai-candidates-merge-sources dict-cands ai-cands)))
    (should (listp merged))
    (should (member "漢字" merged))
    (should (member "感じ" merged))
    (should (member "幹事" merged))))

(nskk-deftest nskk-ai-candidates-cache-test
  "キャッシュ機能のテスト"
  :tags '(:unit :ai :candidates)
  (nskk-ai-candidates-clear)

  ;; 初回生成（キャッシュミス）
  (let ((cands1 (nskk-ai-candidates-generate "てすと" '("テスト"))))
    (should (listp cands1)))

  (let ((stats (nskk-ai-candidates-statistics)))
    (should (= (plist-get stats :cache-misses) 1))
    (should (= (plist-get stats :cache-hits) 0)))

  ;; 2回目（キャッシュヒット）
  (let ((cands2 (nskk-ai-candidates-generate "てすと" '("テスト"))))
    (should (listp cands2)))

  (let ((stats (nskk-ai-candidates-statistics)))
    (should (= (plist-get stats :cache-hits) 1))))

(nskk-deftest nskk-ai-candidates-accuracy-test
  "精度測定のテスト"
  :tags '(:unit :ai :candidates)
  (nskk-ai-candidates-clear)

  ;; いくつかのフィードバックを生成
  (dotimes (i 5)
    (nskk-ai-candidates-learn-selection "test" "候補1" '("候補1" "候補2" "候補3")))

  (let ((accuracy (nskk-ai-candidates-measure-accuracy 10)))
    (should (plistp accuracy))
    (should (numberp (plist-get accuracy :top1-accuracy)))
    (should (numberp (plist-get accuracy :top3-accuracy)))))

;;; nskk-ai-learning.el のテスト

(nskk-deftest nskk-ai-learning-online-update-test
  "オンライン学習のテスト"
  :tags '(:unit :ai :learning)
  (nskk-ai-learning-reset-model)

  (let ((features (vector 1.0 0.5 0.3 0.8)))
    (nskk-ai-learning-online-update features "候補1")

    (let ((stats (nskk-ai-learning-statistics)))
      (should (> (plist-get stats :total-updates) 0)))))

(nskk-deftest nskk-ai-learning-batch-training-test
  "バッチ学習のテスト"
  :tags '(:unit :ai :learning)
  (nskk-ai-learning-reset-model)

  ;; バッチサイズ分のサンプルを追加
  (dotimes (i 32)
    (let ((features (vector (random 100) (random 100) (random 100))))
      (nskk-ai-learning-online-update features (format "候補%d" i))))

  (let ((stats (nskk-ai-learning-statistics)))
    (should (> (plist-get stats :training-steps) 0))))

(nskk-deftest nskk-ai-learning-compress-model-test
  "モデル圧縮のテスト"
  :tags '(:unit :ai :learning)
  (nskk-ai-learning-reset-model)

  (let ((compressed (nskk-ai-learning-compress-model)))
    (should (nskk-ai-learning-model-p compressed))
    (should (vectorp (nskk-ai-learning-model-weights compressed)))
    (should (< (length (nskk-ai-learning-model-weights compressed))
              (length (nskk-ai-learning-model-weights nskk-ai-learning-model))))))

(nskk-deftest nskk-ai-learning-transfer-test
  "転移学習のテスト"
  :tags '(:unit :ai :learning)
  (let ((knowledge (nskk-ai-learning-transfer-from-domain
                   "general" "technical")))
    (should (nskk-ai-transfer-knowledge-p knowledge))
    (should (equal (nskk-ai-transfer-knowledge-source-domain knowledge) "general"))
    (should (equal (nskk-ai-transfer-knowledge-target-domain knowledge) "technical"))))

(nskk-deftest nskk-ai-learning-evaluate-test
  "モデル評価のテスト"
  :tags '(:unit :ai :learning)
  (nskk-ai-learning-reset-model)

  ;; テストサンプルを作成
  (let ((test-samples
         (list (nskk-ai-learning-sample--create
                :features (vector 1.0 0.5 0.3)
                :label "候補1")
               (nskk-ai-learning-sample--create
                :features (vector 0.2 0.8 0.6)
                :label "候補2"))))

    (let ((eval-result (nskk-ai-learning-evaluate-model test-samples)))
      (should (plistp eval-result))
      (should (numberp (plist-get eval-result :accuracy)))
      (should (numberp (plist-get eval-result :avg-loss)))
      (should (= (plist-get eval-result :sample-count) 2)))))

;;; 統合テスト

(nskk-deftest nskk-ai-integration-full-workflow-test
  "AI統合ワークフローのテスト"
  :tags '(:integration :ai)
  ;; クリーンアップ
  (nskk-ai-context-clear)
  (nskk-ai-pattern-clear)
  (nskk-ai-candidates-clear)
  (nskk-ai-learning-reset-model)

  ;; 1. 文脈学習
  (nskk-ai-context-learn-text "私は学生です")
  (nskk-ai-context-learn-text "彼は社会人です")

  ;; 2. パターン学習
  (nskk-ai-pattern-learn "がくせい" "学生" '("私は"))
  (nskk-ai-pattern-learn "しゃかいじん" "社会人" '("彼は"))

  ;; 3. 候補生成
  (let ((candidates (nskk-ai-candidates-generate "がくせい" '("学生" "岳生"))))
    (should (listp candidates))
    (should (member "学生" candidates)))

  ;; 4. 選択学習
  (nskk-ai-candidates-learn-selection "がくせい" "学生" '("学生" "岳生"))

  ;; 5. 統計確認
  (let ((context-stats (nskk-ai-context-statistics))
        (pattern-stats (nskk-ai-pattern-statistics))
        (cand-stats (nskk-ai-candidates-statistics)))
    (should (> (plist-get context-stats :bigram-count) 0))
    (should (> (plist-get pattern-stats :pattern-history-count) 0))
    (should (> (plist-get cand-stats :total-queries) 0))))

;;; パフォーマンステスト

(nskk-deftest nskk-ai-performance-context-analysis-test
  "文脈解析のパフォーマンステスト"
  :tags '(:performance :ai :context)
  (let ((text "これは長めのテキストです。自然言語処理は面白い分野です。"))
    (let ((result (nskk-test-measure-time
                    (nskk-ai-context-parse-sentence text))))
      (should (< (cdr result) 0.005))  ; < 5ms
      (message "Context parsing time: %.6f seconds" (cdr result)))))

(nskk-deftest nskk-ai-performance-scoring-test
  "スコアリングのパフォーマンステスト"
  :tags '(:performance :ai :context)
  (nskk-ai-context-clear)
  (nskk-ai-context-learn-text "今日は良い天気です")

  (let ((result (nskk-test-measure-time
                  (nskk-ai-context-score-candidate "てんき" "天気"))))
    (should (< (cdr result) 0.002))  ; < 2ms
    (message "Scoring time: %.6f seconds" (cdr result))))

(nskk-deftest nskk-ai-performance-candidate-generation-test
  "候補生成のパフォーマンステスト"
  :tags '(:performance :ai :candidates)
  (let ((result (nskk-test-measure-time
                  (nskk-ai-candidates-generate "かんじ" '("漢字" "感じ" "幹事")))))
    (should (< (cdr result) 0.020))  ; < 20ms
    (message "Candidate generation time: %.6f seconds" (cdr result))))

(nskk-deftest nskk-ai-performance-learning-update-test
  "学習更新のパフォーマンステスト"
  :tags '(:performance :ai :learning)
  (let ((features (vector 1.0 0.5 0.3 0.8 0.2)))
    (let ((result (nskk-test-measure-time
                    (nskk-ai-learning-online-update features "候補"))))
      (should (< (cdr result) 0.050))  ; < 50ms
      (message "Learning update time: %.6f seconds" (cdr result)))))

;;; メモリリークテスト

(nskk-deftest nskk-ai-memory-leak-test
  "メモリリークテスト"
  :tags '(:unit :ai :memory)
  (nskk-ai-context-clear)
  (nskk-ai-pattern-clear)

  ;; 大量のデータを学習
  (dotimes (i 1000)
    (nskk-ai-context-learn-text (format "テキスト%d" i))
    (nskk-ai-pattern-learn (format "test%d" i)
                          (format "テスト%d" i)
                          (list (format "context%d" i))))

  ;; エントリ数が制限内に収まっているか確認
  (let ((context-stats (nskk-ai-context-statistics))
        (pattern-stats (nskk-ai-pattern-statistics)))
    (should (<= (plist-get context-stats :bigram-count)
               nskk-ai-context-max-ngrams))
    (should (<= (plist-get pattern-stats :pattern-history-count)
               10000))))

;;; エッジケーステスト

(nskk-deftest nskk-ai-edge-case-empty-input-test
  "空入力のエッジケーステスト"
  :tags '(:unit :ai :edge-case)
  (should (null (nskk-ai-context-parse-sentence "")))
  (should (null (nskk-ai-context-parse-sentence nil)))

  ;; 空入力でもエラーにならないことを確認
  (should (listp (nskk-ai-candidates-generate "" nil)))
  (should (listp (nskk-ai-candidates-generate "test" nil))))

(nskk-deftest nskk-ai-edge-case-large-input-test
  "大きな入力のエッジケーステスト"
  :tags '(:unit :ai :edge-case)
  (let ((large-text (make-string 10000 ?あ)))
    ;; 大きな入力でもエラーにならないことを確認
    (should (listp (nskk-ai-context-parse-sentence large-text)))))

(nskk-deftest nskk-ai-edge-case-special-chars-test
  "特殊文字のエッジケーステスト"
  :tags '(:unit :ai :edge-case)
  (let ((text "🎉😀日本語🌸"))
    (let ((words (nskk-ai-context-parse-sentence text)))
      (should (listp words)))))

;;; 統計情報テスト

(nskk-deftest nskk-ai-statistics-test
  "統計情報のテスト"
  :tags '(:unit :ai)
  (let ((context-stats (nskk-ai-context-statistics))
        (pattern-stats (nskk-ai-pattern-statistics))
        (cand-stats (nskk-ai-candidates-statistics))
        (learning-stats (nskk-ai-learning-statistics)))

    (should (plistp context-stats))
    (should (plistp pattern-stats))
    (should (plistp cand-stats))
    (should (plistp learning-stats))

    ;; キーの存在確認
    (should (plist-member context-stats :bigram-count))
    (should (plist-member pattern-stats :pattern-history-count))
    (should (plist-member cand-stats :total-queries))
    (should (plist-member learning-stats :training-steps))))

(provide 'nskk-ai-test)

;;; nskk-ai-test.el ends here
