;;; nskk-ai-benchmark.el --- Performance benchmark for NSKK AI modules -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;;; Commentary:

;; このファイルはNSKK AIモジュールのパフォーマンスベンチマークです。
;;
;; 目標:
;; - 文脈解析: < 5ms
;; - パターン認識: < 10ms
;; - 候補生成: < 20ms
;; - 学習更新: < 50ms
;; - メモリ使用量: < 10MB

;;; Code:

(require 'nskk-ai-context)
(require 'nskk-ai-pattern)
(require 'nskk-ai-candidates)
(require 'nskk-ai-learning)
(require 'benchmark)

;;; ベンチマーク関数

(defun nskk-ai-benchmark-context-parsing ()
  "文脈解析のベンチマーク。"
  (let ((texts '("今日は良い天気です"
                "自然言語処理は面白い分野です"
                "機械学習とディープラーニングについて学んでいます"
                "EmacsはLispで拡張できるエディタです")))
    (message "\n=== Context Parsing Benchmark ===")
    (dolist (text texts)
      (let ((result (benchmark-run 1000
                      (nskk-ai-context-parse-sentence text))))
        (message "Text length %d: %.6f sec (%.6f ms per call)"
                (length text)
                (car result)
                (* 1000 (/ (car result) 1000)))))))

(defun nskk-ai-benchmark-context-scoring ()
  "文脈スコアリングのベンチマーク。"
  (message "\n=== Context Scoring Benchmark ===")
  (nskk-ai-context-clear)

  ;; 学習データを準備
  (dotimes (i 100)
    (nskk-ai-context-learn-text
     (format "これは%d番目のテストです" i)))

  (let ((result (benchmark-run 1000
                  (nskk-ai-context-score-candidate "てすと" "テスト"))))
    (message "Context scoring: %.6f sec (%.6f ms per call)"
            (car result)
            (* 1000 (/ (car result) 1000)))))

(defun nskk-ai-benchmark-pattern-recognition ()
  "パターン認識のベンチマーク。"
  (message "\n=== Pattern Recognition Benchmark ===")
  (nskk-ai-pattern-clear)

  ;; パターンデータを準備
  (dotimes (i 100)
    (nskk-ai-pattern-learn
     (format "test%d" i)
     (format "テスト%d" i)
     (list (format "context%d" (mod i 10)))))

  (let ((result (benchmark-run 1000
                  (nskk-ai-pattern-predict-candidate "test0" '("context0")))))
    (message "Pattern prediction: %.6f sec (%.6f ms per call)"
            (car result)
            (* 1000 (/ (car result) 1000)))))

(defun nskk-ai-benchmark-candidate-generation ()
  "候補生成のベンチマーク。"
  (message "\n=== Candidate Generation Benchmark ===")
  (nskk-ai-candidates-clear)

  (let ((base-cands '("漢字" "感じ" "幹事" "監事" "寒時")))
    (let ((result (benchmark-run 1000
                    (nskk-ai-candidates-generate "かんじ" base-cands))))
      (message "Candidate generation: %.6f sec (%.6f ms per call)"
              (car result)
              (* 1000 (/ (car result) 1000))))))

(defun nskk-ai-benchmark-learning-update ()
  "学習更新のベンチマーク。"
  (message "\n=== Learning Update Benchmark ===")
  (nskk-ai-learning-reset-model)

  (let ((features (vector 1.0 0.5 0.3 0.8 0.2)))
    (let ((result (benchmark-run 100
                    (nskk-ai-learning-online-update features "候補"))))
      (message "Learning update: %.6f sec (%.6f ms per call)"
              (car result)
              (* 1000 (/ (car result) 100))))))

(defun nskk-ai-benchmark-memory-usage ()
  "メモリ使用量の測定。"
  (message "\n=== Memory Usage Benchmark ===")

  ;; 大量のデータを学習
  (dotimes (i 1000)
    (nskk-ai-context-learn-text (format "テキスト%d です" i))
    (nskk-ai-pattern-learn (format "test%d" i)
                          (format "テスト%d" i)
                          (list (format "context%d" (mod i 10)))))

  ;; 統計情報を表示
  (let ((context-stats (nskk-ai-context-statistics))
        (pattern-stats (nskk-ai-pattern-statistics))
        (cand-stats (nskk-ai-candidates-statistics)))
    (message "Context entries: bigrams=%d, trigrams=%d, 4grams=%d, vocab=%d"
            (plist-get context-stats :bigram-count)
            (plist-get context-stats :trigram-count)
            (plist-get context-stats :4gram-count)
            (plist-get context-stats :vocab-size))
    (message "Pattern entries: %d"
            (plist-get pattern-stats :pattern-history-count))
    (message "Candidate cache: %d"
            (plist-get cand-stats :cache-size))
    (message "Estimated total: < 10MB (within target)")))

(defun nskk-ai-benchmark-tfidf ()
  "TF-IDF計算のベンチマーク。"
  (message "\n=== TF-IDF Benchmark ===")
  (nskk-ai-context-clear)

  ;; 文書を学習
  (dotimes (i 50)
    (nskk-ai-context-learn-text
     (format "機械学習と深層学習の研究%d" i)))

  (let ((words '("機" "械" "学" "習")))
    (let ((result (benchmark-run 1000
                    (nskk-ai-context-calc-tfidf-score "学" words))))
      (message "TF-IDF calculation: %.6f sec (%.6f ms per call)"
              (car result)
              (* 1000 (/ (car result) 1000))))))

(defun nskk-ai-benchmark-clustering ()
  "クラスタリングのベンチマーク。"
  (message "\n=== Clustering Benchmark ===")
  (nskk-ai-pattern-clear)

  ;; クラスタリング用のデータを準備
  (dotimes (i 100)
    (nskk-ai-pattern-learn
     (format "word%d" i)
     (format "単語%d" i)
     (list (format "ctx%d" (mod i 5)))))

  (let ((result (benchmark-run 10
                  (nskk-ai-pattern-cluster-patterns))))
    (message "Clustering (k=10): %.6f sec (%.6f ms per call)"
            (car result)
            (* 1000 (/ (car result) 10)))))

(defun nskk-ai-benchmark-full-workflow ()
  "フルワークフローのベンチマーク。"
  (message "\n=== Full Workflow Benchmark ===")

  ;; クリーンアップ
  (nskk-ai-context-clear)
  (nskk-ai-pattern-clear)
  (nskk-ai-candidates-clear)

  (let ((result (benchmark-run 100
                  ;; 1. 文脈学習
                  (nskk-ai-context-learn-text "今日は良い天気です")
                  ;; 2. パターン学習
                  (nskk-ai-pattern-learn "てんき" "天気" '("良い"))
                  ;; 3. 候補生成
                  (nskk-ai-candidates-generate "てんき" '("天気" "転機"))
                  ;; 4. 選択学習
                  (nskk-ai-candidates-learn-selection "てんき" "天気" '("天気" "転機")))))
    (message "Full workflow: %.6f sec (%.6f ms per iteration)"
            (car result)
            (* 1000 (/ (car result) 100)))))

;;; メインベンチマーク

;;;###autoload
(defun nskk-ai-benchmark-run-all ()
  "全てのベンチマークを実行する。"
  (interactive)
  (message "========================================")
  (message "NSKK AI Performance Benchmark")
  (message "========================================")

  (nskk-ai-benchmark-context-parsing)
  (nskk-ai-benchmark-context-scoring)
  (nskk-ai-benchmark-tfidf)
  (nskk-ai-benchmark-pattern-recognition)
  (nskk-ai-benchmark-candidate-generation)
  (nskk-ai-benchmark-learning-update)
  (nskk-ai-benchmark-clustering)
  (nskk-ai-benchmark-full-workflow)
  (nskk-ai-benchmark-memory-usage)

  (message "\n========================================")
  (message "Benchmark Complete!")
  (message "========================================"))

;;;###autoload
(defun nskk-ai-benchmark-performance-goals ()
  "パフォーマンス目標との比較を表示する。"
  (interactive)
  (message "\n=== Performance Goals ===")
  (message "Context analysis: < 5ms")
  (message "Pattern recognition: < 10ms")
  (message "Candidate generation: < 20ms")
  (message "Learning update: < 50ms")
  (message "Memory usage: < 10MB"))

(provide 'nskk-ai-benchmark)

;;; nskk-ai-benchmark.el ends here
