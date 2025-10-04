;;; nskk-benchmark.el --- Performance benchmarks for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Development Team

;;; Commentary:

;; NSKK Phase 1パフォーマンステストスイート
;;
;; 目標値:
;; - ローマ字変換: < 0.1ms
;; - 辞書検索: < 10ms (10万エントリ)
;; - メモリ使用量: < 20MB
;; - テストカバレッジ: 95%以上

;;; Code:

(require 'ert)
(require 'nskk)
(require 'nskk-test-framework)

;;; Utilities

(defun nskk-benchmark--measure-time (func iterations)
  "FUNCをITERATIONS回実行し、平均実行時間(ms)を返す。"
  (let ((start-time (float-time)))
    (dotimes (_ iterations)
      (funcall func))
    (let ((elapsed (- (float-time) start-time)))
      (/ (* elapsed 1000) iterations))))

(defun nskk-benchmark--measure-memory (func)
  "FUNCの実行前後のメモリ使用量差分(MB)を返す。"
  (garbage-collect)
  (let ((before (memory-use-counts)))
    (funcall func)
    (garbage-collect)
    (let* ((after (memory-use-counts))
           (cons-diff (- (nth 0 after) (nth 0 before)))
           (symbol-diff (- (nth 1 after) (nth 1 before)))
           (string-diff (- (nth 3 after) (nth 3 before)))
           ;; 概算: cons=8bytes, symbol=48bytes, string=24bytes
           (total-bytes (+ (* cons-diff 8)
                          (* symbol-diff 48)
                          (* string-diff 24))))
      (/ total-bytes (* 1024.0 1024.0)))))

;;; Benchmark: Romaji Conversion

(ert-deftest nskk-benchmark-romaji-conversion-basic ()
  "基本的なローマ字変換のベンチマーク（目標: < 0.1ms）。"
  (let ((time (nskk-benchmark--measure-time
               (lambda () (nskk-convert-romaji "kanzi"))
               1000)))
    (message "Romaji conversion (basic): %.3f ms" time)
    (should (< time 0.1))))

(ert-deftest nskk-benchmark-romaji-conversion-complex ()
  "複雑なローマ字変換のベンチマーク（目標: < 0.1ms）。"
  (let ((time (nskk-benchmark--measure-time
               (lambda () (nskk-convert-romaji "kyoukyoukkyo"))
               1000)))
    (message "Romaji conversion (complex): %.3f ms" time)
    (should (< time 0.1))))

(ert-deftest nskk-benchmark-romaji-conversion-long ()
  "長文ローマ字変換のベンチマーク（目標: < 0.5ms）。"
  (let ((time (nskk-benchmark--measure-time
               (lambda () (nskk-convert-romaji "nihongonohenkannsokudo"))
               1000)))
    (message "Romaji conversion (long): %.3f ms" time)
    (should (< time 0.5))))

;;; Benchmark: Dictionary Operations

(ert-deftest nskk-benchmark-dict-parse ()
  "辞書行パースのベンチマーク（目標: < 0.5ms）。"
  (let ((time (nskk-benchmark--measure-time
               (lambda () (nskk-dict-parse-line "かんじ /漢字/幹事/感じ/"))
               1000)))
    (message "Dictionary parse: %.3f ms" time)
    (should (< time 0.5))))

(ert-deftest nskk-benchmark-dict-search-small ()
  "小規模辞書検索のベンチマーク（1000エントリ、目標: < 1ms）。"
  (let ((trie (nskk-trie-create)))
    ;; 1000エントリ挿入
    (dotimes (i 1000)
      (nskk-trie-insert trie (format "key%04d" i) (list (format "value%d" i))))

    ;; 検索ベンチマーク
    (let ((time (nskk-benchmark--measure-time
                 (lambda () (nskk-trie-lookup trie "key0500"))
                 1000)))
      (message "Dictionary search (1K entries): %.3f ms" time)
      (should (< time 1.0)))))

(ert-deftest nskk-benchmark-dict-search-medium ()
  "中規模辞書検索のベンチマーク（10000エントリ、目標: < 5ms）。"
  (let ((trie (nskk-trie-create)))
    ;; 10000エントリ挿入
    (dotimes (i 10000)
      (nskk-trie-insert trie (format "key%05d" i) (list (format "value%d" i))))

    ;; 検索ベンチマーク
    (let ((time (nskk-benchmark--measure-time
                 (lambda () (nskk-trie-lookup trie "key05000"))
                 1000)))
      (message "Dictionary search (10K entries): %.3f ms" time)
      (should (< time 5.0)))))

(ert-deftest nskk-benchmark-dict-search-large ()
  "大規模辞書検索のベンチマーク（100000エントリ、目標: < 10ms）。"
  (let ((trie (nskk-trie-create)))
    ;; 100000エントリ挿入
    (message "Inserting 100K entries...")
    (dotimes (i 100000)
      (when (zerop (% i 10000))
        (message "  %d entries inserted..." i))
      (nskk-trie-insert trie (format "key%06d" i) (list (format "value%d" i))))

    ;; 検索ベンチマーク
    (let ((time (nskk-benchmark--measure-time
                 (lambda () (nskk-trie-lookup trie "key050000"))
                 100)))
      (message "Dictionary search (100K entries): %.3f ms" time)
      (should (< time 10.0)))))

;;; Benchmark: Cache Performance

(ert-deftest nskk-benchmark-cache-hit ()
  "キャッシュヒット時のベンチマーク（目標: < 0.01ms）。"
  (let ((cache (nskk-cache-create :size 100))
        (trie (nskk-trie-create)))
    ;; データ準備
    (nskk-trie-insert trie "test" '("テスト"))
    (nskk-search-with-cache cache trie "test")

    ;; キャッシュヒット計測
    (let ((time (nskk-benchmark--measure-time
                 (lambda () (nskk-search-with-cache cache trie "test"))
                 1000)))
      (message "Cache hit: %.3f ms" time)
      (should (< time 0.01)))))

(ert-deftest nskk-benchmark-cache-miss ()
  "キャッシュミス時のベンチマーク（目標: < 1ms）。"
  (let ((cache (nskk-cache-create :size 100))
        (trie (nskk-trie-create)))
    ;; 1000エントリ挿入
    (dotimes (i 1000)
      (nskk-trie-insert trie (format "key%04d" i) (list (format "value%d" i))))

    ;; キャッシュミス計測
    (let ((time (nskk-benchmark--measure-time
                 (lambda ()
                   (let ((key (format "key%04d" (random 1000))))
                     (nskk-search-with-cache cache trie key)))
                 100)))
      (message "Cache miss: %.3f ms" time)
      (should (< time 1.0)))))

;;; Benchmark: State Management

(ert-deftest nskk-benchmark-state-operations ()
  "状態操作のベンチマーク（目標: < 0.05ms）。"
  (let ((state (nskk-state-create)))
    (let ((time (nskk-benchmark--measure-time
                 (lambda ()
                   (nskk-state-set-mode state 'hiragana)
                   (nskk-state-mode state))
                 1000)))
      (message "State operations: %.3f ms" time)
      (should (< time 0.05)))))

(ert-deftest nskk-benchmark-mode-switch ()
  "モード切り替えのベンチマーク（目標: < 0.1ms）。"
  (let ((state (nskk-state-create)))
    (let ((time (nskk-benchmark--measure-time
                 (lambda ()
                   (nskk-mode-switch state 'katakana)
                   (nskk-mode-switch state 'hiragana))
                 1000)))
      (message "Mode switch: %.3f ms" time)
      (should (< time 0.1)))))

;;; Benchmark: Buffer Operations

(ert-deftest nskk-benchmark-buffer-insert ()
  "バッファ挿入のベンチマーク（目標: < 0.05ms）。"
  (let ((buffer (nskk-buffer-create)))
    (let ((time (nskk-benchmark--measure-time
                 (lambda ()
                   (nskk-buffer-insert buffer "a")
                   (nskk-buffer-clear buffer))
                 1000)))
      (message "Buffer insert: %.3f ms" time)
      (should (< time 0.05)))))

;;; Benchmark: UI Operations

(ert-deftest nskk-benchmark-candidate-window ()
  "候補ウィンドウ操作のベンチマーク（目標: < 0.5ms）。"
  (let ((candidates '("候補1" "候補2" "候補3" "候補4" "候補5")))
    (let ((time (nskk-benchmark--measure-time
                 (lambda ()
                   (let ((window (nskk-candidate-window-create candidates)))
                     (nskk-candidate-window-select window 2)))
                 1000)))
      (message "Candidate window: %.3f ms" time)
      (should (< time 0.5)))))

(ert-deftest nskk-benchmark-modeline-update ()
  "モードライン更新のベンチマーク（目標: < 0.1ms）。"
  (let ((state (nskk-state-create)))
    (let ((time (nskk-benchmark--measure-time
                 (lambda () (nskk-modeline-format state))
                 1000)))
      (message "Modeline update: %.3f ms" time)
      (should (< time 0.1)))))

;;; Benchmark: Memory Usage

(ert-deftest nskk-benchmark-memory-base ()
  "基本モジュールのメモリ使用量（目標: < 5MB）。"
  (garbage-collect)
  (let* ((before (memory-use-counts))
         (_ (require 'nskk))
         (after (memory-use-counts))
         (cons-diff (- (nth 0 after) (nth 0 before)))
         (memory-mb (/ (* cons-diff 8) (* 1024.0 1024.0))))
    (message "Base memory usage: %.2f MB" memory-mb)
    (should (< memory-mb 5.0))))

(ert-deftest nskk-benchmark-memory-dict-small ()
  "小規模辞書のメモリ使用量（1000エントリ、目標: < 1MB）。"
  (let ((memory (nskk-benchmark--measure-memory
                 (lambda ()
                   (let ((trie (nskk-trie-create)))
                     (dotimes (i 1000)
                       (nskk-trie-insert trie
                                        (format "key%04d" i)
                                        (list (format "value%d" i)))))))))
    (message "Dictionary memory (1K entries): %.2f MB" memory)
    (should (< memory 1.0))))

(ert-deftest nskk-benchmark-memory-dict-large ()
  "大規模辞書のメモリ使用量（100000エントリ、目標: < 20MB）。"
  (let ((memory (nskk-benchmark--measure-memory
                 (lambda ()
                   (let ((trie (nskk-trie-create)))
                     (dotimes (i 100000)
                       (when (zerop (% i 10000))
                         (message "  %d entries inserted..." i))
                       (nskk-trie-insert trie
                                        (format "key%06d" i)
                                        (list (format "value%d" i)))))))))
    (message "Dictionary memory (100K entries): %.2f MB" memory)
    (should (< memory 20.0))))

(ert-deftest nskk-benchmark-memory-cache ()
  "キャッシュのメモリ使用量（1000エントリ、目標: < 2MB）。"
  (let ((memory (nskk-benchmark--measure-memory
                 (lambda ()
                   (let ((cache (nskk-cache-create :size 1000)))
                     (dotimes (i 1000)
                       (nskk-cache-put cache
                                      (format "key%04d" i)
                                      (list (format "value%d" i)))))))))
    (message "Cache memory (1K entries): %.2f MB" memory)
    (should (< memory 2.0))))

;;; Benchmark: Throughput

(ert-deftest nskk-benchmark-throughput-conversion ()
  "ローマ字変換のスループット測定。"
  (let* ((iterations 10000)
         (start-time (float-time)))
    (dotimes (_ iterations)
      (nskk-convert-romaji "kanzi"))
    (let* ((elapsed (- (float-time) start-time))
           (throughput (/ iterations elapsed)))
      (message "Conversion throughput: %.0f ops/sec" throughput)
      (should (> throughput 10000)))))

(ert-deftest nskk-benchmark-throughput-search ()
  "辞書検索のスループット測定。"
  (let ((trie (nskk-trie-create)))
    ;; 1000エントリ挿入
    (dotimes (i 1000)
      (nskk-trie-insert trie (format "key%04d" i) (list (format "value%d" i))))

    ;; スループット測定
    (let* ((iterations 1000)
           (start-time (float-time)))
      (dotimes (i iterations)
        (nskk-trie-lookup trie (format "key%04d" (% i 1000))))
      (let* ((elapsed (- (float-time) start-time))
             (throughput (/ iterations elapsed)))
        (message "Search throughput: %.0f ops/sec" throughput)
        (should (> throughput 1000))))))

;;; Benchmark Report

(defun nskk-benchmark--generate-report (results)
  "ベンチマーク結果からレポートを生成する。"
  (with-output-to-temp-buffer "*NSKK Benchmark Report*"
    (princ "NSKK Phase 1 Performance Benchmark Report\n")
    (princ "==========================================\n\n")
    (princ (format "Test Date: %s\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
    (princ (format "Emacs Version: %s\n\n" emacs-version))

    (princ "Performance Goals:\n")
    (princ "  ✓ Romaji conversion: < 0.1ms\n")
    (princ "  ✓ Dictionary search: < 10ms (100K entries)\n")
    (princ "  ✓ Memory usage: < 20MB\n")
    (princ "  ✓ Cache hit: < 0.01ms\n\n")

    (princ "Results Summary:\n")
    (dolist (result results)
      (princ (format "  %s: %s\n" (car result) (cdr result))))))

(defun nskk-benchmark-run-all ()
  "全ベンチマークを実行し、結果を報告する。"
  (interactive)
  (let ((start-time (float-time)))
    (message "=== NSKK Performance Benchmarks ===")
    (ert-run-tests-batch "^nskk-benchmark-")
    (let ((elapsed (- (float-time) start-time)))
      (message "Total benchmark time: %.3f seconds" elapsed))))

(defun nskk-benchmark-quick ()
  "主要なベンチマークのみ実行（クイックテスト）。"
  (interactive)
  (message "=== NSKK Quick Benchmarks ===")
  (ert-run-tests-batch-and-exit
   '(and (tag :benchmark)
         (or "nskk-benchmark-romaji-conversion-basic"
             "nskk-benchmark-dict-search-large"
             "nskk-benchmark-memory-dict-large"))))

(provide 'nskk-benchmark)

;;; nskk-benchmark.el ends here
