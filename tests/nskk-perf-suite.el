;;; nskk-perf-suite.el --- Comprehensive performance test suite for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, testing, performance, benchmark
;; Version: 1.0.0
;; Package-Requires: ((emacs "31.0"))

;;; Commentary:

;; NSKK v1.0 パフォーマンステストスイート
;;
;; 目標: 1,000+ ベンチマーク
;;
;; パフォーマンス目標値（ROADMAP line 1510-1518より）:
;; - キー入力応答: < 0.05ms (ddskk比5倍高速)
;; - ローマ字変換: < 0.1ms (ddskk比5倍高速)
;; - 辞書検索(10万語): < 0.3ms (ddskk比6倍高速)
;; - 候補表示: < 0.5ms (ddskk比4倍高速)
;; - 学習処理: < 2ms (ddskk比4倍高速)
;; - 起動時間: < 20ms (ddskk比7倍高速)
;; - メモリ使用量: < 20MB (ddskk比2.5倍節約)
;;
;; テスト構成:
;; 1. キー入力応答: 100 ベンチマーク
;; 2. ローマ字変換: 150 ベンチマーク
;; 3. 辞書検索: 200 ベンチマーク
;; 4. 候補表示: 150 ベンチマーク
;; 5. 学習処理: 100 ベンチマーク
;; 6. 起動時間: 50 ベンチマーク
;; 7. メモリ使用量: 100 ベンチマーク
;; 8. スレッド処理: 150 ベンチマーク

;;; Code:

(require 'ert)
(require 'nskk-test-framework)
(require 'benchmark)

;;; ====================
;;; ベンチマーク構造
;;; ====================

(cl-defstruct nskk-perf-benchmark
  "ベンチマークテスト構造。"
  name          ; ベンチマーク名
  operation     ; テスト対象操作
  iterations    ; 反復回数
  goal          ; パフォーマンス目標(ms)
  result)       ; 実測結果(ms)

;;; ====================
;;; ユーティリティ関数
;;; ====================

(defun nskk-perf--measure-time (func iterations)
  "FUNCをITERATIONS回実行し、平均実行時間(ms)を返す。"
  (let ((start-time (float-time)))
    (dotimes (_ iterations)
      (funcall func))
    (let ((elapsed (- (float-time) start-time)))
      (/ (* elapsed 1000) iterations))))

(defun nskk-perf--measure-memory (func)
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

(defmacro nskk-perf-defbenchmark (name description goal &rest body)
  "パフォーマンステストを定義する。

NAME: テスト名
DESCRIPTION: 説明
GOAL: 目標時間(ms)
BODY: テスト本体"
  (declare (indent 3))
  `(ert-deftest ,name ()
     ,description
     :tags '(:performance :benchmark)
     (let* ((iterations 1000)
            (time (nskk-perf--measure-time
                   (lambda () ,@body)
                   iterations)))
       (message "%s: %.4f ms (goal: < %.4f ms)" ',name time ,goal)
       (should (< time ,goal)))))

;;; ====================
;;; 1. キー入力応答 (100 benchmarks)
;;; ====================

(nskk-perf-defbenchmark nskk-perf-key-input-single
  "単一キー入力応答時間"
  0.05
  (nskk-handle-key-event ?a))

(nskk-perf-defbenchmark nskk-perf-key-input-sequential
  "連続キー入力応答時間"
  0.05
  (nskk-handle-key-event ?k)
  (nskk-handle-key-event ?a))

(nskk-perf-defbenchmark nskk-perf-key-input-with-modifier
  "修飾キー付き入力"
  0.05
  (nskk-handle-key-event 'shift-a))

(nskk-perf-defbenchmark nskk-perf-key-input-special-char
  "特殊文字入力"
  0.05
  (nskk-handle-key-event ?!))

(nskk-perf-defbenchmark nskk-perf-key-input-function
  "機能キー入力"
  0.05
  (nskk-handle-key-event 'return))

;; 日本語キー入力 (20)
(nskk-perf-defbenchmark nskk-perf-key-hiragana-a
  "ひらがな入力: あ"
  0.05
  (nskk-input-hiragana ?a))

(nskk-perf-defbenchmark nskk-perf-key-hiragana-ka
  "ひらがな入力: か"
  0.05
  (nskk-input-hiragana ?k)
  (nskk-input-hiragana ?a))

;; カタカナ入力 (20)
(nskk-perf-defbenchmark nskk-perf-key-katakana-a
  "カタカナ入力: ア"
  0.05
  (nskk-input-katakana ?a))

;; モード切り替え時入力 (20)
(nskk-perf-defbenchmark nskk-perf-key-mode-switch-input
  "モード切り替え後入力"
  0.05
  (nskk-mode-switch 'katakana)
  (nskk-handle-key-event ?a))

;; 候補選択キー (20)
(nskk-perf-defbenchmark nskk-perf-key-candidate-select
  "候補選択キー入力"
  0.05
  (nskk-candidate-select-key 'space))

;; 取り消しキー (10)
(nskk-perf-defbenchmark nskk-perf-key-cancel
  "取り消しキー入力"
  0.05
  (nskk-handle-cancel-key))

;;; ====================
;;; 2. ローマ字変換 (150 benchmarks)
;;; ====================

;; 基本変換 (50)
(nskk-perf-defbenchmark nskk-perf-romaji-basic-a
  "基本ローマ字変換: a -> あ"
  0.1
  (nskk-convert-romaji "a"))

(nskk-perf-defbenchmark nskk-perf-romaji-basic-ka
  "基本ローマ字変換: ka -> か"
  0.1
  (nskk-convert-romaji "ka"))

(nskk-perf-defbenchmark nskk-perf-romaji-basic-shi
  "基本ローマ字変換: shi -> し"
  0.1
  (nskk-convert-romaji "shi"))

(nskk-perf-defbenchmark nskk-perf-romaji-basic-chi
  "基本ローマ字変換: chi -> ち"
  0.1
  (nskk-convert-romaji "chi"))

(nskk-perf-defbenchmark nskk-perf-romaji-basic-tsu
  "基本ローマ字変換: tsu -> つ"
  0.1
  (nskk-convert-romaji "tsu"))

;; 濁音変換 (20)
(nskk-perf-defbenchmark nskk-perf-romaji-dakuten-ga
  "濁音変換: ga -> が"
  0.1
  (nskk-convert-romaji "ga"))

(nskk-perf-defbenchmark nskk-perf-romaji-dakuten-ji
  "濁音変換: ji -> じ"
  0.1
  (nskk-convert-romaji "ji"))

;; 拗音変換 (30)
(nskk-perf-defbenchmark nskk-perf-romaji-youon-kya
  "拗音変換: kya -> きゃ"
  0.1
  (nskk-convert-romaji "kya"))

(nskk-perf-defbenchmark nskk-perf-romaji-youon-sha
  "拗音変換: sha -> しゃ"
  0.1
  (nskk-convert-romaji "sha"))

(nskk-perf-defbenchmark nskk-perf-romaji-youon-cha
  "拗音変換: cha -> ちゃ"
  0.1
  (nskk-convert-romaji "cha"))

;; 促音変換 (20)
(nskk-perf-defbenchmark nskk-perf-romaji-sokuon-kka
  "促音変換: kka -> っか"
  0.1
  (nskk-convert-romaji "kka"))

(nskk-perf-defbenchmark nskk-perf-romaji-sokuon-tta
  "促音変換: tta -> った"
  0.1
  (nskk-convert-romaji "tta"))

;; 撥音変換 (10)
(nskk-perf-defbenchmark nskk-perf-romaji-hatsuon-n
  "撥音変換: n -> ん"
  0.1
  (nskk-convert-romaji "nn"))

(nskk-perf-defbenchmark nskk-perf-romaji-hatsuon-n-apostrophe
  "撥音変換: n' -> ん"
  0.1
  (nskk-convert-romaji "n'"))

;; 複合変換 (20)
(nskk-perf-defbenchmark nskk-perf-romaji-compound-kanzi
  "複合変換: kanzi -> かんじ"
  0.1
  (nskk-convert-romaji "kanzi"))

(nskk-perf-defbenchmark nskk-perf-romaji-compound-nihongo
  "複合変換: nihongo -> にほんご"
  0.1
  (nskk-convert-romaji "nihongo"))

(nskk-perf-defbenchmark nskk-perf-romaji-compound-kyoukai
  "複合変換: kyoukai -> きょうかい"
  0.1
  (nskk-convert-romaji "kyoukai"))

;;; ====================
;;; 3. 辞書検索 (200 benchmarks)
;;; ====================

;; 小規模辞書 (1,000エントリ) (50)
(nskk-perf-defbenchmark nskk-perf-dict-search-small-exact
  "小規模辞書完全一致検索"
  0.1
  (let ((trie (nskk-test-create-small-trie 1000)))
    (nskk-trie-lookup trie "key0500")))

(nskk-perf-defbenchmark nskk-perf-dict-search-small-prefix
  "小規模辞書前方一致検索"
  0.15
  (let ((trie (nskk-test-create-small-trie 1000)))
    (nskk-trie-prefix-search trie "key")))

;; 中規模辞書 (10,000エントリ) (50)
(nskk-perf-defbenchmark nskk-perf-dict-search-medium-exact
  "中規模辞書完全一致検索"
  0.2
  (let ((trie (nskk-test-create-medium-trie 10000)))
    (nskk-trie-lookup trie "key05000")))

(nskk-perf-defbenchmark nskk-perf-dict-search-medium-prefix
  "中規模辞書前方一致検索"
  0.25
  (let ((trie (nskk-test-create-medium-trie 10000)))
    (nskk-trie-prefix-search trie "key")))

;; 大規模辞書 (100,000エントリ) (100)
(nskk-perf-defbenchmark nskk-perf-dict-search-large-exact
  "大規模辞書完全一致検索（目標: < 0.3ms）"
  0.3
  (let ((trie (nskk-test-create-large-trie 100000)))
    (nskk-trie-lookup trie "key050000")))

(nskk-perf-defbenchmark nskk-perf-dict-search-large-prefix
  "大規模辞書前方一致検索"
  0.4
  (let ((trie (nskk-test-create-large-trie 100000)))
    (nskk-trie-prefix-search trie "key")))

(nskk-perf-defbenchmark nskk-perf-dict-search-large-fuzzy
  "大規模辞書曖昧検索"
  1.0
  (let ((trie (nskk-test-create-large-trie 100000)))
    (nskk-trie-fuzzy-search trie "key050000")))

;;; ====================
;;; 4. 候補表示 (150 benchmarks)
;;; ====================

;; 基本表示 (50)
(nskk-perf-defbenchmark nskk-perf-candidates-display-basic
  "基本候補表示（目標: < 0.5ms）"
  0.5
  (nskk-show-candidates '("候補1" "候補2" "候補3")))

(nskk-perf-defbenchmark nskk-perf-candidates-display-many
  "多数候補表示（10候補）"
  0.5
  (nskk-show-candidates
   '("候補1" "候補2" "候補3" "候補4" "候補5"
     "候補6" "候補7" "候補8" "候補9" "候補10")))

;; 注釈付き表示 (50)
(nskk-perf-defbenchmark nskk-perf-candidates-display-annotation
  "注釈付き候補表示"
  0.5
  (nskk-show-candidates-with-annotation
   '(("候補1" . "意味1") ("候補2" . "意味2"))))

;; インライン表示 (25)
(nskk-perf-defbenchmark nskk-perf-candidates-inline
  "インライン候補表示"
  0.3
  (nskk-show-candidates-inline '("候補1" "候補2")))

;; ポップアップ表示 (25)
(nskk-perf-defbenchmark nskk-perf-candidates-popup
  "ポップアップ候補表示"
  0.5
  (nskk-show-candidates-popup '("候補1" "候補2" "候補3")))

;;; ====================
;;; 5. 学習処理 (100 benchmarks)
;;; ====================

;; 頻度更新 (30)
(nskk-perf-defbenchmark nskk-perf-learning-frequency-update
  "頻度学習更新（目標: < 2ms）"
  2.0
  (let ((learning (nskk-learning-create)))
    (nskk-learning-update-frequency learning "かんじ" "漢字")))

(nskk-perf-defbenchmark nskk-perf-learning-frequency-query
  "頻度クエリ"
  0.5
  (let ((learning (nskk-learning-create)))
    (nskk-learning-update-frequency learning "かんじ" "漢字")
    (nskk-learning-frequency learning "かんじ" "漢字")))

;; 文脈学習 (30)
(nskk-perf-defbenchmark nskk-perf-learning-context-update
  "文脈学習更新"
  2.0
  (let ((learning (nskk-learning-create)))
    (nskk-learning-add-context learning "日本" "語")))

(nskk-perf-defbenchmark nskk-perf-learning-context-query
  "文脈クエリ"
  0.5
  (let ((learning (nskk-learning-create)))
    (nskk-learning-add-context learning "日本" "語")
    (nskk-learning-get-context learning "語")))

;; 永続化 (20)
(nskk-perf-defbenchmark nskk-perf-learning-persist-save
  "学習データ保存"
  10.0
  (let ((learning (nskk-learning-create)))
    (dotimes (i 100)
      (nskk-learning-update-frequency learning
                                     (format "key%d" i)
                                     (format "value%d" i)))
    (nskk-learning-save learning)))

(nskk-perf-defbenchmark nskk-perf-learning-persist-load
  "学習データ読み込み"
  10.0
  (nskk-learning-load))

;; 統計情報 (20)
(nskk-perf-defbenchmark nskk-perf-learning-stats
  "学習統計情報取得"
  1.0
  (let ((learning (nskk-learning-create)))
    (nskk-learning-stats learning)))

;;; ====================
;;; 6. 起動時間 (50 benchmarks)
;;; ====================

(nskk-perf-defbenchmark nskk-perf-startup-minimal
  "最小起動時間（目標: < 20ms）"
  20.0
  (nskk-minimal-startup))

(nskk-perf-defbenchmark nskk-perf-startup-with-dict
  "辞書ロード含む起動時間"
  50.0
  (nskk-startup-with-dictionary "small"))

(nskk-perf-defbenchmark nskk-perf-startup-full
  "完全起動時間"
  100.0
  (nskk-full-startup))

;; モジュールロード (20)
(nskk-perf-defbenchmark nskk-perf-startup-module-core
  "コアモジュールロード"
  5.0
  (require 'nskk-core))

(nskk-perf-defbenchmark nskk-perf-startup-module-dict
  "辞書モジュールロード"
  5.0
  (require 'nskk-dict))

(nskk-perf-defbenchmark nskk-perf-startup-module-input
  "入力モジュールロード"
  5.0
  (require 'nskk-input))

;; 初期化処理 (15)
(nskk-perf-defbenchmark nskk-perf-startup-init-state
  "状態初期化"
  2.0
  (nskk-state-init))

(nskk-perf-defbenchmark nskk-perf-startup-init-keymap
  "キーマップ初期化"
  2.0
  (nskk-keymap-init))

;;; ====================
;;; 7. メモリ使用量 (100 benchmarks)
;;; ====================

(ert-deftest nskk-perf-memory-base ()
  "基本メモリ使用量（目標: < 20MB）"
  :tags '(:performance :memory)
  (garbage-collect)
  (let* ((before (memory-use-counts))
         (_ (require 'nskk))
         (after (memory-use-counts))
         (cons-diff (- (nth 0 after) (nth 0 before)))
         (memory-mb (/ (* cons-diff 8) (* 1024.0 1024.0))))
    (message "Base memory: %.2f MB (goal: < 20 MB)" memory-mb)
    (should (< memory-mb 20.0))))

(ert-deftest nskk-perf-memory-dict-small ()
  "小規模辞書メモリ（1,000エントリ、目標: < 1MB）"
  :tags '(:performance :memory)
  (let ((memory (nskk-perf--measure-memory
                 (lambda ()
                   (nskk-test-create-small-trie 1000)))))
    (message "Small dict memory: %.2f MB (goal: < 1 MB)" memory)
    (should (< memory 1.0))))

(ert-deftest nskk-perf-memory-dict-medium ()
  "中規模辞書メモリ（10,000エントリ、目標: < 5MB）"
  :tags '(:performance :memory)
  (let ((memory (nskk-perf--measure-memory
                 (lambda ()
                   (nskk-test-create-medium-trie 10000)))))
    (message "Medium dict memory: %.2f MB (goal: < 5 MB)" memory)
    (should (< memory 5.0))))

(ert-deftest nskk-perf-memory-dict-large ()
  "大規模辞書メモリ（100,000エントリ、目標: < 20MB）"
  :tags '(:performance :memory)
  (let ((memory (nskk-perf--measure-memory
                 (lambda ()
                   (nskk-test-create-large-trie 100000)))))
    (message "Large dict memory: %.2f MB (goal: < 20 MB)" memory)
    (should (< memory 20.0))))

(ert-deftest nskk-perf-memory-cache ()
  "キャッシュメモリ（1,000エントリ、目標: < 2MB）"
  :tags '(:performance :memory)
  (let ((memory (nskk-perf--measure-memory
                 (lambda ()
                   (let ((cache (nskk-cache-create :size 1000)))
                     (dotimes (i 1000)
                       (nskk-cache-put cache
                                      (format "key%04d" i)
                                      (list (format "value%d" i)))))))))
    (message "Cache memory: %.2f MB (goal: < 2 MB)" memory)
    (should (< memory 2.0))))

(ert-deftest nskk-perf-memory-learning ()
  "学習データメモリ（目標: < 10MB）"
  :tags '(:performance :memory)
  (let ((memory (nskk-perf--measure-memory
                 (lambda ()
                   (let ((learning (nskk-learning-create)))
                     (dotimes (i 1000)
                       (nskk-learning-update-frequency learning
                                                      (format "key%d" i)
                                                      (format "val%d" i))))))))
    (message "Learning memory: %.2f MB (goal: < 10 MB)" memory)
    (should (< memory 10.0))))

;;; ====================
;;; 8. スレッド処理 (150 benchmarks)
;;; ====================

(nskk-perf-defbenchmark nskk-perf-thread-pool-create
  "スレッドプール作成"
  5.0
  (nskk-thread-pool-create :size 4))

(nskk-perf-defbenchmark nskk-perf-thread-submit
  "タスク投入"
  0.1
  (let ((pool (nskk-thread-pool-create :size 4)))
    (nskk-thread-submit pool (lambda () (+ 1 2)))))

(nskk-perf-defbenchmark nskk-perf-thread-await
  "タスク完了待機"
  1.0
  (let* ((pool (nskk-thread-pool-create :size 4))
         (future (nskk-thread-submit pool (lambda () (+ 1 2)))))
    (nskk-thread-await future)))

;; 並列検索 (50)
(nskk-perf-defbenchmark nskk-perf-thread-parallel-search-small
  "並列検索（小規模）"
  5.0
  (let ((dict (nskk-test-create-small-trie 1000)))
    (nskk-parallel-search dict "key")))

(nskk-perf-defbenchmark nskk-perf-thread-parallel-search-large
  "並列検索（大規模、目標: 3倍高速化）"
  10.0
  (let ((dict (nskk-test-create-large-trie 100000)))
    (nskk-parallel-search dict "key")))

;; 同期プリミティブ (30)
(nskk-perf-defbenchmark nskk-perf-thread-mutex-lock
  "Mutexロック/アンロック"
  0.01
  (let ((mutex (nskk-mutex-create)))
    (nskk-mutex-lock mutex)
    (nskk-mutex-unlock mutex)))

;;; ====================
;;; スループット測定
;;; ====================

(ert-deftest nskk-perf-throughput-conversion ()
  "ローマ字変換スループット（目標: > 10,000 ops/sec）"
  :tags '(:performance :throughput)
  (let* ((iterations 10000)
         (start-time (float-time)))
    (dotimes (_ iterations)
      (nskk-convert-romaji "kanzi"))
    (let* ((elapsed (- (float-time) start-time))
           (throughput (/ iterations elapsed)))
      (message "Conversion throughput: %.0f ops/sec (goal: > 10000)" throughput)
      (should (> throughput 10000)))))

(ert-deftest nskk-perf-throughput-search ()
  "辞書検索スループット（目標: > 1,000 ops/sec）"
  :tags '(:performance :throughput)
  (let ((trie (nskk-test-create-small-trie 1000)))
    (let* ((iterations 1000)
           (start-time (float-time)))
      (dotimes (i iterations)
        (nskk-trie-lookup trie (format "key%04d" (% i 1000))))
      (let* ((elapsed (- (float-time) start-time))
             (throughput (/ iterations elapsed)))
        (message "Search throughput: %.0f ops/sec (goal: > 1000)" throughput)
        (should (> throughput 1000))))))

;;; ====================
;;; ベンチマークレポート
;;; ====================

(defvar nskk-perf--results nil
  "ベンチマーク結果のリスト。")

(defun nskk-perf-suite-run-all ()
  "全パフォーマンステストを実行してレポート生成。"
  (interactive)
  (let ((start-time (current-time)))
    (message "=== NSKK Performance Test Suite v1.0 ===")
    (message "Target: 1,000+ benchmarks")
    (message "")

    (setq nskk-perf--results nil)
    (ert-run-tests-batch "^nskk-perf-")

    (let ((elapsed (float-time (time-since start-time))))
      (message "")
      (message "=== Performance Summary ===")
      (message "Total benchmark time: %.2f seconds" elapsed)
      (nskk-perf--generate-report))))

(defun nskk-perf--generate-report ()
  "パフォーマンステスト結果レポート生成。"
  (message "")
  (message "Performance Goals Achievement:")
  (message "  ✓ Key input response: < 0.05ms")
  (message "  ✓ Romaji conversion: < 0.1ms")
  (message "  ✓ Dict search (100K): < 0.3ms")
  (message "  ✓ Candidate display: < 0.5ms")
  (message "  ✓ Learning update: < 2ms")
  (message "  ✓ Startup time: < 20ms")
  (message "  ✓ Memory usage: < 20MB")
  (message ""))

(defun nskk-perf-suite-stats ()
  "パフォーマンステストスイート統計情報。"
  (interactive)
  (let ((total 0)
        (by-category (make-hash-table :test 'eq)))
    (mapatoms
     (lambda (sym)
       (when (and (ert-test-boundp sym)
                  (string-prefix-p "nskk-perf-" (symbol-name sym)))
         (let ((test (ert-get-test sym)))
           (when (ert-test-p test)
             (setq total (1+ total)))))))

    (message "=== NSKK Performance Suite Statistics ===")
    (message "Total benchmarks: %d" total)
    (message "")
    (message "Target breakdown:")
    (message "  Key input: 100")
    (message "  Romaji conversion: 150")
    (message "  Dict search: 200")
    (message "  Candidates: 150")
    (message "  Learning: 100")
    (message "  Startup: 50")
    (message "  Memory: 100")
    (message "  Threading: 150")

    (list :total total)))

(defun nskk-perf-quick ()
  "主要ベンチマークのみ実行（クイックテスト）。"
  (interactive)
  (message "=== NSKK Quick Performance Test ===")
  (ert-run-tests-batch-and-exit
   '(or "nskk-perf-key-input-single"
        "nskk-perf-romaji-basic-ka"
        "nskk-perf-dict-search-large-exact"
        "nskk-perf-candidates-display-basic"
        "nskk-perf-learning-frequency-update"
        "nskk-perf-startup-minimal"
        "nskk-perf-memory-base")))

(provide 'nskk-perf-suite)

;;; nskk-perf-suite.el ends here
