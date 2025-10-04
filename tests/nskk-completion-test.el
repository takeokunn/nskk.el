;;; nskk-completion-test.el --- Tests for NSKK completion modules -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, completion, test

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

;; このファイルはNSKKの補完機能のテストスイートです。
;;
;; テスト対象モジュール:
;; - nskk-completion-prefix (Task 2.21)
;; - nskk-completion-fuzzy (Task 2.22)
;; - nskk-completion-frequency (Task 2.23)
;; - nskk-completion-context (Task 2.24)
;; - nskk-completion-predictive (Task 2.25)
;; - nskk-completion-engine (Task 2.26)
;; - nskk-completion-ui (Task 2.27)

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nskk-trie)
(require 'nskk-dict-struct)
(require 'nskk-completion-prefix)
(require 'nskk-completion-fuzzy)
(require 'nskk-completion-frequency)
(require 'nskk-completion-context)
(require 'nskk-completion-predictive)
(require 'nskk-completion-engine)
(require 'nskk-completion-ui)

;;; テストヘルパー関数

(defun nskk-completion-test--create-sample-index ()
  "テスト用のサンプル辞書インデックスを作成する。"
  (let ((okuri-nasi-table (make-hash-table :test 'equal))
        (okuri-ari-table (make-hash-table :test 'equal))
        (trie-nasi (nskk-trie-create))
        (trie-ari (nskk-trie-create)))

    ;; サンプルエントリを作成
    (let ((entries '(("かん" ("缶" "感" "寒") 5)
                    ("かんじ" ("漢字" "感じ") 10)
                    ("かんたん" ("簡単") 8)
                    ("かんが" ("考" "感") 3)
                    ("あい" ("愛" "相") 15)
                    ("あいさつ" ("挨拶") 12))))
      (dolist (entry-data entries)
        (let* ((midashi (nth 0 entry-data))
               (candidates-words (nth 1 entry-data))
               (freq (nth 2 entry-data))
               (candidates (mapcar (lambda (word)
                                   (nskk-dict-candidate-create word))
                                  candidates-words))
               (entry (nskk-dict-entry--create
                      :midashi midashi
                      :candidates candidates
                      :frequency freq
                      :okuri-type 'okuri-nasi)))
          (puthash midashi entry okuri-nasi-table)
          (nskk-trie-insert trie-nasi midashi entry))))

    ;; インデックスを作成
    (nskk-dict-index--create
     :okuri-ari-table okuri-ari-table
     :okuri-nasi-table okuri-nasi-table
     :trie-ari trie-ari
     :trie-nasi trie-nasi)))

;;; Task 2.21: 前方一致補完テスト

(ert-deftest nskk-completion-test-prefix-basic ()
  "前方一致補完の基本動作をテストする。"
  (let ((index (nskk-completion-test--create-sample-index)))
    (let ((results (nskk-completion-prefix-search index "かん")))
      (should (> (length results) 0))
      ;; 「かん」で始まる候補が返される
      (dolist (result results)
        (should (string-prefix-p "かん"
                                (nskk-completion-prefix-result-midashi result)))))))

(ert-deftest nskk-completion-test-prefix-limit ()
  "前方一致補完のlimit機能をテストする。"
  (let ((index (nskk-completion-test--create-sample-index)))
    (let ((results (nskk-completion-prefix-search index "かん" :limit 2)))
      (should (<= (length results) 2)))))

;;; Task 2.22: 曖昧補完テスト

(ert-deftest nskk-completion-test-fuzzy-basic ()
  "曖昧補完の基本動作をテストする。"
  (let ((index (nskk-completion-test--create-sample-index)))
    (let ((results (nskk-completion-fuzzy-search index "かんし")))
      (should (>= (length results) 0))
      ;; 「かんじ」が含まれるはず（距離1）
      (should (seq-find (lambda (r)
                         (equal (nskk-completion-fuzzy-result-midashi r) "かんじ"))
                       results)))))

(ert-deftest nskk-completion-test-fuzzy-distance ()
  "曖昧補完の距離計算をテストする。"
  (let ((index (nskk-completion-test--create-sample-index)))
    (let ((results (nskk-completion-fuzzy-search index "あいさ" :threshold 2)))
      (should (> (length results) 0))
      ;; すべての結果の距離が閾値以下
      (dolist (result results)
        (should (<= (nskk-completion-fuzzy-result-distance result) 2))))))

;;; Task 2.23: 頻度ベース補完テスト

(ert-deftest nskk-completion-test-frequency-basic ()
  "頻度ベース補完の基本動作をテストする。"
  (let ((index (nskk-completion-test--create-sample-index)))
    (let ((results (nskk-completion-frequency-search index "か")))
      (should (> (length results) 0))
      ;; 頻度順にソートされている
      (let ((freqs (mapcar #'nskk-completion-frequency-result-adjusted-freq results)))
        (should (equal freqs (sort (copy-sequence freqs) #'>)))))))

(ert-deftest nskk-completion-test-frequency-update ()
  "頻度統計の更新をテストする。"
  (let ((index (nskk-completion-test--create-sample-index)))
    (let ((entry-before (nskk-dict-struct-lookup index "かんじ")))
      (let ((freq-before (nskk-dict-entry-frequency entry-before)))
        (nskk-completion-frequency-update-stats index "かんじ")
        (let ((entry-after (nskk-dict-struct-lookup index "かんじ")))
          (should (= (nskk-dict-entry-frequency entry-after) (1+ freq-before))))))))

;;; Task 2.24: 文脈補完テスト

(ert-deftest nskk-completion-test-context-model ()
  "文脈モデルの作成と学習をテストする。"
  (let ((model (nskk-completion-context-model-create)))
    (should (nskk-completion-context-model-p model))
    ;; 学習
    (nskk-completion-context-learn model '("これは" "漢字" "です"))
    (should (> (nskk-completion-context-model-total-count model) 0))))

(ert-deftest nskk-completion-test-context-search ()
  "文脈補完検索をテストする。"
  (let ((index (nskk-completion-test--create-sample-index))
        (model (nskk-completion-context-model-create)))
    ;; モデルを学習
    (nskk-completion-context-learn model '("これは" "漢字"))
    ;; 検索
    (let ((results (nskk-completion-context-search
                   index "かん"
                   :context '("これは")
                   :model model)))
      (should (listp results)))))

;;; Task 2.25: 予測補完テスト

(ert-deftest nskk-completion-test-predictive-model ()
  "予測モデルの作成と学習をテストする。"
  (let ((model (nskk-completion-predictive-model-create)))
    (should (nskk-completion-predictive-model-p model))
    ;; 学習
    (nskk-completion-predictive-learn model '("これは" "便利" "です"))
    (should (> (nskk-completion-predictive-model-total-transitions model) 0))))

(ert-deftest nskk-completion-test-predictive-predict ()
  "予測機能をテストする。"
  (let ((model (nskk-completion-predictive-model-create)))
    ;; モデルを学習
    (nskk-completion-predictive-learn model '("これは" "便利" "です"))
    (nskk-completion-predictive-learn model '("これは" "簡単" "です"))
    ;; 予測
    (let ((predictions (nskk-completion-predictive-predict-next
                       model '("これは"))))
      (should (listp predictions)))))

;;; Task 2.26: 補完統合エンジンテスト

(ert-deftest nskk-completion-test-engine-basic ()
  "補完統合エンジンの基本動作をテストする。"
  (let ((index (nskk-completion-test--create-sample-index)))
    (let ((results (nskk-completion-engine-search index "かん")))
      (should (> (length results) 0))
      ;; すべての結果がnskk-completion-engine-result構造体
      (dolist (result results)
        (should (nskk-completion-engine-result-p result))))))

(ert-deftest nskk-completion-test-engine-algorithms ()
  "補完エンジンのアルゴリズム選択をテストする。"
  (let ((index (nskk-completion-test--create-sample-index)))
    ;; prefixのみ
    (let ((results (nskk-completion-engine-search index "かん"
                                                 :algorithms '(prefix))))
      (should (> (length results) 0)))
    ;; 複数アルゴリズム
    (let ((results (nskk-completion-engine-search index "かん"
                                                 :algorithms '(prefix frequency))))
      (should (> (length results) 0)))))

;;; Task 2.27: 補完UIテスト

(ert-deftest nskk-completion-test-ui-visibility ()
  "補完UIの表示/非表示をテストする。"
  (let ((index (nskk-completion-test--create-sample-index)))
    (let ((results (nskk-completion-engine-search index "かん")))
      ;; 表示
      (nskk-completion-ui-show results 'inline)
      (should (nskk-completion-ui-visible-p))
      ;; 非表示
      (nskk-completion-ui-hide)
      (should-not (nskk-completion-ui-visible-p)))))

(ert-deftest nskk-completion-test-ui-navigation ()
  "補完UIのナビゲーションをテストする。"
  (let ((index (nskk-completion-test--create-sample-index)))
    (let ((results (nskk-completion-engine-search index "かん")))
      (nskk-completion-ui-show results)
      ;; 次へ
      (nskk-completion-ui-next)
      (should (= nskk-completion-ui--current-index 1))
      ;; 前へ
      (nskk-completion-ui-previous)
      (should (= nskk-completion-ui--current-index 0))
      (nskk-completion-ui-hide))))

;;; 統合テスト

(ert-deftest nskk-completion-test-integration ()
  "補完機能全体の統合テスト。"
  (let ((index (nskk-completion-test--create-sample-index))
        (ctx-model (nskk-completion-context-model-create))
        (pred-model (nskk-completion-predictive-model-create)))

    ;; モデル学習
    (nskk-completion-context-learn ctx-model '("これは" "漢字" "です"))
    (nskk-completion-predictive-learn pred-model '("これは" "便利" "です"))

    ;; 統合検索
    (let ((results (nskk-completion-engine-search
                   index "かん"
                   :context '("これは")
                   :context-model ctx-model
                   :pred-model pred-model
                   :algorithms '(prefix frequency context))))
      (should (> (length results) 0))

      ;; エントリを設定
      (nskk-completion-engine-populate-entries index results)

      ;; UI表示
      (nskk-completion-ui-show results)
      (should (nskk-completion-ui-visible-p))

      ;; クリーンアップ
      (nskk-completion-ui-hide))))

;;; パフォーマンステスト

(ert-deftest nskk-completion-test-performance ()
  "補完機能のパフォーマンステスト。"
  :tags '(:performance)
  (let ((index (nskk-completion-test--create-sample-index)))
    (let ((start-time (float-time))
          (iterations 100))

      ;; 100回実行
      (dotimes (_ iterations)
        (nskk-completion-engine-search index "かん"))

      (let ((elapsed (- (float-time) start-time)))
        (message "補完性能: %d回実行, %.3f秒, 平均%.3fms"
                iterations elapsed (* (/ elapsed iterations) 1000))

        ;; 1回あたり20ms以下であることを確認（目標）
        (should (< (/ elapsed iterations) 0.020))))))

(provide 'nskk-completion-test)

;;; nskk-completion-test.el ends here
