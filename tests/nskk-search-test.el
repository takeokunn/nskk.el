;;; nskk-search-test.el --- Tests for NSKK search algorithms -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, dictionary, search, test

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

;; Task 1.18: 検索アルゴリズム実装のテストスイート
;;
;; テスト項目:
;; - 完全一致検索（存在するキー、存在しないキー）
;; - 前方一致検索（複数結果、結果なし）
;; - 部分一致検索（中間一致、後方一致）
;; - ファジー検索（類似キー）
;; - 送り仮名タイプ指定
;; - パフォーマンステスト
;; - トライ木統合後の互換性テスト

;;; Code:

(require 'ert)
(require 'nskk-search)
(require 'nskk-dict-struct)
(require 'nskk-trie)

;;; テストフィクスチャ

(defvar nskk-search-test--sample-index nil
  "テスト用サンプルインデックス。")

(defun nskk-search-test--create-sample-index ()
  "テスト用のサンプル辞書インデックスを作成する。"
  (let ((okuri-ari-table (make-hash-table :test 'equal))
        (okuri-nasi-table (make-hash-table :test 'equal))
        (trie-ari (nskk-trie-create))
        (trie-nasi (nskk-trie-create)))

    ;; 送り仮名なしエントリ
    (let ((entries '(("かんじ" ("漢字" "感じ"))
                     ("かんたん" ("簡単"))
                     ("かん" ("缶" "感"))
                     ("かんき" ("換気"))
                     ("にほん" ("日本"))
                     ("にほんご" ("日本語"))
                     ("じしょ" ("辞書"))
                     ("けんさく" ("検索")))))
      (dolist (entry-data entries)
        (let* ((midashi (car entry-data))
               (words (cadr entry-data))
               (candidates (mapcar (lambda (word)
                                    (nskk-dict-candidate-create word))
                                  words))
               (entry (nskk-dict-entry-create midashi candidates 'okuri-nasi)))
          (puthash midashi entry okuri-nasi-table)
          (nskk-trie-insert trie-nasi midashi entry))))

    ;; 送り仮名ありエントリ
    (let ((entries '(("おくr" ("送"))
                     ("かんがえr" ("考"))
                     ("かえr" ("変")))))
      (dolist (entry-data entries)
        (let* ((midashi (car entry-data))
               (words (cadr entry-data))
               (candidates (mapcar (lambda (word)
                                    (nskk-dict-candidate-create word))
                                  words))
               (entry (nskk-dict-entry-create midashi candidates 'okuri-ari)))
          (puthash midashi entry okuri-ari-table)
          (nskk-trie-insert trie-ari midashi entry))))

    ;; インデックスを作成
    (nskk-dict-index--create
     :okuri-ari-table okuri-ari-table
     :okuri-nasi-table okuri-nasi-table
     :trie-ari trie-ari
     :trie-nasi trie-nasi)))

(defun nskk-search-test--setup ()
  "テストのセットアップ。"
  (setq nskk-search-test--sample-index (nskk-search-test--create-sample-index)))

(defun nskk-search-test--teardown ()
  "テストのクリーンアップ。"
  (setq nskk-search-test--sample-index nil))

;;; 完全一致検索テスト

(ert-deftest nskk-search-test-exact-existing-key ()
  "完全一致検索: 存在するキーを検索できる。"
  (nskk-search-test--setup)
  (let ((result (nskk-search nskk-search-test--sample-index "かんじ" 'exact)))
    (should (nskk-dict-entry-p result))
    (should (equal (nskk-dict-entry-midashi result) "かんじ"))
    (should (equal (nskk-dict-candidate-word
                   (car (nskk-dict-entry-candidates result)))
                  "漢字")))
  (nskk-search-test--teardown))

(ert-deftest nskk-search-test-exact-nonexistent-key ()
  "完全一致検索: 存在しないキーはnilを返す。"
  (nskk-search-test--setup)
  (let ((result (nskk-search nskk-search-test--sample-index "ふそんざい" 'exact)))
    (should (null result)))
  (nskk-search-test--teardown))

(ert-deftest nskk-search-test-exact-okuri-ari ()
  "完全一致検索: 送り仮名ありエントリを検索できる。"
  (nskk-search-test--setup)
  (let ((result (nskk-search nskk-search-test--sample-index "おくr" 'exact 'okuri-ari)))
    (should (nskk-dict-entry-p result))
    (should (equal (nskk-dict-entry-midashi result) "おくr"))
    (should (eq (nskk-dict-entry-okuri-type result) 'okuri-ari)))
  (nskk-search-test--teardown))

(ert-deftest nskk-search-test-exact-okuri-nasi ()
  "完全一致検索: 送り仮名なしエントリを検索できる。"
  (nskk-search-test--setup)
  (let ((result (nskk-search nskk-search-test--sample-index "かんじ" 'exact 'okuri-nasi)))
    (should (nskk-dict-entry-p result))
    (should (equal (nskk-dict-entry-midashi result) "かんじ"))
    (should (eq (nskk-dict-entry-okuri-type result) 'okuri-nasi)))
  (nskk-search-test--teardown))

;;; 前方一致検索テスト

(ert-deftest nskk-search-test-prefix-multiple-results ()
  "前方一致検索: 複数の結果を取得できる。"
  (nskk-search-test--setup)
  (let ((results (nskk-search nskk-search-test--sample-index "かん" 'prefix)))
    (should (listp results))
    (should (>= (length results) 3))
    ;; 全ての結果が「かん」で始まるか確認
    (dolist (result results)
      (should (string-prefix-p "かん" (car result)))))
  (nskk-search-test--teardown))

(ert-deftest nskk-search-test-prefix-no-results ()
  "前方一致検索: 結果がない場合は空リストを返す。"
  (nskk-search-test--setup)
  (let ((results (nskk-search nskk-search-test--sample-index "ふそんざい" 'prefix)))
    (should (null results)))
  (nskk-search-test--teardown))

(ert-deftest nskk-search-test-prefix-with-limit ()
  "前方一致検索: limitを指定すると結果が制限される。"
  (nskk-search-test--setup)
  (let ((results (nskk-search nskk-search-test--sample-index "かん" 'prefix nil 2)))
    (should (listp results))
    (should (<= (length results) 2)))
  (nskk-search-test--teardown))

(ert-deftest nskk-search-test-prefix-okuri-type ()
  "前方一致検索: 送り仮名タイプを指定できる。"
  (nskk-search-test--setup)
  ;; 送り仮名なしのみ
  (let ((results (nskk-search nskk-search-test--sample-index "かん" 'prefix 'okuri-nasi)))
    (should (listp results))
    (dolist (result results)
      (should (eq (nskk-dict-entry-okuri-type (cdr result)) 'okuri-nasi))))
  ;; 送り仮名ありのみ
  (let ((results (nskk-search nskk-search-test--sample-index "かん" 'prefix 'okuri-ari)))
    (should (listp results))
    (dolist (result results)
      (should (eq (nskk-dict-entry-okuri-type (cdr result)) 'okuri-ari))))
  (nskk-search-test--teardown))

;;; 部分一致検索テスト

(ert-deftest nskk-search-test-partial-middle-match ()
  "部分一致検索: 中間一致を検索できる。"
  (nskk-search-test--setup)
  (let ((results (nskk-search nskk-search-test--sample-index "ほん" 'partial)))
    (should (listp results))
    ;; 「にほん」「にほんご」が含まれるべき
    (should (cl-some (lambda (r) (equal (car r) "にほん")) results))
    (should (cl-some (lambda (r) (equal (car r) "にほんご")) results)))
  (nskk-search-test--teardown))

(ert-deftest nskk-search-test-partial-end-match ()
  "部分一致検索: 後方一致を検索できる。"
  (nskk-search-test--setup)
  (let ((results (nskk-search nskk-search-test--sample-index "ご" 'partial)))
    (should (listp results))
    ;; 「にほんご」が含まれるべき
    (should (cl-some (lambda (r) (equal (car r) "にほんご")) results)))
  (nskk-search-test--teardown))

(ert-deftest nskk-search-test-partial-with-limit ()
  "部分一致検索: limitを指定すると結果が制限される。"
  (nskk-search-test--setup)
  (let ((results (nskk-search nskk-search-test--sample-index "ん" 'partial nil 3)))
    (should (listp results))
    (should (<= (length results) 3)))
  (nskk-search-test--teardown))

;;; ファジー検索テスト

(ert-deftest nskk-search-test-fuzzy-similar-keys ()
  "ファジー検索: 類似キーを検索できる。"
  (nskk-search-test--setup)
  ;; 「かんじ」に対して「かんき」は距離2（j→k、削除）
  (let ((results (nskk-search nskk-search-test--sample-index "かんじ" 'fuzzy)))
    (should (listp results))
    ;; 完全一致（距離0）が含まれる
    (should (cl-some (lambda (r) (equal (car r) "かんじ")) results))
    ;; 類似キーが含まれる可能性がある
    (when (> nskk-search-fuzzy-threshold 1)
      (should (cl-some (lambda (r) (equal (car r) "かんき")) results))))
  (nskk-search-test--teardown))

(ert-deftest nskk-search-test-fuzzy-with-limit ()
  "ファジー検索: limitを指定すると結果が制限される。"
  (nskk-search-test--setup)
  (let ((results (nskk-search nskk-search-test--sample-index "かん" 'fuzzy nil 2)))
    (should (listp results))
    (should (<= (length results) 2)))
  (nskk-search-test--teardown))

(ert-deftest nskk-search-test-fuzzy-distance-sorted ()
  "ファジー検索: 結果が距離順にソートされる。"
  (nskk-search-test--setup)
  (let ((results (nskk-search nskk-search-test--sample-index "かん" 'fuzzy)))
    (should (listp results))
    ;; 距離が昇順になっているか確認
    (let ((prev-distance -1))
      (dolist (result results)
        (let ((distance (cddr result)))
          (should (>= distance prev-distance))
          (setq prev-distance distance)))))
  (nskk-search-test--teardown))

;;; エラーハンドリングテスト

(ert-deftest nskk-search-test-empty-query ()
  "エラーハンドリング: 空クエリでエラーをシグナルする。"
  (nskk-search-test--setup)
  (should-error (nskk-search nskk-search-test--sample-index "" 'exact)
                :type 'nskk-dict-search-invalid-query)
  (nskk-search-test--teardown))

(ert-deftest nskk-search-test-invalid-index ()
  "エラーハンドリング: 無効なインデックスでエラーをシグナルする。"
  (should-error (nskk-search "invalid-index" "かんじ" 'exact)
                :type 'nskk-dict-search-invalid-index))

(ert-deftest nskk-search-test-invalid-search-type ()
  "エラーハンドリング: 無効な検索タイプでエラーをシグナルする。"
  (nskk-search-test--setup)
  (should-error (nskk-search nskk-search-test--sample-index "かんじ" 'invalid)
                :type 'nskk-dict-search-invalid-query)
  (nskk-search-test--teardown))

;;; Levenshtein距離テスト

(ert-deftest nskk-search-test-levenshtein-identical ()
  "Levenshtein距離: 同一文字列の距離は0。"
  (should (= (nskk-search--levenshtein-distance "かんじ" "かんじ") 0)))

(ert-deftest nskk-search-test-levenshtein-insertion ()
  "Levenshtein距離: 挿入操作を正しく計算する。"
  (should (= (nskk-search--levenshtein-distance "かん" "かんじ") 1)))

(ert-deftest nskk-search-test-levenshtein-deletion ()
  "Levenshtein距離: 削除操作を正しく計算する。"
  (should (= (nskk-search--levenshtein-distance "かんじ" "かん") 1)))

(ert-deftest nskk-search-test-levenshtein-substitution ()
  "Levenshtein距離: 置換操作を正しく計算する。"
  (should (= (nskk-search--levenshtein-distance "かんじ" "かんき") 1)))

(ert-deftest nskk-search-test-levenshtein-multiple ()
  "Levenshtein距離: 複数の操作を正しく計算する。"
  (should (= (nskk-search--levenshtein-distance "かんじ" "けんさく") 3)))

;;; ソート機能テスト

(ert-deftest nskk-search-test-sort-by-frequency ()
  "ソート: 頻度順ソートが正しく動作する。"
  (nskk-search-test--setup)
  ;; 頻度を設定
  (let ((entry1 (gethash "かんじ" (nskk-dict-index-okuri-nasi-table
                                   nskk-search-test--sample-index)))
        (entry2 (gethash "かんたん" (nskk-dict-index-okuri-nasi-table
                                    nskk-search-test--sample-index))))
    (setf (nskk-dict-entry-frequency entry1) 10)
    (setf (nskk-dict-entry-frequency entry2) 5)

    ;; 頻度順ソート
    (let* ((nskk-search-sort-method 'frequency)
           (results (nskk-search nskk-search-test--sample-index "かん" 'prefix)))
      ;; かんじ（頻度10）がかんたん（頻度5）より前に来る
      (should (string= (car (car results)) "かんじ"))))
  (nskk-search-test--teardown))

(ert-deftest nskk-search-test-sort-by-kana ()
  "ソート: 五十音順ソートが正しく動作する。"
  (nskk-search-test--setup)
  (let* ((nskk-search-sort-method 'kana)
         (results (nskk-search nskk-search-test--sample-index "かん" 'prefix)))
    ;; 五十音順になっているか確認
    (let ((prev-key ""))
      (dolist (result results)
        (should (string< prev-key (car result)))
        (setq prev-key (car result)))))
  (nskk-search-test--teardown))

;;; トライ木統合テスト

(ert-deftest nskk-search-test-trie-integration ()
  "トライ木統合: トライ木が正しく構築されている。"
  (nskk-search-test--setup)
  (let ((trie-nasi (nskk-dict-index-trie-nasi nskk-search-test--sample-index)))
    (should (nskk-trie-p trie-nasi))
    ;; トライ木にエントリが含まれているか確認
    (should (nskk-trie-has-key-p trie-nasi "かんじ"))
    (should (nskk-trie-has-key-p trie-nasi "にほん")))
  (nskk-search-test--teardown))

(ert-deftest nskk-search-test-trie-prefix-search ()
  "トライ木統合: トライ木を使った前方一致検索が動作する。"
  (nskk-search-test--setup)
  (let* ((trie-nasi (nskk-dict-index-trie-nasi nskk-search-test--sample-index))
         (results (nskk-trie-prefix-search trie-nasi "かん")))
    (should (listp results))
    (should (>= (length results) 3))
    ;; 全ての結果が「かん」で始まるか確認
    (dolist (result results)
      (should (string-prefix-p "かん" (car result)))))
  (nskk-search-test--teardown))

;;; パフォーマンステスト

(ert-deftest nskk-search-test-performance-exact ()
  "パフォーマンス: 完全一致検索が目標時間内に完了する（< 0.1ms）。"
  (nskk-search-test--setup)
  (let ((start-time (float-time))
        (iterations 100))
    (dotimes (_ iterations)
      (nskk-search nskk-search-test--sample-index "かんじ" 'exact))
    (let* ((elapsed (- (float-time) start-time))
           (avg-time (/ elapsed iterations)))
      ;; 平均で0.1ms以下
      (should (< avg-time 0.0001))
      (message "Exact search avg time: %.6f ms" (* avg-time 1000))))
  (nskk-search-test--teardown))

(ert-deftest nskk-search-test-performance-prefix ()
  "パフォーマンス: 前方一致検索が妥当な時間で完了する。"
  (nskk-search-test--setup)
  (let ((start-time (float-time))
        (iterations 100))
    (dotimes (_ iterations)
      (nskk-search nskk-search-test--sample-index "かん" 'prefix))
    (let* ((elapsed (- (float-time) start-time))
           (avg-time (/ elapsed iterations)))
      ;; 小規模データセットでは非常に高速であるべき
      (should (< avg-time 0.001))
      (message "Prefix search avg time: %.6f ms" (* avg-time 1000))))
  (nskk-search-test--teardown))

;;; ユーティリティ関数テスト

(ert-deftest nskk-search-test-entry-count ()
  "ユーティリティ: エントリ数カウントが正しく動作する。"
  (nskk-search-test--setup)
  ;; 全エントリ数
  (let ((total-count (nskk-search-entry-count nskk-search-test--sample-index)))
    (should (> total-count 0)))
  ;; 送り仮名なしのみ
  (let ((nasi-count (nskk-search-entry-count nskk-search-test--sample-index
                                             nil nil 'okuri-nasi)))
    (should (> nasi-count 0)))
  ;; 送り仮名ありのみ
  (let ((ari-count (nskk-search-entry-count nskk-search-test--sample-index
                                           nil nil 'okuri-ari)))
    (should (> ari-count 0)))
  (nskk-search-test--teardown))

(provide 'nskk-search-test)

;;; nskk-search-test.el ends here
