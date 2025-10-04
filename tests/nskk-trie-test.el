;;; nskk-trie-test.el --- Tests for nskk-trie -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, dictionary, trie, test

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

;; このファイルは nskk-trie.el のテストスイートです。
;;
;; テスト項目:
;; - トライ木の基本操作（作成、挿入、検索、削除）
;; - 前方一致検索
;; - エッジケース（空文字列、長いキー、Unicode）
;; - パフォーマンス測定
;; - シリアライズ/デシリアライズ
;; - 統計情報

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nskk-trie)

;;; テストヘルパー関数

(defun nskk-trie-test--create-sample-trie ()
  "テスト用のサンプルトライ木を作成する。"
  (let ((trie (nskk-trie-create)))
    (nskk-trie-insert trie "かんじ" "漢字")
    (nskk-trie-insert trie "かん" "缶")
    (nskk-trie-insert trie "かんたん" "簡単")
    (nskk-trie-insert trie "あい" "愛")
    (nskk-trie-insert trie "あいさつ" "挨拶")
    (nskk-trie-insert trie "わたし" "私")
    trie))

(defun nskk-trie-test--create-large-trie (count)
  "大規模テスト用のトライ木を作成する。

引数:
  COUNT - 挿入するキーの数

戻り値:
  nskk-trie構造体"
  (let ((trie (nskk-trie-create)))
    (dotimes (i count)
      (nskk-trie-insert trie (format "key%05d" i) (format "value%d" i)))
    trie))

;;; 基本操作テスト

(ert-deftest nskk-trie-test-create ()
  "トライ木の作成をテストする。"
  (let ((trie (nskk-trie-create)))
    (should (nskk-trie-p trie))
    (should (nskk-trie-node-p (nskk-trie-root trie)))
    (should (= 0 (nskk-trie-size trie)))
    (should (nskk-trie-empty-p trie))))

(ert-deftest nskk-trie-test-insert-single ()
  "単一キーの挿入をテストする。"
  (let ((trie (nskk-trie-create)))
    ;; 1文字キー
    (nskk-trie-insert trie "a" "value-a")
    (should (= 1 (nskk-trie-size trie)))
    (should (not (nskk-trie-empty-p trie)))

    ;; 複数文字キー
    (nskk-trie-insert trie "abc" "value-abc")
    (should (= 2 (nskk-trie-size trie)))

    ;; 日本語キー
    (nskk-trie-insert trie "かんじ" "漢字")
    (should (= 3 (nskk-trie-size trie)))))

(ert-deftest nskk-trie-test-insert-duplicate ()
  "重複キーの挿入をテストする（上書き）。"
  (let ((trie (nskk-trie-create)))
    (nskk-trie-insert trie "key" "value1")
    (should (= 1 (nskk-trie-size trie)))

    ;; 同じキーを再挿入（上書き）
    (nskk-trie-insert trie "key" "value2")
    (should (= 1 (nskk-trie-size trie)))

    ;; 値が更新されているか確認
    (let ((result (nskk-trie-lookup trie "key")))
      (should (cdr result))
      (should (equal "value2" (car result))))))

(ert-deftest nskk-trie-test-insert-prefix-keys ()
  "プレフィックス関係にあるキーの挿入をテストする。"
  (let ((trie (nskk-trie-create)))
    (nskk-trie-insert trie "かん" "缶")
    (nskk-trie-insert trie "かんじ" "漢字")
    (nskk-trie-insert trie "かんたん" "簡単")

    (should (= 3 (nskk-trie-size trie)))

    ;; すべてのキーが検索できることを確認
    (should (nskk-trie-has-key-p trie "かん"))
    (should (nskk-trie-has-key-p trie "かんじ"))
    (should (nskk-trie-has-key-p trie "かんたん"))))

(ert-deftest nskk-trie-test-insert-errors ()
  "不正な挿入パラメータのエラーハンドリングをテストする。"
  (let ((trie (nskk-trie-create)))
    ;; 空文字列キー
    (should-error (nskk-trie-insert trie "" "value"))

    ;; 非文字列キー
    (should-error (nskk-trie-insert trie 123 "value"))
    (should-error (nskk-trie-insert trie nil "value"))))

;;; 検索テスト

(ert-deftest nskk-trie-test-lookup ()
  "完全一致検索をテストする。"
  (let ((trie (nskk-trie-test--create-sample-trie)))

    ;; 存在するキー
    (let ((result (nskk-trie-lookup trie "かんじ")))
      (should (cdr result))
      (should (equal "漢字" (car result))))

    ;; 存在しないキー
    (let ((result (nskk-trie-lookup trie "zzz")))
      (should (not (cdr result)))
      (should (null (car result))))

    ;; プレフィックスのみ存在（完全一致しない）
    (let ((result (nskk-trie-lookup trie "かんた")))
      (should (not (cdr result))))))

(ert-deftest nskk-trie-test-has-key-p ()
  "キー存在判定をテストする。"
  (let ((trie (nskk-trie-test--create-sample-trie)))

    ;; 存在するキー
    (should (nskk-trie-has-key-p trie "かんじ"))
    (should (nskk-trie-has-key-p trie "あい"))

    ;; 存在しないキー
    (should (not (nskk-trie-has-key-p trie "zzz")))
    (should (not (nskk-trie-has-key-p trie "かんた")))))

(ert-deftest nskk-trie-test-lookup-errors ()
  "不正な検索パラメータのエラーハンドリングをテストする。"
  (let ((trie (nskk-trie-test--create-sample-trie)))
    ;; 非文字列キー
    (should-error (nskk-trie-lookup trie 123))
    (should-error (nskk-trie-lookup trie nil))))

;;; 削除テスト

(ert-deftest nskk-trie-test-delete ()
  "削除操作をテストする。"
  (let ((trie (nskk-trie-test--create-sample-trie)))
    (should (= 6 (nskk-trie-size trie)))

    ;; キーを削除
    (should (nskk-trie-delete trie "かんじ"))
    (should (= 5 (nskk-trie-size trie)))
    (should (not (nskk-trie-has-key-p trie "かんじ")))

    ;; 他のキーは影響を受けない
    (should (nskk-trie-has-key-p trie "かん"))
    (should (nskk-trie-has-key-p trie "かんたん"))))

(ert-deftest nskk-trie-test-delete-nonexistent ()
  "存在しないキーの削除をテストする。"
  (let ((trie (nskk-trie-test--create-sample-trie)))
    (should (= 6 (nskk-trie-size trie)))

    ;; 存在しないキーの削除
    (should (not (nskk-trie-delete trie "zzz")))
    (should (= 6 (nskk-trie-size trie)))))

(ert-deftest nskk-trie-test-delete-then-insert ()
  "削除後の再挿入をテストする。"
  (let ((trie (nskk-trie-test--create-sample-trie)))
    ;; 削除
    (nskk-trie-delete trie "かんじ")
    (should (not (nskk-trie-has-key-p trie "かんじ")))

    ;; 再挿入
    (nskk-trie-insert trie "かんじ" "新漢字")
    (should (nskk-trie-has-key-p trie "かんじ"))

    (let ((result (nskk-trie-lookup trie "かんじ")))
      (should (cdr result))
      (should (equal "新漢字" (car result))))))

(ert-deftest nskk-trie-test-delete-prefix-key ()
  "プレフィックスキーの削除をテストする。"
  (let ((trie (nskk-trie-create)))
    (nskk-trie-insert trie "かん" "缶")
    (nskk-trie-insert trie "かんじ" "漢字")

    ;; プレフィックスキーを削除
    (nskk-trie-delete trie "かん")
    (should (not (nskk-trie-has-key-p trie "かん")))

    ;; 長いキーは影響を受けない
    (should (nskk-trie-has-key-p trie "かんじ"))))

;;; 前方一致検索テスト

(ert-deftest nskk-trie-test-prefix-search ()
  "前方一致検索をテストする。"
  (let ((trie (nskk-trie-test--create-sample-trie)))

    ;; "かん"で始まるキー
    (let ((results (nskk-trie-prefix-search trie "かん")))
      (should (= 3 (length results)))
      (should (cl-every (lambda (entry)
                         (string-prefix-p "かん" (car entry)))
                       results)))

    ;; "あ"で始まるキー
    (let ((results (nskk-trie-prefix-search trie "あ")))
      (should (= 2 (length results)))
      (should (cl-member "あい" results :key #'car :test #'equal))
      (should (cl-member "あいさつ" results :key #'car :test #'equal)))))

(ert-deftest nskk-trie-test-prefix-search-empty ()
  "空プレフィックスの検索をテストする（全キー取得）。"
  (let ((trie (nskk-trie-test--create-sample-trie)))
    (let ((results (nskk-trie-prefix-search trie "")))
      (should (= 6 (length results))))))

(ert-deftest nskk-trie-test-prefix-search-no-match ()
  "マッチしないプレフィックスの検索をテストする。"
  (let ((trie (nskk-trie-test--create-sample-trie)))
    (let ((results (nskk-trie-prefix-search trie "zzz")))
      (should (null results)))))

(ert-deftest nskk-trie-test-prefix-search-exact-match ()
  "完全一致もプレフィックス検索に含まれることをテストする。"
  (let ((trie (nskk-trie-test--create-sample-trie)))
    (let ((results (nskk-trie-prefix-search trie "あい")))
      (should (>= (length results) 1))
      (should (cl-member "あい" results :key #'car :test #'equal))
      (should (cl-member "あいさつ" results :key #'car :test #'equal)))))

(ert-deftest nskk-trie-test-prefix-search-with-limit ()
  "制限付き前方一致検索をテストする。"
  (let ((trie (nskk-trie-test--create-sample-trie)))

    ;; 最大1件
    (let ((results (nskk-trie-prefix-search trie "かん" 1)))
      (should (= 1 (length results))))

    ;; 最大2件
    (let ((results (nskk-trie-prefix-search trie "かん" 2)))
      (should (= 2 (length results))))

    ;; 制限より少ない結果
    (let ((results (nskk-trie-prefix-search trie "あ" 10)))
      (should (= 2 (length results))))))

;;; エッジケーステスト

(ert-deftest nskk-trie-test-empty-trie ()
  "空のトライ木に対する操作をテストする。"
  (let ((trie (nskk-trie-create)))
    ;; 検索
    (should (not (nskk-trie-has-key-p trie "key")))
    (should (not (cdr (nskk-trie-lookup trie "key"))))

    ;; 前方一致検索
    (should (null (nskk-trie-prefix-search trie "prefix")))

    ;; 削除
    (should (not (nskk-trie-delete trie "key")))

    ;; キー一覧
    (should (null (nskk-trie-keys trie)))))

(ert-deftest nskk-trie-test-single-character-keys ()
  "1文字キーのテストをする。"
  (let ((trie (nskk-trie-create)))
    (nskk-trie-insert trie "a" "val-a")
    (nskk-trie-insert trie "b" "val-b")
    (nskk-trie-insert trie "あ" "val-あ")

    (should (= 3 (nskk-trie-size trie)))
    (should (nskk-trie-has-key-p trie "a"))
    (should (nskk-trie-has-key-p trie "b"))
    (should (nskk-trie-has-key-p trie "あ"))))

(ert-deftest nskk-trie-test-long-keys ()
  "長いキー（100文字以上）のテストをする。"
  (let ((trie (nskk-trie-create))
        (long-key (make-string 150 ?あ)))

    (nskk-trie-insert trie long-key "long-value")
    (should (nskk-trie-has-key-p trie long-key))

    (let ((result (nskk-trie-lookup trie long-key)))
      (should (cdr result))
      (should (equal "long-value" (car result))))))

(ert-deftest nskk-trie-test-unicode-characters ()
  "Unicode文字（日本語）のテストをする。"
  (let ((trie (nskk-trie-create)))
    ;; ひらがな
    (nskk-trie-insert trie "ひらがな" "平仮名")

    ;; カタカナ
    (nskk-trie-insert trie "カタカナ" "片仮名")

    ;; 漢字
    (nskk-trie-insert trie "漢字" "kanji")

    ;; 混在
    (nskk-trie-insert trie "あいUeお" "mixed")

    (should (= 4 (nskk-trie-size trie)))
    (should (nskk-trie-has-key-p trie "ひらがな"))
    (should (nskk-trie-has-key-p trie "カタカナ"))
    (should (nskk-trie-has-key-p trie "漢字"))
    (should (nskk-trie-has-key-p trie "あいUeお"))))

;;; ユーティリティテスト

(ert-deftest nskk-trie-test-clear ()
  "トライ木のクリアをテストする。"
  (let ((trie (nskk-trie-test--create-sample-trie)))
    (should (= 6 (nskk-trie-size trie)))

    ;; クリア
    (nskk-trie-clear trie)
    (should (= 0 (nskk-trie-size trie)))
    (should (nskk-trie-empty-p trie))
    (should (null (nskk-trie-keys trie)))))

(ert-deftest nskk-trie-test-keys ()
  "すべてのキーを取得するテストをする。"
  (let ((trie (nskk-trie-test--create-sample-trie)))
    (let ((keys (nskk-trie-keys trie)))
      (should (= 6 (length keys)))
      (should (cl-member "かんじ" keys :test #'equal))
      (should (cl-member "かん" keys :test #'equal))
      (should (cl-member "かんたん" keys :test #'equal))
      (should (cl-member "あい" keys :test #'equal))
      (should (cl-member "あいさつ" keys :test #'equal))
      (should (cl-member "わたし" keys :test #'equal)))))

(ert-deftest nskk-trie-test-statistics ()
  "統計情報の取得をテストする。"
  (let ((trie (nskk-trie-test--create-sample-trie)))
    (let ((stats (nskk-trie-statistics trie)))
      (should (plist-get stats :size))
      (should (= 6 (plist-get stats :size)))

      (should (plist-get stats :node-count))
      (should (> (plist-get stats :node-count) 0))

      (should (plist-get stats :max-depth))
      (should (> (plist-get stats :max-depth) 0))

      (should (numberp (plist-get stats :avg-depth)))
      (should (> (plist-get stats :avg-depth) 0))

      (should (numberp (plist-get stats :memory-usage)))
      (should (> (plist-get stats :memory-usage) 0)))))

;;; シリアライズテスト

(ert-deftest nskk-trie-test-serialize ()
  "シリアライズをテストする。"
  (let* ((trie (nskk-trie-test--create-sample-trie))
         (data (nskk-trie-serialize trie)))

    (should (plist-get data :version))
    (should (equal "1.0" (plist-get data :version)))

    (should (plist-get data :size))
    (should (= 6 (plist-get data :size)))

    (should (plist-get data :entries))
    (should (= 6 (length (plist-get data :entries))))))

(ert-deftest nskk-trie-test-deserialize ()
  "デシリアライズをテストする。"
  (let* ((original-trie (nskk-trie-test--create-sample-trie))
         (data (nskk-trie-serialize original-trie))
         (restored-trie (nskk-trie-deserialize data)))

    ;; サイズが一致
    (should (= (nskk-trie-size original-trie)
              (nskk-trie-size restored-trie)))

    ;; すべてのキーが復元されている
    (should (nskk-trie-has-key-p restored-trie "かんじ"))
    (should (nskk-trie-has-key-p restored-trie "かん"))
    (should (nskk-trie-has-key-p restored-trie "かんたん"))

    ;; 値も正しく復元されている
    (let ((result (nskk-trie-lookup restored-trie "かんじ")))
      (should (cdr result))
      (should (equal "漢字" (car result))))))

(ert-deftest nskk-trie-test-save-and-load ()
  "ファイル保存と読み込みをテストする。"
  (let* ((trie (nskk-trie-test--create-sample-trie))
         (temp-file (make-temp-file "nskk-trie-test-")))

    (unwind-protect
        (progn
          ;; ファイルに保存
          (nskk-trie-save-to-file trie temp-file)
          (should (file-exists-p temp-file))

          ;; ファイルから読み込み
          (let ((loaded-trie (nskk-trie-load-from-file temp-file)))
            (should (= (nskk-trie-size trie)
                      (nskk-trie-size loaded-trie)))

            ;; すべてのキーと値が一致
            (should (nskk-trie-has-key-p loaded-trie "かんじ"))
            (let ((result (nskk-trie-lookup loaded-trie "かんじ")))
              (should (cdr result))
              (should (equal "漢字" (car result))))))

      ;; クリーンアップ
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

;;; パフォーマンステスト

(ert-deftest nskk-trie-test-performance-insert ()
  "挿入パフォーマンスをテストする。"
  :tags '(:performance)
  (let ((trie (nskk-trie-create))
        (count 10000)
        (start-time (float-time)))

    ;; 10,000キーを挿入
    (dotimes (i count)
      (nskk-trie-insert trie (format "key%05d" i) (format "value%d" i)))

    (let ((elapsed (- (float-time) start-time)))
      (message "Insert %d keys: %.3f sec (%.1f keys/sec)"
               count elapsed (/ count elapsed))

      ;; 目標: 10,000キー < 3秒
      (should (< elapsed 3.0))
      (should (= count (nskk-trie-size trie))))))

(ert-deftest nskk-trie-test-performance-lookup ()
  "検索パフォーマンスをテストする。"
  :tags '(:performance)
  (let ((trie (nskk-trie-test--create-large-trie 10000))
        (iterations 1000)
        (start-time (float-time)))

    ;; 1,000回検索
    (dotimes (i iterations)
      (let ((key (format "key%05d" (random 10000))))
        (nskk-trie-lookup trie key)))

    (let ((elapsed (- (float-time) start-time)))
      (message "Lookup %d times: %.3f sec (%.1f lookups/sec)"
               iterations elapsed (/ iterations elapsed))

      ;; 目標: 1回あたり < 1ms
      (should (< (/ elapsed iterations) 0.001)))))

(ert-deftest nskk-trie-test-performance-prefix-search ()
  "前方一致検索パフォーマンスをテストする。"
  :tags '(:performance)
  (let ((trie (nskk-trie-test--create-large-trie 10000))
        (start-time (float-time)))

    ;; 100件の前方一致検索
    (let ((results (nskk-trie-prefix-search trie "key" 100)))
      (let ((elapsed (- (float-time) start-time)))
        (message "Prefix search (100 results): %.3f sec" elapsed)

        ;; 目標: < 100ms（実用上十分な性能）
        (should (< elapsed 0.1))
        (should (= 100 (length results)))))))

(ert-deftest nskk-trie-test-memory-usage ()
  "メモリ使用量をテストする。"
  :tags '(:performance)
  (let ((trie (nskk-trie-test--create-large-trie 100000)))
    (let ((stats (nskk-trie-statistics trie)))
      (let ((memory (plist-get stats :memory-usage)))
        (message "Memory usage for 100,000 keys: %.2f MB"
                 (/ memory 1048576.0))

        ;; 目標: < 30MB
        (should (< memory (* 30 1048576)))))))

(provide 'nskk-trie-test)

;;; nskk-trie-test.el ends here
