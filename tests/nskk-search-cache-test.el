;;; nskk-search-cache-test.el --- Tests for cache-integrated search -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, tests

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

;; このファイルはキャッシュ統合検索のテストを提供します。
;;
;; テストケース:
;; - 初回検索（キャッシュミス）
;; - 2回目検索（キャッシュヒット）
;; - 異なる検索タイプで別々にキャッシュ
;; - 候補リスト形式の検索結果キャッシュ

;;; Code:

(require 'ert)
(require 'nskk-search)
(require 'nskk-cache)
(require 'nskk-trie)

(ert-deftest nskk-search-cache-test-cache-miss ()
  "初回検索（キャッシュミス）のテスト。"
  (let ((trie (nskk-trie-create))
        (cache (nskk-cache-create :size 10)))
    (nskk-trie-insert trie "test" '("テスト"))

    (let ((result (nskk-search-with-cache cache trie "test" 'exact)))
      (should (equal result '("テスト"))))))

(ert-deftest nskk-search-cache-test-cache-hit ()
  "2回目検索（キャッシュヒット）のテスト。"
  (let ((trie (nskk-trie-create))
        (cache (nskk-cache-create :size 10)))
    (nskk-trie-insert trie "test" '("テスト"))

    ;; 初回検索でキャッシュに保存
    (nskk-search-with-cache cache trie "test" 'exact)

    ;; 2回目検索でキャッシュヒット
    (let ((result (nskk-search-with-cache cache trie "test" 'exact)))
      (should (equal result '("テスト")))
      (should (> (nskk-cache-hit-rate cache) 0)))))

(ert-deftest nskk-search-cache-test-different-search-types ()
  "異なる検索タイプで別々にキャッシュされることのテスト。"
  (let ((trie (nskk-trie-create))
        (cache (nskk-cache-create :size 10)))
    (nskk-trie-insert trie "test" '("テスト"))

    ;; 異なる検索タイプで検索
    (nskk-search-with-cache cache trie "test" 'exact)
    (nskk-search-with-cache cache trie "test" 'prefix)

    ;; 2つの異なるキャッシュエントリが作成される
    (should (= (nskk-cache-size cache) 2))))

(ert-deftest nskk-search-cache-test-okuri-type-difference ()
  "送り仮名タイプが異なる場合に別々にキャッシュされることのテスト。"
  (let ((trie (nskk-trie-create))
        (cache (nskk-cache-create :size 10)))
    (nskk-trie-insert trie "test" '("テスト"))

    ;; 異なる送り仮名タイプで検索
    (nskk-search-with-cache cache trie "test" 'exact 'okuri-ari)
    (nskk-search-with-cache cache trie "test" 'exact 'okuri-nasi)
    (nskk-search-with-cache cache trie "test" 'exact nil)

    ;; 3つの異なるキャッシュエントリが作成される
    (should (= (nskk-cache-size cache) 3))))

(ert-deftest nskk-search-cache-test-nil-result ()
  "検索結果がnilの場合もキャッシュされることのテスト。"
  (let ((trie (nskk-trie-create))
        (cache (nskk-cache-create :size 10)))
    ;; 存在しないキーで検索
    (let ((result1 (nskk-search-with-cache cache trie "nonexistent" 'exact)))
      (should (null result1)))

    ;; nilの結果はキャッシュされない（nskk-cache-putの仕様）
    ;; 2回目の検索でも結果はnil
    (let ((result2 (nskk-search-with-cache cache trie "nonexistent" 'exact)))
      (should (null result2)))))

(ert-deftest nskk-search-cache-test-multiple-values ()
  "複数の候補値を持つ検索のキャッシュテスト。"
  (let ((trie (nskk-trie-create))
        (cache (nskk-cache-create :size 10)))
    (nskk-trie-insert trie "test" '("テスト1" "テスト2" "テスト3"))

    ;; 初回検索
    (let ((result1 (nskk-search-with-cache cache trie "test" 'exact)))
      (should (equal result1 '("テスト1" "テスト2" "テスト3"))))

    ;; キャッシュヒット確認
    (let ((result2 (nskk-search-with-cache cache trie "test" 'exact)))
      (should (equal result2 '("テスト1" "テスト2" "テスト3")))
      (should (> (nskk-cache-hit-rate cache) 0)))))

(provide 'nskk-search-cache-test)

;;; nskk-search-cache-test.el ends here
