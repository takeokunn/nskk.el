;;; nskk-macro-optimize-test.el --- Tests for nskk-macro-optimize.el -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; This file is part of NSKK.

;;; Commentary:

;; nskk-macro-optimize.el の単体テスト。

;;; Code:

(require 'ert)
(require 'nskk-macro-optimize)

;;; 定数畳み込みテスト

(ert-deftest nskk-macro-optimize-test-const-fold ()
  "定数畳み込みマクロのテスト。"
  (should (= (nskk-macro-const-fold (+ 1 2 3)) 6))
  (should (= (nskk-macro-const-fold (* 2 3 4)) 24))
  (should (string= (nskk-macro-const-fold (concat "hello" " " "world"))
                   "hello world")))

;;; インライン化マクロテスト

(ert-deftest nskk-macro-optimize-test-inline-string-concat ()
  "文字列連結インライン化のテスト。"
  (should (string= (nskk-macro-inline-string-concat "hello" "world")
                   "helloworld"))
  (should (string= (nskk-macro-inline-string-concat "" "test")
                   "test")))

(ert-deftest nskk-macro-optimize-test-inline-char-equal ()
  "文字比較インライン化のテスト。"
  (should (nskk-macro-inline-char-equal ?a ?a))
  (should-not (nskk-macro-inline-char-equal ?a ?b)))

(ert-deftest nskk-macro-optimize-test-inline-string-length ()
  "文字列長インライン化のテスト。"
  (should (= (nskk-macro-inline-string-length "hello") 5))
  (should (= (nskk-macro-inline-string-length "") 0)))

;;; ループアンローリングテスト

(ert-deftest nskk-macro-optimize-test-unroll-loop-2 ()
  "2回ループアンローリングのテスト。"
  (let ((sum 0))
    (nskk-macro-unroll-loop-2
     (setq sum (+ sum 1))
     (setq sum (+ sum 2)))
    (should (= sum 3))))

(ert-deftest nskk-macro-optimize-test-unroll-loop-4 ()
  "4回ループアンローリングのテスト。"
  (let ((sum 0))
    (nskk-macro-unroll-loop-4
     (setq sum (+ sum 1))
     (setq sum (+ sum 2))
     (setq sum (+ sum 3))
     (setq sum (+ sum 4)))
    (should (= sum 10))))

;;; 高速アクセスマクロテスト

(ert-deftest nskk-macro-optimize-test-fast-car ()
  "高速car操作のテスト。"
  (should (= (nskk-macro-fast-car '(1 2 3)) 1))
  (should (null (nskk-macro-fast-car nil))))

(ert-deftest nskk-macro-optimize-test-fast-cdr ()
  "高速cdr操作のテスト。"
  (should (equal (nskk-macro-fast-cdr '(1 2 3)) '(2 3)))
  (should (null (nskk-macro-fast-cdr nil))))

(ert-deftest nskk-macro-optimize-test-fast-nth ()
  "高速nth操作のテスト。"
  (should (= (nskk-macro-fast-nth 0 '(1 2 3)) 1))
  (should (= (nskk-macro-fast-nth 2 '(1 2 3)) 3)))

;;; 条件分岐最適化テスト

(ert-deftest nskk-macro-optimize-test-likely ()
  "likely分岐のテスト。"
  (should (= (nskk-macro-likely t 1 2) 1))
  (should (= (nskk-macro-likely nil 1 2) 2)))

(ert-deftest nskk-macro-optimize-test-unlikely ()
  "unlikely分岐のテスト。"
  (should (= (nskk-macro-unlikely t 1 2) 1))
  (should (= (nskk-macro-unlikely nil 1 2) 2)))

;;; ハッシュテーブル最適化テスト

(ert-deftest nskk-macro-optimize-test-make-hash-table-fast ()
  "高速ハッシュテーブル作成のテスト。"
  (let ((ht (nskk-macro-make-hash-table-fast 100)))
    (should (hash-table-p ht))
    (should (eq (hash-table-test ht) 'equal))))

(ert-deftest nskk-macro-optimize-test-hash-table-get ()
  "高速ハッシュテーブル取得のテスト。"
  (let ((ht (make-hash-table :test 'equal)))
    (puthash "key" "value" ht)
    (should (string= (nskk-macro-hash-table-get ht "key" nil) "value"))
    (should (string= (nskk-macro-hash-table-get ht "missing" "default") "default"))))

;;; 文字列処理最適化テスト

(ert-deftest nskk-macro-optimize-test-string-equal-fast ()
  "高速文字列比較のテスト。"
  (should (nskk-macro-string-equal-fast "hello" "hello"))
  (should-not (nskk-macro-string-equal-fast "hello" "world")))

(ert-deftest nskk-macro-optimize-test-string-empty-p ()
  "文字列空チェックのテスト。"
  (should (nskk-macro-string-empty-p ""))
  (should-not (nskk-macro-string-empty-p "hello")))

;;; 数値演算最適化テスト

(ert-deftest nskk-macro-optimize-test-inc ()
  "インクリメントのテスト。"
  (let ((x 5))
    (nskk-macro-inc x)
    (should (= x 6))))

(ert-deftest nskk-macro-optimize-test-dec ()
  "デクリメントのテスト。"
  (let ((x 5))
    (nskk-macro-dec x)
    (should (= x 4))))

(ert-deftest nskk-macro-optimize-test-add ()
  "加算のテスト。"
  (let ((x 5))
    (nskk-macro-add x 3)
    (should (= x 8))))

;;; 配列アクセス最適化テスト

(ert-deftest nskk-macro-optimize-test-aref-safe ()
  "安全な配列アクセスのテスト。"
  (let ((arr [1 2 3]))
    (should (= (nskk-macro-aref-safe arr 0) 1))
    (should (= (nskk-macro-aref-safe arr 2) 3))
    (should (null (nskk-macro-aref-safe arr 10)))))

(ert-deftest nskk-macro-optimize-test-aref-fast ()
  "高速配列アクセスのテスト。"
  (let ((arr [1 2 3]))
    (should (= (nskk-macro-aref-fast arr 0) 1))
    (should (= (nskk-macro-aref-fast arr 2) 3))))

;;; ビット演算最適化テスト

(ert-deftest nskk-macro-optimize-test-bit-set-p ()
  "ビットセットチェックのテスト。"
  (should (nskk-macro-bit-set-p #b1010 1))
  (should (nskk-macro-bit-set-p #b1010 3))
  (should-not (nskk-macro-bit-set-p #b1010 0))
  (should-not (nskk-macro-bit-set-p #b1010 2)))

(ert-deftest nskk-macro-optimize-test-bit-set ()
  "ビットセットのテスト。"
  (should (= (nskk-macro-bit-set #b0000 0) #b0001))
  (should (= (nskk-macro-bit-set #b0000 2) #b0100)))

(ert-deftest nskk-macro-optimize-test-bit-clear ()
  "ビットクリアのテスト。"
  (should (= (nskk-macro-bit-clear #b1111 0) #b1110))
  (should (= (nskk-macro-bit-clear #b1111 2) #b1011)))

;;; 統計情報テスト

(ert-deftest nskk-macro-optimize-test-stats ()
  "統計情報のテスト。"
  (let ((stats (nskk-macro-optimize-stats)))
    (should (plist-member stats :macro-count))
    (should (plist-member stats :inline-functions))
    (should (plist-member stats :optimization-level))
    (should (= (plist-get stats :macro-count) 26))))

(provide 'nskk-macro-optimize-test)

;;; nskk-macro-optimize-test.el ends here
