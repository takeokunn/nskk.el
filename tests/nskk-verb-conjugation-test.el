;;; nskk-verb-conjugation-test.el --- Tests for NSKK verb conjugation -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; This file is part of NSKK.

;;; Commentary:

;; 動詞活用エンジンのテスト

;;; Code:

(require 'ert)
(require 'nskk-verb-conjugation)
(require 'nskk-test-framework)

;;; 五段活用のテスト

(ert-deftest nskk-verb-conjugation-test/godan-ka-gyou ()
  "五段活用（か行）のテスト。"
  ;; 書く
  (should (equal (nskk-conjugate-verb "書" 'godan 'mizen-nai) "か"))
  (should (equal (nskk-conjugate-verb "書" 'godan 'mizen-u) "こ"))
  (should (equal (nskk-conjugate-verb "書" 'godan 'renyou) "き"))
  (should (equal (nskk-conjugate-verb "書" 'godan 'shushi) "く"))
  (should (equal (nskk-conjugate-verb "書" 'godan 'rentai) "く"))
  (should (equal (nskk-conjugate-verb "書" 'godan 'katei) "け"))
  (should (equal (nskk-conjugate-verb "書" 'godan 'meirei) "け")))

(ert-deftest nskk-verb-conjugation-test/godan-ga-gyou ()
  "五段活用（が行）のテスト。"
  ;; 泳ぐ
  (should (equal (nskk-conjugate-verb "泳" 'godan 'mizen-nai) "が"))
  (should (equal (nskk-conjugate-verb "泳" 'godan 'mizen-u) "ご"))
  (should (equal (nskk-conjugate-verb "泳" 'godan 'renyou) "ぎ"))
  (should (equal (nskk-conjugate-verb "泳" 'godan 'shushi) "ぐ"))
  (should (equal (nskk-conjugate-verb "泳" 'godan 'rentai) "ぐ"))
  (should (equal (nskk-conjugate-verb "泳" 'godan 'katei) "げ"))
  (should (equal (nskk-conjugate-verb "泳" 'godan 'meirei) "げ")))

(ert-deftest nskk-verb-conjugation-test/godan-sa-gyou ()
  "五段活用（さ行）のテスト。"
  ;; 話す
  (should (equal (nskk-conjugate-verb "話" 'godan 'mizen-nai) "さ"))
  (should (equal (nskk-conjugate-verb "話" 'godan 'mizen-u) "そ"))
  (should (equal (nskk-conjugate-verb "話" 'godan 'renyou) "し"))
  (should (equal (nskk-conjugate-verb "話" 'godan 'shushi) "す"))
  (should (equal (nskk-conjugate-verb "話" 'godan 'rentai) "す"))
  (should (equal (nskk-conjugate-verb "話" 'godan 'katei) "せ"))
  (should (equal (nskk-conjugate-verb "話" 'godan 'meirei) "せ")))

(ert-deftest nskk-verb-conjugation-test/godan-ta-gyou ()
  "五段活用（た行）のテスト。"
  ;; 打つ
  (should (equal (nskk-conjugate-verb "打" 'godan 'mizen-nai) "た"))
  (should (equal (nskk-conjugate-verb "打" 'godan 'mizen-u) "と"))
  (should (equal (nskk-conjugate-verb "打" 'godan 'renyou) "ち"))
  (should (equal (nskk-conjugate-verb "打" 'godan 'shushi) "つ"))
  (should (equal (nskk-conjugate-verb "打" 'godan 'rentai) "つ"))
  (should (equal (nskk-conjugate-verb "打" 'godan 'katei) "て"))
  (should (equal (nskk-conjugate-verb "打" 'godan 'meirei) "て")))

(ert-deftest nskk-verb-conjugation-test/godan-na-gyou ()
  "五段活用（な行）のテスト。"
  ;; 死ぬ
  (should (equal (nskk-conjugate-verb "死" 'godan 'mizen-nai) "な"))
  (should (equal (nskk-conjugate-verb "死" 'godan 'mizen-u) "の"))
  (should (equal (nskk-conjugate-verb "死" 'godan 'renyou) "に"))
  (should (equal (nskk-conjugate-verb "死" 'godan 'shushi) "ぬ"))
  (should (equal (nskk-conjugate-verb "死" 'godan 'rentai) "ぬ"))
  (should (equal (nskk-conjugate-verb "死" 'godan 'katei) "ne"))
  (should (equal (nskk-conjugate-verb "死" 'godan 'meirei) "ne")))

(ert-deftest nskk-verb-conjugation-test/godan-ba-gyou ()
  "五段活用（ば行）のテスト。"
  ;; 遊ぶ
  (should (equal (nskk-conjugate-verb "遊" 'godan 'mizen-nai) "ば"))
  (should (equal (nskk-conjugate-verb "遊" 'godan 'mizen-u) "ぼ"))
  (should (equal (nskk-conjugate-verb "遊" 'godan 'renyou) "び"))
  (should (equal (nskk-conjugate-verb "遊" 'godan 'shushi) "ぶ"))
  (should (equal (nskk-conjugate-verb "遊" 'godan 'rentai) "ぶ"))
  (should (equal (nskk-conjugate-verb "遊" 'godan 'katei) "べ"))
  (should (equal (nskk-conjugate-verb "遊" 'godan 'meirei) "べ")))

(ert-deftest nskk-verb-conjugation-test/godan-ma-gyou ()
  "五段活用（ま行）のテスト。"
  ;; 読む
  (should (equal (nskk-conjugate-verb "読" 'godan 'mizen-nai) "ま"))
  (should (equal (nskk-conjugate-verb "読" 'godan 'mizen-u) "も"))
  (should (equal (nskk-conjugate-verb "読" 'godan 'renyou) "み"))
  (should (equal (nskk-conjugate-verb "読" 'godan 'shushi) "む"))
  (should (equal (nskk-conjugate-verb "読" 'godan 'rentai) "む"))
  (should (equal (nskk-conjugate-verb "読" 'godan 'katei) "め"))
  (should (equal (nskk-conjugate-verb "読" 'godan 'meirei) "め")))

(ert-deftest nskk-verb-conjugation-test/godan-ra-gyou ()
  "五段活用（ら行）のテスト。"
  ;; 取る
  (should (equal (nskk-conjugate-verb "取" 'godan 'mizen-nai) "ら"))
  (should (equal (nskk-conjugate-verb "取" 'godan 'mizen-u) "ろ"))
  (should (equal (nskk-conjugate-verb "取" 'godan 'renyou) "り"))
  (should (equal (nskk-conjugate-verb "取" 'godan 'shushi) "る"))
  (should (equal (nskk-conjugate-verb "取" 'godan 'rentai) "る"))
  (should (equal (nskk-conjugate-verb "取" 'godan 'katei) "れ"))
  (should (equal (nskk-conjugate-verb "取" 'godan 'meirei) "れ")))

(ert-deftest nskk-verb-conjugation-test/godan-wa-gyou ()
  "五段活用（わ行）のテスト。"
  ;; 買う
  (should (equal (nskk-conjugate-verb "買" 'godan 'mizen-nai) "わ"))
  (should (equal (nskk-conjugate-verb "買" 'godan 'mizen-u) "お"))
  (should (equal (nskk-conjugate-verb "買" 'godan 'renyou) "い"))
  (should (equal (nskk-conjugate-verb "買" 'godan 'shushi) "う"))
  (should (equal (nskk-conjugate-verb "買" 'godan 'rentai) "う"))
  (should (equal (nskk-conjugate-verb "買" 'godan 'katei) "え"))
  (should (equal (nskk-conjugate-verb "買" 'godan 'meirei) "え")))

;;; 上一段活用のテスト

(ert-deftest nskk-verb-conjugation-test/kami-ichidan ()
  "上一段活用のテスト。"
  ;; 見る
  (should (equal (nskk-conjugate-verb "見" 'kami-ichidan 'mizen-nai) "見"))
  (should (equal (nskk-conjugate-verb "見" 'kami-ichidan 'mizen-u) "見よ"))
  (should (equal (nskk-conjugate-verb "見" 'kami-ichidan 'renyou) "見"))
  (should (equal (nskk-conjugate-verb "見" 'kami-ichidan 'shushi) "見る"))
  (should (equal (nskk-conjugate-verb "見" 'kami-ichidan 'rentai) "見る"))
  (should (equal (nskk-conjugate-verb "見" 'kami-ichidan 'katei) "見れ"))
  (should (equal (nskk-conjugate-verb "見" 'kami-ichidan 'meirei) "見ろ"))

  ;; 起きる
  (should (equal (nskk-conjugate-verb "起き" 'kami-ichidan 'mizen-nai) "起き"))
  (should (equal (nskk-conjugate-verb "起き" 'kami-ichidan 'renyou) "起き"))
  (should (equal (nskk-conjugate-verb "起き" 'kami-ichidan 'shushi) "起きる")))

;;; 下一段活用のテスト

(ert-deftest nskk-verb-conjugation-test/shimo-ichidan ()
  "下一段活用のテスト。"
  ;; 食べる
  (should (equal (nskk-conjugate-verb "食べ" 'shimo-ichidan 'mizen-nai) "食べ"))
  (should (equal (nskk-conjugate-verb "食べ" 'shimo-ichidan 'mizen-u) "食べよ"))
  (should (equal (nskk-conjugate-verb "食べ" 'shimo-ichidan 'renyou) "食べ"))
  (should (equal (nskk-conjugate-verb "食べ" 'shimo-ichidan 'shushi) "食べる"))
  (should (equal (nskk-conjugate-verb "食べ" 'shimo-ichidan 'rentai) "食べる"))
  (should (equal (nskk-conjugate-verb "食べ" 'shimo-ichidan 'katei) "食べれ"))
  (should (equal (nskk-conjugate-verb "食べ" 'shimo-ichidan 'meirei) "食べろ"))

  ;; 寝る
  (should (equal (nskk-conjugate-verb "寝" 'shimo-ichidan 'mizen-nai) "寝"))
  (should (equal (nskk-conjugate-verb "寝" 'shimo-ichidan 'renyou) "寝"))
  (should (equal (nskk-conjugate-verb "寝" 'shimo-ichidan 'shushi) "寝る")))

;;; サ変活用のテスト

(ert-deftest nskk-verb-conjugation-test/sa-hen ()
  "サ変活用のテスト。"
  ;; する
  (should (equal (nskk-conjugate-verb "し" 'sa-hen 'mizen-nai) "し"))
  (should (equal (nskk-conjugate-verb "し" 'sa-hen 'mizen-u) "しよ"))
  (should (equal (nskk-conjugate-verb "し" 'sa-hen 'renyou) "し"))
  (should (equal (nskk-conjugate-verb "し" 'sa-hen 'shushi) "する"))
  (should (equal (nskk-conjugate-verb "し" 'sa-hen 'rentai) "する"))
  (should (equal (nskk-conjugate-verb "し" 'sa-hen 'katei) "すれ"))
  (should (equal (nskk-conjugate-verb "し" 'sa-hen 'meirei) "しろ")))

;;; カ変活用のテスト

(ert-deftest nskk-verb-conjugation-test/ka-hen ()
  "カ変活用のテスト。"
  ;; 来る
  (should (equal (nskk-conjugate-verb "来" 'ka-hen 'mizen-nai) "来"))
  (should (equal (nskk-conjugate-verb "来" 'ka-hen 'mizen-u) "来よ"))
  (should (equal (nskk-conjugate-verb "来" 'ka-hen 'renyou) "来"))
  (should (equal (nskk-conjugate-verb "来" 'ka-hen 'shushi) "来る"))
  (should (equal (nskk-conjugate-verb "来" 'ka-hen 'rentai) "来る"))
  (should (equal (nskk-conjugate-verb "来" 'ka-hen 'katei) "来れ"))
  (should (equal (nskk-conjugate-verb "来" 'ka-hen 'meirei) "来い")))

;;; 全活用形取得のテスト

(ert-deftest nskk-verb-conjugation-test/get-all-forms ()
  "全活用形取得のテスト。"
  (let ((forms (nskk-verb-get-all-forms "書" 'godan)))
    (should (= (length forms) 7))
    (should (equal (cdr (assq 'mizen-nai forms)) "か"))
    (should (equal (cdr (assq 'renyou forms)) "き"))))

;;; エラーハンドリングのテスト

(ert-deftest nskk-verb-conjugation-test/invalid-type ()
  "無効な活用型のテスト。"
  (should-error (nskk-conjugate-verb "書" 'invalid-type 'mizen-nai)))

(ert-deftest nskk-verb-conjugation-test/invalid-form ()
  "無効な活用形のテスト。"
  (should-error (nskk-conjugate-verb "書" 'godan 'invalid-form)))

(ert-deftest nskk-verb-conjugation-test/invalid-stem ()
  "無効な語幹のテスト。"
  (should-error (nskk-conjugate-verb 123 'godan 'mizen-nai)))

;;; パフォーマンステスト

(ert-deftest nskk-verb-conjugation-test/performance ()
  "活用処理のパフォーマンステスト（目標: < 30ms）。"
  :tags '(:performance)
  (let ((start-time (current-time))
        (iterations 10000))
    ;; 10000回の活用処理
    (dotimes (_ iterations)
      (nskk-conjugate-verb "書" 'godan 'renyou))
    (let* ((end-time (current-time))
           (elapsed-ms (* 1000.0 (float-time (time-subtract end-time start-time))))
           (per-call-ms (/ elapsed-ms iterations)))
      (message "活用処理: %.3f ms/call (%d回)" per-call-ms iterations)
      ;; 1回あたり0.03ms以下であること（30ms/1000回）
      (should (< per-call-ms 0.03)))))

(provide 'nskk-verb-conjugation-test)

;;; nskk-verb-conjugation-test.el ends here
