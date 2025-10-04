;;; nskk-complex-conjugation-test.el --- Tests for NSKK complex conjugation -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; This file is part of NSKK.

;;; Commentary:

;; 複雑活用エンジンのテスト

;;; Code:

(require 'ert)
(require 'nskk-complex-conjugation)
(require 'nskk-test-framework)

;;; 使役形のテスト

(ert-deftest nskk-complex-conjugation-test/causative-godan ()
  "五段活用の使役形テスト。"
  ;; 書く→書かせ
  (should (equal (nskk-complex-conjugate "書" 'godan 'causative) "書かせ"))
  ;; 読む→読ませ
  (should (equal (nskk-complex-conjugate "読" 'godan 'causative) "読ませ"))
  ;; 泳ぐ→泳がせ
  (should (equal (nskk-complex-conjugate "泳" 'godan 'causative) "泳がせ")))

(ert-deftest nskk-complex-conjugation-test/causative-ichidan ()
  "一段活用の使役形テスト。"
  ;; 見る→見させ
  (should (equal (nskk-complex-conjugate "見" 'kami-ichidan 'causative) "見させ"))
  ;; 食べる→食べさせ
  (should (equal (nskk-complex-conjugate "食べ" 'shimo-ichidan 'causative) "食べさせ")))

(ert-deftest nskk-complex-conjugation-test/causative-irregular ()
  "不規則活用の使役形テスト。"
  ;; する→させ
  (should (equal (nskk-complex-conjugate "し" 'sa-hen 'causative) "させ"))
  ;; 来る→来させ
  (should (equal (nskk-complex-conjugate "来" 'ka-hen 'causative) "来させ")))

;;; 受身形のテスト

(ert-deftest nskk-complex-conjugation-test/passive-godan ()
  "五段活用の受身形テスト。"
  ;; 書く→書かれ
  (should (equal (nskk-complex-conjugate "書" 'godan 'passive) "書かれ"))
  ;; 読む→読まれ
  (should (equal (nskk-complex-conjugate "読" 'godan 'passive) "読まれ"))
  ;; 話す→話され
  (should (equal (nskk-complex-conjugate "話" 'godan 'passive) "話され")))

(ert-deftest nskk-complex-conjugation-test/passive-ichidan ()
  "一段活用の受身形テスト。"
  ;; 見る→見られ
  (should (equal (nskk-complex-conjugate "見" 'kami-ichidan 'passive) "見られ"))
  ;; 食べる→食べられ
  (should (equal (nskk-complex-conjugate "食べ" 'shimo-ichidan 'passive) "食べられ")))

(ert-deftest nskk-complex-conjugation-test/passive-irregular ()
  "不規則活用の受身形テスト。"
  ;; する→される
  (should (equal (nskk-complex-conjugate "し" 'sa-hen 'passive) "され"))
  ;; 来る→来られ
  (should (equal (nskk-complex-conjugate "来" 'ka-hen 'passive) "来られ")))

;;; 可能形のテスト

(ert-deftest nskk-complex-conjugation-test/potential-godan ()
  "五段活用の可能形テスト。"
  ;; 書く→書け
  (should (equal (nskk-complex-conjugate "書" 'godan 'potential) "書け"))
  ;; 読む→読め
  (should (equal (nskk-complex-conjugate "読" 'godan 'potential) "読め"))
  ;; 泳ぐ→泳げ
  (should (equal (nskk-complex-conjugate "泳" 'godan 'potential) "泳げ")))

(ert-deftest nskk-complex-conjugation-test/potential-ichidan ()
  "一段活用の可能形テスト。"
  (let ((nskk-complex-allow-ra-nuki nil))
    ;; ら抜きなし（標準形）
    ;; 見る→見られ
    (should (equal (nskk-complex-conjugate "見" 'kami-ichidan 'potential) "見られ"))
    ;; 食べる→食べられ
    (should (equal (nskk-complex-conjugate "食べ" 'shimo-ichidan 'potential) "食べられ")))

  (let ((nskk-complex-allow-ra-nuki t))
    ;; ら抜き許可
    ;; 見る→見れ
    (should (equal (nskk-complex-conjugate "見" 'kami-ichidan 'potential) "見れ"))
    ;; 食べる→食べれ
    (should (equal (nskk-complex-conjugate "食べ" 'shimo-ichidan 'potential) "食べれ"))))

(ert-deftest nskk-complex-conjugation-test/potential-irregular ()
  "不規則活用の可能形テスト。"
  ;; する→でき
  (should (equal (nskk-complex-conjugate "し" 'sa-hen 'potential) "でき"))
  ;; 来る→来られ
  (should (equal (nskk-complex-conjugate "来" 'ka-hen 'potential) "来られ")))

;;; 使役受身形のテスト

(ert-deftest nskk-complex-conjugation-test/causative-passive-godan ()
  "五段活用の使役受身形テスト。"
  ;; 書く→書かせられ
  (should (equal (nskk-complex-conjugate "書" 'godan 'causative-passive) "書かせられ"))
  ;; 読む→読ませられ
  (should (equal (nskk-complex-conjugate "読" 'godan 'causative-passive) "読ませられ")))

(ert-deftest nskk-complex-conjugation-test/causative-passive-ichidan ()
  "一段活用の使役受身形テスト。"
  ;; 見る→見させられ
  (should (equal (nskk-complex-conjugate "見" 'kami-ichidan 'causative-passive) "見させられ"))
  ;; 食べる→食べさせられ
  (should (equal (nskk-complex-conjugate "食べ" 'shimo-ichidan 'causative-passive) "食べさせられ")))

(ert-deftest nskk-complex-conjugation-test/causative-passive-irregular ()
  "不規則活用の使役受身形テスト。"
  ;; する→させられ
  (should (equal (nskk-complex-conjugate "し" 'sa-hen 'causative-passive) "させられ"))
  ;; 来る→来させられ
  (should (equal (nskk-complex-conjugate "来" 'ka-hen 'causative-passive) "来させられ")))

;;; 尊敬形のテスト

(ert-deftest nskk-complex-conjugation-test/honorific-godan ()
  "五段活用の尊敬形テスト。"
  ;; 書く→書かれ
  (should (equal (nskk-complex-conjugate "書" 'godan 'honorific) "書かれ"))
  ;; 読む→読まれ
  (should (equal (nskk-complex-conjugate "読" 'godan 'honorific) "読まれ")))

(ert-deftest nskk-complex-conjugation-test/honorific-ichidan ()
  "一段活用の尊敬形テスト。"
  ;; 見る→見られ
  (should (equal (nskk-complex-conjugate "見" 'kami-ichidan 'honorific) "見られ"))
  ;; 食べる→食べられ
  (should (equal (nskk-complex-conjugate "食べ" 'shimo-ichidan 'honorific) "食べられ")))

;;; 謙譲形のテスト

(ert-deftest nskk-complex-conjugation-test/humble-godan ()
  "五段活用の謙譲形テスト。"
  ;; 書く→お書きし
  (should (equal (nskk-complex-conjugate "書" 'godan 'humble) "お書きし"))
  ;; 読む→お読みし
  (should (equal (nskk-complex-conjugate "読" 'godan 'humble) "お読みし")))

(ert-deftest nskk-complex-conjugation-test/humble-ichidan ()
  "一段活用の謙譲形テスト。"
  ;; 見る→お見し
  (should (equal (nskk-complex-conjugate "見" 'kami-ichidan 'humble) "お見し"))
  ;; 食べる→お食べし
  (should (equal (nskk-complex-conjugate "食べ" 'shimo-ichidan 'humble) "お食べし")))

;;; 全複合活用形取得のテスト

(ert-deftest nskk-complex-conjugation-test/get-all-forms ()
  "全複合活用形取得のテスト。"
  (let ((forms (nskk-complex-get-all-forms "書" 'godan)))
    (should (= (length forms) 6))
    (should (equal (cdr (assq 'causative forms)) "書かせ"))
    (should (equal (cdr (assq 'passive forms)) "書かれ"))
    (should (equal (cdr (assq 'potential forms)) "書け"))
    (should (equal (cdr (assq 'causative-passive forms)) "書かせられ"))))

;;; 実用例のテスト

(ert-deftest nskk-complex-conjugation-test/practical-examples ()
  "実用的な使用例のテスト。"
  ;; 「書かせる」（使役）
  (should (equal (nskk-complex-conjugate "書" 'godan 'causative) "書かせ"))

  ;; 「読まれる」（受身）
  (should (equal (nskk-complex-conjugate "読" 'godan 'passive) "読まれ"))

  ;; 「見られる」（可能）
  (let ((nskk-complex-allow-ra-nuki nil))
    (should (equal (nskk-complex-conjugate "見" 'kami-ichidan 'potential) "見られ")))

  ;; 「食べさせられる」（使役受身）
  (should (equal (nskk-complex-conjugate "食べ" 'shimo-ichidan 'causative-passive) "食べさせられ"))

  ;; 「お書きする」（謙譲）
  (should (equal (nskk-complex-conjugate "書" 'godan 'humble) "お書きし")))

;;; エラーハンドリングのテスト

(ert-deftest nskk-complex-conjugation-test/invalid-verb-type ()
  "無効な活用型のテスト。"
  (should-error (nskk-complex-conjugate "書" 'invalid-type 'causative)))

(ert-deftest nskk-complex-conjugation-test/invalid-complex-type ()
  "無効な複合活用型のテスト。"
  (should-error (nskk-complex-conjugate "書" 'godan 'invalid-complex)))

(ert-deftest nskk-complex-conjugation-test/invalid-stem ()
  "無効な語幹のテスト。"
  (should-error (nskk-complex-conjugate 123 'godan 'causative)))

;;; エッジケースのテスト

(ert-deftest nskk-complex-conjugation-test/edge-cases ()
  "エッジケースのテスト。"
  ;; ら抜き言葉の制御
  (let ((nskk-complex-allow-ra-nuki t))
    (should (equal (nskk-complex-conjugate "見" 'kami-ichidan 'potential) "見れ")))

  (let ((nskk-complex-allow-ra-nuki nil))
    (should (equal (nskk-complex-conjugate "見" 'kami-ichidan 'potential) "見られ"))))

;;; パフォーマンステスト

(ert-deftest nskk-complex-conjugation-test/performance ()
  "複雑活用処理のパフォーマンステスト（目標: < 30ms）。"
  :tags '(:performance)
  (let ((start-time (current-time))
        (iterations 10000))
    ;; 10000回の複雑活用処理
    (dotimes (_ iterations)
      (nskk-complex-conjugate "書" 'godan 'causative))
    (let* ((end-time (current-time))
           (elapsed-ms (* 1000.0 (float-time (time-subtract end-time start-time))))
           (per-call-ms (/ elapsed-ms iterations)))
      (message "複雑活用処理: %.3f ms/call (%d回)" per-call-ms iterations)
      ;; 1回あたり0.03ms以下であること（30ms/1000回）
      (should (< per-call-ms 0.03)))))

(provide 'nskk-complex-conjugation-test)

;;; nskk-complex-conjugation-test.el ends here
