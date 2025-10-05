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

;;; 追加の五段活用テスト（各行の網羅性確保）

(ert-deftest nskk-verb-conjugation-test/godan-hiragana-stem ()
  "ひらがな語幹の五段活用テスト。"
  ;; かく（書く）
  (should (equal (nskk-conjugate-verb "かく" 'godan 'mizen-nai) "かか"))
  (should (equal (nskk-conjugate-verb "かく" 'godan 'renyou) "かき"))
  (should (equal (nskk-conjugate-verb "かく" 'godan 'shushi) "かく"))

  ;; およぐ（泳ぐ）
  (should (equal (nskk-conjugate-verb "およぐ" 'godan 'mizen-nai) "およが"))
  (should (equal (nskk-conjugate-verb "およぐ" 'godan 'renyou) "およぎ"))

  ;; はなす（話す）
  (should (equal (nskk-conjugate-verb "はなす" 'godan 'mizen-nai) "はなさ"))
  (should (equal (nskk-conjugate-verb "はなす" 'godan 'renyou) "はなし")))

(ert-deftest nskk-verb-conjugation-test/godan-complex ()
  "複雑な五段活用のテスト。"
  ;; 走る（ら行）
  (should (equal (nskk-conjugate-verb "走" 'godan 'mizen-nai) "ら"))
  (should (equal (nskk-conjugate-verb "走" 'godan 'mizen-u) "ろ"))
  (should (equal (nskk-conjugate-verb "走" 'godan 'renyou) "り"))

  ;; 立つ（た行）
  (should (equal (nskk-conjugate-verb "立" 'godan 'mizen-nai) "た"))
  (should (equal (nskk-conjugate-verb "立" 'godan 'renyou) "ち"))
  (should (equal (nskk-conjugate-verb "立" 'godan 'shushi) "つ")))

(ert-deftest nskk-verb-conjugation-test/godan-all-rows ()
  "五段活用全行の終止形テスト。"
  (should (equal (nskk-conjugate-verb "書" 'godan 'shushi) "く"))   ; か行
  (should (equal (nskk-conjugate-verb "泳" 'godan 'shushi) "ぐ"))   ; が行
  (should (equal (nskk-conjugate-verb "話" 'godan 'shushi) "す"))   ; さ行
  (should (equal (nskk-conjugate-verb "打" 'godan 'shushi) "つ"))   ; た行
  (should (equal (nskk-conjugate-verb "死" 'godan 'shushi) "ぬ"))   ; な行
  (should (equal (nskk-conjugate-verb "遊" 'godan 'shushi) "ぶ"))   ; ば行
  (should (equal (nskk-conjugate-verb "読" 'godan 'shushi) "む"))   ; ま行
  (should (equal (nskk-conjugate-verb "取" 'godan 'shushi) "る"))   ; ら行
  (should (equal (nskk-conjugate-verb "買" 'godan 'shushi) "う")))  ; わ行

(ert-deftest nskk-verb-conjugation-test/godan-all-rows-mizen-u ()
  "五段活用全行の未然形（う/よう）テスト。"
  (should (equal (nskk-conjugate-verb "書" 'godan 'mizen-u) "こ"))   ; か行
  (should (equal (nskk-conjugate-verb "泳" 'godan 'mizen-u) "ご"))   ; が行
  (should (equal (nskk-conjugate-verb "話" 'godan 'mizen-u) "そ"))   ; さ行
  (should (equal (nskk-conjugate-verb "打" 'godan 'mizen-u) "と"))   ; た行
  (should (equal (nskk-conjugate-verb "死" 'godan 'mizen-u) "の"))   ; な行
  (should (equal (nskk-conjugate-verb "遊" 'godan 'mizen-u) "ぼ"))   ; ば行
  (should (equal (nskk-conjugate-verb "読" 'godan 'mizen-u) "も"))   ; ま行
  (should (equal (nskk-conjugate-verb "取" 'godan 'mizen-u) "ろ"))   ; ら行
  (should (equal (nskk-conjugate-verb "買" 'godan 'mizen-u) "お")))  ; わ行

;;; 追加の一段活用テスト

(ert-deftest nskk-verb-conjugation-test/ichidan-various ()
  "様々な一段活用動詞のテスト。"
  ;; 上一段: 着る
  (should (equal (nskk-conjugate-verb "着" 'kami-ichidan 'mizen-nai) "着"))
  (should (equal (nskk-conjugate-verb "着" 'kami-ichidan 'shushi) "着る"))

  ;; 下一段: 受ける
  (should (equal (nskk-conjugate-verb "受け" 'shimo-ichidan 'mizen-nai) "受け"))
  (should (equal (nskk-conjugate-verb "受け" 'shimo-ichidan 'shushi) "受ける"))

  ;; 下一段: 答える
  (should (equal (nskk-conjugate-verb "答え" 'shimo-ichidan 'renyou) "答え"))
  (should (equal (nskk-conjugate-verb "答え" 'shimo-ichidan 'katei) "答えれ")))

(ert-deftest nskk-verb-conjugation-test/ichidan-all-forms ()
  "一段活用の全活用形網羅テスト。"
  ;; 見る（上一段）全活用形
  (should (equal (nskk-conjugate-verb "見" 'kami-ichidan 'mizen-nai) "見"))
  (should (equal (nskk-conjugate-verb "見" 'kami-ichidan 'mizen-u) "見よ"))
  (should (equal (nskk-conjugate-verb "見" 'kami-ichidan 'renyou) "見"))
  (should (equal (nskk-conjugate-verb "見" 'kami-ichidan 'shushi) "見る"))
  (should (equal (nskk-conjugate-verb "見" 'kami-ichidan 'rentai) "見る"))
  (should (equal (nskk-conjugate-verb "見" 'kami-ichidan 'katei) "見れ"))
  (should (equal (nskk-conjugate-verb "見" 'kami-ichidan 'meirei) "見ろ")))

;;; 不規則活用の包括的テスト

(ert-deftest nskk-verb-conjugation-test/sa-hen-all-forms ()
  "サ変活用の全活用形網羅テスト。"
  (should (equal (nskk-conjugate-verb "し" 'sa-hen 'mizen-nai) "し"))
  (should (equal (nskk-conjugate-verb "し" 'sa-hen 'mizen-u) "しよ"))
  (should (equal (nskk-conjugate-verb "し" 'sa-hen 'renyou) "し"))
  (should (equal (nskk-conjugate-verb "し" 'sa-hen 'shushi) "する"))
  (should (equal (nskk-conjugate-verb "し" 'sa-hen 'rentai) "する"))
  (should (equal (nskk-conjugate-verb "し" 'sa-hen 'katei) "すれ"))
  (should (equal (nskk-conjugate-verb "し" 'sa-hen 'meirei) "しろ")))

(ert-deftest nskk-verb-conjugation-test/ka-hen-all-forms ()
  "カ変活用の全活用形網羅テスト。"
  (should (equal (nskk-conjugate-verb "来" 'ka-hen 'mizen-nai) "来"))
  (should (equal (nskk-conjugate-verb "来" 'ka-hen 'mizen-u) "来よ"))
  (should (equal (nskk-conjugate-verb "来" 'ka-hen 'renyou) "来"))
  (should (equal (nskk-conjugate-verb "来" 'ka-hen 'shushi) "来る"))
  (should (equal (nskk-conjugate-verb "来" 'ka-hen 'rentai) "来る"))
  (should (equal (nskk-conjugate-verb "来" 'ka-hen 'katei) "来れ"))
  (should (equal (nskk-conjugate-verb "来" 'ka-hen 'meirei) "来い")))

;;; 実用的な活用パターンテスト

(ert-deftest nskk-verb-conjugation-test/practical-patterns ()
  "実用的な活用パターンのテスト。"
  ;; 「ない」形（否定）
  (should (equal (nskk-conjugate-verb "書" 'godan 'mizen-nai) "か"))      ; 書かない
  (should (equal (nskk-conjugate-verb "見" 'kami-ichidan 'mizen-nai) "見")) ; 見ない
  (should (equal (nskk-conjugate-verb "し" 'sa-hen 'mizen-nai) "し"))      ; しない

  ;; 「ます」形（丁寧）
  (should (equal (nskk-conjugate-verb "書" 'godan 'renyou) "き"))      ; 書きます
  (should (equal (nskk-conjugate-verb "見" 'kami-ichidan 'renyou) "見")) ; 見ます
  (should (equal (nskk-conjugate-verb "来" 'ka-hen 'renyou) "来"))      ; 来ます

  ;; 「よう」形（意志・勧誘）
  (should (equal (nskk-conjugate-verb "書" 'godan 'mizen-u) "こ"))      ; 書こう
  (should (equal (nskk-conjugate-verb "見" 'kami-ichidan 'mizen-u) "見よ")) ; 見よう
  (should (equal (nskk-conjugate-verb "し" 'sa-hen 'mizen-u) "しよ")))   ; しよう

(ert-deftest nskk-verb-conjugation-test/edge-cases ()
  "エッジケースのテスト。"
  ;; な行の仮定形・命令形（「ね」になる特殊ケース）
  (should (equal (nskk-conjugate-verb "死" 'godan 'katei) "ne"))
  (should (equal (nskk-conjugate-verb "死" 'godan 'meirei) "ne"))

  ;; 各活用型の連体形（終止形と同じ）
  (should (equal (nskk-conjugate-verb "書" 'godan 'rentai)
                 (nskk-conjugate-verb "書" 'godan 'shushi)))
  (should (equal (nskk-conjugate-verb "見" 'kami-ichidan 'rentai)
                 (nskk-conjugate-verb "見" 'kami-ichidan 'shushi))))

;;; 統合テスト

(ert-deftest nskk-verb-conjugation-test/all-types-integration ()
  "全活用型の統合テスト。"
  (let ((test-cases '(("書" godan "く")
                      ("見" kami-ichidan "見る")
                      ("食べ" shimo-ichidan "食べる")
                      ("し" sa-hen "する")
                      ("来" ka-hen "来る"))))
    (dolist (case test-cases)
      (let ((stem (nth 0 case))
            (type (nth 1 case))
            (expected-shushi (nth 2 case)))
        (should (equal (nskk-conjugate-verb stem type 'shushi) expected-shushi))))))

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

(ert-deftest nskk-verb-conjugation-test/performance-all-types ()
  "全活用型のパフォーマンステスト。"
  :tags '(:performance)
  (let ((start-time (current-time))
        (iterations 1000))
    ;; 各活用型1000回ずつ
    (dotimes (_ iterations)
      (nskk-conjugate-verb "書" 'godan 'renyou)
      (nskk-conjugate-verb "見" 'kami-ichidan 'renyou)
      (nskk-conjugate-verb "食べ" 'shimo-ichidan 'renyou)
      (nskk-conjugate-verb "し" 'sa-hen 'renyou)
      (nskk-conjugate-verb "来" 'ka-hen 'renyou))
    (let* ((end-time (current-time))
           (elapsed-ms (* 1000.0 (float-time (time-subtract end-time start-time))))
           (total-calls (* iterations 5))
           (per-call-ms (/ elapsed-ms total-calls)))
      (message "全活用型処理: %.3f ms/call (%d回)" per-call-ms total-calls)
      ;; 1回あたり0.03ms以下であること
      (should (< per-call-ms 0.03)))))

;;; カタカナ語幹のテスト

(ert-deftest nskk-verb-conjugation-test/godan-katakana-stem ()
  "カタカナ語幹の五段活用テスト。"
  ;; カク（書く）
  (should (equal (nskk-conjugate-verb "カク" 'godan 'mizen-nai) "カか"))
  (should (equal (nskk-conjugate-verb "カク" 'godan 'renyou) "カき"))
  (should (equal (nskk-conjugate-verb "カク" 'godan 'shushi) "カく"))

  ;; オヨグ（泳ぐ）
  (should (equal (nskk-conjugate-verb "オヨグ" 'godan 'mizen-nai) "オヨが"))
  (should (equal (nskk-conjugate-verb "オヨグ" 'godan 'renyou) "オヨぎ")))

;;; 未知の語幹テスト

(ert-deftest nskk-verb-conjugation-test/unknown-kanji-stem ()
  "辞書にない漢字語幹のテスト。"
  ;; デフォルトでka行として扱われる
  (let ((result (nskk-conjugate-verb "未" 'godan 'mizen-nai)))
    (should (stringp result))
    ;; デフォルト行（ka）の活用になる
    (should (equal result "か"))))

;;; 文字列formのテスト

(ert-deftest nskk-verb-conjugation-test/string-form ()
  "文字列formを使った活用テスト。"
  ;; formが文字列の場合はそのまま連結
  (should (equal (nskk-conjugate-verb "書" 'godan "ます") "書ます"))
  (should (equal (nskk-conjugate-verb "見" 'kami-ichidan "た") "見た")))

;;; キャッシュ関連のテスト

(ert-deftest nskk-verb-conjugation-test/cache-key ()
  "キャッシュキー生成のテスト。"
  (let ((key (nskk-verb--cache-key "書" 'godan 'renyou)))
    (should (stringp key))
    (should (string-match-p "書" key))
    (should (string-match-p "godan" key))
    (should (string-match-p "renyou" key))))

(ert-deftest nskk-verb-conjugation-test/clear-cache ()
  "キャッシュクリアのテスト。"
  ;; キャッシュにデータを追加
  (puthash "test" "value" nskk-verb--conjugation-cache)
  (should (> (hash-table-count nskk-verb--conjugation-cache) 0))
  ;; クリア
  (nskk-verb-clear-cache)
  (should (= (hash-table-count nskk-verb--conjugation-cache) 0)))

;;; print-all-forms のテスト

(ert-deftest nskk-verb-conjugation-test/print-all-forms-output ()
  "print-all-forms の出力テスト。"
  (let ((forms (nskk-verb-get-all-forms "書" 'godan)))
    ;; 全活用形を手動で表示（interactiveではない）
    (with-output-to-temp-buffer "*Test Verb*"
      (dolist (entry forms)
        (princ (format "%s: %s\n" (car entry) (cdr entry)))))
    ;; バッファが作成されることを確認
    (should (get-buffer "*Test Verb*"))
    ;; クリーンアップ
    (kill-buffer "*Test Verb*")))

;;; 空文字列・nil語幹のテスト

(ert-deftest nskk-verb-conjugation-test/empty-stem ()
  "空語幹のテスト。"
  ;; 空文字列の語幹はsubstringでエラーが発生する
  (should-error (nskk-conjugate-verb "" 'godan 'mizen-nai) :type 'args-out-of-range))

(ert-deftest nskk-verb-conjugation-test/nil-stem-handling ()
  "nil語幹の処理テスト。"
  ;; 内部関数でのnil処理
  (should (null (nskk-verb--conjugate-godan nil 'renyou)))
  (should (null (nskk-verb--conjugate-ichidan nil 'renyou))))

;;; 一段活用の境界値テスト

(ert-deftest nskk-verb-conjugation-test/ichidan-nil-ending ()
  "一段活用でendingがnilの場合のテスト。"
  ;; 存在しない活用形を指定（内部的にnilになる）
  (should (null (nskk-verb--conjugate-ichidan "見" nil))))

;;; サ変・カ変の全分岐テスト

(ert-deftest nskk-verb-conjugation-test/sa-hen-default ()
  "サ変活用のデフォルト分岐テスト。"
  (should (null (nskk-verb--conjugate-sa-hen "し" nil)))
  (should (null (nskk-verb--conjugate-sa-hen "し" 'invalid))))

(ert-deftest nskk-verb-conjugation-test/ka-hen-default ()
  "カ変活用のデフォルト分岐テスト。"
  (should (null (nskk-verb--conjugate-ka-hen "来" nil)))
  (should (null (nskk-verb--conjugate-ka-hen "来" 'invalid))))

;;; normalize-godan-ending のテスト

(ert-deftest nskk-verb-conjugation-test/normalize-godan-na-gyou ()
  "な行の正規化テスト。"
  ;; な行の仮定形・命令形は「ne」になる
  (should (equal (nskk-verb--normalize-godan-ending "ね" 'na 'katei) "ne"))
  (should (equal (nskk-verb--normalize-godan-ending "ね" 'na 'meirei) "ne"))
  ;; な行以外や他の活用形は変わらない
  (should (equal (nskk-verb--normalize-godan-ending "か" 'ka 'katei) "か"))
  (should (equal (nskk-verb--normalize-godan-ending "な" 'na 'renyou) "な")))

;;; get-all-formsの詳細テスト

(ert-deftest nskk-verb-conjugation-test/get-all-forms-complete ()
  "get-all-formsの完全性テスト。"
  (let ((forms (nskk-verb-get-all-forms "書" 'godan)))
    ;; 全7活用形が含まれることを確認
    (should (= (length forms) 7))
    (should (assq 'mizen-nai forms))
    (should (assq 'mizen-u forms))
    (should (assq 'renyou forms))
    (should (assq 'shushi forms))
    (should (assq 'rentai forms))
    (should (assq 'katei forms))
    (should (assq 'meirei forms))))

;;; print-all-formsのテスト

(ert-deftest nskk-verb-conjugation-test/print-all-forms ()
  "print-all-formsの出力テスト。"
  ;; with-output-to-temp-bufferの副作用として*NSKK Verb Conjugation*が作成される
  (nskk-verb-print-all-forms "書" "godan")
  (with-current-buffer "*NSKK Verb Conjugation*"
    ;; バッファに出力されたことを確認
    (should (> (buffer-size) 0))
    ;; 活用形が含まれることを確認
    (goto-char (point-min))
    (should (search-forward "mizen-nai" nil t))
    (should (search-forward "renyou" nil t)))
  (kill-buffer "*NSKK Verb Conjugation*"))

(ert-deftest nskk-verb-conjugation-test/print-all-forms-ichidan ()
  "一段活用のprint-all-formsテスト。"
  (nskk-verb-print-all-forms "見" "kami-ichidan")
  (with-current-buffer "*NSKK Verb Conjugation*"
    (should (> (buffer-size) 0))
    (goto-char (point-min))
    (should (search-forward "kami-ichidan" nil t)))
  (kill-buffer "*NSKK Verb Conjugation*"))

(provide 'nskk-verb-conjugation-test)

;;; nskk-verb-conjugation-test.el ends here
