;;; nskk-adjective-conjugation-test.el --- Tests for NSKK adjective conjugation -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; This file is part of NSKK.

;;; Commentary:

;; 形容詞活用エンジンのテスト

;;; Code:

(require 'ert)
(require 'nskk-adjective-conjugation)
(require 'nskk-test-framework)

;;; イ形容詞のテスト

(ert-deftest nskk-adjective-conjugation-test/i-adjective-basic ()
  "イ形容詞の基本活用テスト（高い）。"
  ;; 語幹から
  (should (equal (nskk-conjugate-adjective "高" 'i-adjective 'mizen) "高かろ"))
  (should (equal (nskk-conjugate-adjective "高" 'i-adjective 'renyou-ku) "高く"))
  (should (equal (nskk-conjugate-adjective "高" 'i-adjective 'renyou-katta) "高かっ"))
  (should (equal (nskk-conjugate-adjective "高" 'i-adjective 'shushi) "高い"))
  (should (equal (nskk-conjugate-adjective "高" 'i-adjective 'rentai) "高い"))
  (should (equal (nskk-conjugate-adjective "高" 'i-adjective 'katei) "高けれ"))

  ;; 「い」付きの語幹から
  (should (equal (nskk-conjugate-adjective "高い" 'i-adjective 'renyou-ku) "高く"))
  (should (equal (nskk-conjugate-adjective "高い" 'i-adjective 'renyou-katta) "高かっ")))

(ert-deftest nskk-adjective-conjugation-test/i-adjective-various ()
  "様々なイ形容詞の活用テスト。"
  ;; 美しい
  (should (equal (nskk-conjugate-adjective "美しい" 'i-adjective 'renyou-ku) "美しく"))
  (should (equal (nskk-conjugate-adjective "美しい" 'i-adjective 'katei) "美しけれ"))

  ;; 楽しい
  (should (equal (nskk-conjugate-adjective "楽し" 'i-adjective 'renyou-ku) "楽しく"))
  (should (equal (nskk-conjugate-adjective "楽し" 'i-adjective 'renyou-katta) "楽しかっ"))

  ;; 暑い
  (should (equal (nskk-conjugate-adjective "暑" 'i-adjective 'renyou-ku) "暑く"))
  (should (equal (nskk-conjugate-adjective "暑" 'i-adjective 'katei) "暑けれ"))

  ;; 寒い
  (should (equal (nskk-conjugate-adjective "寒" 'i-adjective 'renyou-ku) "寒く"))
  (should (equal (nskk-conjugate-adjective "寒" 'i-adjective 'renyou-katta) "寒かっ")))

(ert-deftest nskk-adjective-conjugation-test/i-adjective-negative ()
  "イ形容詞の否定形テスト。"
  ;; 高くない
  (should (equal (nskk-conjugate-adjective "高" 'i-adjective 'renyou-ku) "高く"))
  ;; 高くなかった
  (should (equal (nskk-conjugate-adjective "高" 'i-adjective 'renyou-katta) "高かっ")))

;;; ナ形容詞のテスト

(ert-deftest nskk-adjective-conjugation-test/na-adjective-basic ()
  "ナ形容詞の基本活用テスト（静か）。"
  (should (equal (nskk-conjugate-adjective "静か" 'na-adjective 'mizen) "静かだろ"))
  (should (equal (nskk-conjugate-adjective "静か" 'na-adjective 'renyou-ni) "静かに"))
  (should (equal (nskk-conjugate-adjective "静か" 'na-adjective 'renyou-datta) "静かだっ"))
  (should (equal (nskk-conjugate-adjective "静か" 'na-adjective 'shushi-da) "静かだ"))
  (should (equal (nskk-conjugate-adjective "静か" 'na-adjective 'rentai-na) "静かな"))
  (should (equal (nskk-conjugate-adjective "静か" 'na-adjective 'katei-nara) "静かなら")))

(ert-deftest nskk-adjective-conjugation-test/na-adjective-various ()
  "様々なナ形容詞の活用テスト。"
  ;; きれい
  (should (equal (nskk-conjugate-adjective "きれい" 'na-adjective 'renyou-ni) "きれいに"))
  (should (equal (nskk-conjugate-adjective "きれい" 'na-adjective 'rentai-na) "きれいな"))

  ;; 元気
  (should (equal (nskk-conjugate-adjective "元気" 'na-adjective 'renyou-ni) "元気に"))
  (should (equal (nskk-conjugate-adjective "元気" 'na-adjective 'shushi-da) "元気だ"))

  ;; 簡単
  (should (equal (nskk-conjugate-adjective "簡単" 'na-adjective 'renyou-ni) "簡単に"))
  (should (equal (nskk-conjugate-adjective "簡単" 'na-adjective 'rentai-na) "簡単な"))

  ;; 便利
  (should (equal (nskk-conjugate-adjective "便利" 'na-adjective 'renyou-ni) "便利に"))
  (should (equal (nskk-conjugate-adjective "便利" 'na-adjective 'renyou-datta) "便利だっ")))

;;; 特殊形容詞のテスト

(ert-deftest nskk-adjective-conjugation-test/special-adjectives ()
  "特殊形容詞のテスト（いい/よい）。"
  ;; 「いい」の活用
  (should (equal (nskk-adjective-conjugate-special "いい" 'renyou-ku) "よく"))
  (should (equal (nskk-adjective-conjugate-special "いい" 'renyou-katta) "よかっ"))
  (should (equal (nskk-adjective-conjugate-special "いい" 'shushi) "いい"))
  (should (equal (nskk-adjective-conjugate-special "いい" 'katei) "よけれ"))

  ;; 「よい」の活用
  (should (equal (nskk-adjective-conjugate-special "よい" 'renyou-ku) "よく"))
  (should (equal (nskk-adjective-conjugate-special "よい" 'shushi) "よい")))

;;; 全活用形取得のテスト

(ert-deftest nskk-adjective-conjugation-test/get-all-forms-i ()
  "イ形容詞の全活用形取得テスト。"
  (let ((forms (nskk-adjective-get-all-forms "高" 'i-adjective)))
    (should (= (length forms) 6))
    (should (equal (cdr (assq 'renyou-ku forms)) "高く"))
    (should (equal (cdr (assq 'renyou-katta forms)) "高かっ"))
    (should (equal (cdr (assq 'katei forms)) "高けれ"))))

(ert-deftest nskk-adjective-conjugation-test/get-all-forms-na ()
  "ナ形容詞の全活用形取得テスト。"
  (let ((forms (nskk-adjective-get-all-forms "静か" 'na-adjective)))
    (should (= (length forms) 6))
    (should (equal (cdr (assq 'renyou-ni forms)) "静かに"))
    (should (equal (cdr (assq 'rentai-na forms)) "静かな"))
    (should (equal (cdr (assq 'katei-nara forms)) "静かなら"))))

;;; 逆変換テスト

(ert-deftest nskk-adjective-conjugation-test/get-base-form-i ()
  "イ形容詞の逆変換テスト。"
  (should (equal (nskk-adjective-get-base-form "高く" 'i-adjective) "高い"))
  (should (equal (nskk-adjective-get-base-form "高かっ" 'i-adjective) "高い"))
  (should (equal (nskk-adjective-get-base-form "高けれ" 'i-adjective) "高い"))
  (should (equal (nskk-adjective-get-base-form "高い" 'i-adjective) "高い")))

(ert-deftest nskk-adjective-conjugation-test/get-base-form-na ()
  "ナ形容詞の逆変換テスト。"
  (should (equal (nskk-adjective-get-base-form "静かに" 'na-adjective) "静か"))
  (should (equal (nskk-adjective-get-base-form "静かだ" 'na-adjective) "静か"))
  (should (equal (nskk-adjective-get-base-form "静かな" 'na-adjective) "静か"))
  (should (equal (nskk-adjective-get-base-form "静かなら" 'na-adjective) "静か")))

;;; エラーハンドリングのテスト

(ert-deftest nskk-adjective-conjugation-test/invalid-type ()
  "無効な活用型のテスト。"
  (should-error (nskk-conjugate-adjective "高" 'invalid-type 'renyou-ku)))

(ert-deftest nskk-adjective-conjugation-test/invalid-form ()
  "無効な活用形のテスト。"
  (should-error (nskk-conjugate-adjective "高" 'i-adjective 'invalid-form)))

(ert-deftest nskk-adjective-conjugation-test/invalid-stem ()
  "無効な語幹のテスト。"
  (should-error (nskk-conjugate-adjective 123 'i-adjective 'renyou-ku)))

;;; 実用例のテスト

(ert-deftest nskk-adjective-conjugation-test/practical-examples ()
  "実用的な使用例のテスト。"
  ;; 「高くなる」
  (should (equal (nskk-conjugate-adjective "高" 'i-adjective 'renyou-ku) "高く"))

  ;; 「美しかった」
  (should (equal (nskk-conjugate-adjective "美しい" 'i-adjective 'renyou-katta) "美しかっ"))

  ;; 「静かにする」
  (should (equal (nskk-conjugate-adjective "静か" 'na-adjective 'renyou-ni) "静かに"))

  ;; 「元気だった」
  (should (equal (nskk-conjugate-adjective "元気" 'na-adjective 'renyou-datta) "元気だっ"))

  ;; 「きれいな花」
  (should (equal (nskk-conjugate-adjective "きれい" 'na-adjective 'rentai-na) "きれいな")))

;;; パフォーマンステスト

(ert-deftest nskk-adjective-conjugation-test/performance ()
  "活用処理のパフォーマンステスト（目標: < 30ms）。"
  :tags '(:performance)
  (let ((start-time (current-time))
        (iterations 10000))
    ;; 10000回の活用処理
    (dotimes (_ iterations)
      (nskk-conjugate-adjective "高" 'i-adjective 'renyou-ku))
    (let* ((end-time (current-time))
           (elapsed-ms (* 1000.0 (float-time (time-subtract end-time start-time))))
           (per-call-ms (/ elapsed-ms iterations)))
      (message "形容詞活用処理: %.3f ms/call (%d回)" per-call-ms iterations)
      ;; 1回あたり0.03ms以下であること（30ms/1000回）
      (should (< per-call-ms 0.03)))))

(provide 'nskk-adjective-conjugation-test)

;;; nskk-adjective-conjugation-test.el ends here
