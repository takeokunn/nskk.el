;;; nskk-scenario-hiragana.el --- Basic hiragana input scenario -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Development Team

;;; Commentary:
;; 基本的なひらがな入力のシナリオテスト

;;; Code:

(require 'nskk-scenario-dsl)

(nskk-defscenario basic-hiragana-input
  "基本的なひらがな入力: ローマ字からひらがなへの変換"
  :tags '(:basic :beginner :hiragana)
  :initial-mode 'hiragana

  (step "5母音の入力"
    (type "aiueo")
    (expect-pending "あいうえお"))

  (step "Enterキーで確定"
    (press 'return)
    (expect-buffer-contains "あいうえお")
    (expect-no-pending))

  (step "か行の入力"
    (type "kakikukeko")
    (expect-pending "かきくけこ"))

  (step "再度確定"
    (press 'return)
    (expect-buffer-contains "あいうえおかきくけこ")
    (expect-no-pending))

  (step "モード確認"
    (expect-mode 'hiragana)))

(nskk-defscenario hiragana-youon-input
  "ひらがな拗音入力: きゃ、しゃ等の入力"
  :tags '(:basic :beginner :hiragana :youon)
  :initial-mode 'hiragana

  (step "きゃ、きゅ、きょの入力"
    (type "kyakyukyo")
    (expect-pending "きゃきゅきょ"))

  (step "確定"
    (press 'return)
    (expect-buffer-contains "きゃきゅきょ")
    (expect-no-pending)))

(nskk-defscenario hiragana-sokuon-input
  "ひらがな促音入力: っの処理"
  :tags '(:basic :beginner :hiragana :sokuon)
  :initial-mode 'hiragana

  (step "促音を含む入力"
    (type "kakkokukku")
    (expect-pending "かっこくっく"))

  (step "確定"
    (press 'return)
    (expect-buffer-contains "かっこくっく")
    (expect-no-pending)))

(provide 'nskk-scenario-hiragana)
;;; nskk-scenario-hiragana.el ends here
