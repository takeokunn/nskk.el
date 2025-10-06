;;; nskk-scenario-katakana.el --- Basic katakana input scenario -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Development Team

;;; Commentary:
;; カタカナ入力のシナリオテスト

;;; Code:

(require 'nskk-scenario-dsl)

(nskk-defscenario basic-katakana-input
  "基本的なカタカナ入力: ローマ字からカタカナへの変換"
  :tags '(:basic :beginner :katakana)
  :initial-mode 'katakana

  (step "カタカナ5母音の入力"
    (type "aiueo")
    (expect-pending "アイウエオ"))

  (step "Enterキーで確定"
    (press 'return)
    (expect-buffer-contains "アイウエオ")
    (expect-no-pending))

  (step "カタカナか行の入力"
    (type "kakikukeko")
    (expect-pending "カキクケコ"))

  (step "再度確定"
    (press 'return)
    (expect-buffer-contains "アイウエオカキクケコ")
    (expect-no-pending))

  (step "モード確認"
    (expect-mode 'katakana)))

(nskk-defscenario katakana-youon-input
  "カタカナ拗音入力: キャ、シャ等の入力"
  :tags '(:basic :beginner :katakana :youon)
  :initial-mode 'katakana

  (step "キャ、キュ、キョの入力"
    (type "kyakyukyo")
    (expect-pending "キャキュキョ"))

  (step "確定"
    (press 'return)
    (expect-buffer-contains "キャキュキョ")
    (expect-no-pending)))

(nskk-defscenario mode-toggle-hiragana-katakana
  "ひらがな⇔カタカナモード切り替え"
  :tags '(:basic :beginner :mode-switch)
  :initial-mode 'hiragana

  (step "初期モード確認"
    (expect-mode 'hiragana))

  (step "ひらがな入力"
    (type "hiragana")
    (expect-pending "ひらがな"))

  (step "確定"
    (press 'return)
    (expect-buffer-contains "ひらがな"))

  (step "カタカナモードに切り替え (qキー)"
    (press "q")
    (expect-mode 'katakana))

  (step "カタカナ入力"
    (type "katakana")
    (expect-pending "カタカナ"))

  (step "確定"
    (press 'return)
    (expect-buffer-contains "ひらがなカタカナ"))

  (step "ひらがなモードに戻る (再度qキー)"
    (press "q")
    (expect-mode 'hiragana)))

(provide 'nskk-scenario-katakana)
;;; nskk-scenario-katakana.el ends here
