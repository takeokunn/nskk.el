;;; nskk-scenario-kanji.el --- Basic kanji conversion scenario -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Development Team

;;; Commentary:
;; 基本的な漢字変換のシナリオテスト

;;; Code:

(require 'nskk-scenario-dsl)

(nskk-defscenario basic-kanji-conversion
  "基本的な漢字変換: ひらがなから漢字への変換"
  :tags '(:basic :intermediate :kanji :conversion)
  :initial-mode 'hiragana

  (step "ひらがな入力 (変換前)"
    (type "kanji")
    (expect-pending "かんじ"))

  (step "Spaceキーで変換開始"
    (press 'space)
    (expect-candidates '("漢字" "幹事" "感じ")))

  (step "最初の候補が選択されていることを確認"
    (expect-candidate-selected "漢字"))

  (step "Enterキーで確定"
    (press 'return)
    (expect-buffer-contains "漢字")
    (expect-no-pending)))

(nskk-defscenario kanji-candidate-selection
  "漢字変換候補の選択: 次候補・前候補の操作"
  :tags '(:basic :intermediate :kanji :candidates)
  :initial-mode 'hiragana

  (step "ひらがな入力"
    (type "ai")
    (expect-pending "あい"))

  (step "変換開始"
    (press 'space)
    (expect-candidates '("愛" "哀" "藍")))

  (step "初期候補確認"
    (expect-candidate-selected "愛"))

  (step "次候補へ (Spaceキー)"
    (press 'space)
    (expect-candidate-selected "哀"))

  (step "さらに次候補へ"
    (press 'space)
    (expect-candidate-selected "藍"))

  (step "前候補へ戻る (xキー)"
    (press "x")
    (expect-candidate-selected "哀"))

  (step "確定"
    (press 'return)
    (expect-buffer-contains "哀")
    (expect-no-pending)))

(nskk-defscenario kanji-conversion-not-found
  "辞書に存在しない見出し語の処理"
  :tags '(:basic :intermediate :kanji :error-handling)
  :initial-mode 'hiragana

  (step "存在しない見出し語を入力"
    (type "zzzzzzzz")
    (expect-pending "っっっっっっz"))

  (step "変換を試みる"
    (press 'space)
    ;; 候補が見つからない場合の挙動は実装依存
    ;; ここでは単に未確定入力が保持されることを期待
    (expect-pending "っっっっっっz"))

  (step "Escapeキーでキャンセル (C-g)"
    (press "C-g")
    (expect-no-pending)))

(provide 'nskk-scenario-kanji)
;;; nskk-scenario-kanji.el ends here
