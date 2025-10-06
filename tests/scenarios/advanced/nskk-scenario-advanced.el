;;; nskk-scenario-advanced.el --- Advanced scenarios -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Development Team

;;; Commentary:
;; 高度機能と学習・注釈・入力切り替えのシナリオテスト

;;; Code:

(require 'nskk-scenario-dsl)
(require 'nskk-annotation-display)
(require 'nskk-annotation-parser)
(require 'nskk-learning-frequency)

(nskk-defscenario advanced-feature-workflow
  "高度機能統合シナリオ"
  :tags '(:advanced :dictionary :learning :annotation :mode-switch)
  :initial-mode 'hiragana

  (step "ユーザー辞書を登録"
    (nskk-scenario-register-dictionary-entry "しごと" '("仕事" "私事"))
    (type "sigoto")
    (expect-pending "しごと"))

  (step "ユーザー辞書が優先されることを確認"
    (press 'space)
    (expect-candidates '("仕事" "私事"))
    (expect-candidate-selected "仕事"))

  (step "候補確定と学習"
    (press 'return)
    (expect-buffer-contains "仕事")
    (expect-no-pending)
    (when (fboundp 'nskk-update-frequency)
      (nskk-update-frequency "しごと" "仕事"))
    (nskk-scenario-expect-frequency "しごと" "仕事" 1.0))

  (step "注釈を表示"
    (let ((nskk-annotation-display-delay 0)
          (nskk-annotation-display-duration nil)
          (nskk-annotation-display-style 'inline))
      (nskk-show-annotation (nskk-parse-annotation "management role")))
    (nskk-scenario-expect-annotation "management role"))

  (step "カタカナモードに切り替え"
    (press "q")
    (expect-mode 'katakana)
    (type "kanri")
    (press 'return)
    (expect-buffer-contains "カンリ")
    (expect-no-pending))

  (step "ひらがなモードに戻る"
    (press "q")
    (expect-mode 'hiragana))

  (step "漢字辞書を準備"
    (nskk-scenario-register-dictionary-entry "かんり" '("管理" "監理" "官吏")))

  (step "漢字変換候補の数字選択"
    (type "kanri")
    (let ((candidates (nskk-scenario-begin-conversion "かんり")))
      (should (equal candidates '("管理" "監理" "官吏"))))
    (press "3")
    (press 'return)
    (expect-buffer-contains "官吏")
    (expect-no-pending)
    (when (fboundp 'nskk-update-frequency)
      (nskk-update-frequency "かんり" "官吏"))
    (nskk-scenario-expect-frequency "かんり" "官吏" 1.0))

  (step "英数モードへ切り替え"
    (press "l")
    (expect-mode 'latin)
    (type " AI")
    (expect-buffer-contains "AI"))

  (step "全角英数モードでの入力"
    (press "\\")
    (expect-mode 'zenkaku-latin)
    (type "2025")
    (expect-buffer-contains "AI２０２５"))

  (step "ひらがなに戻して入力継続"
    (press "C-j")
    (expect-mode 'hiragana)
    (type "a")
    (expect-buffer-contains "あ")))

(provide 'nskk-scenario-advanced)
;;; nskk-scenario-advanced.el ends here
