;;; nskk-scenario-mode.el --- Mode cycle scenarios -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Development Team

;;; Commentary:
;; 入力モードの切り替えを検証するシナリオテスト

;;; Code:

(require 'cl-lib)
(require 'nskk-scenario-dsl)

(nskk-defscenario mode-cycle-mixed-input
  "モード切り替えで文字種が期待通りになることを確認"
  :tags '(:mode :cycle)
  :initial-mode 'hiragana

  (step "ひらがなで入力"
    (type "kanji")
    (should (string= (buffer-string) "かんじ"))
    (expect-mode 'hiragana))

  (step "半角英数モードに切り替え"
    (press "l")
    (expect-mode 'latin)
    (type "ABC")
    (should (string= (buffer-string) "かんじABC")))

  (step "全角英数モードに切り替え"
    (press "\\")
    (expect-mode 'zenkaku-latin)
    (type "123")
    (should (string= (buffer-string) "かんじABC１２３")))

  (step "ひらがなに戻して追加入力"
    (press "C-j")
    (expect-mode 'hiragana)
    (type "ta")
    (should (string= (buffer-string) "かんじABC１２３た"))))

(provide 'nskk-scenario-mode)
;;; nskk-scenario-mode.el ends here
