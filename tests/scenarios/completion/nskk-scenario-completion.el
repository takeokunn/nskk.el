;;; nskk-scenario-completion.el --- Completion scenarios -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Development Team

;;; Commentary:
;; 補完処理の挙動を確認するシナリオテスト

;;; Code:

(require 'cl-lib)
(require 'nskk-scenario-dsl)

(nskk-defscenario completion-prefix-expansion
  "Tab補完で読みが拡張されることを確認"
  :tags '(:completion :basic)
  :initial-mode 'hiragana

  (step "辞書を登録"
    (nskk-scenario-register-dictionary-entry "しんかん" '("新刊"))
    (nskk-scenario-register-dictionary-entry "しんかんせん" '("新幹線")))

  (step "読みの一部を入力"
    (type "shinkan")
    (expect-pending "しんかん"))

  (step "Tab補完を実行"
    (press 'tab)
    (should (string= (nskk-buffer-pending-text) "しんかん"))
    (let ((candidates (nskk-scenario-begin-conversion "しんかん")))
      (should (equal candidates '("新刊")))))

  (step "さらに入力して第二候補を表示"
    (type "sen")
    (should (string= (nskk-buffer-pending-text) "しんかんせん"))
    (let ((candidates (nskk-scenario-begin-conversion "しんかんせん")))
      (should (equal candidates '("新幹線")))))

  (step "候補を確定"
    (press 'return)
    (expect-buffer-contains "新幹線")
    (expect-no-pending)))

(nskk-defscenario completion-no-op-when-unknown
  "補完対象がない場合に入力が変化しないことを確認"
  :tags '(:completion :edge)
  :initial-mode 'hiragana

  (step "未知語を入力"
    (type "xyz")
    (expect-pending "xyz"))

  (step "Tab補完を実行"
    (press 'tab)
    (expect-pending "xyz")
    (should-not (nskk-state-candidates nskk-current-state))))

(provide 'nskk-scenario-completion)
;;; nskk-scenario-completion.el ends here
