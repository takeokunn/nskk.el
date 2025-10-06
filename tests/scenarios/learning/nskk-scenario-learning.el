;;; nskk-scenario-learning.el --- Learning-oriented scenarios -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Development Team

;;; Commentary:
;; 学習関連挙動を検証するシナリオテスト

;;; Code:

(require 'cl-lib)
(require 'nskk-scenario-dsl)
(require 'nskk-learning-frequency)

(nskk-defscenario learning-frequency-preference
  "頻度学習が候補スコアを更新することを確認"
  :tags '(:learning :frequency)
  :initial-mode 'hiragana

  (step "辞書を登録"
    (nskk-scenario-register-dictionary-entry "かいぎ" '("会議" "怪儀" "懐疑")))

  (step "初回変換で第一候補を選択"
    (type "kaigi")
    (let ((candidates (nskk-scenario-begin-conversion "かいぎ")))
      (should (equal candidates '("会議" "怪儀" "懐疑"))))
    (press 'return)
    (expect-buffer-contains "会議")
    (expect-no-pending))

  (step "バッファをクリア"
    (erase-buffer))

  (step "第二候補を複数回選択して学習"
    (dotimes (_ 3)
      (type "kaigi")
      (nskk-scenario-begin-conversion "かいぎ")
      (press "2")
      (press 'return)
      (erase-buffer)))

  (step "学習結果を確認"
    (let ((score-primary (nskk-get-frequency-score "かいぎ" "会議"))
          (score-secondary (nskk-get-frequency-score "かいぎ" "怪儀")))
      (should (> score-secondary score-primary)))))

(nskk-defscenario learning-frequency-reset-on-cancel
  "変換キャンセル時に候補がクリアされることを確認"
  :tags '(:learning :cancel)
  :initial-mode 'hiragana

  (step "辞書を登録"
    (nskk-scenario-register-dictionary-entry "かんりしょく" '("管理職" "官吏職")))

  (step "変換開始"
    (type "kanrishoku")
    (nskk-scenario-begin-conversion "かんりしょく")
    (expect-candidates '("管理職" "官吏職")))

  (step "キャンセル操作"
    (press "C-g")
    (expect-no-pending)
    (should-not (nskk-state-candidates nskk-current-state))))

(provide 'nskk-scenario-learning)
;;; nskk-scenario-learning.el ends here
