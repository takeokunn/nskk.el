;;; nskk-scenario-okurigana.el --- Okurigana scenarios -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Development Team

;;; Commentary:
;; 送り仮名処理のシナリオテスト

;;; Code:

(require 'nskk-scenario-dsl)

(nskk-defscenario okurigana-ichidan-verb
  "一段動詞の送り仮名と漢字変換"
  :tags '(:basic :intermediate :okurigana :verb)
  :initial-mode 'hiragana

  (step "辞書エントリを準備"
    (nskk-scenario-register-dictionary-entry "たべる" '("食べる" "食べた")))

  (step "動詞をひらがなで入力"
    (type "taberu")
    (expect-pending "たべる"))

  (step "変換候補を表示"
    (press 'space)
    (expect-candidates '("食べる" "食べた"))
    (expect-candidate-selected "食べる"))

  (step "候補を確定"
    (press 'return)
    (expect-buffer-contains "食べる")
    (expect-no-pending))

  (step "学習スコアを確認"
    (when (fboundp 'nskk-update-frequency)
      (nskk-update-frequency "たべる" "食べる"))
    (nskk-scenario-expect-frequency "たべる" "食べる" 1.0)))

(nskk-defscenario okurigana-i-adjective
  "形容詞の送り仮名と漢字変換"
  :tags '(:basic :intermediate :okurigana :adjective)
  :initial-mode 'hiragana

  (step "辞書エントリを準備"
    (nskk-scenario-register-dictionary-entry "うつくしい" '("美しい" "美しかった")))

  (step "形容詞を入力"
    (type "utsukushii")
    (expect-pending "うつくしい"))

  (step "候補リストを表示"
    (let ((candidates (nskk-scenario-begin-conversion "うつくしい")))
      (should (equal candidates '("美しい" "美しかった")))))

  (step "候補を数字キーで選択"
    (press "2")
    (should (= (nskk-state-candidate-index nskk-current-state) 1)))

  (step "候補を戻して確定"
    (press "x")
    (press 'return)
    (expect-buffer-contains "美しい")
    (expect-no-pending))

  (step "学習結果を確認"
    (when (fboundp 'nskk-update-frequency)
      (nskk-update-frequency "うつくしい" "美しい"))
    (nskk-scenario-expect-frequency "うつくしい" "美しい" 1.0)))

(provide 'nskk-scenario-okurigana)
;;; nskk-scenario-okurigana.el ends here
