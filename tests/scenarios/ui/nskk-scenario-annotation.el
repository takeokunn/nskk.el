;;; nskk-scenario-annotation.el --- Annotation display scenarios -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Development Team

;;; Commentary:
;; 注釈表示スタイルの切り替えを検証するシナリオテスト

;;; Code:

(require 'cl-lib)
(require 'nskk-scenario-dsl)
(require 'nskk-annotation-display)
(require 'nskk-annotation-parser)

(nskk-defscenario annotation-style-cycle
  "注釈表示スタイルのトグル挙動を確認"
  :tags '(:ui :annotation)
  :initial-mode 'hiragana

  (step "inlineスタイルで注釈を表示"
    (setq nskk-annotation-display-style 'inline
          nskk-annotation-display-delay 0
          nskk-annotation-display-duration nil)
    (nskk-show-annotation (nskk-parse-annotation "inline"))
    (nskk-scenario-expect-annotation "inline"))

  (step "スタイルをecho-areaへ変更"
    (setq nskk-annotation-display-style 'echo-area)
    (nskk-show-annotation (nskk-parse-annotation "echo"))
    (nskk-scenario-expect-annotation "echo"))

  (step "スタイルをoverlayへ変更"
    (setq nskk-annotation-display-style 'overlay)
    (nskk-show-annotation (nskk-parse-annotation "overlay"))
    (nskk-scenario-expect-annotation "overlay"))

  (step "表示を無効化"
    (setq nskk-annotation-display-style 'none)
    (nskk-hide-annotation)
    (should-not (nskk-annotation-display-current-p))))

(provide 'nskk-scenario-annotation)
;;; nskk-scenario-annotation.el ends here
