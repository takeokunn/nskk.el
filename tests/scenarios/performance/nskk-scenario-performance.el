;;; nskk-scenario-performance.el --- Performance scenarios -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Development Team

;;; Commentary:
;; 長文入力や連続変換によるパフォーマンス確認シナリオ

;;; Code:

(require 'cl-lib)
(require 'nskk-scenario-dsl)

(nskk-defscenario performance-long-input
  "長文入力と連続変換の検証"
  :tags '(:performance :stress)
  :initial-mode 'hiragana

  (step "長文を連続入力"
    (let* ((romaji (make-string 200 ?a))
           (expected (make-string 200 ?あ)))
      (cl-loop for chunk-start from 0 below (length romaji) by 20
               for chunk = (substring romaji chunk-start (min (length romaji) (+ chunk-start 20)))
               do (type chunk))
      (should (equal (nskk-buffer-pending-text) expected))))

  (step "長文を確定"
    (press 'return)
    (expect-no-pending)
    (should (= (length (buffer-string)) 200)))

  (step "連続で漢字変換を行う"
    (dotimes (_ 3)
      (type "kanri")
      (press 'space)
      (press 'return))
    (expect-no-pending))

  (step "結果文字列を検証"
    (should (string-match-p (regexp-quote "管理管理管理") (buffer-string)))))

(provide 'nskk-scenario-performance)
;;; nskk-scenario-performance.el ends here
