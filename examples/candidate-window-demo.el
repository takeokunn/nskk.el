;;; candidate-window-demo.el --- Demo for nskk-candidate-window -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; This file is part of NSKK.

;;; Commentary:

;; nskk-candidate-window.el の動作デモ
;; 各種機能の使用例を提供

;;; Code:

(require 'nskk-candidate-window)

;;; 基本的な使用例

(defun nskk-demo-basic-candidates ()
  "基本的な候補ウィンドウの表示デモ。"
  (interactive)
  (with-current-buffer (get-buffer-create "*NSKK Candidate Demo*")
    (erase-buffer)
    (insert "候補ウィンドウのデモ\n\n")
    (insert "カーソルをここに移動して実行してください → ")
    (switch-to-buffer (current-buffer))
    (goto-char (point-max))

    ;; 候補を表示
    (nskk-show-candidates '("漢字" "感じ" "幹事" "監事" "完治"))))

(defun nskk-demo-annotated-candidates ()
  "注釈付き候補の表示デモ。"
  (interactive)
  (with-current-buffer (get-buffer-create "*NSKK Candidate Demo*")
    (erase-buffer)
    (insert "注釈付き候補のデモ\n\n")
    (insert "カーソルをここに移動して実行してください → ")
    (switch-to-buffer (current-buffer))
    (goto-char (point-max))

    ;; 注釈付き候補を表示
    (let ((candidates '((:text "漢字" :annotation "Chinese character")
                        (:text "感じ" :annotation "feeling")
                        (:text "幹事" :annotation "organizer")
                        (:text "監事" :annotation "inspector")
                        (:text "完治" :annotation "complete recovery"))))
      (nskk-show-candidates candidates))))

(defun nskk-demo-paging ()
  "ページング機能のデモ。"
  (interactive)
  (with-current-buffer (get-buffer-create "*NSKK Candidate Demo*")
    (erase-buffer)
    (insert "ページング機能のデモ\n\n")
    (insert "大量の候補を表示します\n")
    (insert "n: 次のページ、p: 前のページ、f: 最初、l: 最後\n\n")
    (insert "カーソルをここに移動して実行してください → ")
    (switch-to-buffer (current-buffer))
    (goto-char (point-max))

    ;; 大量の候補を生成
    (let ((candidates (cl-loop for i from 1 to 20
                               collect (format "候補%d" i))))
      (nskk-show-candidates candidates)
      (setf (nskk-candidate-window-page-size nskk-candidate-window--current) 5)

      ;; キーバインドを設定
      (local-set-key (kbd "n") (lambda () (interactive) (nskk-scroll-candidates 'next)))
      (local-set-key (kbd "p") (lambda () (interactive) (nskk-scroll-candidates 'previous)))
      (local-set-key (kbd "f") (lambda () (interactive) (nskk-scroll-candidates 'first)))
      (local-set-key (kbd "l") (lambda () (interactive) (nskk-scroll-candidates 'last)))
      (local-set-key (kbd "q") (lambda () (interactive) (nskk-hide-candidates)))

      (message "n/p/f/l でページ移動、q で閉じる"))))

(defun nskk-demo-selection ()
  "候補選択のデモ。"
  (interactive)
  (with-current-buffer (get-buffer-create "*NSKK Candidate Demo*")
    (erase-buffer)
    (insert "候補選択のデモ\n\n")
    (insert "矢印キーで候補を選択できます\n")
    (insert "↓: 次の候補、↑: 前の候補、RET: 選択確定\n\n")
    (insert "カーソルをここに移動して実行してください → ")
    (switch-to-buffer (current-buffer))
    (goto-char (point-max))

    ;; 候補を表示
    (nskk-show-candidates '("選択肢1" "選択肢2" "選択肢3" "選択肢4" "選択肢5"))

    ;; キーバインドを設定
    (local-set-key (kbd "<down>") #'nskk-candidate-window-next)
    (local-set-key (kbd "<up>") #'nskk-candidate-window-previous)
    (local-set-key (kbd "RET")
                   (lambda ()
                     (interactive)
                     (let ((selected (nskk-candidate-window-current-selection)))
                       (message "選択された候補: %s" selected)
                       (nskk-hide-candidates))))
    (local-set-key (kbd "q") (lambda () (interactive) (nskk-hide-candidates)))

    (message "↓/↑ で選択、RET で確定、q で閉じる")))

(defun nskk-demo-custom-style ()
  "カスタムスタイルのデモ。"
  (interactive)
  (with-current-buffer (get-buffer-create "*NSKK Candidate Demo*")
    (erase-buffer)
    (insert "カスタムスタイルのデモ\n\n")
    (insert "候補番号スタイル: アルファベット\n")
    (insert "区切り文字: ' / '\n\n")
    (insert "カーソルをここに移動して実行してください → ")
    (switch-to-buffer (current-buffer))
    (goto-char (point-max))

    ;; カスタム設定で表示
    (let ((nskk-candidate-number-style 'alphabet)
          (nskk-candidate-separator " / "))
      (nskk-show-candidates '("りんご" "ゴリラ" "ラッパ" "パン" "ンジャメナ")))

    (local-set-key (kbd "q") (lambda () (interactive) (nskk-hide-candidates)))))

(defun nskk-demo-all ()
  "全デモを順番に実行。"
  (interactive)
  (message "=== NSKK候補ウィンドウ デモ ===")
  (message "1. 基本的な候補表示")
  (nskk-demo-basic-candidates)
  (sit-for 3)
  (nskk-hide-candidates)

  (message "2. 注釈付き候補")
  (nskk-demo-annotated-candidates)
  (sit-for 3)
  (nskk-hide-candidates)

  (message "3. ページング機能（n/p で操作）")
  (nskk-demo-paging))

;;; インタラクティブな使用例

(defun nskk-demo-interactive ()
  "インタラクティブなデモを起動。"
  (interactive)
  (let ((choice (read-char-choice
                 "デモを選択: [1]基本 [2]注釈 [3]ページング [4]選択 [5]カスタム [a]全て [q]終了: "
                 '(?1 ?2 ?3 ?4 ?5 ?a ?q))))
    (pcase choice
      (?1 (nskk-demo-basic-candidates))
      (?2 (nskk-demo-annotated-candidates))
      (?3 (nskk-demo-paging))
      (?4 (nskk-demo-selection))
      (?5 (nskk-demo-custom-style))
      (?a (nskk-demo-all))
      (?q (message "デモを終了しました")))))

(provide 'candidate-window-demo)
;;; candidate-window-demo.el ends here
