;;; nskk-candidate-window-test.el --- Tests for nskk-candidate-window.el -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; This file is part of NSKK.

;;; Commentary:

;; nskk-candidate-window.el のテストスイート
;; ERTフレームワークを使用した包括的なテストを実装

;;; Code:

(require 'ert)
(require 'nskk-candidate-window)
(require 'nskk-test-framework)

;;; データ正規化テスト

(ert-deftest nskk-candidate-window-test-normalize-string ()
  "文字列候補の正規化をテストする。"
  (let ((result (nskk-candidate-window--normalize-candidate "候補")))
    (should (string= result "候補"))))

(ert-deftest nskk-candidate-window-test-normalize-plist ()
  "plist候補の正規化をテストする。"
  (let* ((input '(:text "候補" :annotation "注釈"))
         (result (nskk-candidate-window--normalize-candidate input)))
    (should (equal result input))))

(ert-deftest nskk-candidate-window-test-normalize-other ()
  "その他の型候補の正規化をテストする。"
  (let ((result (nskk-candidate-window--normalize-candidate 123)))
    (should (stringp result))
    (should (string= result "123"))))

(ert-deftest nskk-candidate-window-test-get-text-string ()
  "文字列候補からテキスト取得をテストする。"
  (let ((text (nskk-candidate-window--get-text "候補")))
    (should (string= text "候補"))))

(ert-deftest nskk-candidate-window-test-get-text-plist ()
  "plist候補からテキスト取得をテストする。"
  (let ((text (nskk-candidate-window--get-text
               '(:text "候補" :annotation "注釈"))))
    (should (string= text "候補"))))

(ert-deftest nskk-candidate-window-test-get-annotation-string ()
  "文字列候補から注釈取得をテストする（注釈なし）。"
  (let ((annotation (nskk-candidate-window--get-annotation "候補")))
    (should (null annotation))))

(ert-deftest nskk-candidate-window-test-get-annotation-plist ()
  "plist候補から注釈取得をテストする。"
  (let ((nskk-candidate-show-annotations t)
        (annotation (nskk-candidate-window--get-annotation
                     '(:text "候補" :annotation "注釈"))))
    (should (string= annotation "注釈"))))

(ert-deftest nskk-candidate-window-test-get-annotation-disabled ()
  "注釈表示が無効な場合のテスト。"
  (let ((nskk-candidate-show-annotations nil))
    (let ((annotation (nskk-candidate-window--get-annotation
                       '(:text "候補" :annotation "注釈"))))
      (should (null annotation)))))

;;; 番号フォーマットテスト

(ert-deftest nskk-candidate-window-test-format-number-arabic ()
  "アラビア数字での番号フォーマットをテストする。"
  (let ((nskk-candidate-number-style 'arabic))
    (should (string= (nskk-candidate-window--format-number 0) "1"))
    (should (string= (nskk-candidate-window--format-number 5) "6"))
    (should (string= (nskk-candidate-window--format-number 9) "10"))))

(ert-deftest nskk-candidate-window-test-format-number-alphabet ()
  "アルファベットでの番号フォーマットをテストする。"
  (let ((nskk-candidate-number-style 'alphabet))
    (should (string= (nskk-candidate-window--format-number 0) "a"))
    (should (string= (nskk-candidate-window--format-number 5) "f"))
    (should (string= (nskk-candidate-window--format-number 25) "z"))
    ;; 26を超えた場合は数字にフォールバック
    (should (string= (nskk-candidate-window--format-number 26) "27"))))

;;; 候補フォーマットテスト

(ert-deftest nskk-candidate-window-test-format-candidate-simple ()
  "シンプルな候補のフォーマットをテストする。"
  (let ((nskk-candidate-show-index t)
        (nskk-candidate-number-style 'arabic)
        (nskk-candidate-show-annotations t)
        (result (nskk-candidate-window--format-candidate "候補" 0 nil)))
    (should (stringp result))
    (should (string-match-p "1:" result))
    (should (string-match-p "候補" result))))

(ert-deftest nskk-candidate-window-test-format-candidate-with-annotation ()
  "注釈付き候補のフォーマットをテストする。"
  (let ((nskk-candidate-show-index t)
        (nskk-candidate-number-style 'arabic)
        (nskk-candidate-show-annotations t)
        (nskk-candidate-annotation-separator ": ")
        (candidate '(:text "候補" :annotation "注釈")))
    (let ((result (nskk-candidate-window--format-candidate candidate 0 nil)))
      (should (stringp result))
      (should (string-match-p "候補" result))
      (should (string-match-p "注釈" result)))))

(ert-deftest nskk-candidate-window-test-format-candidate-selected ()
  "選択中候補のフォーマットをテストする。"
  (let ((nskk-candidate-show-index t)
        (result (nskk-candidate-window--format-candidate "候補" 0 t)))
    (should (stringp result))
    ;; 選択中のフェイスが適用されていることを確認
    (should (text-property-any 0 (length result) 'face 'nskk-candidate-selected-face result))))

(ert-deftest nskk-candidate-window-test-format-candidate-no-index ()
  "番号なし候補のフォーマットをテストする。"
  (let ((nskk-candidate-show-index nil))
    (let ((result (nskk-candidate-window--format-candidate "候補" 0 nil)))
      (should (stringp result))
      (should-not (string-match-p "^[0-9]+:" result)))))

;;; ページフォーマットテスト

(ert-deftest nskk-candidate-window-test-format-page-single ()
  "単一ページのフォーマットをテストする。"
  (let* ((candidates '("候補1" "候補2" "候補3"))
         (window (nskk-candidate-window--create
                  :candidates candidates
                  :selected-index 0
                  :page-size 7
                  :current-page 0))
         (result (nskk-candidate-window--format-page window)))
    (should (stringp result))
    (should (string-match-p "候補1" result))
    (should (string-match-p "候補2" result))
    (should (string-match-p "候補3" result))
    ;; ページ情報は表示されない（1ページのみ）
    (should-not (string-match-p "\\[1/1\\]" result))))

(ert-deftest nskk-candidate-window-test-format-page-multiple ()
  "複数ページのフォーマットをテストする。"
  (let* ((candidates '("候補1" "候補2" "候補3" "候補4" "候補5"
                       "候補6" "候補7" "候補8" "候補9" "候補10"))
         (window (nskk-candidate-window--create
                  :candidates candidates
                  :selected-index 0
                  :page-size 3
                  :current-page 0))
         (result (nskk-candidate-window--format-page window)))
    (should (stringp result))
    ;; 最初の3つのみ表示
    (should (string-match-p "候補1" result))
    (should (string-match-p "候補2" result))
    (should (string-match-p "候補3" result))
    (should-not (string-match-p "候補4" result))
    ;; ページ情報が表示される
    (should (string-match-p "\\[1/4\\]" result))))

(ert-deftest nskk-candidate-window-test-format-page-second ()
  "2ページ目のフォーマットをテストする。"
  (let* ((candidates '("候補1" "候補2" "候補3" "候補4" "候補5"
                       "候補6" "候補7" "候補8"))
         (window (nskk-candidate-window--create
                  :candidates candidates
                  :selected-index 3
                  :page-size 3
                  :current-page 1))
         (result (nskk-candidate-window--format-page window)))
    (should (stringp result))
    ;; 2ページ目の候補が表示される
    (should (string-match-p "候補4" result))
    (should (string-match-p "候補5" result))
    (should (string-match-p "候補6" result))
    (should-not (string-match-p "候補1" result))
    (should (string-match-p "\\[2/3\\]" result))))

;;; 表示位置計算テスト

(ert-deftest nskk-candidate-window-test-calculate-position ()
  "表示位置の計算をテストする。"
  (with-temp-buffer
    (insert "line1\nline2\nline3\n")
    (goto-char (point-min))
    (let ((pos-info (nskk-candidate-window--calculate-position (point))))
      (should (consp pos-info))
      (should (integerp (car pos-info)))
      (should (integerp (cdr pos-info))))))

;;; ウィンドウ表示・非表示テスト

(ert-deftest nskk-candidate-window-test-show-hide ()
  "候補ウィンドウの表示・非表示をテストする。"
  (with-temp-buffer
    (insert "test\n")
    (goto-char (point-min))
    (let ((window (nskk-show-candidates '("候補1" "候補2" "候補3"))))
      (should (nskk-candidate-window-p window))
      (should (overlayp (nskk-candidate-window-overlay window)))
      (should (nskk-candidate-window-visible-p))

      (nskk-hide-candidates)
      (should-not (nskk-candidate-window-visible-p)))))

(ert-deftest nskk-candidate-window-test-show-with-position ()
  "位置指定での候補ウィンドウ表示をテストする。"
  (with-temp-buffer
    (insert "test\n")
    (let* ((pos (point-min))
           (window (nskk-show-candidates '("候補1" "候補2") pos)))
      (should (nskk-candidate-window-p window))
      (should (= (nskk-candidate-window-position window) pos))
      (nskk-hide-candidates))))

(ert-deftest nskk-candidate-window-test-show-empty-candidates ()
  "空の候補リストでの表示をテストする。"
  (with-temp-buffer
    (insert "test\n")
    (let ((window (nskk-show-candidates '())))
      (should (nskk-candidate-window-p window))
      (should (null (nskk-candidate-window-candidates window)))
      (nskk-hide-candidates))))

;;; 候補更新テスト

(ert-deftest nskk-candidate-window-test-update-candidates ()
  "候補の更新をテストする。"
  (with-temp-buffer
    (insert "test\n")
    (let* ((initial-candidates '("候補1" "候補2"))
           (new-candidates '("新候補1" "新候補2" "新候補3"))
           (window (nskk-show-candidates initial-candidates)))

      (nskk-update-candidates new-candidates 1)

      (should (= (length (nskk-candidate-window-candidates window)) 3))
      (should (= (nskk-candidate-window-selected-index window) 1))

      (nskk-hide-candidates))))

(ert-deftest nskk-candidate-window-test-update-candidates-page-change ()
  "候補更新時のページ変更をテストする。"
  (with-temp-buffer
    (insert "test\n")
    (let* ((candidates '("候補1" "候補2" "候補3" "候補4" "候補5"
                         "候補6" "候補7" "候補8"))
           (window (nskk-show-candidates candidates)))

      (setf (nskk-candidate-window-page-size window) 3)

      ;; 2ページ目の候補を選択
      (nskk-update-candidates candidates 4)

      (should (= (nskk-candidate-window-selected-index window) 4))
      (should (= (nskk-candidate-window-current-page window) 1))

      (nskk-hide-candidates))))

;;; スクロールテスト

(ert-deftest nskk-candidate-window-test-scroll-next ()
  "次ページへのスクロールをテストする。"
  (with-temp-buffer
    (insert "test\n")
    (let* ((candidates '("候補1" "候補2" "候補3" "候補4" "候補5"
                         "候補6" "候補7" "候補8"))
           (window (nskk-show-candidates candidates)))

      (setf (nskk-candidate-window-page-size window) 3)

      ;; 次ページへ
      (nskk-scroll-candidates 'next)
      (should (= (nskk-candidate-window-current-page window) 1))

      ;; さらに次へ
      (nskk-scroll-candidates 'next)
      (should (= (nskk-candidate-window-current-page window) 2))

      ;; 最後のページで次へ（変化なし）
      (nskk-scroll-candidates 'next)
      (should (= (nskk-candidate-window-current-page window) 2))

      (nskk-hide-candidates))))

(ert-deftest nskk-candidate-window-test-scroll-previous ()
  "前ページへのスクロールをテストする。"
  (with-temp-buffer
    (insert "test\n")
    (let* ((candidates '("候補1" "候補2" "候補3" "候補4" "候補5"
                         "候補6" "候補7" "候補8"))
           (window (nskk-show-candidates candidates)))

      (setf (nskk-candidate-window-page-size window) 3)

      ;; 2ページ目へ移動
      (nskk-scroll-candidates 'next)
      (should (= (nskk-candidate-window-current-page window) 1))

      ;; 前ページへ
      (nskk-scroll-candidates 'previous)
      (should (= (nskk-candidate-window-current-page window) 0))

      ;; 最初のページで前へ（変化なし）
      (nskk-scroll-candidates 'previous)
      (should (= (nskk-candidate-window-current-page window) 0))

      (nskk-hide-candidates))))

(ert-deftest nskk-candidate-window-test-scroll-first-last ()
  "最初/最後のページへのスクロールをテストする。"
  (with-temp-buffer
    (insert "test\n")
    (let* ((candidates '("候補1" "候補2" "候補3" "候補4" "候補5"
                         "候補6" "候補7" "候補8"))
           (window (nskk-show-candidates candidates)))

      (setf (nskk-candidate-window-page-size window) 3)

      ;; 最後のページへ
      (nskk-scroll-candidates 'last)
      (should (= (nskk-candidate-window-current-page window) 2))

      ;; 最初のページへ
      (nskk-scroll-candidates 'first)
      (should (= (nskk-candidate-window-current-page window) 0))

      (nskk-hide-candidates))))

(ert-deftest nskk-candidate-window-test-scroll-not-displayed ()
  "ウィンドウ非表示時のスクロールエラーをテストする。"
  (should-error (nskk-scroll-candidates 'next)))

;;; 候補選択テスト

(ert-deftest nskk-candidate-window-test-select-candidate ()
  "候補選択をテストする。"
  (with-temp-buffer
    (insert "test\n")
    (let* ((candidates '("候補1" "候補2" "候補3" "候補4" "候補5"))
           (window (nskk-show-candidates candidates)))

      (nskk-select-candidate 2)
      (should (= (nskk-candidate-window-selected-index window) 2))

      (nskk-select-candidate 4)
      (should (= (nskk-candidate-window-selected-index window) 4))

      (nskk-hide-candidates))))

(ert-deftest nskk-candidate-window-test-select-candidate-invalid ()
  "無効なインデックスでの候補選択をテストする。"
  (with-temp-buffer
    (insert "test\n")
    (let* ((candidates '("候補1" "候補2" "候補3"))
           (window (nskk-show-candidates candidates)))

      ;; 範囲外のインデックス（変化なし）
      (nskk-select-candidate 10)
      (should (= (nskk-candidate-window-selected-index window) 0))

      (nskk-select-candidate -1)
      (should (= (nskk-candidate-window-selected-index window) 0))

      (nskk-hide-candidates))))

(ert-deftest nskk-candidate-window-test-select-candidate-page-change ()
  "候補選択時のページ変更をテストする。"
  (with-temp-buffer
    (insert "test\n")
    (let* ((candidates '("候補1" "候補2" "候補3" "候補4" "候補5"
                         "候補6" "候補7" "候補8"))
           (window (nskk-show-candidates candidates)))

      (setf (nskk-candidate-window-page-size window) 3)

      ;; 2ページ目の候補を選択
      (nskk-select-candidate 5)
      (should (= (nskk-candidate-window-current-page window) 1))

      (nskk-hide-candidates))))

(ert-deftest nskk-candidate-window-test-select-not-displayed ()
  "ウィンドウ非表示時の選択エラーをテストする。"
  (should-error (nskk-select-candidate 0)))

;;; 現在選択候補取得テスト

(ert-deftest nskk-candidate-window-test-current-selection ()
  "現在選択中の候補取得をテストする。"
  (with-temp-buffer
    (insert "test\n")
    (let ((candidates '("候補1" "候補2" "候補3")))
      (nskk-show-candidates candidates)

      (should (string= (nskk-candidate-window-current-selection) "候補1"))

      (nskk-select-candidate 1)
      (should (string= (nskk-candidate-window-current-selection) "候補2"))

      (nskk-hide-candidates))))

(ert-deftest nskk-candidate-window-test-current-selection-plist ()
  "plist候補の現在選択取得をテストする。"
  (with-temp-buffer
    (insert "test\n")
    (let ((candidates '((:text "候補1" :annotation "注釈1")
                        (:text "候補2" :annotation "注釈2"))))
      (nskk-show-candidates candidates)

      (let ((selected (nskk-candidate-window-current-selection)))
        (should (listp selected))
        (should (string= (plist-get selected :text) "候補1")))

      (nskk-hide-candidates))))

(ert-deftest nskk-candidate-window-test-current-selection-not-displayed ()
  "ウィンドウ非表示時の選択候補取得をテストする。"
  (should (null (nskk-candidate-window-current-selection))))

;;; ユーティリティ関数テスト

(ert-deftest nskk-candidate-window-test-next-previous ()
  "次/前候補選択をテストする。"
  (with-temp-buffer
    (insert "test\n")
    (let ((candidates '("候補1" "候補2" "候補3" "候補4" "候補5")))
      (nskk-show-candidates candidates)

      ;; 次へ
      (nskk-candidate-window-next)
      (should (= (nskk-candidate-window-selected-index
                  nskk-candidate-window--current) 1))

      (nskk-candidate-window-next)
      (should (= (nskk-candidate-window-selected-index
                  nskk-candidate-window--current) 2))

      ;; 前へ
      (nskk-candidate-window-previous)
      (should (= (nskk-candidate-window-selected-index
                  nskk-candidate-window--current) 1))

      (nskk-hide-candidates))))

(ert-deftest nskk-candidate-window-test-next-at-end ()
  "最後の候補で次へ移動をテストする。"
  (with-temp-buffer
    (insert "test\n")
    (let ((candidates '("候補1" "候補2" "候補3")))
      (nskk-show-candidates candidates)

      ;; 最後の候補へ
      (nskk-select-candidate 2)

      ;; 次へ（変化なし）
      (nskk-candidate-window-next)
      (should (= (nskk-candidate-window-selected-index
                  nskk-candidate-window--current) 2))

      (nskk-hide-candidates))))

(ert-deftest nskk-candidate-window-test-previous-at-start ()
  "最初の候補で前へ移動をテストする。"
  (with-temp-buffer
    (insert "test\n")
    (let ((candidates '("候補1" "候補2" "候補3")))
      (nskk-show-candidates candidates)

      ;; 前へ（変化なし）
      (nskk-candidate-window-previous)
      (should (= (nskk-candidate-window-selected-index
                  nskk-candidate-window--current) 0))

      (nskk-hide-candidates))))

(ert-deftest nskk-candidate-window-test-page-info ()
  "ページ情報取得をテストする。"
  (with-temp-buffer
    (insert "test\n")
    (let* ((candidates '("候補1" "候補2" "候補3" "候補4" "候補5"
                         "候補6" "候補7" "候補8"))
           (window (nskk-show-candidates candidates)))

      (setf (nskk-candidate-window-page-size window) 3)

      (let ((info (nskk-candidate-window-page-info)))
        (should (consp info))
        (should (= (car info) 1))
        (should (= (cdr info) 3)))

      ;; 2ページ目へ
      (nskk-scroll-candidates 'next)

      (let ((info (nskk-candidate-window-page-info)))
        (should (= (car info) 2))
        (should (= (cdr info) 3)))

      (nskk-hide-candidates))))

(ert-deftest nskk-candidate-window-test-page-info-not-displayed ()
  "ウィンドウ非表示時のページ情報取得をテストする。"
  (should (null (nskk-candidate-window-page-info))))

;;; UI描画テスト

(ert-deftest nskk-candidate-window-test-overlay-creation ()
  "overlayの作成をテストする。"
  (with-temp-buffer
    (insert "line1\nline2\nline3\n")
    (goto-char (point-min))
    (let ((overlay (nskk-candidate-window--create-overlay (point))))
      (should (overlayp overlay))
      (should (overlay-get overlay 'window))
      (should (= (overlay-get overlay 'priority) 1000))
      (delete-overlay overlay))))

(ert-deftest nskk-candidate-window-test-overlay-update ()
  "overlayの更新をテストする。"
  (with-temp-buffer
    (insert "line1\nline2\nline3\n")
    (goto-char (point-min))
    (let ((overlay (nskk-candidate-window--create-overlay (point))))
      (nskk-candidate-window--update-overlay overlay "test content")
      (should (overlay-get overlay 'before-string))
      (should (string-match-p "test content" (overlay-get overlay 'before-string)))
      (delete-overlay overlay))))

(ert-deftest nskk-candidate-window-test-overlay-delete ()
  "overlayの削除をテストする。"
  (with-temp-buffer
    (insert "line1\nline2\nline3\n")
    (goto-char (point-min))
    (let ((overlay (nskk-candidate-window--create-overlay (point))))
      (nskk-candidate-window--delete-overlay overlay)
      ;; 削除後はoverlayが無効になる
      (should-not (overlay-buffer overlay)))))

;;; 統合テスト

(ert-deftest nskk-candidate-window-test-integration-basic ()
  "基本的な統合テスト。"
  (with-temp-buffer
    (insert "test\n")
    (goto-char (point-min))

    ;; 表示
    (let ((candidates '("候補1" "候補2" "候補3" "候補4" "候補5")))
      (nskk-show-candidates candidates)
      (should (nskk-candidate-window-visible-p))

      ;; 選択変更
      (nskk-candidate-window-next)
      (should (string= (nskk-candidate-window-current-selection) "候補2"))

      ;; 更新
      (nskk-update-candidates '("新候補1" "新候補2") 0)
      (should (string= (nskk-candidate-window-current-selection) "新候補1"))

      ;; 非表示
      (nskk-hide-candidates)
      (should-not (nskk-candidate-window-visible-p)))))

(ert-deftest nskk-candidate-window-test-integration-paging ()
  "ページング統合テスト。"
  (with-temp-buffer
    (insert "test\n")
    (goto-char (point-min))

    (let ((candidates '("候補1" "候補2" "候補3" "候補4" "候補5"
                        "候補6" "候補7" "候補8" "候補9" "候補10")))
      (nskk-show-candidates candidates)

      (setf (nskk-candidate-window-page-size nskk-candidate-window--current) 3)

      ;; 最初のページ
      (should (= (car (nskk-candidate-window-page-info)) 1))

      ;; 次のページへ
      (nskk-scroll-candidates 'next)
      (should (= (car (nskk-candidate-window-page-info)) 2))

      ;; 候補選択（ページ跨ぎ）
      (nskk-select-candidate 8)
      (should (= (car (nskk-candidate-window-page-info)) 3))

      ;; 最初のページへ
      (nskk-scroll-candidates 'first)
      (should (= (car (nskk-candidate-window-page-info)) 1))

      (nskk-hide-candidates))))

(ert-deftest nskk-candidate-window-test-integration-annotation ()
  "注釈表示の統合テスト。"
  (with-temp-buffer
    (insert "test\n")
    (goto-char (point-min))

    (let ((nskk-candidate-show-annotations t)
          (candidates '((:text "候補1" :annotation "注釈1")
                        (:text "候補2" :annotation "注釈2")
                        (:text "候補3" :annotation "注釈3"))))
      (nskk-show-candidates candidates)

      (let ((selected (nskk-candidate-window-current-selection)))
        (should (string= (plist-get selected :text) "候補1"))
        (should (string= (plist-get selected :annotation) "注釈1")))

      (nskk-candidate-window-next)

      (let ((selected (nskk-candidate-window-current-selection)))
        (should (string= (plist-get selected :text) "候補2"))
        (should (string= (plist-get selected :annotation) "注釈2")))

      (nskk-hide-candidates))))

(provide 'nskk-candidate-window-test)
;;; nskk-candidate-window-test.el ends here
