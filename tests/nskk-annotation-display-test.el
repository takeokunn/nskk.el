;;; nskk-annotation-display-test.el --- Tests for nskk-annotation-display.el -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; This file is part of NSKK.

;;; Commentary:

;; nskk-annotation-display.el のテストスイート

;;; Code:

(require 'ert)
(require 'nskk-annotation-display)
(require 'nskk-annotation-parser)

;;; 表示・非表示のテスト

(ert-deftest nskk-annotation-display-test-show-hide ()
  "注釈の表示・非表示をテストする。"
  (with-temp-buffer
    (let ((ann (nskk-parse-annotation "test annotation"))
          (nskk-annotation-display-delay 0))  ; 遅延なし
      ;; 表示
      (nskk-show-annotation ann)
      ;; タイマーが実行されるまで少し待つ
      (sit-for 0.05)
      (should (nskk-annotation-display-current-p))
      (should (equal (nskk-annotation-display-get-current) ann))

      ;; 非表示
      (nskk-hide-annotation)
      (should-not (nskk-annotation-display-current-p))
      (should-not (nskk-annotation-display-get-current)))))

(ert-deftest nskk-annotation-display-test-show-nil ()
  "nil注釈の表示をテストする。"
  (with-temp-buffer
    (nskk-show-annotation nil)
    (should-not (nskk-annotation-display-current-p))))

(ert-deftest nskk-annotation-display-test-auto-hide ()
  "自動非表示をテストする（タイマーベース）。"
  (with-temp-buffer
    (let ((nskk-annotation-display-duration 0.1)
          (nskk-annotation-display-delay 0)
          (ann (nskk-parse-annotation "test")))
      (nskk-show-annotation ann)
      (should (nskk-annotation-display-current-p))
      ;; タイマーが実行されるまで待機
      (sleep-for 0.2)
      (should-not (nskk-annotation-display-current-p)))))

;;; テキスト整形のテスト

(ert-deftest nskk-annotation-display-test-format-text ()
  "テキスト整形をテストする。"
  (let ((ann (nskk-parse-annotation "simple text")))
    (should (stringp (nskk-annotation-display--format-text ann)))))

(ert-deftest nskk-annotation-display-test-wrap-text ()
  "テキスト折り返しをテストする。"
  (let* ((long-text (make-string 100 ?a))
         (wrapped (nskk-annotation-display--wrap-text long-text 50)))
    (should (string-match-p "\n" wrapped))))

(ert-deftest nskk-annotation-display-test-wrap-short-text ()
  "短いテキストの折り返しテスト（折り返し不要）。"
  (let* ((short-text "short")
         (wrapped (nskk-annotation-display--wrap-text short-text 50)))
    (should (equal wrapped short-text))))

;;; 候補との統合のテスト

(ert-deftest nskk-annotation-display-test-with-candidate ()
  "候補と注釈の結合をテストする。"
  (let ((nskk-annotation-display-style 'inline)
        (ann (nskk-parse-annotation "test")))
    (should (string-match-p "test"
                           (nskk-annotation-display-with-candidate "候補" ann)))))

(ert-deftest nskk-annotation-display-test-with-candidate-nil ()
  "注釈なし候補のテスト。"
  (should (equal (nskk-annotation-display-with-candidate "候補" nil)
                "候補")))

(ert-deftest nskk-annotation-display-test-candidates-with-annotations ()
  "複数候補と注釈の処理をテストする。"
  (let ((candidates '(("候補1" . "ann1") ("候補2" . "ann2") ("候補3" . nil)))
        (nskk-annotation-display-style 'none))
    (let ((result (nskk-annotation-display-candidates-with-annotations candidates)))
      (should (= (length result) 3))
      (should (stringp (nth 0 result)))
      (should (stringp (nth 1 result)))
      (should (stringp (nth 2 result))))))

;;; スタイル切り替えのテスト

(ert-deftest nskk-annotation-display-test-toggle-style ()
  "表示スタイル切り替えをテストする。"
  (let ((original-style nskk-annotation-display-style))
    (nskk-annotation-display-toggle-style)
    (should-not (eq nskk-annotation-display-style original-style))
    ;; 元に戻す
    (setq nskk-annotation-display-style original-style)))

;;; クリーンアップのテスト

(ert-deftest nskk-annotation-display-test-cleanup ()
  "クリーンアップをテストする。"
  (with-temp-buffer
    (let ((ann (nskk-parse-annotation "test"))
          (nskk-annotation-display-delay 0))
      (nskk-show-annotation ann)
      (sit-for 0.05)
      (should (nskk-annotation-display-current-p))
      (nskk-annotation-display-cleanup)
      (should-not (nskk-annotation-display-current-p)))))

(provide 'nskk-annotation-display-test)

;;; nskk-annotation-display-test.el ends here
