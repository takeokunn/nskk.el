;;; nskk-custom-annotation-test.el --- Tests for nskk-custom-annotation.el -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; This file is part of NSKK.

;;; Commentary:

;; nskk-custom-annotation.el のテストスイート

;;; Code:

(require 'ert)
(require 'nskk-custom-annotation)

;;; 基本操作のテスト

(ert-deftest nskk-custom-annotation-test-add-get ()
  "注釈の追加と取得をテストする。"
  (let ((nskk-custom-annotation-auto-save nil))
    ;; テーブルをクリア
    (clrhash nskk-custom-annotation--table)

    ;; 注釈を追加
    (nskk-custom-annotation-add "かんじ" "漢字" "Chinese characters")

    ;; 取得
    (should (equal (nskk-custom-annotation-get "かんじ" "漢字")
                  "Chinese characters"))))

(ert-deftest nskk-custom-annotation-test-get-nonexistent ()
  "存在しない注釈の取得をテストする。"
  (clrhash nskk-custom-annotation--table)
  (should (null (nskk-custom-annotation-get "ない" "候補"))))

(ert-deftest nskk-custom-annotation-test-edit ()
  "注釈の編集をテストする。"
  (let ((nskk-custom-annotation-auto-save nil))
    (clrhash nskk-custom-annotation--table)

    ;; 追加
    (nskk-custom-annotation-add "かんじ" "漢字" "old annotation")

    ;; 編集
    (nskk-custom-annotation-edit "かんじ" "漢字" "new annotation")

    ;; 確認
    (should (equal (nskk-custom-annotation-get "かんじ" "漢字")
                  "new annotation"))))

(ert-deftest nskk-custom-annotation-test-remove ()
  "注釈の削除をテストする。"
  (let ((nskk-custom-annotation-auto-save nil))
    (clrhash nskk-custom-annotation--table)

    ;; 追加
    (nskk-custom-annotation-add "かんじ" "漢字" "annotation")

    ;; 削除
    (nskk-custom-annotation-remove "かんじ" "漢字")

    ;; 確認
    (should (null (nskk-custom-annotation-get "かんじ" "漢字")))))

(ert-deftest nskk-custom-annotation-test-has-p ()
  "注釈の有無判定をテストする。"
  (let ((nskk-custom-annotation-auto-save nil))
    (clrhash nskk-custom-annotation--table)

    ;; 注釈を追加
    (nskk-custom-annotation-add "かんじ" "漢字" "annotation")

    (should (nskk-custom-annotation-has-p "かんじ" "漢字"))
    (should-not (nskk-custom-annotation-has-p "ない" "候補"))))

;;; テンプレート機能のテスト

(ert-deftest nskk-custom-annotation-test-template ()
  "テンプレートを使用した注釈追加をテストする。"
  (let ((nskk-custom-annotation-auto-save nil)
        (nskk-custom-annotation-enable-templates t))
    (clrhash nskk-custom-annotation--table)

    ;; テンプレートを使用
    (nskk-custom-annotation-add-from-template
     "かんじ" "漢字" 'english "Chinese characters")

    ;; 取得
    (let ((ann (nskk-custom-annotation-get "かんじ" "漢字")))
      (should (string-match-p "Chinese characters" ann)))))

(ert-deftest nskk-custom-annotation-test-template-disabled ()
  "テンプレート無効時のテスト。"
  (let ((nskk-custom-annotation-enable-templates-orig nskk-custom-annotation-enable-templates)
        (nskk-custom-annotation-auto-save nil))
    (unwind-protect
        (progn
          (setq nskk-custom-annotation-enable-templates nil)
          ;; 無効時は何も起きない（nilを返す）
          (should-not
           (nskk-custom-annotation-add-from-template
            "かんじ" "漢字" 'english "test")))
      (setq nskk-custom-annotation-enable-templates nskk-custom-annotation-enable-templates-orig))))

;;; ファイルI/Oのテスト

(ert-deftest nskk-custom-annotation-test-save-load ()
  "注釈の保存と読み込みをテストする。"
  (let ((temp-file (make-temp-file "nskk-annotation-test"))
        (nskk-custom-annotation-auto-save nil))
    (unwind-protect
        (progn
          ;; テーブルをクリア
          (clrhash nskk-custom-annotation--table)

          ;; 注釈を追加
          (nskk-custom-annotation-add "かんじ" "漢字" "annotation1")
          (nskk-custom-annotation-add "あい" "愛" "annotation2")

          ;; 保存
          (nskk-custom-annotation-save temp-file)
          (should (file-exists-p temp-file))

          ;; クリア
          (clrhash nskk-custom-annotation--table)
          (should (= (hash-table-count nskk-custom-annotation--table) 0))

          ;; 読み込み
          (nskk-custom-annotation-load temp-file)
          (should (= (hash-table-count nskk-custom-annotation--table) 2))
          (should (equal (nskk-custom-annotation-get "かんじ" "漢字")
                        "annotation1"))
          (should (equal (nskk-custom-annotation-get "あい" "愛")
                        "annotation2")))
      ;; クリーンアップ
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest nskk-custom-annotation-test-save-if-modified ()
  "変更時のみ保存をテストする。"
  (let* ((temp-file (make-temp-file "nskk-annotation-test"))
         (nskk-custom-annotation-auto-save nil)
         (nskk-custom-annotation-file temp-file))
    (unwind-protect
        (progn
          (clrhash nskk-custom-annotation--table)
          (setq nskk-custom-annotation--modified nil)

          ;; 未変更状態では保存されない
          (nskk-custom-annotation-save-if-modified)

          ;; 注釈を追加（変更あり）
          (nskk-custom-annotation-add "かんじ" "漢字" "annotation")
          (should nskk-custom-annotation--modified)

          ;; 保存
          (nskk-custom-annotation-save-if-modified)
          (should-not nskk-custom-annotation--modified))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

;;; 一括操作のテスト

(ert-deftest nskk-custom-annotation-test-clear ()
  "全注釈削除をテストする（非対話的）。"
  (let ((nskk-custom-annotation-auto-save nil))
    (clrhash nskk-custom-annotation--table)

    ;; 注釈を追加
    (nskk-custom-annotation-add "かんじ" "漢字" "annotation")
    (should (> (hash-table-count nskk-custom-annotation--table) 0))

    ;; クリア（非対話的にテスト）
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
      (nskk-custom-annotation-clear))

    (should (= (hash-table-count nskk-custom-annotation--table) 0))))

(ert-deftest nskk-custom-annotation-test-export ()
  "SKK形式へのエクスポートをテストする。"
  (let ((temp-file (make-temp-file "nskk-annotation-export-test"))
        (nskk-custom-annotation-auto-save nil))
    (unwind-protect
        (progn
          (clrhash nskk-custom-annotation--table)

          ;; 注釈を追加
          (nskk-custom-annotation-add "かんじ" "漢字" "Chinese")
          (nskk-custom-annotation-add "かんじ" "幹事" "organizer")

          ;; エクスポート
          (nskk-custom-annotation-export temp-file)
          (should (file-exists-p temp-file))

          ;; ファイル内容を確認
          (with-temp-buffer
            (insert-file-contents temp-file)
            (goto-char (point-min))
            (should (search-forward "かんじ" nil t))))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

;;; 統計機能のテスト

(ert-deftest nskk-custom-annotation-test-statistics ()
  "統計情報生成をテストする。"
  (let ((nskk-custom-annotation-auto-save nil))
    (clrhash nskk-custom-annotation--table)

    ;; 注釈を追加
    (nskk-custom-annotation-add "かんじ" "漢字" "annotation1")
    (nskk-custom-annotation-add "かんじ" "幹事" "annotation2")
    (nskk-custom-annotation-add "あい" "愛" "annotation3")

    (let ((stats (nskk-custom-annotation-statistics)))
      (should (= (plist-get stats :total) 3))
      (should (= (plist-get stats :unique-midashi) 2))
      (should (> (plist-get stats :avg-length) 0)))))

;;; フックのテスト

(ert-deftest nskk-custom-annotation-test-hooks ()
  "フックの動作をテストする。"
  (let ((nskk-custom-annotation-auto-save nil)
        (hook-called nil))
    (clrhash nskk-custom-annotation--table)

    ;; フックを設定
    (add-hook 'nskk-custom-annotation-after-add-hook
              (lambda (_m _c _a) (setq hook-called t)))

    ;; 注釈を追加
    (nskk-custom-annotation-add "かんじ" "漢字" "annotation")

    ;; フックが呼ばれたことを確認
    (should hook-called)

    ;; フックをクリア
    (setq nskk-custom-annotation-after-add-hook nil)))

(provide 'nskk-custom-annotation-test)

;;; nskk-custom-annotation-test.el ends here
