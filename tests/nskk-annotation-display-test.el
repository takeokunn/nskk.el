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

;;; 遅延表示のテスト

(ert-deftest nskk-annotation-display-test-delayed-show ()
  "遅延表示をテストする。"
  (with-temp-buffer
    (let ((ann (nskk-parse-annotation "delayed"))
          (nskk-annotation-display-delay 0.1)
          (nskk-annotation-display-duration nil))
      ;; 遅延表示を開始
      (nskk-show-annotation ann)
      ;; すぐには表示されない
      (should-not (nskk-annotation-display-current-p))
      ;; タイマー実行後は表示される
      (sleep-for 0.15)
      (should (nskk-annotation-display-current-p))
      ;; クリーンアップ
      (nskk-hide-annotation))))

;;; 画像注釈のテスト

(ert-deftest nskk-annotation-display-test-image-annotation ()
  "画像注釈のテスト（パス不正）。"
  (let ((nskk-annotation-display-enable-images t)
        (ann (nskk-parse-annotation "test")))
    ;; 画像が存在しない場合の処理をテスト
    (cl-letf (((symbol-function 'nskk-annotation-parser-get-media-path)
               (lambda (_) "/nonexistent/path.png")))
      (let ((formatted (nskk-annotation-display--format-image ann)))
        (should (or (null formatted)
                   (string-match-p "Image error" formatted)))))))

(ert-deftest nskk-annotation-display-test-format-with-image ()
  "画像を含む注釈の整形テスト。"
  (let ((nskk-annotation-display-enable-images t)
        (ann (nskk-parse-annotation "test with image")))
    ;; has-media-pがtrueを返すようにする
    (cl-letf (((symbol-function 'nskk-annotation-parser-has-media-p)
               (lambda (_) t))
              ((symbol-function 'nskk-annotation-media-type)
               (lambda (_) 'image))
              ((symbol-function 'nskk-annotation-parser-get-media-path)
               (lambda (_) "/tmp/test.png")))
      (let ((text (nskk-annotation-display--format-text ann)))
        (should (stringp text))))))

(ert-deftest nskk-annotation-display-test-format-no-image ()
  "画像なし注釈の整形テスト。"
  (let ((nskk-annotation-display-enable-images t)
        (ann (nskk-parse-annotation "no image")))
    (cl-letf (((symbol-function 'nskk-annotation-parser-has-media-p)
               (lambda (_) nil)))
      (let ((text (nskk-annotation-display--format-text ann)))
        (should (stringp text))))))

;;; describe関数のテスト

(ert-deftest nskk-annotation-display-test-describe ()
  "注釈表示状態の説明をテストする。"
  (with-temp-buffer
    (let ((ann (nskk-parse-annotation "test"))
          (nskk-annotation-display-delay 0)
          (nskk-annotation-display-duration nil))
      ;; 注釈を表示
      (nskk-show-annotation ann)
      (sit-for 0.05)
      ;; describe実行（メッセージ出力のテスト）
      (cl-letf (((symbol-function 'nskk-annotation-text)
                 (lambda (_) "test-text")))
        (let ((result (nskk-annotation-display-describe)))
          ;; messageの返り値は文字列
          (should (stringp result))))
      ;; クリーンアップ
      (nskk-hide-annotation))))

(ert-deftest nskk-annotation-display-test-describe-no-annotation ()
  "注釈非表示時のdescribeテスト。"
  (with-temp-buffer
    (nskk-hide-annotation)
    (let ((result (nskk-annotation-display-describe)))
      ;; messageの返り値は文字列
      (should (stringp result)))))

;;; 最大高さ切り詰めのテスト

(ert-deftest nskk-annotation-display-test-max-height-truncate ()
  "最大高さを超える注釈の切り詰めテスト。"
  (let ((nskk-annotation-display-max-height 3)
        (ann (nskk-annotation--create
              :text (string-join (make-list 10 "line") "\n"))))
    (let ((text (nskk-annotation-display--format-text ann)))
      (should (<= (length (split-string text "\n")) 3)))))

;;; wrap-textの追加テスト

(ert-deftest nskk-annotation-display-test-wrap-text-first-word ()
  "折り返し時の最初の単語テスト。"
  (let ((text "first second third fourth")
        (nskk-annotation-display-max-width 10))
    (let ((wrapped (nskk-annotation-display--wrap-text text 10)))
      (should (string-match-p "\n" wrapped)))))

;;; 実際の画像ファイルを使ったテスト

(ert-deftest nskk-annotation-display-test-image-creation-with-actual-file ()
  "実際の画像ファイル作成時のパラメータテスト。"
  (let ((nskk-annotation-display-enable-images t)
        (ann (nskk-parse-annotation "test image"))
        (test-image-path (make-temp-file "nskk-test-image" nil ".png")))
    ;; 1x1の透明PNGを作成
    (with-temp-file test-image-path
      (insert (string-as-unibyte
               (apply #'concat
                      (mapcar #'char-to-string
                              '(137 80 78 71 13 10 26 10  ; PNG signature
                                0 0 0 13 73 72 68 82      ; IHDR chunk
                                0 0 0 1 0 0 0 1           ; 1x1 dimensions
                                8 6 0 0 0 31 21 196 137   ; color type, etc
                                0 0 0 10 73 68 65 84      ; IDAT chunk
                                8 153 99 0 1 0 0 5 0 1    ; data
                                13 10 43 180
                                0 0 0 0 73 69 78 68       ; IEND chunk
                                174 66 96 130))))))
    (unwind-protect
        (cl-letf (((symbol-function 'nskk-annotation-parser-has-media-p)
                   (lambda (_) t))
                  ((symbol-function 'nskk-annotation-media-type)
                   (lambda (_) 'image))
                  ((symbol-function 'nskk-annotation-parser-get-media-path)
                   (lambda (_) test-image-path)))
          (let ((image-text (nskk-annotation-display--format-image ann)))
            (should (stringp image-text))
            (should (get-text-property 0 'display image-text))))
      (delete-file test-image-path))))

;;; wrap-textの全ケースカバー

(ert-deftest nskk-annotation-display-test-wrap-text-empty-start ()
  "wrap-textで最初が空の場合のテスト。"
  (let ((text "word1 word2 word3"))
    ;; 非常に短い幅で折り返し - 最初のwordケースをトリガー
    (let ((wrapped (nskk-annotation-display--wrap-text text 5)))
      (should (string-match-p "word1" wrapped)))))

(provide 'nskk-annotation-display-test)

;;; nskk-annotation-display-test.el ends here
