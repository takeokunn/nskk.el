;;; nskk-annotation-parser-test.el --- Tests for nskk-annotation-parser.el -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; This file is part of NSKK.

;;; Commentary:

;; nskk-annotation-parser.el のテストスイート

;;; Code:

(require 'ert)
(require 'nskk-annotation-parser)

;;; 基本パースのテスト

(ert-deftest nskk-annotation-parser-test-parse-simple ()
  "単純注釈のパースをテストする。"
  (let ((ann (nskk-parse-annotation "love")))
    (should (nskk-annotation-p ann))
    (should (eq (nskk-annotation-type ann) 'simple))
    (should (equal (nskk-annotation-text ann) "love"))
    (should (equal (nskk-annotation-description ann) "love"))))

(ert-deftest nskk-annotation-parser-test-parse-nil ()
  "nil注釈のパースをテストする。"
  (should (null (nskk-parse-annotation nil)))
  (should (null (nskk-parse-annotation ""))))

(ert-deftest nskk-annotation-parser-test-parse-long-text ()
  "長い注釈のパースをテストする（最大長チェック）。"
  (let* ((long-text (make-string 2000 ?a))
         (ann (nskk-parse-annotation long-text)))
    (should (nskk-annotation-p ann))
    (should (<= (length (nskk-annotation-text ann))
                nskk-annotation-parser-max-length))))

;;; マルチメディア注釈のテスト

(ert-deftest nskk-annotation-parser-test-parse-image ()
  "画像注釈のパースをテストする。"
  (let ((ann (nskk-parse-annotation "img:/path/to/image.png")))
    (should (eq (nskk-annotation-type ann) 'multimedia))
    (should (eq (nskk-annotation-media-type ann) 'image))
    (should (equal (nskk-annotation-path ann) "/path/to/image.png"))))

(ert-deftest nskk-annotation-parser-test-parse-sound ()
  "音声注釈のパースをテストする。"
  (let ((ann (nskk-parse-annotation "snd:/path/to/sound.wav")))
    (should (eq (nskk-annotation-type ann) 'multimedia))
    (should (eq (nskk-annotation-media-type ann) 'sound))
    (should (equal (nskk-annotation-path ann) "/path/to/sound.wav"))))

(ert-deftest nskk-annotation-parser-test-parse-video ()
  "動画注釈のパースをテストする。"
  (let ((ann (nskk-parse-annotation "video:/path/to/video.mp4")))
    (should (eq (nskk-annotation-type ann) 'multimedia))
    (should (eq (nskk-annotation-media-type ann) 'video))
    (should (equal (nskk-annotation-path ann) "/path/to/video.mp4"))))

(ert-deftest nskk-annotation-parser-test-parse-url ()
  "URL注釈のパースをテストする。"
  (let ((ann (nskk-parse-annotation "http://example.com/page.html")))
    (should (eq (nskk-annotation-type ann) 'multimedia))
    (should (eq (nskk-annotation-media-type ann) 'url))
    (should (equal (nskk-annotation-path ann) "http://example.com/page.html"))))

(ert-deftest nskk-annotation-parser-test-multimedia-disabled ()
  "マルチメディア注釈無効時のテスト。"
  (let ((nskk-annotation-parser-enable-multimedia-orig nskk-annotation-parser-enable-multimedia))
    (unwind-protect
        (progn
          (setq nskk-annotation-parser-enable-multimedia nil)
          (let ((ann (nskk-parse-annotation "img:/path/to/image.png")))
            (should (eq (nskk-annotation-type ann) 'simple))))
      (setq nskk-annotation-parser-enable-multimedia nskk-annotation-parser-enable-multimedia-orig))))

;;; 構造化注釈のテスト

(ert-deftest nskk-annotation-parser-test-parse-json ()
  "JSON注釈のパースをテストする。"
  (let ((ann (nskk-parse-annotation "{\"key\":\"value\"}")))
    (should (eq (nskk-annotation-type ann) 'structured))
    (should (nskk-annotation-data ann))))

(ert-deftest nskk-annotation-parser-test-parse-plist ()
  "plist注釈のパースをテストする。"
  (let ((ann (nskk-parse-annotation "(key value)")))
    (should (eq (nskk-annotation-type ann) 'structured))))

(ert-deftest nskk-annotation-parser-test-structured-disabled ()
  "構造化注釈無効時のテスト。"
  (let ((nskk-annotation-parser-enable-structured-orig nskk-annotation-parser-enable-structured))
    (unwind-protect
        (progn
          (setq nskk-annotation-parser-enable-structured nil)
          (let ((ann (nskk-parse-annotation "{\"key\":\"value\"}")))
            (should (eq (nskk-annotation-type ann) 'simple))))
      (setq nskk-annotation-parser-enable-structured nskk-annotation-parser-enable-structured-orig))))

;;; 複合注釈のテスト

(ert-deftest nskk-annotation-parser-test-parse-compound ()
  "複合注釈のパースをテストする。"
  (let ((ann (nskk-parse-annotation "love;affection")))
    (should (eq (nskk-annotation-type ann) 'compound))
    (should (= (length (nskk-annotation-components ann)) 2))))

(ert-deftest nskk-annotation-parser-test-parse-compound-complex ()
  "複雑な複合注釈のパースをテストする。"
  (let ((ann (nskk-parse-annotation "example;img:/path/to/img.png")))
    (should (eq (nskk-annotation-type ann) 'compound))
    (should (= (length (nskk-annotation-components ann)) 2))
    (let ((comp1 (nth 0 (nskk-annotation-components ann)))
          (comp2 (nth 1 (nskk-annotation-components ann))))
      (should (eq (nskk-annotation-type comp1) 'simple))
      (should (eq (nskk-annotation-type comp2) 'multimedia)))))

;;; エスケープ処理のテスト

(ert-deftest nskk-annotation-parser-test-escape ()
  "エスケープ処理をテストする。"
  (should (equal (nskk-annotation-parser-escape "a;b")
                "a\\;b"))
  (should (equal (nskk-annotation-parser-escape "a/b")
                "a\\/b")))

(ert-deftest nskk-annotation-parser-test-unescape ()
  "エスケープ解除をテストする。"
  (should (equal (nskk-annotation-parser-unescape "a\\;b")
                "a;b"))
  (should (equal (nskk-annotation-parser-unescape "a\\/b")
                "a/b")))

;;; ユーティリティ関数のテスト

(ert-deftest nskk-annotation-parser-test-get-description ()
  "説明文取得をテストする。"
  (let ((ann (nskk-parse-annotation "love")))
    (should (equal (nskk-annotation-parser-get-description ann)
                  "love"))))

(ert-deftest nskk-annotation-parser-test-has-media-p ()
  "メディア有無判定をテストする。"
  (let ((ann1 (nskk-parse-annotation "img:/path/to/image.png"))
        (ann2 (nskk-parse-annotation "simple text")))
    (should (nskk-annotation-parser-has-media-p ann1))
    (should-not (nskk-annotation-parser-has-media-p ann2))))

(ert-deftest nskk-annotation-parser-test-get-media-path ()
  "メディアパス取得をテストする。"
  (let ((ann (nskk-parse-annotation "img:/path/to/image.png")))
    (should (equal (nskk-annotation-parser-get-media-path ann)
                  "/path/to/image.png"))))

(ert-deftest nskk-annotation-parser-test-to-string ()
  "文字列変換をテストする。"
  (let ((ann (nskk-parse-annotation "love")))
    (should (equal (nskk-annotation-parser-to-string ann)
                  "love"))))

;;; 統計機能のテスト

(ert-deftest nskk-annotation-parser-test-statistics ()
  "統計情報生成をテストする。"
  (let* ((ann1 (nskk-parse-annotation "simple"))
         (ann2 (nskk-parse-annotation "img:/path/to/image.png"))
         (ann3 (nskk-parse-annotation "{\"key\":\"value\"}"))
         (ann4 (nskk-parse-annotation "a;b"))
         (anns (list ann1 ann2 ann3 ann4))
         (stats (nskk-annotation-parser-statistics anns)))
    (should (= (plist-get stats :total) 4))
    (should (= (plist-get stats :simple) 1))
    (should (= (plist-get stats :multimedia) 1))
    (should (= (plist-get stats :structured) 1))
    (should (= (plist-get stats :compound) 1))))

(provide 'nskk-annotation-parser-test)

;;; nskk-annotation-parser-test.el ends here
