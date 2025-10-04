;;; nskk-progress-test.el --- Tests for nskk-progress -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; This file is part of NSKK.

;;; Commentary:

;; Tests for nskk-progress.el

;;; Code:

(require 'ert)
(require 'nskk-progress)

;;; プログレスレポーターテスト

(ert-deftest nskk-progress-test-create ()
  "プログレスレポーター作成テスト。"
  (let ((reporter (nskk-progress-create "テスト中..." 0 100)))
    (should reporter)
    (should (nskk-progress-reporter-p reporter))
    (should (string= (nskk-progress-reporter-message reporter) "テスト中..."))
    (should (= (nskk-progress-reporter-min-value reporter) 0))
    (should (= (nskk-progress-reporter-max-value reporter) 100))
    (should (= (nskk-progress-reporter-current reporter) 0))

    ;; クリーンアップ
    (nskk-progress-done reporter)))

(ert-deftest nskk-progress-test-update ()
  "プログレス更新テスト。"
  (let ((reporter (nskk-progress-create "テスト中..." 0 100)))

    ;; 更新
    (nskk-progress-update reporter 50)
    (should (= (nskk-progress-reporter-current reporter) 50))

    (nskk-progress-update reporter 100)
    (should (= (nskk-progress-reporter-current reporter) 100))

    ;; クリーンアップ
    (nskk-progress-done reporter)))

(ert-deftest nskk-progress-test-calculate-percentage ()
  "パーセンテージ計算テスト。"
  (should (= (nskk-progress--calculate-percentage 0 0 100) 0.0))
  (should (= (nskk-progress--calculate-percentage 50 0 100) 50.0))
  (should (= (nskk-progress--calculate-percentage 100 0 100) 100.0))
  (should (= (nskk-progress--calculate-percentage 25 0 100) 25.0))

  ;; 範囲が異なる場合
  (should (= (nskk-progress--calculate-percentage 5 0 10) 50.0)))

(ert-deftest nskk-progress-test-make-bar ()
  "プログレスバー生成テスト。"
  (let ((nskk-progress-bar-width 10)
        (nskk-progress-bar-char-filled "■")
        (nskk-progress-bar-char-empty "□"))

    ;; 0%
    (let ((bar (nskk-progress--make-bar 0.0)))
      (should (string-match-p "\\[□+\\]" bar)))

    ;; 50%
    (let ((bar (nskk-progress--make-bar 50.0)))
      (should (string-match-p "\\[■+□+\\]" bar)))

    ;; 100%
    (let ((bar (nskk-progress--make-bar 100.0)))
      (should (string-match-p "\\[■+\\]" bar)))))

(ert-deftest nskk-progress-test-with-reporter-macro ()
  "with-reporterマクロテスト。"
  (let ((completed nil))
    (nskk-progress-with-reporter "テスト中..." 0 10
      (dotimes (i 10)
        (nskk-progress-update nskk-progress--current-reporter i))
      (setq completed t))

    (should completed)
    ;; マクロ終了後はレポーターがクリーンアップされる
    (should-not nskk-progress--current-reporter)))

;;; スピナーテスト

(ert-deftest nskk-progress-test-spinner-start ()
  "スピナー開始テスト。"
  (let ((spinner (nskk-progress-spinner-start "処理中...")))
    (should spinner)
    (should (nskk-progress-spinner-p spinner))
    (should (string= (nskk-progress-spinner-message spinner) "処理中..."))
    (should (nskk-progress-spinner-timer spinner))

    ;; クリーンアップ
    (nskk-progress-spinner-stop)))

(ert-deftest nskk-progress-test-spinner-stop ()
  "スピナー停止テスト。"
  (let ((spinner (nskk-progress-spinner-start "処理中...")))
    (should spinner)

    ;; 停止
    (nskk-progress-spinner-stop)
    (should-not nskk-progress--current-spinner)))

(ert-deftest nskk-progress-test-spinner-tick ()
  "スピナーフレーム更新テスト。"
  (let* ((frames ["⠋" "⠙" "⠹"])
         (spinner (nskk-progress-spinner--create
                   :message "テスト"
                   :frames frames
                   :frame-index 0)))

    ;; 初期状態
    (should (= (nskk-progress-spinner-frame-index spinner) 0))

    ;; ティック
    (nskk-progress--spinner-tick spinner)
    (should (= (nskk-progress-spinner-frame-index spinner) 1))

    (nskk-progress--spinner-tick spinner)
    (should (= (nskk-progress-spinner-frame-index spinner) 2))

    ;; 最後のフレーム後は最初に戻る
    (nskk-progress--spinner-tick spinner)
    (should (= (nskk-progress-spinner-frame-index spinner) 0))))

(ert-deftest nskk-progress-test-with-spinner-macro ()
  "with-spinnerマクロテスト。"
  (let ((completed nil))
    (nskk-progress-with-spinner "処理中..."
      (sleep-for 0.01)
      (setq completed t))

    (should completed)
    ;; マクロ終了後はスピナーが停止される
    (should-not nskk-progress--current-spinner)))

;;; クリーンアップテスト

(ert-deftest nskk-progress-test-cleanup ()
  "クリーンアップテスト。"
  (let ((reporter (nskk-progress-create "テスト1" 0 100))
        (spinner (nskk-progress-spinner-start "テスト2")))

    (should nskk-progress--current-reporter)
    (should nskk-progress--current-spinner)

    ;; クリーンアップ
    (nskk-progress-cleanup)

    (should-not nskk-progress--current-reporter)
    (should-not nskk-progress--current-spinner)))

;;; 表示タイプテスト

(ert-deftest nskk-progress-test-display-type ()
  "表示タイプ設定テスト。"
  (let ((nskk-progress-display-type 'minibuffer))
    (let ((reporter (nskk-progress-create "ミニバッファ" 0 100)))
      (nskk-progress-update reporter 50)
      (nskk-progress-done reporter)))

  (let ((nskk-progress-display-type 'modeline))
    (let ((reporter (nskk-progress-create "モードライン" 0 100)))
      (nskk-progress-update reporter 50)
      (should (> (length nskk-progress--modeline-string) 0))
      (nskk-progress-done reporter)))

  (let ((nskk-progress-display-type 'both))
    (let ((reporter (nskk-progress-create "両方" 0 100)))
      (nskk-progress-update reporter 50)
      (should (> (length nskk-progress--modeline-string) 0))
      (nskk-progress-done reporter))))

;;; モードライン統合テスト

(ert-deftest nskk-progress-test-mode-line-format ()
  "モードライン フォーマットテスト。"
  ;; 空の場合
  (setq nskk-progress--modeline-string "")
  (should-not (nskk-progress-mode-line-format))

  ;; プログレス表示中
  (setq nskk-progress--modeline-string "テスト中... [50%]")
  (let ((format (nskk-progress-mode-line-format)))
    (should format)
    (should (string-match-p "テスト中" format))))

(provide 'nskk-progress-test)

;;; nskk-progress-test.el ends here
