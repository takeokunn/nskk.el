;;; nskk-input-nicola-test.el --- Tests for NICOLA input method -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, nicola, test

;; This file is part of NSKK.

;;; Commentary:

;; NICOLA (親指シフト) 入力方式のテスト。
;;
;; テストカバレッジ:
;; - 通常打鍵テスト
;; - 左親指シフトテスト
;; - 右親指シフトテスト
;; - 同時打鍵タイミングテスト
;; - パフォーマンステスト
;; - 状態管理テスト
;; - エッジケーステスト

;;; Code:

(require 'ert)
(require 'nskk-input-nicola)

;;; 通常打鍵テスト

(ert-deftest nskk-input-nicola-test-normal-lookup ()
  "通常打鍵のルックアップテスト。"
  ;; 上段
  (should (equal (nskk-input-nicola-lookup "q") "。"))
  (should (equal (nskk-input-nicola-lookup "w") "か"))
  (should (equal (nskk-input-nicola-lookup "e") "た"))
  (should (equal (nskk-input-nicola-lookup "i") "く"))
  ;; 中段
  (should (equal (nskk-input-nicola-lookup "a") "う"))
  (should (equal (nskk-input-nicola-lookup "s") "し"))
  (should (equal (nskk-input-nicola-lookup "k") "き"))
  (should (equal (nskk-input-nicola-lookup "l") "い"))
  ;; 下段
  (should (equal (nskk-input-nicola-lookup "x") "ひ"))
  (should (equal (nskk-input-nicola-lookup "c") "す"))
  (should (equal (nskk-input-nicola-lookup "m") "そ")))

(ert-deftest nskk-input-nicola-test-normal-process ()
  "通常打鍵のプロセステスト。"
  (nskk-input-nicola-reset-state)
  (should (equal (nskk-input-nicola-process "k") "き"))
  (should (equal (nskk-input-nicola-process "a") "う"))
  (should (equal (nskk-input-nicola-process "n") "め")))

;;; 左親指シフトテスト

(ert-deftest nskk-input-nicola-test-left-shift-lookup ()
  "左親指シフトのルックアップテスト。"
  ;; 上段
  (should (equal (nskk-input-nicola-lookup "q" 'left) "ぁ"))
  (should (equal (nskk-input-nicola-lookup "w" 'left) "が"))
  (should (equal (nskk-input-nicola-lookup "e" 'left) "だ"))
  ;; 中段
  (should (equal (nskk-input-nicola-lookup "a" 'left) "を"))
  (should (equal (nskk-input-nicola-lookup "s" 'left) "あ"))
  (should (equal (nskk-input-nicola-lookup "k" 'left) "れ"))
  ;; 下段
  (should (equal (nskk-input-nicola-lookup "z" 'left) "ぅ"))
  (should (equal (nskk-input-nicola-lookup "x" 'left) "ー"))
  (should (equal (nskk-input-nicola-lookup "c" 'left) "ろ")))

(ert-deftest nskk-input-nicola-test-left-shift-process ()
  "左親指シフトのプロセステスト。"
  (nskk-input-nicola-reset-state)
  (should (equal (nskk-input-nicola-process "a" 'left) "を"))
  (should (equal (nskk-input-nicola-process "k" 'left) "れ"))
  (should (equal (nskk-input-nicola-process "s" 'left) "あ")))

;;; 右親指シフトテスト

(ert-deftest nskk-input-nicola-test-right-shift-lookup ()
  "右親指シフトのルックアップテスト。"
  ;; 上段
  (should (equal (nskk-input-nicola-lookup "q" 'right) "ぉ"))
  (should (equal (nskk-input-nicola-lookup "w" 'right) "り"))
  (should (equal (nskk-input-nicola-lookup "e" 'right) "の"))
  ;; 中段
  (should (equal (nskk-input-nicola-lookup "a" 'right) "ぃ"))
  (should (equal (nskk-input-nicola-lookup "s" 'right) "ら"))
  (should (equal (nskk-input-nicola-lookup "k" 'right) "ぎ"))
  ;; 下段
  (should (equal (nskk-input-nicola-lookup "z" 'right) "ぇ"))
  (should (equal (nskk-input-nicola-lookup "x" 'right) "じ"))
  (should (equal (nskk-input-nicola-lookup "/" 'right) "っ")))

(ert-deftest nskk-input-nicola-test-right-shift-process ()
  "右親指シフトのプロセステスト。"
  (nskk-input-nicola-reset-state)
  (should (equal (nskk-input-nicola-process "a" 'right) "ぃ"))
  (should (equal (nskk-input-nicola-process "k" 'right) "ぎ"))
  (should (equal (nskk-input-nicola-process "/" 'right) "っ")))

;;; 同時打鍵タイミングテスト

(ert-deftest nskk-input-nicola-test-simultaneous-within-window ()
  "同時打鍵ウィンドウ内のテスト。"
  (nskk-input-nicola-reset-state)
  (let ((nskk-input-nicola-simultaneous-window 0.1))
    ;; 最初のキー入力
    (nskk-input-nicola-process "k")
    ;; 50ms待機（ウィンドウ内）
    (sleep-for 0.05)
    ;; 2つ目のキー入力（同時打鍵として扱われる）
    (let ((result (nskk-input-nicola-process "a" 'left)))
      ;; 同時打鍵として処理され、最初のキーのシフト状態は無視される
      (should (equal result "を")))))

(ert-deftest nskk-input-nicola-test-simultaneous-outside-window ()
  "同時打鍵ウィンドウ外のテスト。"
  (nskk-input-nicola-reset-state)
  (let ((nskk-input-nicola-simultaneous-window 0.1))
    ;; 最初のキー入力
    (should (equal (nskk-input-nicola-process "k") "き"))
    ;; 150ms待機（ウィンドウ外）
    (sleep-for 0.15)
    ;; 2つ目のキー入力（通常打鍵として扱われる）
    (should (equal (nskk-input-nicola-process "a") "う"))))

(ert-deftest nskk-input-nicola-test-simultaneous-shift-first ()
  "親指シフトキーを先に押した場合のテスト。"
  (nskk-input-nicola-reset-state)
  (let ((nskk-input-nicola-simultaneous-window 0.1))
    ;; 親指シフトキーを先に押す（仮想的にnilを処理）
    (nskk-input-nicola-process "" 'left)
    ;; すぐにキーを押す
    (sleep-for 0.02)
    ;; 同時打鍵として処理される
    (let ((result (nskk-input-nicola-process "k")))
      (should (equal result "れ")))))

;;; 状態管理テスト

(ert-deftest nskk-input-nicola-test-state-reset ()
  "状態リセットテスト。"
  (nskk-input-nicola-reset-state)
  (should (null nskk-input-nicola--last-key-time))
  (should (null nskk-input-nicola--last-key))
  (should (null nskk-input-nicola--pending-shift))

  ;; 状態を変更
  (nskk-input-nicola-process "k")
  (should nskk-input-nicola--last-key-time)

  ;; リセット
  (nskk-input-nicola-reset-state)
  (should (null nskk-input-nicola--last-key-time))
  (should (null nskk-input-nicola--last-key))
  (should (null nskk-input-nicola--pending-shift)))

(ert-deftest nskk-input-nicola-test-state-after-simultaneous ()
  "同時打鍵後の状態テスト。"
  (nskk-input-nicola-reset-state)
  (let ((nskk-input-nicola-simultaneous-window 0.1))
    ;; 最初のキー
    (nskk-input-nicola-process "k")
    ;; 同時打鍵
    (sleep-for 0.05)
    (nskk-input-nicola-process "a" 'left)
    ;; 同時打鍵後は状態がリセットされる
    (should (null nskk-input-nicola--last-key-time))
    (should (null nskk-input-nicola--last-key))
    (should (null nskk-input-nicola--pending-shift))))

;;; ハッシュテーブルテスト

(ert-deftest nskk-input-nicola-test-hash-table-initialization ()
  "ハッシュテーブル初期化テスト。"
  (nskk-input-nicola-init-hash-tables)
  (should nskk-input-nicola-hash-table)
  (should nskk-input-nicola-left-shift-hash-table)
  (should nskk-input-nicola-right-shift-hash-table)

  ;; エントリ数チェック
  (should (> (hash-table-count nskk-input-nicola-hash-table) 0))
  (should (> (hash-table-count nskk-input-nicola-left-shift-hash-table) 0))
  (should (> (hash-table-count nskk-input-nicola-right-shift-hash-table) 0)))

(ert-deftest nskk-input-nicola-test-hash-table-lookup ()
  "ハッシュテーブルルックアップテスト。"
  (nskk-input-nicola-init-hash-tables)
  (should (equal (gethash "k" nskk-input-nicola-hash-table) "き"))
  (should (equal (gethash "k" nskk-input-nicola-left-shift-hash-table) "れ"))
  (should (equal (gethash "k" nskk-input-nicola-right-shift-hash-table) "ぎ")))

;;; 候補取得テスト

(ert-deftest nskk-input-nicola-test-get-candidates ()
  "候補取得テスト。"
  (let ((candidates (nskk-input-nicola-get-candidates "k")))
    (should (member "k" candidates)))

  (let ((candidates (nskk-input-nicola-get-candidates "k" 'left)))
    (should (member "k" candidates)))

  (let ((candidates (nskk-input-nicola-get-candidates "k" 'right)))
    (should (member "k" candidates))))

;;; 統計情報テスト

(ert-deftest nskk-input-nicola-test-stats ()
  "統計情報テスト。"
  (let ((stats (nskk-input-nicola-stats)))
    (should (plist-get stats :total))
    (should (plist-get stats :base))
    (should (plist-get stats :left-shift))
    (should (plist-get stats :right-shift))
    (should (plist-get stats :simultaneous-window))
    (should (floatp (plist-get stats :simultaneous-window)))))

;;; パフォーマンステスト

(ert-deftest nskk-input-nicola-test-lookup-performance ()
  "ルックアップパフォーマンステスト。"
  (nskk-input-nicola-init-hash-tables)
  (let ((start-time (float-time))
        (iterations 10000))
    (dotimes (_ iterations)
      (nskk-input-nicola-lookup "k"))
    (let* ((end-time (float-time))
           (elapsed (- end-time start-time))
           (per-call (/ elapsed iterations)))
      ;; 1呼び出しあたり0.5ms未満であることを確認
      (should (< per-call 0.0005)))))

(ert-deftest nskk-input-nicola-test-lookup-fast-performance ()
  "高速ルックアップパフォーマンステスト。"
  (nskk-input-nicola-init-hash-tables)
  (let ((start-time (float-time))
        (iterations 10000))
    (dotimes (_ iterations)
      (nskk-input-nicola-lookup-fast "k" nil))
    (let* ((end-time (float-time))
           (elapsed (- end-time start-time))
           (per-call (/ elapsed iterations)))
      ;; defsubstによるインライン化で高速化されることを確認
      (should (< per-call 0.0005)))))

(ert-deftest nskk-input-nicola-test-process-performance ()
  "プロセス処理パフォーマンステスト。"
  (nskk-input-nicola-reset-state)
  (let ((start-time (float-time))
        (iterations 1000))
    (dotimes (_ iterations)
      (nskk-input-nicola-process "k"))
    (let* ((end-time (float-time))
           (elapsed (- end-time start-time))
           (per-call (/ elapsed iterations)))
      ;; 同時打鍵判定を含めて1呼び出しあたり1ms未満であることを確認
      (should (< per-call 0.001)))))

;;; エッジケーステスト

(ert-deftest nskk-input-nicola-test-unknown-key ()
  "未知のキーのテスト。"
  (should (null (nskk-input-nicola-lookup "unknown")))
  (should (null (nskk-input-nicola-lookup "unknown" 'left)))
  (should (null (nskk-input-nicola-lookup "unknown" 'right))))

(ert-deftest nskk-input-nicola-test-empty-key ()
  "空キーのテスト。"
  (should (null (nskk-input-nicola-lookup "")))
  (should (null (nskk-input-nicola-process ""))))

(ert-deftest nskk-input-nicola-test-register ()
  "登録関数テスト。"
  (nskk-input-nicola-register)
  (should nskk-input-nicola-hash-table)
  (should nskk-input-nicola-left-shift-hash-table)
  (should nskk-input-nicola-right-shift-hash-table))

(ert-deftest nskk-input-nicola-test-customizable-window ()
  "カスタマイズ可能なウィンドウテスト。"
  (let ((original-window nskk-input-nicola-simultaneous-window))
    ;; ウィンドウサイズを変更
    (setq nskk-input-nicola-simultaneous-window 0.2)
    (should (= nskk-input-nicola-simultaneous-window 0.2))

    ;; 元に戻す
    (setq nskk-input-nicola-simultaneous-window original-window)))

(provide 'nskk-input-nicola-test)

;;; nskk-input-nicola-test.el ends here
