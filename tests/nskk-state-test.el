;;; nskk-state-test.el --- Tests for nskk-state.el -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; This file is part of NSKK.

;;; Commentary:

;; nskk-state.el のテストスイート
;; ERTフレームワークを使用した包括的なテストを実装

;;; Code:

(require 'ert)
(require 'nskk-state)

;;; 状態生成テスト

(ert-deftest nskk-state-test-create-default ()
  "デフォルト状態の生成をテストする。"
  (let ((state (nskk-state-create)))
    (should (nskk-state-p state))
    (should (eq (nskk-state-mode state) 'hiragana))
    (should (string-empty-p (nskk-state-input-buffer state)))
    (should (string-empty-p (nskk-state-conversion-buffer state)))
    (should (null (nskk-state-candidates state)))
    (should (= (nskk-state-candidate-index state) 0))))

(ert-deftest nskk-state-test-create-with-mode ()
  "特定のモードを指定した状態生成をテストする。"
  (let ((state (nskk-state-create :mode 'katakana)))
    (should (eq (nskk-state-mode state) 'katakana))))

(ert-deftest nskk-state-test-create-invalid-mode ()
  "無効なモードで状態生成するとエラーになることをテストする。"
  (should-error (nskk-state-create :mode 'invalid-mode)))

;;; モード検査テスト

(ert-deftest nskk-state-test-valid-mode-p ()
  "モードの有効性判定をテストする。"
  (should (nskk-state-valid-mode-p 'hiragana))
  (should (nskk-state-valid-mode-p 'katakana))
  (should (nskk-state-valid-mode-p 'latin))
  (should (nskk-state-valid-mode-p 'conversion))
  (should-not (nskk-state-valid-mode-p 'invalid))
  (should-not (nskk-state-valid-mode-p nil)))

(ert-deftest nskk-state-test-mode-description ()
  "モード説明の取得をテストする。"
  (should (equal (nskk-state-mode-description 'hiragana) "ひらがな"))
  (should (equal (nskk-state-mode-description 'katakana) "カタカナ"))
  (should (equal (nskk-state-mode-description 'latin) "半角英数")))

(ert-deftest nskk-state-test-mode-indicator ()
  "モードインジケーターの取得をテストする。"
  (should (equal (nskk-state-mode-indicator 'hiragana) "あ"))
  (should (equal (nskk-state-mode-indicator 'katakana) "ア"))
  (should (equal (nskk-state-mode-indicator 'latin) "A"))
  (should (equal (nskk-state-mode-indicator 'conversion) "▼")))

(ert-deftest nskk-state-test-in-conversion-p ()
  "変換モード判定をテストする。"
  (let ((state1 (nskk-state-create :mode 'hiragana))
        (state2 (nskk-state-create :mode 'conversion)))
    (should-not (nskk-state-in-conversion-p state1))
    (should (nskk-state-in-conversion-p state2))))

(ert-deftest nskk-state-test-empty-p ()
  "空状態判定をテストする。"
  (let ((state (nskk-state-create)))
    (should (nskk-state-empty-p state))
    (nskk-state-append-input state ?k)
    (should-not (nskk-state-empty-p state))))

;;; 状態更新テスト

(ert-deftest nskk-state-test-set-mode ()
  "モード設定をテストする。"
  (let ((state (nskk-state-create)))
    (should (eq (nskk-state-mode state) 'hiragana))
    (nskk-state-set-mode state 'katakana)
    (should (eq (nskk-state-mode state) 'katakana))
    (should (eq (nskk-state-previous-mode state) 'hiragana))))

(ert-deftest nskk-state-test-restore-previous-mode ()
  "前回のモード復元をテストする。"
  (let ((state (nskk-state-create)))
    (nskk-state-set-mode state 'katakana)
    (nskk-state-set-mode state 'latin)
    (should (eq (nskk-state-mode state) 'latin))
    (nskk-state-restore-previous-mode state)
    (should (eq (nskk-state-mode state) 'katakana))))

(ert-deftest nskk-state-test-restore-previous-mode-default ()
  "前回のモードがない場合のモード復元をテストする。"
  (let ((state (nskk-state-create :mode 'katakana)))
    (setf (nskk-state-previous-mode state) nil)
    (nskk-state-restore-previous-mode state)
    (should (eq (nskk-state-mode state) 'hiragana))))

(ert-deftest nskk-state-test-clear-input ()
  "入力バッファクリアをテストする。"
  (let ((state (nskk-state-create)))
    (nskk-state-append-input state ?k)
    (nskk-state-append-input state ?a)
    (should (equal (nskk-state-input-buffer state) "ka"))
    (nskk-state-clear-input state)
    (should (string-empty-p (nskk-state-input-buffer state)))))

(ert-deftest nskk-state-test-clear-conversion ()
  "変換データクリアをテストする。"
  (let ((state (nskk-state-create)))
    (setf (nskk-state-conversion-buffer state) "かんじ")
    (setf (nskk-state-candidates state) '("漢字" "感じ" "幹事"))
    (setf (nskk-state-candidate-index state) 1)
    (setf (nskk-state-okuri-char state) ?r)
    (nskk-state-clear-conversion state)
    (should (string-empty-p (nskk-state-conversion-buffer state)))
    (should (null (nskk-state-candidates state)))
    (should (= (nskk-state-candidate-index state) 0))
    (should (null (nskk-state-okuri-char state)))))

(ert-deftest nskk-state-test-append-input ()
  "入力文字追加をテストする。"
  (let ((state (nskk-state-create)))
    (nskk-state-append-input state ?k)
    (should (equal (nskk-state-input-buffer state) "k"))
    (nskk-state-append-input state ?a)
    (should (equal (nskk-state-input-buffer state) "ka"))
    (nskk-state-append-input state ?n)
    (should (equal (nskk-state-input-buffer state) "kan"))))

;;; 候補管理テスト

(ert-deftest nskk-state-test-set-candidates ()
  "候補設定をテストする。"
  (let ((state (nskk-state-create))
        (candidates '("漢字" "感じ" "幹事")))
    (nskk-state-set-candidates state candidates)
    (should (equal (nskk-state-candidates state) candidates))
    (should (= (nskk-state-candidate-index state) 0))))

(ert-deftest nskk-state-test-set-candidates-with-index ()
  "候補とインデックスの同時設定をテストする。"
  (let ((state (nskk-state-create))
        (candidates '("漢字" "感じ" "幹事")))
    (nskk-state-set-candidates state candidates 1)
    (should (= (nskk-state-candidate-index state) 1))))

(ert-deftest nskk-state-test-current-candidate ()
  "現在の候補取得をテストする。"
  (let ((state (nskk-state-create))
        (candidates '("漢字" "感じ" "幹事")))
    (nskk-state-set-candidates state candidates)
    (should (equal (nskk-state-current-candidate state) "漢字"))
    (nskk-state-set-candidates state candidates 1)
    (should (equal (nskk-state-current-candidate state) "感じ"))))

(ert-deftest nskk-state-test-next-candidate ()
  "次候補選択をテストする。"
  (let ((state (nskk-state-create))
        (candidates '("漢字" "感じ" "幹事")))
    (nskk-state-set-candidates state candidates)
    (should (= (nskk-state-next-candidate state) 1))
    (should (equal (nskk-state-current-candidate state) "感じ"))
    (should (= (nskk-state-next-candidate state) 2))
    (should (equal (nskk-state-current-candidate state) "幹事"))))

(ert-deftest nskk-state-test-next-candidate-wrap ()
  "次候補選択の循環をテストする。"
  (let ((state (nskk-state-create))
        (candidates '("漢字" "感じ" "幹事")))
    (nskk-state-set-candidates state candidates 2)
    (should (= (nskk-state-next-candidate state) 0))
    (should (equal (nskk-state-current-candidate state) "漢字"))))

(ert-deftest nskk-state-test-previous-candidate ()
  "前候補選択をテストする。"
  (let ((state (nskk-state-create))
        (candidates '("漢字" "感じ" "幹事")))
    (nskk-state-set-candidates state candidates 2)
    (should (= (nskk-state-previous-candidate state) 1))
    (should (equal (nskk-state-current-candidate state) "感じ"))
    (should (= (nskk-state-previous-candidate state) 0))
    (should (equal (nskk-state-current-candidate state) "漢字"))))

(ert-deftest nskk-state-test-previous-candidate-wrap ()
  "前候補選択の循環をテストする。"
  (let ((state (nskk-state-create))
        (candidates '("漢字" "感じ" "幹事")))
    (nskk-state-set-candidates state candidates)
    (should (= (nskk-state-previous-candidate state) 2))
    (should (equal (nskk-state-current-candidate state) "幹事"))))

;;; 状態遷移テスト

(ert-deftest nskk-state-test-can-transition-p ()
  "状態遷移可能性判定をテストする。"
  (should (nskk-state-can-transition-p 'hiragana 'katakana))
  (should (nskk-state-can-transition-p 'hiragana 'latin))
  (should (nskk-state-can-transition-p 'katakana 'hiragana))
  (should (nskk-state-can-transition-p 'latin 'conversion)))

(ert-deftest nskk-state-test-transition-success ()
  "正常な状態遷移をテストする。"
  (let ((state (nskk-state-create)))
    (should (nskk-state-transition state 'katakana))
    (should (eq (nskk-state-mode state) 'katakana))))

(ert-deftest nskk-state-test-transition-same-mode ()
  "同じモードへの遷移をテストする。"
  (let ((state (nskk-state-create)))
    (should (nskk-state-transition state 'hiragana))
    (should (eq (nskk-state-mode state) 'hiragana))))

(ert-deftest nskk-state-test-transition-with-force ()
  "強制遷移をテストする。"
  (let ((state (nskk-state-create)))
    (should (nskk-state-transition state 'katakana t))
    (should (eq (nskk-state-mode state) 'katakana))))

;;; プロパティ管理テスト

(ert-deftest nskk-state-test-get-property ()
  "プロパティ取得をテストする。"
  (let ((state (nskk-state-create)))
    (nskk-state-set-property state :test-key "test-value")
    (should (equal (nskk-state-get-property state :test-key) "test-value"))))

(ert-deftest nskk-state-test-get-property-default ()
  "プロパティのデフォルト値取得をテストする。"
  (let ((state (nskk-state-create)))
    (should (equal (nskk-state-get-property state :nonexistent "default")
                   "default"))))

(ert-deftest nskk-state-test-set-property ()
  "プロパティ設定をテストする。"
  (let ((state (nskk-state-create)))
    (nskk-state-set-property state :key1 "value1")
    (nskk-state-set-property state :key2 123)
    (should (equal (nskk-state-get-property state :key1) "value1"))
    (should (= (nskk-state-get-property state :key2) 123))))

(ert-deftest nskk-state-test-remove-property ()
  "プロパティ削除をテストする。"
  (let ((state (nskk-state-create)))
    (nskk-state-set-property state :key1 "value1")
    (nskk-state-set-property state :key2 "value2")
    (should (nskk-state-get-property state :key1))
    (nskk-state-remove-property state :key1)
    (should-not (nskk-state-get-property state :key1))
    (should (nskk-state-get-property state :key2))))

;;; マーカー管理テスト

(ert-deftest nskk-state-test-set-markers ()
  "マーカー設定をテストする。"
  (with-temp-buffer
    (insert "test buffer")
    (let ((state (nskk-state-create)))
      (nskk-state-set-markers state (point-min) (point-max))
      (should (markerp (nskk-state-marker-start state)))
      (should (markerp (nskk-state-marker-end state)))
      (should (= (marker-position (nskk-state-marker-start state)) 1))
      (should (= (marker-position (nskk-state-marker-end state)) 12)))))

(ert-deftest nskk-state-test-clear-markers ()
  "マーカークリアをテストする。"
  (with-temp-buffer
    (insert "test buffer")
    (let ((state (nskk-state-create)))
      (nskk-state-set-markers state (point-min) (point-max))
      (should (nskk-state-marker-start state))
      (nskk-state-clear-markers state)
      (should-not (nskk-state-marker-start state))
      (should-not (nskk-state-marker-end state)))))

(ert-deftest nskk-state-test-marker-region ()
  "マーカー領域取得をテストする。"
  (with-temp-buffer
    (insert "test buffer")
    (let ((state (nskk-state-create)))
      (nskk-state-set-markers state 3 8)
      (let ((region (nskk-state-marker-region state)))
        (should (consp region))
        (should (= (car region) 3))
        (should (= (cdr region) 8))))))

;;; 複合操作テスト

(ert-deftest nskk-state-test-complex-input-scenario ()
  "複雑な入力シナリオをテストする。"
  (let ((state (nskk-state-create)))
    ;; 初期状態確認
    (should (eq (nskk-state-mode state) 'hiragana))
    (should (nskk-state-empty-p state))

    ;; 入力開始
    (nskk-state-append-input state ?k)
    (nskk-state-append-input state ?a)
    (nskk-state-append-input state ?n)
    (should (equal (nskk-state-input-buffer state) "kan"))

    ;; 変換モードへ遷移
    (nskk-state-transition state 'conversion)
    (should (eq (nskk-state-mode state) 'conversion))

    ;; 候補設定
    (nskk-state-set-candidates state '("漢" "感" "缶"))
    (should (equal (nskk-state-current-candidate state) "漢"))

    ;; 候補選択
    (nskk-state-next-candidate state)
    (should (equal (nskk-state-current-candidate state) "感"))

    ;; クリア
    (nskk-state-clear-all state)
    (should (nskk-state-empty-p state))))

(ert-deftest nskk-state-test-mode-switching-scenario ()
  "モード切り替えシナリオをテストする。"
  (let ((state (nskk-state-create)))
    ;; ひらがな → カタカナ
    (nskk-state-transition state 'katakana)
    (should (eq (nskk-state-mode state) 'katakana))
    (should (eq (nskk-state-previous-mode state) 'hiragana))

    ;; カタカナ → 英数
    (nskk-state-transition state 'latin)
    (should (eq (nskk-state-mode state) 'latin))
    (should (eq (nskk-state-previous-mode state) 'katakana))

    ;; 前回モード(katakana)に戻る
    (nskk-state-restore-previous-mode state)
    (should (eq (nskk-state-mode state) 'katakana))

    ;; さらに前回モード(latin)に戻る
    (nskk-state-restore-previous-mode state)
    (should (eq (nskk-state-mode state) 'latin))))

;;; バッファローカル変数テスト

(ert-deftest nskk-state-test-buffer-local-init ()
  "バッファローカル状態初期化をテストする。"
  (with-temp-buffer
    (should-not nskk-current-state)
    (nskk-state-init)
    (should nskk-current-state)
    (should (nskk-state-p nskk-current-state))))

(ert-deftest nskk-state-test-buffer-local-cleanup ()
  "バッファローカル状態クリーンアップをテストする。"
  (with-temp-buffer
    (nskk-state-init)
    (should nskk-current-state)
    (nskk-state-cleanup)
    (should-not nskk-current-state)))

(provide 'nskk-state-test)

;;; nskk-state-test.el ends here
