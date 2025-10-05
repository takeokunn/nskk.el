;;; nskk-integration-test.el --- Integration tests for NSKK Phase 1 -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Development Team

;;; Commentary:

;; NSKK Phase 1統合テストスイート
;;
;; 以下のテストシナリオをカバー:
;; 1. 基本フロー: ローマ字入力 → ひらがな変換 → 漢字変換 → 確定
;; 2. 辞書フロー: 辞書読み込み → インデックス構築 → 検索 → キャッシュ
;; 3. UIフロー: キー入力 → モードライン更新 → 候補表示 → 選択
;; 4. エラーフロー: 辞書エラー → フォールバック → リカバリー

;;; Code:

(require 'ert)
(require 'nskk)
(require 'nskk-test-framework)
(require 'nskk-test-fixtures)

(defmacro nskk-integration--should-convert (input expected)
  "`nskk-convert-romaji' の確定文字列が EXPECTED と一致することを検証する。"
  `(let* ((res (nskk-convert-romaji ,input))
          (converted (nskk-converter-result-converted res))
          (pending (nskk-converter-result-pending res)))
     (should (equal converted ,expected))
     (should (string= pending ""))))

;;; Test Suite: Module Integration

(ert-deftest nskk-integration-test-all-modules-loaded ()
  "全モジュールが正常にロードされているか確認する。"
  (should (featurep 'nskk))
  (should (featurep 'nskk-romaji-tables))
  (should (featurep 'nskk-converter))
  (should (featurep 'nskk-special-chars))
  (should (featurep 'nskk-optimize))
  (should (featurep 'nskk-state))
  (should (featurep 'nskk-mode-switch))
  (should (featurep 'nskk-buffer))
  (should (featurep 'nskk-events))
  (should (featurep 'nskk-dict-parser))
  (should (featurep 'nskk-dict-struct))
  (should (featurep 'nskk-dict-io))
  (should (featurep 'nskk-dict-errors))
  (should (featurep 'nskk-trie))
  (should (featurep 'nskk-search))
  (should (featurep 'nskk-cache))
  (should (featurep 'nskk-index))
  (should (featurep 'nskk-keymap))
  (should (featurep 'nskk-candidate-window))
  (should (featurep 'nskk-minibuffer))
  (should (featurep 'nskk-modeline)))

(ert-deftest nskk-integration-test-no-circular-dependencies ()
  "循環依存がないことを確認する。"
  ;; 全モジュールが正常にロードできた時点で循環依存はない
  (should (nskk-health-check)))

(ert-deftest nskk-integration-test-version-info ()
  "バージョン情報が正しく設定されているか確認する。"
  (should (string= nskk-version "0.1.0")))

;;; Test Suite: Basic Flow Integration

(ert-deftest nskk-integration-test-basic-romaji-to-hiragana ()
  "ローマ字からひらがなへの基本変換フローをテストする。"
  (let ((inputs '("a" "ka" "kya" "kka" "nn"))
        (expected '("あ" "か" "きゃ" "っか" "ん")))
    (cl-loop for input in inputs
             for exp in expected
             do (nskk-integration--should-convert input exp))))

(ert-deftest nskk-integration-test-state-and-conversion ()
  "状態管理と変換処理の統合テスト。"
  (let ((state (nskk-state-create)))
    ;; 初期状態確認
    (should (eq (nskk-state-mode state) 'hiragana))

    ;; 変換実行
    (nskk-integration--should-convert "aiueo" "あいうえお")

    ;; 状態が維持されているか確認
    (should (eq (nskk-state-mode state) 'hiragana))))

(ert-deftest nskk-integration-test-buffer-and-conversion ()
  "バッファ管理と変換処理の統合テスト。"
  (let ((buffer (nskk-buffer-create)))
    ;; バッファに文字挿入
    (nskk-buffer-insert buffer "k")
    (should (equal (nskk-buffer-content buffer) "k"))

    (nskk-buffer-insert buffer "a")
    (should (equal (nskk-buffer-content buffer) "ka"))

    ;; バッファ内容を変換
    (nskk-integration--should-convert (nskk-buffer-content buffer) "か")

    ;; バッファクリア
    (nskk-buffer-clear buffer)
    (should (equal (nskk-buffer-content buffer) ""))))

;;; Test Suite: Dictionary Flow Integration

(ert-deftest nskk-integration-test-dict-parse-and-struct ()
  "辞書パースとデータ構造の統合テスト。"
  ;; テスト辞書データ
  (let* ((dict-line "あい /愛/哀/藍/")
         (entry (nskk-dict-parse-line dict-line)))
    ;; パース結果確認
    (should (nskk-dict-entry-p entry))
    (should (equal (nskk-dict-entry-midasi entry) "あい"))
    (should (equal (length (nskk-dict-entry-candidates entry)) 3))))

(ert-deftest nskk-integration-test-dict-to-trie ()
  "辞書データからトライ木構築までの統合テスト。"
  (let ((trie (nskk-trie-create))
        (entries '(("あい" . ("愛" "哀" "藍"))
                   ("あお" . ("青" "蒼"))
                   ("あか" . ("赤" "朱")))))
    ;; トライ木にエントリ挿入
    (dolist (entry entries)
      (let ((midasi (car entry))
            (candidates (cdr entry)))
        (nskk-trie-insert trie midasi candidates)))

    ;; 検索テスト
    (should (equal (nskk-trie-lookup-values trie "あい") '("愛" "哀" "藍")))
    (should (equal (nskk-trie-lookup-values trie "あお") '("青" "蒼")))
    (should (equal (nskk-trie-lookup-values trie "あか") '("赤" "朱")))
    (should (null (nskk-trie-lookup-values trie "あさ")))))

(ert-deftest nskk-integration-test-dict-search-cache ()
  "辞書検索とキャッシュの統合テスト。"
  (let ((trie (nskk-trie-create))
        (cache (nskk-cache-create :size 10)))
    ;; テストデータ挿入
    (nskk-trie-insert trie "かんじ" '("漢字" "幹事"))

    ;; 初回検索（キャッシュミス）
    (let ((result (nskk-search-with-cache cache trie "かんじ")))
      (should (equal result '("漢字" "幹事"))))

    ;; 2回目検索（キャッシュヒット）
    (let ((result (nskk-search-with-cache cache trie "かんじ")))
      (should (equal result '("漢字" "幹事"))))

    ;; キャッシュヒット確認
    (should (> (nskk-cache-hit-rate cache) 0))))

(ert-deftest nskk-integration-test-dict-index ()
  "辞書インデックス構築と検索の統合テスト。"
  (let* ((entries '(("あい" . ("愛" "哀"))
                    ("あいて" . ("相手"))
                    ("かんじ" . ("漢字" "幹事"))))
         (trie (nskk-trie-create)))
    ;; エントリ挿入
    (dolist (entry entries)
      (nskk-trie-insert trie (car entry) (cdr entry)))

    ;; インデックス構築
    (let ((index (nskk-index-build trie)))
      (should (nskk-index-p index))

      ;; 前方一致検索
      (let ((results (nskk-index-prefix-search index "あい")))
        (should (member "あい" results))
        (should (member "あいて" results))))))

;;; Test Suite: UI Flow Integration

(ert-deftest nskk-integration-test-keymap-setup ()
  "キーマップが正しく設定されているか確認する。"
  (should (boundp 'nskk-mode-map))
  (should (keymapp nskk-mode-map)))

(ert-deftest nskk-integration-test-modeline-update ()
  "モードライン更新の統合テスト。"
  (let ((state (nskk-state-create)))
    ;; ひらがなモード
    (nskk-state-set-mode state 'hiragana)
    (let ((indicator (nskk-modeline-format state)))
      (should (stringp indicator))
      (should (string-match-p "ひらがな\\|あ" indicator)))

    ;; カタカナモード
    (nskk-state-set-mode state 'katakana)
    (let ((indicator (nskk-modeline-format state)))
      (should (stringp indicator))
      (should (string-match-p "カタカナ\\|カ" indicator)))))

(ert-deftest nskk-integration-test-candidate-window-display ()
  "候補ウィンドウ表示の統合テスト。"
  (let ((candidates '("漢字" "幹事" "感じ")))
    ;; 候補ウィンドウ作成
    (let ((window (nskk-candidate-window-create candidates)))
      (should (nskk-candidate-window-p window))
      (should (equal (nskk-candidate-window-candidates window) candidates))

      ;; 候補選択
      (nskk-candidate-window-select window 1)
      (should (equal (nskk-candidate-window-current window) "幹事")))))

;;; Test Suite: Error Flow Integration

(ert-deftest nskk-integration-test-dict-error-handling ()
  "辞書エラーハンドリングの統合テスト。"
  ;; 不正な辞書行
  (should-error (nskk-dict-parse-line "invalid line"))

  ;; エラーからの復帰
  (let ((valid-entry (nskk-dict-parse-line "あい /愛/")))
    (should (nskk-dict-entry-p valid-entry))))

(ert-deftest nskk-integration-test-search-fallback ()
  "検索失敗時のフォールバック動作テスト。"
  (let ((trie (nskk-trie-create)))
    ;; 存在しないキーの検索
    (let ((result (nskk-trie-lookup-values trie "zzz")))
      (should (null result))
      ;; フォールバック動作確認（空リスト返却）
      (should (listp result)))))

;;; Test Suite: End-to-End Flow

(ert-deftest nskk-integration-test-e2e-input-to-display ()
  "入力から表示までのエンドツーエンドテスト。"
  (let ((state (nskk-state-create))
        (buffer (nskk-buffer-create))
        (trie (nskk-trie-create)))
    ;; 辞書データ準備
    (nskk-trie-insert trie "かんじ" '("漢字" "幹事" "感じ"))

    ;; ローマ字入力シミュレーション
    (dolist (char '("k" "a" "n" "j" "i"))
      (nskk-buffer-insert buffer char))

    ;; バッファ内容を変換
    (let* ((result (nskk-convert-romaji (nskk-buffer-content buffer)))
           (hiragana (nskk-converter-result-converted result)))
      (should (string= (nskk-converter-result-pending result) ""))
      (should (equal hiragana "かんじ"))

      ;; 辞書検索
      (let ((candidates (nskk-trie-lookup-values trie hiragana)))
        (should (equal candidates '("漢字" "幹事" "感じ")))

        ;; 候補ウィンドウ表示
        (let ((window (nskk-candidate-window-create candidates)))
          (should (nskk-candidate-window-p window))

          ;; 候補選択
          (nskk-candidate-window-select window 0)
          (should (equal (nskk-candidate-window-current window) "漢字")))))))

(ert-deftest nskk-integration-test-e2e-with-cache ()
  "キャッシュを含むエンドツーエンドテスト。"
  (let ((trie (nskk-trie-create))
        (cache (nskk-cache-create :size 10)))
    ;; 辞書データ準備
    (nskk-trie-insert trie "あい" '("愛" "哀" "藍"))

    ;; 初回検索
    (let ((result1 (nskk-search-with-cache cache trie "あい")))
      (should (equal result1 '("愛" "哀" "藍"))))

    ;; キャッシュヒット確認
    (let ((result2 (nskk-search-with-cache cache trie "あい")))
      (should (equal result2 '("愛" "哀" "藍")))
      (should (> (nskk-cache-hit-rate cache) 0)))))

;;; Test Suite: Data Flow Verification

(ert-deftest nskk-integration-test-data-flow-romaji-to-kanji ()
  "ローマ字→ひらがな→漢字のデータフロー検証。"
  (let ((trie (nskk-trie-create)))
    ;; 辞書データ準備
    (nskk-trie-insert trie "にほん" '("日本"))

    ;; ローマ字→ひらがな
    (let* ((result (nskk-convert-romaji "nihon"))
           (hiragana (nskk-converter-result-converted result)))
      (should (string= (nskk-converter-result-pending result) ""))
      (should (equal hiragana "にほん"))

      ;; ひらがな→漢字候補
      (let ((candidates (nskk-trie-lookup-values trie hiragana)))
        (should (equal candidates '("日本")))))))

(ert-deftest nskk-integration-test-data-flow-state-transitions ()
  "状態遷移のデータフロー検証。"
  (let ((state (nskk-state-create)))
    ;; ひらがな→カタカナ
    (nskk-state-set-mode state 'hiragana)
    (should (eq (nskk-state-mode state) 'hiragana))

    (nskk-mode-switch state 'katakana)
    (should (eq (nskk-state-mode state) 'katakana))

    ;; カタカナ→英数
    (nskk-mode-switch state 'ascii)
    (should (eq (nskk-state-mode state) 'latin))))

;;; Test Suite: Event Propagation

(ert-deftest nskk-integration-test-event-propagation ()
  "イベント伝播の統合テスト。"
  (let ((state (nskk-state-create))
        (event-fired nil))
    ;; イベントハンドラ登録
    (nskk-events-add-handler 'mode-changed
      (lambda (data)
        (setq event-fired t)))

    ;; モード変更
    (nskk-mode-switch state 'katakana)

    ;; イベント発火確認
    (should event-fired)))

;;; Performance Integration Tests

(ert-deftest nskk-integration-test-performance-romaji-conversion ()
  "ローマ字変換のパフォーマンステスト（< 0.1ms目標）。"
  (let ((start-time (float-time))
        (iterations 1000))
    (dotimes (_ iterations)
      (nskk-convert-romaji "kanzi"))
    (let* ((elapsed (- (float-time) start-time))
           (per-call (/ (* elapsed 1000) iterations)))
      ;; 1回あたり0.1ms未満
      (should (< per-call 0.1))
      (message "Romaji conversion: %.3f ms per call" per-call))))

(ert-deftest nskk-integration-test-performance-dict-search ()
  "辞書検索のパフォーマンステスト（< 10ms目標）。"
  (let ((trie (nskk-trie-create)))
    ;; 1000エントリ挿入
    (dotimes (i 1000)
      (nskk-trie-insert trie (format "key%d" i) (list (format "value%d" i))))

    ;; 検索性能測定
    (let ((start-time (float-time))
          (iterations 100))
      (dotimes (i iterations)
        (nskk-trie-lookup trie (format "key%d" (% i 1000))))
      (let* ((elapsed (- (float-time) start-time))
             (per-call (/ (* elapsed 1000) iterations)))
        ;; 1回あたり10ms未満
        (should (< per-call 10))
        (message "Dictionary search: %.3f ms per call" per-call)))))

;;; Test Report

(defun nskk-integration-test-run-all ()
  "全統合テストを実行し、結果を報告する。"
  (interactive)
  (let ((start-time (float-time)))
    (message "=== NSKK Integration Tests ===")
    (ert-run-tests-batch "^nskk-integration-test-")
    (let ((elapsed (- (float-time) start-time)))
      (message "Total elapsed time: %.3f seconds" elapsed))))

(provide 'nskk-integration-test)

;;; nskk-integration-test.el ends here
