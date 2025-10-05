;;; nskk-e2e-basic-test.el --- Basic E2E tests for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Development Team

;;; Commentary:

;; NSKK基本入力フローのE2Eテストスイート
;;
;; カバレッジ向上目標: 52.43% → 75%+
;;
;; テストシナリオ:
;; 1. nskk-mode有効化・無効化
;; 2. 基本的なローマ字→ひらがな変換
;; 3. 漢字変換フロー
;; 4. 候補選択と確定
;; 5. 学習機能統合

;;; Code:

(require 'ert)
(require 'nskk)
(require 'nskk-test-framework)
(require 'nskk-test-fixtures)

;;; ヘルパー関数

(defun nskk-e2e--setup ()
  "E2Eテスト環境のセットアップ"
  (nskk-state-init)
  (nskk-events-clear-listeners nil nil)
  (when (nskk-buffer-has-pending-input-p)
    (nskk-buffer-clear)))

(defun nskk-e2e--teardown ()
  "E2Eテスト環境のクリーンアップ"
  (nskk-state-cleanup)
  (nskk-events-cleanup))

(defmacro nskk-e2e-with-test-buffer (&rest body)
  "テストバッファでE2Eテストを実行するマクロ"
  (declare (indent 0) (debug t))
  `(nskk-test-with-temp-buffer
     (nskk-e2e--setup)
     (unwind-protect
         (progn ,@body)
       (nskk-e2e--teardown))))

(defun nskk-e2e--create-test-dict-file ()
  "テスト用辞書ファイルを作成"
  (let ((dict-file (make-temp-file "nskk-test-dict-" nil ".dat")))
    (with-temp-file dict-file
      (insert ";; okuri-ari entries.\n")
      (insert "かんがk /考/感/\n")
      (insert ";; okuri-nasi entries.\n")
      (insert "かんじ /漢字/幹事/感じ/\n")
      (insert "にほん /日本/\n")
      (insert "あい /愛/哀/藍/\n"))
    dict-file))

(defun nskk-e2e--assert-mode (expected-mode)
  "現在のモードを検証"
  (should (eq (nskk-state-mode nskk-current-state) expected-mode)))

(defun nskk-e2e--assert-pending (expected-text)
  "未確定入力を検証"
  (should (equal (nskk-buffer-pending-text) expected-text)))

(defun nskk-e2e--assert-candidates-visible ()
  "候補ウィンドウが表示されていることを検証"
  (should (nskk-candidate-window-visible-p)))

;;; Test Suite 1: Mode Activation

(nskk-deftest nskk-e2e-mode-activation
  "nskk-modeの有効化・無効化テスト"
  :tags '(:e2e :mode)
  (nskk-test-with-temp-buffer
    ;; 初期状態確認
    (should-not nskk-current-state)

    ;; 状態初期化
    (nskk-state-init)
    (should (nskk-state-p nskk-current-state))

    ;; デフォルトモード確認
    (should (eq (nskk-state-mode nskk-current-state) 'hiragana))

    ;; クリーンアップ
    (nskk-state-cleanup)
    (should-not nskk-current-state)))

(nskk-deftest nskk-e2e-mode-initialization-state
  "状態初期化時の各フィールド確認"
  :tags '(:e2e :mode)
  (nskk-e2e-with-test-buffer
    (should (nskk-state-p nskk-current-state))
    (should (eq (nskk-state-mode nskk-current-state) 'hiragana))
    (should (string= (nskk-state-input-buffer nskk-current-state) ""))
    (should (string= (nskk-state-conversion-buffer nskk-current-state) ""))
    (should (null (nskk-state-candidates nskk-current-state)))
    (should (= (nskk-state-candidate-index nskk-current-state) 0))))

;;; Test Suite 2: Romaji to Hiragana Conversion

(nskk-deftest nskk-e2e-romaji-to-hiragana-simple
  "基本的なローマ字→ひらがな変換フロー（単純パターン）"
  :tags '(:e2e :conversion)
  (nskk-e2e-with-test-buffer
    ;; ひらがなモード確認
    (nskk-e2e--assert-mode 'hiragana)

    ;; ローマ字変換テスト
    (let ((test-cases '(("a" "あ")
                       ("ka" "か")
                       ("ki" "き")
                       ("ku" "く")
                       ("ke" "け")
                       ("ko" "こ"))))
      (dolist (case test-cases)
        (let* ((input (car case))
               (expected (cadr case))
               (result (nskk-convert-romaji input)))
          (should (equal (nskk-converter-result-converted result) expected))
          (should (string= (nskk-converter-result-pending result) "")))))))

(nskk-deftest nskk-e2e-romaji-to-hiragana-complex
  "複雑なローマ字→ひらがな変換フロー"
  :tags '(:e2e :conversion)
  (nskk-e2e-with-test-buffer
    ;; 拗音（ようおん）
    (let ((result (nskk-convert-romaji "kya")))
      (should (equal (nskk-converter-result-converted result) "きゃ"))
      (should (string= (nskk-converter-result-pending result) "")))

    ;; 促音（そくおん）
    (let ((result (nskk-convert-romaji "kka")))
      (should (equal (nskk-converter-result-converted result) "っか"))
      (should (string= (nskk-converter-result-pending result) "")))

    ;; 撥音（はつおん）
    (let ((result (nskk-convert-romaji "nn")))
      (should (equal (nskk-converter-result-converted result) "ん"))
      (should (string= (nskk-converter-result-pending result) "")))

    ;; 連続変換
    (let ((result (nskk-convert-romaji "aiueo")))
      (should (equal (nskk-converter-result-converted result) "あいうえお"))
      (should (string= (nskk-converter-result-pending result) "")))))

(nskk-deftest nskk-e2e-romaji-to-hiragana-pending
  "未確定入力の処理フロー"
  :tags '(:e2e :conversion)
  (nskk-e2e-with-test-buffer
    ;; 未確定状態のテスト
    (let ((result (nskk-convert-romaji "k")))
      (should (equal (nskk-converter-result-converted result) ""))
      (should (equal (nskk-converter-result-pending result) "k")))

    ;; 促音の未確定
    (let ((result (nskk-convert-romaji "kk")))
      ;; "kk"は促音として部分確定されない（次の文字待ち）
      (should (or (equal (nskk-converter-result-pending result) "kk")
                  (equal (nskk-converter-result-pending result) "k"))))))

;;; Test Suite 3: Kanji Conversion Flow

(nskk-deftest nskk-e2e-kanji-conversion-basic
  "ローマ字→ひらがな→漢字変換の基本フロー"
  :tags '(:e2e :conversion :kanji)
  (nskk-e2e-with-test-buffer
    (let ((trie (nskk-trie-create)))
      ;; テスト辞書データ準備
      (nskk-trie-insert trie "かんじ" '("漢字" "幹事" "感じ"))

      ;; ローマ字→ひらがな変換
      (let* ((result (nskk-convert-romaji "kanji"))
             (hiragana (nskk-converter-result-converted result)))
        (should (equal hiragana "かんじ"))
        (should (string= (nskk-converter-result-pending result) ""))

        ;; 辞書検索
        (let ((candidates (nskk-trie-lookup-values trie hiragana)))
          (should (equal candidates '("漢字" "幹事" "感じ"))))))))

(nskk-deftest nskk-e2e-kanji-conversion-with-state
  "状態管理を含む漢字変換フロー"
  :tags '(:e2e :conversion :kanji :state)
  (nskk-e2e-with-test-buffer
    (let ((trie (nskk-trie-create)))
      ;; テスト辞書データ準備
      (nskk-trie-insert trie "にほん" '("日本"))

      ;; ローマ字→ひらがな
      (let* ((result (nskk-convert-romaji "nihon"))
             (hiragana (nskk-converter-result-converted result)))
        (should (equal hiragana "にほん"))

        ;; 状態に変換バッファ設定
        (setf (nskk-state-conversion-buffer nskk-current-state) hiragana)
        (should (equal (nskk-state-conversion-buffer nskk-current-state) "にほん"))

        ;; 辞書検索と候補設定
        (let ((candidates (nskk-trie-lookup-values trie hiragana)))
          (should (equal candidates '("日本")))

          ;; 状態に候補設定
          (nskk-state-set-candidates nskk-current-state candidates)
          (should (equal (nskk-state-candidates nskk-current-state) '("日本")))
          (should (equal (nskk-state-current-candidate nskk-current-state) "日本")))))))

(nskk-deftest nskk-e2e-kanji-conversion-not-found
  "辞書に見つからない場合のフロー"
  :tags '(:e2e :conversion :kanji)
  (nskk-e2e-with-test-buffer
    (let ((trie (nskk-trie-create)))
      ;; 存在しない見出し語を検索
      (let ((candidates (nskk-trie-lookup-values trie "そんざいしない")))
        (should (null candidates))
        (should (listp candidates))))))

;;; Test Suite 4: Candidate Selection

(nskk-deftest nskk-e2e-candidate-selection-basic
  "候補選択の基本フロー"
  :tags '(:e2e :candidates)
  (nskk-e2e-with-test-buffer
    (let ((candidates '("漢字" "幹事" "感じ")))
      ;; 候補ウィンドウ作成
      (let ((window (nskk-candidate-window-create candidates)))
        (should (nskk-candidate-window-p window))
        (should (equal (nskk-candidate-window-candidates window) candidates))

        ;; 初期選択確認
        (should (= (nskk-candidate-window-selected-index window) 0))
        (should (equal (nskk-candidate-window-current window) "漢字"))

        ;; 候補選択変更
        (nskk-candidate-window-select window 1)
        (should (= (nskk-candidate-window-selected-index window) 1))
        (should (equal (nskk-candidate-window-current window) "幹事"))

        (nskk-candidate-window-select window 2)
        (should (= (nskk-candidate-window-selected-index window) 2))
        (should (equal (nskk-candidate-window-current window) "感じ"))))))

(nskk-deftest nskk-e2e-candidate-window-display
  "候補ウィンドウ表示・非表示フロー"
  :tags '(:e2e :candidates :ui)
  (nskk-e2e-with-test-buffer
    (let ((candidates '("愛" "哀" "藍")))
      ;; 初期状態：候補ウィンドウは非表示
      (should-not (nskk-candidate-window-visible-p))

      ;; 候補ウィンドウ表示
      (let ((window (nskk-show-candidates candidates)))
        (should (nskk-candidate-window-p window))
        (should (nskk-candidate-window-visible-p))

        ;; 候補ウィンドウ非表示
        (nskk-hide-candidates)
        (should-not (nskk-candidate-window-visible-p))))))

(nskk-deftest nskk-e2e-candidate-navigation
  "候補ナビゲーションフロー"
  :tags '(:e2e :candidates)
  (nskk-e2e-with-test-buffer
    (let ((candidates '("一" "二" "三" "四" "五")))
      (let ((window (nskk-candidate-window-create candidates)))
        (should (equal (nskk-candidate-window-current window) "一"))

        ;; 次の候補へ
        (nskk-candidate-window-select window 1)
        (should (equal (nskk-candidate-window-current window) "二"))

        (nskk-candidate-window-select window 2)
        (should (equal (nskk-candidate-window-current window) "三"))

        ;; 前の候補へ
        (nskk-candidate-window-select window 1)
        (should (equal (nskk-candidate-window-current window) "二"))

        (nskk-candidate-window-select window 0)
        (should (equal (nskk-candidate-window-current window) "一"))))))

;;; Test Suite 5: Learning Integration

(nskk-deftest nskk-e2e-learning-basic-flow
  "学習機能の基本フロー"
  :tags '(:e2e :learning)
  (nskk-e2e-with-test-buffer
    (let ((trie (nskk-trie-create)))
      ;; 初期辞書データ
      (nskk-trie-insert trie "かんじ" '("漢字" "幹事" "感じ"))

      ;; 変換と候補選択
      (let* ((result (nskk-convert-romaji "kanji"))
             (hiragana (nskk-converter-result-converted result))
             (candidates (nskk-trie-lookup-values trie hiragana)))

        (should (equal candidates '("漢字" "幹事" "感じ")))

        ;; 2番目の候補（幹事）を選択
        (let ((window (nskk-candidate-window-create candidates)))
          (nskk-candidate-window-select window 1)
          (should (equal (nskk-candidate-window-current window) "幹事")))))))

(nskk-deftest nskk-e2e-learning-event-emission
  "学習イベント発行の統合テスト"
  :tags '(:e2e :learning :events)
  (nskk-e2e-with-test-buffer
    (let ((conversion-committed nil))
      ;; 変換確定イベントのリスナー登録
      (nskk-events-add-listener
       :conversion-committed
       (lambda (_event)
         (setq conversion-committed t)))

      ;; イベント発行
      (nskk-events-emit :conversion-committed :midasi "かんじ" :candidate "漢字")

      ;; イベント発火確認
      (should conversion-committed))))

;;; Test Suite 6: Buffer Management

(nskk-deftest nskk-e2e-buffer-insert-and-commit
  "バッファ挿入と確定のフロー"
  :tags '(:e2e :buffer)
  (nskk-e2e-with-test-buffer
    ;; 初期状態確認
    (should-not (nskk-buffer-has-pending-input-p))

    ;; マーカー設定（nskk-buffer-commitに必要）
    (setf (nskk-state-marker-start nskk-current-state) (point-marker))
    (setf (nskk-state-marker-end nskk-current-state) (point-marker))

    ;; 文字列挿入
    (nskk-buffer-insert "か")
    (should (nskk-buffer-has-pending-input-p))
    (should (equal (nskk-buffer-pending-text) "か"))

    ;; 確定
    (nskk-buffer-commit "か")
    (should (string-match-p "か" (buffer-string)))

    ;; クリア
    (nskk-buffer-clear)
    (should-not (nskk-buffer-has-pending-input-p))))

(nskk-deftest nskk-e2e-buffer-pending-management
  "未確定入力管理のフロー"
  :tags '(:e2e :buffer)
  (nskk-e2e-with-test-buffer
    ;; 複数文字挿入
    (nskk-buffer-insert "k")
    (should (equal (nskk-buffer-pending-text) "k"))

    (nskk-buffer-insert "a")
    (should (equal (nskk-buffer-pending-text) "ka"))

    ;; バックスペース
    (nskk-buffer-delete-backward-char 1)
    (should (equal (nskk-buffer-pending-text) "k"))

    ;; 全クリア
    (nskk-buffer-clear)
    (should-not (nskk-buffer-has-pending-input-p))
    (should (null (nskk-buffer-pending-text)))))

;;; Test Suite 7: Mode Switching

(nskk-deftest nskk-e2e-mode-switch-hiragana-katakana
  "ひらがな・カタカナモード切り替えフロー"
  :tags '(:e2e :mode-switch)
  (nskk-e2e-with-test-buffer
    ;; 初期状態：ひらがなモード
    (nskk-e2e--assert-mode 'hiragana)

    ;; カタカナモードへ切り替え
    (nskk-mode-switch nskk-current-state 'katakana)
    (should (eq (nskk-state-mode nskk-current-state) 'katakana))

    ;; ひらがなモードへ戻す
    (nskk-mode-switch nskk-current-state 'hiragana)
    (should (eq (nskk-state-mode nskk-current-state) 'hiragana))))

(nskk-deftest nskk-e2e-mode-switch-with-events
  "モード切り替えとイベント発行"
  :tags '(:e2e :mode-switch :events)
  (nskk-e2e-with-test-buffer
    (let ((mode-switched nil))
      ;; モード切り替えイベントのリスナー登録
      (nskk-events-add-listener
       :mode-switched
       (lambda (_event)
         (setq mode-switched t)))

      ;; モード切り替え
      (nskk-mode-switch nskk-current-state 'katakana)

      ;; イベント発火確認
      (should mode-switched)
      (should (eq (nskk-state-mode nskk-current-state) 'katakana)))))

(nskk-deftest nskk-e2e-mode-cycle
  "モード循環切り替えフロー"
  :tags '(:e2e :mode-switch)
  (nskk-e2e-with-test-buffer
    ;; ひらがな → カタカナ
    (nskk-mode-switch nskk-current-state 'katakana)
    (should (eq (nskk-state-mode nskk-current-state) 'katakana))

    ;; カタカナ → 英数
    (nskk-mode-switch nskk-current-state 'latin)
    (should (eq (nskk-state-mode nskk-current-state) 'latin))

    ;; 英数 → ひらがな
    (nskk-mode-switch nskk-current-state 'hiragana)
    (should (eq (nskk-state-mode nskk-current-state) 'hiragana))))

;;; Test Suite 8: Complete E2E Flow

(nskk-deftest nskk-e2e-complete-input-flow
  "完全な入力フロー：ローマ字入力→変換→候補選択→確定"
  :tags '(:e2e :integration)
  (nskk-e2e-with-test-buffer
    (let ((trie (nskk-trie-create)))
      ;; 辞書準備
      (nskk-trie-insert trie "あい" '("愛" "哀" "藍"))

      ;; 1. ローマ字入力
      (let* ((result (nskk-convert-romaji "ai"))
             (hiragana (nskk-converter-result-converted result)))
        (should (equal hiragana "あい"))

        ;; 2. 辞書検索
        (let ((candidates (nskk-trie-lookup-values trie hiragana)))
          (should (equal candidates '("愛" "哀" "藍")))

          ;; 3. 候補ウィンドウ表示
          (let ((window (nskk-show-candidates candidates)))
            (should (nskk-candidate-window-visible-p))

            ;; 4. 候補選択
            (nskk-select-candidate 0)
            (should (equal (nskk-candidate-window-current-selection) "愛"))

            ;; 5. 確定（マーカー設定必要）
            (setf (nskk-state-marker-start nskk-current-state) (point-marker))
            (setf (nskk-state-marker-end nskk-current-state) (point-marker))
            (nskk-buffer-commit "愛")
            (should (string-match-p "愛" (buffer-string)))

            ;; 6. クリーンアップ
            (nskk-hide-candidates)
            (should-not (nskk-candidate-window-visible-p))))))))

(nskk-deftest nskk-e2e-complete-flow-with-cache
  "キャッシュを含む完全フロー。"
  :tags '(:e2e :integration :cache)
  (nskk-e2e-with-test-buffer
    (let ((trie (nskk-trie-create))
          (cache (nskk-cache-create :size 10)))
      ;; 辞書準備
      (nskk-trie-insert trie "かんじ" '("漢字" "幹事"))

      ;; 初回検索（キャッシュミス）
      (let ((result1 (nskk-search-with-cache cache trie "かんじ")))
        (should (equal result1 '("漢字" "幹事"))))

      ;; 2回目検索（キャッシュヒット）
      (let ((result2 (nskk-search-with-cache cache trie "かんじ")))
        (should (equal result2 '("漢字" "幹事")))
        (should (> (nskk-cache-hit-rate cache) 0))))))

(nskk-deftest nskk-e2e-multiple-conversions
  "複数回の変換フロー"
  :tags '(:e2e :integration)
  (nskk-e2e-with-test-buffer
    (let ((trie (nskk-trie-create)))
      ;; 辞書準備
      (nskk-trie-insert trie "にほん" '("日本"))
      (nskk-trie-insert trie "ご" '("語"))

      ;; マーカー初期化
      (setf (nskk-state-marker-start nskk-current-state) (point-marker))
      (setf (nskk-state-marker-end nskk-current-state) (point-marker))

      ;; 1回目の変換
      (let* ((result1 (nskk-convert-romaji "nihon"))
             (hiragana1 (nskk-converter-result-converted result1))
             (candidates1 (nskk-trie-lookup-values trie hiragana1)))
        (should (equal candidates1 '("日本")))
        (nskk-buffer-commit "日本"))

      ;; マーカー再設定
      (setf (nskk-state-marker-start nskk-current-state) (point-marker))
      (setf (nskk-state-marker-end nskk-current-state) (point-marker))

      ;; 2回目の変換
      (let* ((result2 (nskk-convert-romaji "go"))
             (hiragana2 (nskk-converter-result-converted result2))
             (candidates2 (nskk-trie-lookup-values trie hiragana2)))
        (should (equal candidates2 '("語")))
        (nskk-buffer-commit "語"))

      ;; 結果確認
      (should (string-match-p "日本.*語" (buffer-string))))))

;;; Test Report

(defun nskk-e2e-basic-test-run-all ()
  "全E2E基本テストを実行し、結果を報告する。"
  (interactive)
  (let ((start-time (float-time)))
    (message "=== NSKK E2E Basic Tests ===")
    (ert-run-tests-batch "^nskk-e2e-")
    (let ((elapsed (- (float-time) start-time)))
      (message "Total elapsed time: %.3f seconds" elapsed))))

(provide 'nskk-e2e-basic-test)

;;; nskk-e2e-basic-test.el ends here
