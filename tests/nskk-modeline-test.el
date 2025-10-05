;;; nskk-modeline-test.el --- Tests for nskk-modeline.el -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; This file is part of NSKK.

;;; Commentary:

;; nskk-modeline.el のテストスイート
;; ERTフレームワークを使用した包括的なテストを実装

;;; Code:

(require 'ert)
(require 'nskk-modeline)
(require 'nskk-state)

;;; モードライン状態生成テスト

(ert-deftest nskk-modeline-test-state-create-default ()
  "デフォルトモードライン状態の生成をテストする。"
  (let ((state (nskk-modeline-state-create)))
    (should (nskk-modeline-state-p state))
    (should (eq (nskk-modeline-state-mode state) 'hiragana))
    (should (eq (nskk-modeline-state-state state) 'normal))
    (should (string-empty-p (nskk-modeline-state-indicator state)))
    (should (null (nskk-modeline-state-dict-status state)))
    (should (null (nskk-modeline-state-error-message state)))))

(ert-deftest nskk-modeline-test-state-create-with-params ()
  "パラメータ指定でのモードライン状態生成をテストする。"
  (let ((state (nskk-modeline-state-create
                :mode 'katakana
                :state 'converting
                :dict-status 'loading)))
    (should (eq (nskk-modeline-state-mode state) 'katakana))
    (should (eq (nskk-modeline-state-state state) 'converting))
    (should (eq (nskk-modeline-state-dict-status state) 'loading))))

;;; フェイス取得テスト

(ert-deftest nskk-modeline-test-get-mode-face ()
  "モード別フェイス取得をテストする。"
  (should (eq (nskk-modeline-get-mode-face 'hiragana)
              'nskk-modeline-hiragana-face))
  (should (eq (nskk-modeline-get-mode-face 'katakana)
              'nskk-modeline-katakana-face))
  (should (eq (nskk-modeline-get-mode-face 'latin)
              'nskk-modeline-latin-face))
  (should (eq (nskk-modeline-get-mode-face 'zenkaku-latin)
              'nskk-modeline-zenkaku-latin-face))
  (should (eq (nskk-modeline-get-mode-face 'abbrev)
              'nskk-modeline-abbrev-face))
  (should (eq (nskk-modeline-get-mode-face 'conversion)
              'nskk-modeline-conversion-face)))

(ert-deftest nskk-modeline-test-get-mode-face-unknown ()
  "未知のモードに対するフェイス取得をテストする。"
  (should (eq (nskk-modeline-get-mode-face 'unknown-mode)
              'default)))

;;; 状態インジケーター取得テスト

(ert-deftest nskk-modeline-test-get-state-indicator ()
  "状態インジケーター取得をテストする。"
  (let ((nskk-modeline-show-state-indicator t))
    (should (equal (nskk-modeline-get-state-indicator 'normal) ""))
    (should (equal (nskk-modeline-get-state-indicator 'converting) "▽"))
    (should (equal (nskk-modeline-get-state-indicator 'selecting) "▼"))))

(ert-deftest nskk-modeline-test-get-state-indicator-disabled ()
  "状態インジケーター表示無効時のテスト。"
  (let ((nskk-modeline-show-state-indicator nil))
    (should (equal (nskk-modeline-get-state-indicator 'normal) ""))
    (should (equal (nskk-modeline-get-state-indicator 'converting) ""))
    (should (equal (nskk-modeline-get-state-indicator 'selecting) ""))))

(ert-deftest nskk-modeline-test-get-state-indicator-face ()
  "状態インジケーター用フェイス取得をテストする。"
  (should (eq (nskk-modeline-get-state-indicator-face 'converting)
              'nskk-modeline-converting-indicator-face))
  (should (eq (nskk-modeline-get-state-indicator-face 'selecting)
              'nskk-modeline-selecting-indicator-face))
  (should (eq (nskk-modeline-get-state-indicator-face 'normal)
              'default)))

;;; 辞書状態インジケーター取得テスト

(ert-deftest nskk-modeline-test-get-dict-status-indicator ()
  "辞書状態インジケーター取得をテストする。"
  (let ((nskk-modeline-show-dict-status t))
    (should (equal (nskk-modeline-get-dict-status-indicator nil) ""))
    (should (equal (nskk-modeline-get-dict-status-indicator 'loading) "読"))
    (should (equal (nskk-modeline-get-dict-status-indicator 'error) "!"))))

(ert-deftest nskk-modeline-test-get-dict-status-indicator-disabled ()
  "辞書状態表示無効時のテスト。"
  (let ((nskk-modeline-show-dict-status nil))
    (should (equal (nskk-modeline-get-dict-status-indicator 'loading) ""))
    (should (equal (nskk-modeline-get-dict-status-indicator 'error) ""))))

(ert-deftest nskk-modeline-test-get-dict-status-face ()
  "辞書状態用フェイス取得をテストする。"
  (should (eq (nskk-modeline-get-dict-status-face 'loading)
              'nskk-modeline-dict-loading-face))
  (should (eq (nskk-modeline-get-dict-status-face 'error)
              'nskk-modeline-error-face))
  (should (eq (nskk-modeline-get-dict-status-face nil)
              'default)))

;;; テキスト装飾テスト

(ert-deftest nskk-modeline-test-propertize-with-color ()
  "色使用時のテキスト装飾をテストする。"
  (let ((nskk-modeline-use-color t))
    (let ((result (nskk-modeline-propertize "test" 'bold)))
      (should (equal (substring-no-properties result) "test"))
      (should (get-text-property 0 'face result)))))

(ert-deftest nskk-modeline-test-propertize-without-color ()
  "色未使用時のテキスト装飾をテストする。"
  (let ((nskk-modeline-use-color nil))
    (let ((result (nskk-modeline-propertize "test" 'bold)))
      (should (equal result "test"))
      (should-not (get-text-property 0 'face result)))))

;;; フォーマット展開テスト

(ert-deftest nskk-modeline-test-expand-format-basic ()
  "基本的なフォーマット展開をテストする。"
  (should (equal (nskk-modeline-expand-format "[%m%s]" 'hiragana 'normal nil)
                 "[あ]"))
  (should (equal (nskk-modeline-expand-format "[%m%s]" 'katakana 'converting nil)
                 "[ア▽]"))
  (should (equal (nskk-modeline-expand-format "[%m%s]" 'latin 'selecting nil)
                 "[A▼]")))

(ert-deftest nskk-modeline-test-expand-format-with-dict ()
  "辞書状態を含むフォーマット展開をテストする。"
  (let ((nskk-modeline-show-dict-status t))
    (should (equal (nskk-modeline-expand-format "[%m%s%d]" 'hiragana 'normal 'loading)
                   "[あ読]"))
    (should (equal (nskk-modeline-expand-format "[%m%s%d]" 'hiragana 'normal 'error)
                   "[あ!]"))))

(ert-deftest nskk-modeline-test-expand-format-percent-escape ()
  "パーセント記号のエスケープをテストする。"
  (should (equal (nskk-modeline-expand-format "%%m" 'hiragana 'normal nil)
                 "%m")))

(ert-deftest nskk-modeline-test-expand-format-custom ()
  "カスタムフォーマットの展開をテストする。"
  (should (equal (nskk-modeline-expand-format "NSKK:%m" 'hiragana 'normal nil)
                 "NSKK:あ"))
  (should (equal (nskk-modeline-expand-format "<%m/%s>" 'katakana 'converting nil)
                 "<ア/▽>")))

;;; モードライン文字列生成テスト

(ert-deftest nskk-modeline-test-format-hiragana-normal ()
  "ひらがな通常モードの文字列生成をテストする。"
  (let ((nskk-modeline-format "[%m%s]")
        (nskk-modeline-use-color nil))
    (let ((result (nskk-modeline-format 'hiragana 'normal)))
      (should (string-match-p "あ" (substring-no-properties result))))))

(ert-deftest nskk-modeline-test-format-katakana-converting ()
  "カタカナ変換中モードの文字列生成をテストする。"
  (let ((nskk-modeline-format "[%m%s]")
        (nskk-modeline-use-color nil)
        (nskk-modeline-show-state-indicator t))
    (let ((result (nskk-modeline-format 'katakana 'converting)))
      (let ((plain (substring-no-properties result)))
        (should (string-match-p "ア" plain))
        (should (string-match-p "▽" plain))))))

(ert-deftest nskk-modeline-test-format-latin-selecting ()
  "英数候補選択中モードの文字列生成をテストする。"
  (let ((nskk-modeline-format "[%m%s]")
        (nskk-modeline-use-color nil)
        (nskk-modeline-show-state-indicator t))
    (let ((result (nskk-modeline-format 'latin 'selecting)))
      (let ((plain (substring-no-properties result)))
        (should (string-match-p "A" plain))
        (should (string-match-p "▼" plain))))))

(ert-deftest nskk-modeline-test-format-with-color ()
  "色付きモードライン文字列生成をテストする。"
  (let ((nskk-modeline-format "[%m%s]")
        (nskk-modeline-use-color t))
    (let ((result (nskk-modeline-format 'hiragana 'normal)))
      ;; テキストプロパティが存在することを確認（位置1にモードインジケーターがある）
      (should (get-text-property 1 'face result)))))

;;; NSKK状態からの変換テスト

(ert-deftest nskk-modeline-test-state-from-nskk-state-normal ()
  "通常状態のNSKK状態からの変換をテストする。"
  (let ((nskk-state (nskk-state-create :mode 'hiragana)))
    (let ((ml-state (nskk-modeline-state-from-nskk-state nskk-state)))
      (should (eq (nskk-modeline-state-mode ml-state) 'hiragana))
      (should (eq (nskk-modeline-state-state ml-state) 'normal)))))

(ert-deftest nskk-modeline-test-state-from-nskk-state-converting ()
  "変換中状態のNSKK状態からの変換をテストする。"
  (let ((nskk-state (nskk-state-create :mode 'katakana
                                       :conversion-buffer "かな")))
    (let ((ml-state (nskk-modeline-state-from-nskk-state nskk-state)))
      (should (eq (nskk-modeline-state-mode ml-state) 'katakana))
      (should (eq (nskk-modeline-state-state ml-state) 'converting)))))

(ert-deftest nskk-modeline-test-state-from-nskk-state-selecting ()
  "候補選択中状態のNSKK状態からの変換をテストする。"
  (let ((nskk-state (nskk-state-create :mode 'conversion)))
    (nskk-state-set-candidates nskk-state '("候補1" "候補2"))
    (let ((ml-state (nskk-modeline-state-from-nskk-state nskk-state)))
      (should (eq (nskk-modeline-state-mode ml-state) 'conversion))
      (should (eq (nskk-modeline-state-state ml-state) 'selecting)))))

;;; モードライン更新テスト

(ert-deftest nskk-modeline-test-update-basic ()
  "基本的なモードライン更新をテストする。"
  (with-temp-buffer
    (let ((nskk-modeline-format "[%m%s]")
          (nskk-modeline-use-color nil))
      (nskk-modeline-update 'hiragana 'normal)
      (should (string-match-p "あ" (substring-no-properties nskk-modeline-string))))))

(ert-deftest nskk-modeline-test-update-with-state ()
  "NSKK状態を使用したモードライン更新をテストする。"
  (with-temp-buffer
    (let ((nskk-modeline-format "[%m%s]")
          (nskk-modeline-use-color nil)
          (nskk-current-state (nskk-state-create :mode 'katakana)))
      (nskk-modeline-update)
      (should (string-match-p "ア" (substring-no-properties nskk-modeline-string))))))

(ert-deftest nskk-modeline-test-update-with-dict-status ()
  "辞書状態付きモードライン更新をテストする。"
  (with-temp-buffer
    (let ((nskk-modeline-format "[%m%s]")
          (nskk-modeline-use-color nil)
          (nskk-modeline-show-dict-status t))
      (nskk-modeline-update 'hiragana 'normal 'loading)
      (should (string-match-p "あ" (substring-no-properties nskk-modeline-string))))))

;;; フォーマットカスタマイズテスト

(ert-deftest nskk-modeline-test-custom-format-simple ()
  "シンプルなカスタムフォーマットをテストする。"
  (let ((nskk-modeline-format "<%m>")
        (nskk-modeline-use-color nil))
    (with-temp-buffer
      (nskk-modeline-update 'hiragana 'normal)
      (should (string-match-p "<あ>" (substring-no-properties nskk-modeline-string))))))

(ert-deftest nskk-modeline-test-custom-format-complex ()
  "複雑なカスタムフォーマットをテストする。"
  (let ((nskk-modeline-format "NSKK[%m%s]")
        (nskk-modeline-use-color nil)
        (nskk-modeline-show-state-indicator t))
    (with-temp-buffer
      (nskk-modeline-update 'katakana 'converting)
      (let ((plain (substring-no-properties nskk-modeline-string)))
        (should (string-match-p "NSKK" plain))
        (should (string-match-p "ア" plain))
        (should (string-match-p "▽" plain))))))

;;; モードライン統合テスト

(ert-deftest nskk-modeline-test-install ()
  "モードライン統合のインストールをテストする。"
  (with-temp-buffer
    (let ((mode-line-format (default-value 'mode-line-format)))
      (nskk-modeline-install)
      (should (memq 'nskk-modeline-string mode-line-format))
      (nskk-modeline-uninstall)
      (should-not (memq 'nskk-modeline-string mode-line-format)))))

(ert-deftest nskk-modeline-test-uninstall ()
  "モードライン統合のアンインストールをテストする。"
  (with-temp-buffer
    (let ((mode-line-format '(mode-line-front-space nskk-modeline-string mode-line-position)))
      (nskk-modeline-uninstall)
      (should-not (memq 'nskk-modeline-string mode-line-format))
      (should (equal nskk-modeline-string "")))))

;;; フック関数テスト

(ert-deftest nskk-modeline-test-on-mode-switch ()
  "モード切り替え時のフック関数をテストする。"
  (with-temp-buffer
    (let ((nskk-modeline-format "[%m%s]")
          (nskk-modeline-use-color nil)
          (nskk-current-state (nskk-state-create)))
      (nskk-modeline-on-mode-switch 'hiragana 'katakana)
      (should (string-match-p "ア" (substring-no-properties nskk-modeline-string))))))

;;; 統計情報テスト

(ert-deftest nskk-modeline-test-stats ()
  "統計情報取得をテストする。"
  (let ((stats (nskk-modeline-stats)))
    (should (plist-get stats :format))
    (should (equal (plist-get stats :format) nskk-modeline-format))
    (should (eq (plist-get stats :use-color) nskk-modeline-use-color))
    (should (eq (plist-get stats :show-indicator) nskk-modeline-show-state-indicator))))

;;; エッジケーステスト

(ert-deftest nskk-modeline-test-empty-format ()
  "空フォーマット文字列のテスト。"
  (let ((nskk-modeline-format "")
        (nskk-modeline-use-color nil))
    (with-temp-buffer
      (nskk-modeline-update 'hiragana 'normal)
      (should (equal (substring-no-properties nskk-modeline-string) "")))))

(ert-deftest nskk-modeline-test-all-modes ()
  "全モードの文字列生成をテストする。"
  (let ((nskk-modeline-format "[%m]")
        (nskk-modeline-use-color nil))
    (dolist (mode '(hiragana katakana latin zenkaku-latin abbrev conversion))
      (with-temp-buffer
        (nskk-modeline-update mode 'normal)
        (should (> (length nskk-modeline-string) 0))))))

(ert-deftest nskk-modeline-test-all-states ()
  "全状態の文字列生成をテストする。"
  (let ((nskk-modeline-format "[%m%s]")
        (nskk-modeline-use-color nil)
        (nskk-modeline-show-state-indicator t))
    (dolist (state '(normal converting selecting))
      (with-temp-buffer
        (nskk-modeline-update 'hiragana state)
        (should (> (length nskk-modeline-string) 0))))))

;;; パフォーマンステスト

(ert-deftest nskk-modeline-test-performance-update ()
  "モードライン更新のパフォーマンスをテストする。"
  (with-temp-buffer
    (let ((nskk-modeline-format "[%m%s]")
          (nskk-modeline-use-color t)
          (start-time (current-time)))
      ;; 1000回更新
      (dotimes (_ 1000)
        (nskk-modeline-update 'hiragana 'normal))
      (let ((elapsed (float-time (time-subtract (current-time) start-time))))
        ;; 1000回で1秒未満であることを確認
        (should (< elapsed 1.0))))))

(ert-deftest nskk-modeline-test-performance-format ()
  "文字列フォーマットのパフォーマンスをテストする。"
  (let ((nskk-modeline-format "[%m%s]")
        (nskk-modeline-use-color t)
        (start-time (current-time)))
    ;; 1000回フォーマット
    (dotimes (_ 1000)
      (nskk-modeline-format 'hiragana 'normal))
    (let ((elapsed (float-time (time-subtract (current-time) start-time))))
      ;; 1000回で1秒未満であることを確認
      (should (< elapsed 1.0)))))

;;; 統合テスト

(ert-deftest nskk-modeline-test-integration-full-workflow ()
  "完全なワークフローの統合テストを実行する。"
  (with-temp-buffer
    (let ((nskk-modeline-format "[%m%s]")
          (nskk-modeline-use-color t)
          (nskk-current-state (nskk-state-create)))

      ;; 初期状態
      (nskk-modeline-update)
      (should (> (length nskk-modeline-string) 0))

      ;; モード切り替え
      (nskk-state-set-mode nskk-current-state 'katakana)
      (nskk-modeline-update)
      (should (string-match-p "ア" (substring-no-properties nskk-modeline-string)))

      ;; 変換開始
      (setf (nskk-state-conversion-buffer nskk-current-state) "かな")
      (nskk-modeline-update)

      ;; 候補選択
      (nskk-state-set-mode nskk-current-state 'conversion)
      (nskk-state-set-candidates nskk-current-state '("仮名" "カナ"))
      (nskk-modeline-update)

      ;; 確定
      (nskk-state-clear-all nskk-current-state)
      (nskk-state-set-mode nskk-current-state 'hiragana)
      (nskk-modeline-update)
      (should (string-match-p "あ" (substring-no-properties nskk-modeline-string))))))

;;; 追加のエッジケーステスト

(ert-deftest nskk-modeline-test-nskk-state-format ()
  "nskk-state構造体を直接渡すテスト"
  (let ((nskk-state (nskk-state-create :mode 'katakana)))
    (let ((result (nskk-modeline-format nskk-state)))
      (should (stringp result))
      (should (string-match-p "ア" (substring-no-properties result))))))

(ert-deftest nskk-modeline-test-modeline-state-format ()
  "nskk-modeline-state構造体を直接渡すテスト"
  (let ((ml-state (nskk-modeline-state-create :mode 'hiragana :state 'converting)))
    (let ((result (nskk-modeline-format ml-state)))
      (should (stringp result)))))

(ert-deftest nskk-modeline-test-minimal-params ()
  "最小限のパラメータでのフォーマットテスト"
  (let ((result (nskk-modeline-format 'latin)))
    (should (stringp result))))

(ert-deftest nskk-modeline-test-format-from-state ()
  "nskk-modeline-format-from-state 関数のテスト"
  (let ((ml-state (nskk-modeline-state-create :mode 'katakana :state 'selecting)))
    (let ((result (nskk-modeline-format-from-state ml-state)))
      (should (stringp result)))))

;;; プレビュー機能テスト

(ert-deftest nskk-modeline-test-preview-all-modes-execution ()
  "プレビュー機能の実行テスト"
  ;; エラーなく実行できることを確認
  (should-not (nskk-modeline-preview-all-modes))
  ;; バッファが作成されることを確認
  (should (get-buffer "*NSKK Modeline Preview*"))
  ;; 後片付け
  (kill-buffer "*NSKK Modeline Preview*"))

;;; describe-current テスト

(ert-deftest nskk-modeline-test-describe-current ()
  "describe-current 関数のテスト"
  (with-temp-buffer
    (let ((nskk-modeline-string "[test]"))
      (should-not (nskk-modeline-describe-current)))))

;;; インストール/アンインストールの詳細テスト

(ert-deftest nskk-modeline-test-install-idempotent ()
  "インストールの冪等性テスト"
  (with-temp-buffer
    (let ((mode-line-format (default-value 'mode-line-format)))
      (nskk-modeline-install)
      (let ((count1 (cl-count 'nskk-modeline-string mode-line-format)))
        (nskk-modeline-install)
        (let ((count2 (cl-count 'nskk-modeline-string mode-line-format)))
          ;; 2回インストールしても1つだけ存在するはず
          (should (= count1 count2))))
      (nskk-modeline-uninstall))))

(ert-deftest nskk-modeline-test-install-without-position ()
  "positionが見つからない場合のインストールテスト"
  (with-temp-buffer
    (let ((mode-line-format '(mode-line-front-space))
          (nskk-modeline-position 'non-existent-position))
      (nskk-modeline-install)
      ;; 末尾に追加されるはず
      (should (memq 'nskk-modeline-string mode-line-format))
      (nskk-modeline-uninstall))))

;;; 辞書状態の組み合わせテスト

(ert-deftest nskk-modeline-test-all-dict-status ()
  "全ての辞書状態のテスト"
  (let ((nskk-modeline-format "[%m%d]")
        (nskk-modeline-use-color nil)
        (nskk-modeline-show-dict-status t))
    (dolist (dict-status '(nil loading error))
      (with-temp-buffer
        (nskk-modeline-update 'hiragana 'normal dict-status)
        (should (> (length nskk-modeline-string) 0))))))

;;; フォーマット展開の詳細テスト

(ert-deftest nskk-modeline-test-expand-format-all-placeholders ()
  "全てのプレースホルダーを含むフォーマット展開"
  (let ((nskk-modeline-show-dict-status t)
        (nskk-modeline-show-state-indicator t))
    (let ((result (nskk-modeline-expand-format "[%m%s%d%%]" 'hiragana 'converting 'loading)))
      (should (string-match-p "あ" result))
      (should (string-match-p "▽" result))
      (should (string-match-p "読" result))
      (should (string-match-p "%" result)))))

(ert-deftest nskk-modeline-test-expand-format-no-placeholders ()
  "プレースホルダーなしのフォーマット展開"
  (let ((result (nskk-modeline-expand-format "NSKK" 'hiragana 'normal nil)))
    (should (equal result "NSKK"))))

;;; 状態判定の詳細テスト

(ert-deftest nskk-modeline-test-state-from-nskk-state-all-cases ()
  "全ての状態判定パターンのテスト"
  ;; normal状態
  (let* ((state1 (nskk-state-create :mode 'hiragana))
         (ml-state1 (nskk-modeline-state-from-nskk-state state1)))
    (should (eq (nskk-modeline-state-state ml-state1) 'normal)))

  ;; converting状態
  (let* ((state2 (nskk-state-create :mode 'katakana :conversion-buffer "test"))
         (ml-state2 (nskk-modeline-state-from-nskk-state state2)))
    (should (eq (nskk-modeline-state-state ml-state2) 'converting)))

  ;; selecting状態
  (let* ((state3 (nskk-state-create :mode 'conversion))
         (ml-state3 (progn
                      (nskk-state-set-candidates state3 '("a" "b"))
                      (nskk-modeline-state-from-nskk-state state3))))
    (should (eq (nskk-modeline-state-state ml-state3) 'selecting))))

;;; 色無効時の詳細テスト

(ert-deftest nskk-modeline-test-no-color-all-components ()
  "色無効時の全コンポーネントテスト"
  (let ((nskk-modeline-use-color nil)
        (nskk-modeline-show-state-indicator t)
        (nskk-modeline-show-dict-status t))
    (let ((result (nskk-modeline-format 'hiragana 'converting 'loading)))
      (should (stringp result))
      ;; テキストプロパティがないことを確認
      (should (equal result (substring-no-properties result))))))

;;; nskk-modeline-state-copy テスト

(ert-deftest nskk-modeline-test-state-copy ()
  "状態のコピーテスト"
  (let* ((original (nskk-modeline-state-create :mode 'katakana :state 'converting))
         (copied (nskk-modeline-state-copy original)))
    (should (nskk-modeline-state-p copied))
    (should (eq (nskk-modeline-state-mode copied) 'katakana))
    (should (eq (nskk-modeline-state-state copied) 'converting))
    ;; オリジナルを変更してもコピーは影響を受けない
    (setf (nskk-modeline-state-mode original) 'hiragana)
    (should (eq (nskk-modeline-state-mode copied) 'katakana))))

(provide 'nskk-modeline-test)

;;; nskk-modeline-test.el ends here
