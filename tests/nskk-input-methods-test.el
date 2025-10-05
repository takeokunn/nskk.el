;;; nskk-input-methods-test.el --- Tests for NSKK input methods -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, test
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0"))

;; This file is part of NSKK.

;;; Commentary:

;; 全入力方式の包括的なテスト。
;; 各入力方式が仕様通りに動作することを検証。

;;; Code:

(require 'ert)
(require 'nskk-input-azik)
(require 'nskk-input-act)
(require 'nskk-input-tutcode)
(require 'nskk-input-nicola)
(require 'nskk-input-kana)
(require 'nskk-input-qwerty)
(require 'nskk-input-dvorak)
(require 'nskk-input-colemak)
(require 'nskk-input-custom)
(require 'nskk-input-hybrid)
(require 'nskk-input-switcher)
(require 'nskk-input-loader)

;;; AZIK入力方式のテスト

(ert-deftest nskk-input-azik-lookup-test ()
  "AZIK入力方式の基本変換テスト。"
  ;; 基本変換
  (should (string= (nskk-input-azik-lookup "ka") "か"))
  (should (string= (nskk-input-azik-lookup "ki") "き"))

  ;; AZIK拡張
  (should (string= (nskk-input-azik-lookup "kj") "きゃ"))
  (should (string= (nskk-input-azik-lookup "kl") "きゅ"))
  (should (string= (nskk-input-azik-lookup "ko") "きょ"))

  ;; 促音
  (should (string= (nskk-input-azik-lookup ";") "っ"))

  ;; 存在しない変換
  (should-not (nskk-input-azik-lookup "xyz")))

(ert-deftest nskk-input-azik-candidates-test ()
  "AZIK入力方式の候補取得テスト。"
  (let ((candidates (nskk-input-azik-get-candidates "k")))
    (should (member "ka" candidates))
    (should (member "ki" candidates))
    (should (member "kj" candidates))
    (should (>= (length candidates) 10))))

(ert-deftest nskk-input-azik-stats-test ()
  "AZIK入力方式の統計情報テスト。"
  (let ((stats (nskk-input-azik-stats)))
    (should (plist-get stats :total))
    (should (plist-get stats :azik-extension))
    (should (> (plist-get stats :total) 0))
    (should (> (plist-get stats :azik-extension) 0))))

;;; ACT入力方式のテスト

(ert-deftest nskk-input-act-lookup-test ()
  "ACT入力方式の基本変換テスト。"
  ;; 基本変換
  (should (string= (nskk-input-act-lookup "ka") "か"))

  ;; ACT拡張
  (should (string= (nskk-input-act-lookup "kh") "きゃ"))
  (should (string= (nskk-input-act-lookup "kj") "きゅ"))

  ;; 存在しない変換
  (should-not (nskk-input-act-lookup "xyz")))

(ert-deftest nskk-input-act-performance-test ()
  "ACT入力方式の性能テスト（< 0.1ms）。"
  (let ((start-time (current-time))
        (iterations 1000))
    (dotimes (_ iterations)
      (nskk-input-act-lookup "ka"))
    (let ((elapsed (float-time (time-subtract (current-time) start-time))))
      ;; 平均 < 0.1ms = 0.0001秒
      (should (< (/ elapsed iterations) 0.0001)))))

;;; TUT-code入力方式のテスト

(ert-deftest nskk-input-tutcode-lookup-test ()
  "TUT-code入力方式の基本変換テスト。"
  ;; 2ストローク
  (should (string= (nskk-input-tutcode-lookup "kj") "か"))
  (should (string= (nskk-input-tutcode-lookup "kk") "き"))

  ;; 拗音（3ストローク）
  (should (string= (nskk-input-tutcode-lookup "kja") "きゃ"))

  ;; 存在しない変換
  (should-not (nskk-input-tutcode-lookup "xyz")))

(ert-deftest nskk-input-tutcode-stats-test ()
  "TUT-code入力方式の統計情報テスト。"
  (let ((stats (nskk-input-tutcode-stats)))
    (should (plist-get stats :total))
    (should (plist-get stats :two-stroke))
    (should (plist-get stats :three-stroke))))

;;; NICOLA（親指シフト）入力方式のテスト

(ert-deftest nskk-input-nicola-lookup-test ()
  "NICOLA入力方式の基本変換テスト。"
  ;; 無シフト
  (should (string= (nskk-input-nicola-lookup "k" nil) "き"))
  (should (string= (nskk-input-nicola-lookup "a" nil) "う"))

  ;; 左シフト
  (should (string= (nskk-input-nicola-lookup "k" 'left) "れ"))

  ;; 右シフト
  (should (string= (nskk-input-nicola-lookup "k" 'right) "ぎ"))

  ;; 存在しない変換
  (should-not (nskk-input-nicola-lookup "z" nil)))

(ert-deftest nskk-input-nicola-all-keys-test ()
  "NICOLA入力方式の全キー変換テスト。"
  (let ((all-keys '("q" "w" "e" "r" "t" "y" "u" "i" "o" "p"
                    "a" "s" "d" "f" "g" "h" "j" "k" "l" ";"
                    "z" "x" "c" "v" "b" "n" "m" "," "." "/")))
    (dolist (key all-keys)
      (let ((base (nskk-input-nicola-lookup key nil))
            (left (nskk-input-nicola-lookup key 'left))
            (right (nskk-input-nicola-lookup key 'right)))
        ;; いずれかのシフト状態で変換可能であることを確認
        (should (or base left right))))))

;;; かな入力方式のテスト

(ert-deftest nskk-input-kana-lookup-test ()
  "かな入力方式の基本変換テスト。"
  (should (string= (nskk-input-kana-lookup "k") "の"))
  (should (string= (nskk-input-kana-lookup "a") "ち"))
  (should (string= (nskk-input-kana-lookup "1") "ぬ")))

;;; QWERTY入力方式のテスト

(ert-deftest nskk-input-qwerty-lookup-test ()
  "QWERTY入力方式の基本変換テスト。"
  ;; 標準ローマ字と同じ
  (should (string= (nskk-input-qwerty-lookup "ka") "か"))
  (should (string= (nskk-input-qwerty-lookup "kya") "きゃ"))
  (should (string= (nskk-input-qwerty-lookup "nn") "ん")))

;;; Dvorak入力方式のテスト

(ert-deftest nskk-input-dvorak-basic-test ()
  "Dvorak入力方式の基本動作テスト。"
  ;; Dvorak配列での入力が変換されることを確認
  (let ((result (nskk-input-dvorak-lookup "ta")))
    (should result)
    (should (stringp result))))

;;; Colemak入力方式のテスト

(ert-deftest nskk-input-colemak-basic-test ()
  "Colemak入力方式の基本動作テスト。"
  ;; Colemak配列での入力が変換されることを確認
  (let ((result (nskk-input-colemak-lookup "ta")))
    (should result)
    (should (stringp result))))

;;; カスタム入力方式のテスト

(ert-deftest nskk-input-custom-define-test ()
  "カスタム入力方式の定義テスト。"
  ;; カスタム入力方式を定義
  (nskk-input-custom-define
   'test-custom
   '(("dh" . "で")
     ("kk" . "っか"))
   :base 'azik
   :description "Test custom method"
   :priority 100)

  ;; 定義されたことを確認
  (let ((method (nskk-input-custom-get-method 'test-custom)))
    (should method)
    (should (eq (nskk-input-custom-method-name method) 'test-custom))
    (should (equal (nskk-input-custom-method-description method)
                   "Test custom method")))

  ;; 変換テスト
  (should (string= (nskk-input-custom-lookup 'test-custom "dh") "で"))
  (should (string= (nskk-input-custom-lookup 'test-custom "kk") "っか"))

  ;; ベース入力方式の変換も可能
  (should (string= (nskk-input-custom-lookup 'test-custom "ka") "か"))

  ;; クリーンアップ
  (nskk-input-custom-remove-method 'test-custom))

(ert-deftest nskk-input-custom-management-test ()
  "カスタム入力方式の管理機能テスト。"
  ;; 複数の方式を定義
  (nskk-input-custom-define 'test1 '(("aa" . "あ")) :priority 200)
  (nskk-input-custom-define 'test2 '(("bb" . "い")) :priority 100)

  ;; リスト取得
  (let ((methods (nskk-input-custom-list-methods)))
    (should (member 'test1 methods))
    (should (member 'test2 methods)))

  ;; 削除
  (nskk-input-custom-remove-method 'test1)
  (should-not (member 'test1 (nskk-input-custom-list-methods)))

  ;; クリーンアップ
  (nskk-input-custom-remove-method 'test2))

;;; ハイブリッド入力方式のテスト

(ert-deftest nskk-input-hybrid-configure-test ()
  "ハイブリッド入力方式の設定テスト。"
  ;; 設定
  (nskk-input-hybrid-configure
   :methods '(azik qwerty)
   :fallback 'qwerty
   :auto-select t)

  ;; 設定が反映されていることを確認
  (should (equal nskk-input-hybrid-methods '(azik qwerty)))
  (should (eq nskk-input-hybrid-fallback 'qwerty))
  (should nskk-input-hybrid-auto-select))

(ert-deftest nskk-input-hybrid-lookup-test ()
  "ハイブリッド入力方式の変換テスト。"
  ;; 初期化
  (nskk-input-hybrid-register)

  ;; AZIK拡張が使えることを確認
  (should (string= (nskk-input-hybrid-lookup "kj") "きゃ"))

  ;; 標準ローマ字も使えることを確認
  (should (string= (nskk-input-hybrid-lookup "ka") "か")))

;;; 入力方式切り替え機構のテスト

(ert-deftest nskk-input-switcher-switch-test ()
  "入力方式切り替えテスト。"
  ;; 初期状態
  (let ((initial-method nskk-input-switcher-current-method))

    ;; 切り替え
    (nskk-input-switcher-switch 'azik)
    (should (eq nskk-input-switcher-current-method 'azik))

    ;; 別の方式に切り替え
    (nskk-input-switcher-switch 'act)
    (should (eq nskk-input-switcher-current-method 'act))

    ;; 履歴に記録されていることを確認
    (should (member 'azik nskk-input-switcher-history))

    ;; 元に戻す
    (nskk-input-switcher-switch initial-method)))

(ert-deftest nskk-input-switcher-cycle-test ()
  "入力方式の循環切り替えテスト。"
  (let ((initial-method nskk-input-switcher-current-method)
        (methods nskk-input-switcher-available-methods))

    ;; 循環テスト
    (dotimes (i (length methods))
      (nskk-input-switcher-cycle)
      (should (memq nskk-input-switcher-current-method methods)))

    ;; 元に戻す
    (nskk-input-switcher-switch initial-method)))

;;; 動的ロード機構のテスト

(ert-deftest nskk-input-loader-load-test ()
  "入力方式のロードテスト。"
  ;; 未ロード状態から開始
  (when (nskk-input-loader-loaded-p 'azik)
    (nskk-input-loader-unload 'azik))

  ;; ロード
  (should (nskk-input-loader-load 'azik))

  ;; ロード済みになっていることを確認
  (should (nskk-input-loader-loaded-p 'azik))

  ;; ロード時間が記録されていることを確認
  (when nskk-input-loader-measure-time
    (should (alist-get 'azik nskk-input-loader-load-times))))

(ert-deftest nskk-input-loader-dependencies-test ()
  "依存関係の自動解決テスト。"
  ;; hybrid入力方式は依存関係を持つ
  (when (nskk-input-loader-loaded-p 'hybrid)
    (nskk-input-loader-unload 'hybrid))

  ;; hybridをロード
  (nskk-input-loader-load 'hybrid)

  ;; 依存するazikもロードされていることを確認
  (should (nskk-input-loader-loaded-p 'azik)))

(ert-deftest nskk-input-loader-performance-test ()
  "ロード機構の性能テスト（< 100ms）。"
  ;; azikをアンロード
  (when (nskk-input-loader-loaded-p 'azik)
    (nskk-input-loader-unload 'azik))

  ;; ロード時間測定
  (let ((start-time (current-time)))
    (nskk-input-loader-load 'azik)
    (let ((elapsed (float-time (time-subtract (current-time) start-time))))
      ;; 100ms以内であることを確認
      (should (< elapsed 0.1)))))

;;; 統合テスト

(ert-deftest nskk-input-methods-integration-test ()
  "全入力方式の統合テスト。"
  (let ((methods '(qwerty azik act tutcode nicola kana dvorak colemak)))
    (dolist (method methods)
      ;; ロード
      (nskk-input-loader-load method)

      ;; ロード済みであることを確認
      (should (nskk-input-loader-loaded-p method))

      ;; 統計情報が取得できることを確認
      (let ((stats-fn (intern (format "nskk-input-%s-stats" method))))
        (when (fboundp stats-fn)
          (should (funcall stats-fn)))))))

(ert-deftest nskk-input-methods-consistency-test ()
  "全入力方式の一貫性テスト。"
  ;; 各入力方式が共通のインターフェースを持つことを確認
  (let ((methods '(qwerty azik act tutcode kana)))
    (dolist (method methods)
      ;; lookup関数が存在すること
      (should (fboundp (intern (format "nskk-input-%s-lookup" method))))

      ;; register関数が存在すること
      (should (fboundp (intern (format "nskk-input-%s-register" method))))

      ;; stats関数が存在すること
      (should (fboundp (intern (format "nskk-input-%s-stats" method)))))))

(ert-deftest nskk-input-methods-performance-benchmark ()
  "全入力方式の性能ベンチマーク（各 < 0.1ms）。"
  (let ((test-inputs '("ka" "ki" "ku" "ke" "ko"))
        (methods '((qwerty . nskk-input-qwerty-lookup)
                   (azik . nskk-input-azik-lookup)
                   (act . nskk-input-act-lookup)
                   (kana . nskk-input-kana-lookup))))
    (dolist (method-pair methods)
      (let ((method-name (car method-pair))
            (lookup-fn (cdr method-pair))
            (iterations 100))

        ;; 各入力で性能測定
        (dolist (input test-inputs)
          (let ((start-time (current-time)))
            (dotimes (_ iterations)
              (funcall lookup-fn input))
            (let ((elapsed (float-time (time-subtract (current-time) start-time))))
              ;; 平均 < 0.1ms
              (should (< (/ elapsed iterations) 0.0001)
                      (format "%s lookup for '%s' too slow: %.6f ms"
                              method-name input
                              (* (/ elapsed iterations) 1000))))))))))

(provide 'nskk-input-methods-test)

;;; nskk-input-methods-test.el ends here
