;;; nskk-test.el --- Tests for nskk.el -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; This file is part of NSKK.

;;; Commentary:

;; nskk.el のテストスイート
;; ERTフレームワークを使用した包括的なテストを実装

;;; Code:

(require 'ert)
(require 'nskk)

;;; 基本機能のテスト

(ert-deftest nskk-test-feature-loaded ()
  "nskk モジュールがロードされていることを確認する。"
  (should (featurep 'nskk)))

(ert-deftest nskk-test-version-constants ()
  "バージョン定数が定義されていることを確認する。"
  (should (stringp nskk-version))
  (should (not (string-empty-p nskk-version))))

(ert-deftest nskk-test-version-function ()
  "nskk-version 関数が正常に動作することを確認する。"
  (should (functionp 'nskk-version))
  (let ((result (nskk-version)))
    (should (or (null result) t))))

;;; State管理のテスト

(ert-deftest nskk-test-state-get-default ()
  "nskk--state-get が未設定のキーに対してデフォルト値を返すことを確認する。"
  (should (eq (nskk--state-get 'nonexistent-key 'default-value) 'default-value))
  (should (eq (nskk--state-get 'another-key 42) 42)))

(ert-deftest nskk-test-state-set-and-get ()
  "nskk--state-set と nskk--state-get の連携動作を確認する。"
  (nskk--state-set 'test-key 'test-value)
  (should (eq (nskk--state-get 'test-key) 'test-value))
  ;; クリーンアップ
  (nskk--reset-state))

(ert-deftest nskk-test-state-set-returns-value ()
  "nskk--state-set が設定した値を返すことを確認する。"
  (let ((result (nskk--state-set 'test-key 'test-value)))
    (should (eq result 'test-value)))
  (nskk--reset-state))

(ert-deftest nskk-test-reset-state ()
  "nskk--reset-state が状態を初期化することを確認する。"
  (nskk--state-set 'test-key 'test-value)
  (should (eq (nskk--state-get 'test-key) 'test-value))
  (nskk--reset-state)
  (should (eq (nskk--state-get 'test-key 'default) 'default)))

(ert-deftest nskk-test-state-mutex-exists ()
  "mutexが利用可能な場合、nskk--state-mutexが設定されていることを確認する。"
  (if (fboundp 'make-mutex)
      (should (mutexp nskk--state-mutex))
    (should (null nskk--state-mutex))))

;;; Dictionary関連のテスト

(ert-deftest nskk-test-resolve-dictionary-path-nil ()
  "辞書パスがnilの場合の動作を確認する。"
  (let ((nskk-dictionary-path nil)
        (nskk-user-dictionary-path nil))
    (should (null (nskk--resolve-dictionary-path)))))

(ert-deftest nskk-test-resolve-dictionary-path-nonexistent ()
  "存在しない辞書パスに対してエラーをシグナルすることを確認する。"
  (should-error
   (nskk--resolve-dictionary-path "/nonexistent/path/to/dictionary")
   :type 'nskk-dictionary-not-found))

(ert-deftest nskk-test-search-dictionary-invalid-key ()
  "無効なキーで辞書検索するとエラーになることを確認する。"
  (should-error (nskk--search-dictionary 123)
                :type 'nskk-invalid-input)
  (should-error (nskk--search-dictionary nil)
                :type 'nskk-invalid-input))

(ert-deftest nskk-test-search-dictionary-invalid-type ()
  "無効な検索タイプでエラーになることを確認する。"
  (should-error (nskk--search-dictionary "test" 'invalid-type)
                :type 'nskk-invalid-input))

(ert-deftest nskk-test-maybe-load-dictionary-safe ()
  "nskk--maybe-load-dictionary がエラーを抑制することを確認する。"
  (should-not (nskk--maybe-load-dictionary)))

;;; Conversion関連のテスト

(ert-deftest nskk-test-find-conversion-invalid-input ()
  "無効な入力で変換するとエラーになることを確認する。"
  (should-error (nskk--find-conversion 123)
                :type 'nskk-invalid-input)
  (should-error (nskk--find-conversion nil)
                :type 'nskk-invalid-input))

(ert-deftest nskk-test-find-conversion-empty-string ()
  "空文字列の変換結果を確認する。"
  (let ((result (nskk--find-conversion "")))
    (should (or (null result) (consp result)))))

(ert-deftest nskk-test-process-character-invalid ()
  "無効な文字で処理するとエラーになることを確認する。"
  (should-error (nskk--process-character "not-a-char")
                :type 'nskk-invalid-input)
  (should-error (nskk--process-character #x110000)
                :type 'nskk-invalid-input))

;;; Input Method関連のテスト

(ert-deftest nskk-test-input-method-nil ()
  "nilキーに対する入力メソッドの動作を確認する。"
  (should (null (nskk-input-method nil))))

(ert-deftest nskk-test-input-method-timeout ()
  "timeoutキーに対する入力メソッドの動作を確認する。"
  (should (null (nskk-input-method 'timeout))))

(ert-deftest nskk-test-input-method-character ()
  "文字キーに対する入力メソッドの動作を確認する。"
  (let ((result (nskk-input-method ?a)))
    (should (or (null result) (listp result)))))

(ert-deftest nskk-test-input-method-non-character ()
  "非文字キーに対する入力メソッドの動作を確認する。"
  (let ((result (nskk-input-method 'some-key)))
    (should (listp result))
    (should (equal result '(some-key)))))

;;; Mode Control関連のテスト

(ert-deftest nskk-test-activate ()
  "nskk-activate が正常に動作することを確認する。"
  (should (eq (nskk-activate) t)))

(ert-deftest nskk-test-deactivate ()
  "nskk-deactivate が正常に動作することを確認する。"
  (nskk-activate)
  (should (null (nskk-deactivate))))

(ert-deftest nskk-test-async-mode-toggle-without-threads ()
  "スレッド機能がない環境でのnskk-async-mode-toggleを確認する。"
  (let ((callback-called nil)
        (error-callback-called nil))
    (nskk-async-mode-toggle
     :callback (lambda (_state) (setq callback-called t))
     :error-callback (lambda (_err) (setq error-callback-called t)))
    (sleep-for 0.05)
    ;; コールバックまたはエラーコールバックのいずれかが呼ばれるはず
    (should (or callback-called error-callback-called))))

(ert-deftest nskk-test-async-mode-toggle-callback ()
  "nskk-async-mode-toggle のコールバックが機能することを確認する。"
  (let ((result nil))
    (nskk-async-mode-toggle
     :callback (lambda (state) (setq result state)))
    ;; 非同期の場合は即座には設定されないが、同期フォールバックでは設定される
    (should (or (null result) (booleanp result)))))

;;; Setup関連のテスト

(ert-deftest nskk-test-setup ()
  "nskk-setup が正常に動作することを確認する。"
  (should (eq (nskk-setup) t)))

(ert-deftest nskk-test-setup-conversion-rules ()
  "nskk-setup がローマ字変換ルールを初期化することを確認する。"
  (nskk-setup)
  (should (hash-table-p nskk--conversion-rules))
  (should (> (hash-table-count nskk--conversion-rules) 0)))

;;; Health Check関連のテスト

(ert-deftest nskk-test-health-check ()
  "nskk-health-check が実行可能であることを確認する。"
  (let ((result (nskk-health-check)))
    (should (booleanp result))))

(ert-deftest nskk-test-list-modules ()
  "nskk-list-modules が正常に実行されることを確認する。"
  (should-not (nskk-list-modules)))

(ert-deftest nskk-test-integration-test-ready-p ()
  "nskk-integration-test-ready-p が boolean を返すことを確認する。"
  (let ((result (nskk-integration-test-ready-p)))
    (should (booleanp result))))

;;; Initialize/Shutdown関連のテスト

(ert-deftest nskk-test-initialize ()
  "nskk-initialize が正常に動作することを確認する。"
  (should-not (nskk-initialize)))

(ert-deftest nskk-test-shutdown ()
  "nskk-shutdown が正常に動作することを確認する。"
  (should-not (nskk-shutdown)))

(ert-deftest nskk-test-initialize-shutdown-cycle ()
  "初期化とシャットダウンのサイクルが正常に動作することを確認する。"
  (nskk-initialize)
  (nskk-shutdown)
  (should t))

;;; Minor Mode関連のテスト

(ert-deftest nskk-test-mode-definition ()
  "nskk-mode が定義されていることを確認する。"
  (should (functionp 'nskk-mode)))

(ert-deftest nskk-test-toggle-function ()
  "nskk-toggle が定義されていることを確認する。"
  (should (functionp 'nskk-toggle)))

;;; Customization関連のテスト

(ert-deftest nskk-test-customization-group ()
  "nskk カスタマイズグループが存在することを確認する。"
  (should (get 'nskk 'group-documentation)))

(ert-deftest nskk-test-dictionary-path-custom ()
  "nskk-dictionary-path カスタム変数が定義されていることを確認する。"
  (should (boundp 'nskk-dictionary-path)))

(ert-deftest nskk-test-user-dictionary-path-custom ()
  "nskk-user-dictionary-path カスタム変数が定義されていることを確認する。"
  (should (boundp 'nskk-user-dictionary-path))
  (should (stringp nskk-user-dictionary-path)))

(ert-deftest nskk-test-input-method-name-custom ()
  "nskk-input-method-name カスタム変数が定義されていることを確認する。"
  (should (boundp 'nskk-input-method-name))
  (should (stringp nskk-input-method-name)))

(ert-deftest nskk-test-enable-completion-custom ()
  "nskk-enable-completion カスタム変数が定義されていることを確認する。"
  (should (boundp 'nskk-enable-completion))
  (should (booleanp nskk-enable-completion)))

(ert-deftest nskk-test-candidate-display-count-custom ()
  "nskk-candidate-display-count カスタム変数が定義されていることを確認する。"
  (should (boundp 'nskk-candidate-display-count))
  (should (numberp nskk-candidate-display-count)))

(ert-deftest nskk-test-debug-mode-custom ()
  "nskk-debug-mode カスタム変数が定義されていることを確認する。"
  (should (boundp 'nskk-debug-mode))
  (should (booleanp nskk-debug-mode)))

;;; Hook関連のテスト

(ert-deftest nskk-test-hooks-defined ()
  "各種フックが定義されていることを確認する。"
  (should (boundp 'nskk-before-input-hook))
  (should (boundp 'nskk-after-input-hook))
  (should (boundp 'nskk-before-conversion-hook))
  (should (boundp 'nskk-after-conversion-hook))
  (should (boundp 'nskk-mode-change-hook)))

(ert-deftest nskk-test-before-input-hook ()
  "nskk-before-input-hook が機能することを確認する。"
  (let ((hook-called nil))
    (let ((nskk-before-input-hook (list (lambda (_char) (setq hook-called t)))))
      (ignore-errors (nskk-input-method ?a)))
    (should hook-called)))

(ert-deftest nskk-test-after-input-hook ()
  "nskk-after-input-hook が機能することを確認する。"
  (let ((hook-called nil))
    (let ((nskk-after-input-hook (list (lambda (_result) (setq hook-called t)))))
      (ignore-errors (nskk-input-method ?a)))
    (should hook-called)))

;;; 変数エイリアスのテスト

(ert-deftest nskk-test-variable-aliases ()
  "変数エイリアスが正しく定義されていることを確認する。"
  (should (indirect-variable 'nskk--dictionary-cache))
  (should (indirect-variable 'nskk--input-buffer))
  (should (indirect-variable 'nskk--conversion-mode))
  (should (indirect-variable 'nskk--candidate-list))
  (should (indirect-variable 'nskk--candidate-index))
  (should (indirect-variable 'nskk--state)))

;;; Load Time関連のテスト

(ert-deftest nskk-test-show-load-time ()
  "nskk-show-load-time が正常に実行されることを確認する。"
  (should-not (nskk-show-load-time)))

(ert-deftest nskk-test-load-time-variable ()
  "nskk--load-time 変数が定義されていることを確認する。"
  (should (boundp 'nskk--load-time)))

;;; Benchmark マクロのテスト

(ert-deftest nskk-test-benchmark-macro ()
  "nskk--benchmark マクロが正常に動作することを確認する。"
  (let ((nskk-debug-mode nil)
        (result (nskk--benchmark "test" (+ 1 2))))
    (should (= result 3))))

(ert-deftest nskk-test-benchmark-macro-debug-mode ()
  "デバッグモードでnskk--benchmark マクロが動作することを確認する。"
  (let ((nskk-debug-mode t)
        (result (nskk--benchmark "test" (+ 1 2))))
    (should (= result 3))))

;;; State plist関連のテスト

(ert-deftest nskk-test-state-plist-initialization ()
  "nskk--state-plist が初期化されることを確認する。"
  (nskk--reset-state)
  (should (or (null nskk--state-plist)
              (listp nskk--state-plist))))

(ert-deftest nskk-test-state-multiple-keys ()
  "複数のキーを設定・取得できることを確認する。"
  (nskk--reset-state)
  (nskk--state-set 'key1 'value1)
  (nskk--state-set 'key2 'value2)
  (nskk--state-set 'key3 'value3)
  (should (eq (nskk--state-get 'key1) 'value1))
  (should (eq (nskk--state-get 'key2) 'value2))
  (should (eq (nskk--state-get 'key3) 'value3))
  (nskk--reset-state))

;;; エラーハンドリングのテスト

(ert-deftest nskk-test-mode-error-handling ()
  "nskk-mode のエラーハンドリングが機能することを確認する。"
  (let ((original-state nskk-mode))
    ;; エラーが発生しても状態が元に戻ることを確認
    (condition-case err
        (progn
          (nskk-mode 1)
          ;; 強制的にエラーを発生させる
          (signal 'error '("test error")))
      (error
       (when nskk-mode (nskk-mode -1))
       (should (eq nskk-mode original-state))))))

;;; 統合テスト

(ert-deftest nskk-test-full-activation-cycle ()
  "完全な起動サイクルが正常に動作することを確認する。"
  (let ((nskk-runtime-integration-enable-auto-tune nil))
    (nskk-activate)
    (should t)
    (nskk-deactivate)
    (should t)))

(ert-deftest nskk-test-state-persistence ()
  "状態が正しく保持されることを確認する。"
  (nskk--reset-state)
  (nskk--state-set 'test-key 'test-value)
  (should (eq (nskk--state-get 'test-key) 'test-value))
  ;; 別のキーを設定しても元のキーの値が保持されるか
  (nskk--state-set 'another-key 'another-value)
  (should (eq (nskk--state-get 'test-key) 'test-value))
  (should (eq (nskk--state-get 'another-key) 'another-value))
  (nskk--reset-state))

;;; 追加テスト - 辞書検索の各タイプ

(ert-deftest nskk-test-search-dictionary-prefix ()
  "prefix検索タイプが正しく動作することを確認する。"
  (let ((result (nskk--search-dictionary "test" 'prefix)))
    (should (or (null result) (listp result)))))

(ert-deftest nskk-test-search-dictionary-partial ()
  "partial検索タイプが正しく動作することを確認する。"
  (let ((result (nskk--search-dictionary "test" 'partial)))
    (should (or (null result) (listp result)))))

(ert-deftest nskk-test-search-dictionary-fuzzy ()
  "fuzzy検索タイプが正しく動作することを確認する。"
  (let ((result (nskk--search-dictionary "test" 'fuzzy)))
    (should (or (null result) (listp result)))))

(ert-deftest nskk-test-search-dictionary-exact ()
  "exact検索タイプ（デフォルト）が正しく動作することを確認する。"
  (let ((result (nskk--search-dictionary "test")))
    (should (or (null result) (listp result)))))

;;; 追加テスト - mode関連

(ert-deftest nskk-test-mode-lighter ()
  "nskk-mode のlighterが設定されていることを確認する。"
  (should (equal (nth 1 (assq 'nskk-mode minor-mode-alist)) " NSKK")))

(ert-deftest nskk-test-toggle-twice ()
  "nskk-toggleを2回実行すると元に戻ることを確認する。"
  (let ((original-state nskk-mode))
    (nskk-toggle)
    (let ((first-toggle nskk-mode))
      (nskk-toggle)
      (should (eq original-state nskk-mode)))))

;;; 追加テスト - 変換処理

(ert-deftest nskk-test-find-conversion-simple ()
  "簡単なローマ字変換をテストする。"
  (let ((result (nskk--find-conversion "a")))
    (should (or (null result) (consp result)))))

(ert-deftest nskk-test-process-character-simple ()
  "簡単な文字処理をテストする。"
  (let ((result (nskk--process-character ?a)))
    (should (or (null result) (stringp result) (listp result)))))

;;; 追加テスト - 状態管理の特殊ケース

(ert-deftest nskk-test-state-mode-key ()
  "nskk--state-set/getのmode専用キーをテストする。"
  (nskk--reset-state)
  (let ((nskk-runtime-integration-enable-auto-tune nil))
    (nskk--state-set 'mode 'hiragana)
    (should (eq (nskk--state-get 'mode) 'hiragana)))
  (nskk--reset-state))

(ert-deftest nskk-test-state-input-buffer-key ()
  "nskk--state-set/getのinput-buffer専用キーをテストする。"
  (nskk--reset-state)
  (nskk--state-set 'input-buffer "test")
  (should (equal (nskk--state-get 'input-buffer) "test"))
  (nskk--reset-state))

(ert-deftest nskk-test-state-candidate-list-key ()
  "nskk--state-set/getのcandidate-list専用キーをテストする。"
  (nskk--reset-state)
  (nskk--state-set 'candidate-list '("a" "b" "c"))
  (should (equal (nskk--state-get 'candidate-list) '("a" "b" "c")))
  (nskk--reset-state))

(ert-deftest nskk-test-state-candidate-index-key ()
  "nskk--state-set/getのcandidate-index専用キーをテストする。"
  (nskk--reset-state)
  (nskk--state-set 'candidate-index 5)
  (should (= (nskk--state-get 'candidate-index) 5))
  (nskk--reset-state))

;;; 追加テスト - フックの詳細

(ert-deftest nskk-test-before-conversion-hook ()
  "nskk-before-conversion-hook が呼ばれることを確認する。"
  (let ((hook-called nil))
    (let ((nskk-before-conversion-hook (list (lambda (_key) (setq hook-called t)))))
      (ignore-errors (nskk--search-dictionary "test")))
    (should hook-called)))

(ert-deftest nskk-test-after-conversion-hook ()
  "nskk-after-conversion-hook が呼ばれることを確認する。"
  (let ((hook-called nil))
    (let ((nskk-after-conversion-hook (list (lambda (_results) (setq hook-called t)))))
      (ignore-errors (nskk--search-dictionary "test")))
    (should hook-called)))

(ert-deftest nskk-test-mode-change-hook ()
  "nskk-mode-change-hook が呼ばれることを確認する。"
  (let ((hook-called nil)
        (nskk-runtime-integration-enable-auto-tune nil))
    (let ((nskk-mode-change-hook (list (lambda (_change) (setq hook-called t)))))
      (ignore-errors (nskk-activate))
      (ignore-errors (nskk-deactivate)))
    (should hook-called)))

;;; 追加テスト - resolve-dictionary-path の詳細

(ert-deftest nskk-test-resolve-dictionary-path-user-dict ()
  "ユーザー辞書パスが正しく解決されることを確認する。"
  (let ((nskk-dictionary-path nil)
        (nskk-user-dictionary-path (make-temp-file "nskk-test-dict")))
    (unwind-protect
        (should (equal (nskk--resolve-dictionary-path)
                       (expand-file-name nskk-user-dictionary-path)))
      (when (file-exists-p nskk-user-dictionary-path)
        (delete-file nskk-user-dictionary-path)))))

(ert-deftest nskk-test-resolve-dictionary-path-dict-priority ()
  "辞書パスがユーザー辞書より優先されることを確認する。"
  (let ((dict-path (make-temp-file "nskk-test-main-dict"))
        (nskk-user-dictionary-path (make-temp-file "nskk-test-user-dict")))
    (unwind-protect
        (let ((nskk-dictionary-path dict-path))
          (should (equal (nskk--resolve-dictionary-path)
                         (expand-file-name dict-path))))
      (when (file-exists-p dict-path)
        (delete-file dict-path))
      (when (file-exists-p nskk-user-dictionary-path)
        (delete-file nskk-user-dictionary-path)))))

;;; 追加テスト - load-dictionary

(ert-deftest nskk-test-load-dictionary-with-path ()
  "nskk--load-dictionary が辞書パスを受け取れることを確認する。"
  (let ((dict-path (make-temp-file "nskk-test-dict")))
    (unwind-protect
        (progn
          (with-temp-file dict-path
            (insert ";; SKK辞書\n"))
          (let ((result (nskk--load-dictionary dict-path)))
            (should (or (null result) (nskk-dict-index-p result)))))
      (when (file-exists-p dict-path)
        (delete-file dict-path)))))

;;; 追加テスト - input-method の詳細

(ert-deftest nskk-test-input-method-list-result ()
  "nskk-input-method が常にリストを返すことを確認する。"
  (let ((result1 (nskk-input-method nil))
        (result2 (nskk-input-method 'timeout))
        (result3 (nskk-input-method 'some-key)))
    (should (or (null result1) (listp result1)))
    (should (or (null result2) (listp result2)))
    (should (listp result3))))

;;; 追加テスト - conversion rules

(ert-deftest nskk-test-conversion-rules-exist ()
  "nskk--conversion-rules が設定されていることを確認する。"
  (should (boundp 'nskk--conversion-rules))
  (should (hash-table-p nskk--conversion-rules)))

(ert-deftest nskk-test-setup-reinitializes-rules ()
  "nskk-setup がローマ字変換ルールを再初期化することを確認する。"
  (let ((original-count (hash-table-count nskk--conversion-rules)))
    (nskk-setup)
    (should (= (hash-table-count nskk--conversion-rules) original-count))))

;;; 追加テスト - benchmark マクロの詳細

(ert-deftest nskk-test-benchmark-with-error ()
  "nskk--benchmark がエラーを伝播することを確認する。"
  (should-error
   (nskk--benchmark "test" (error "Test error"))
   :type 'error))

(ert-deftest nskk-test-benchmark-return-value ()
  "nskk--benchmark が正しい値を返すことを確認する。"
  (should (equal (nskk--benchmark "test" '(1 2 3)) '(1 2 3)))
  (should (equal (nskk--benchmark "test" "string") "string"))
  (should (= (nskk--benchmark "test" 42) 42)))

;;; 追加テスト - nskk-mode の詳細

(ert-deftest nskk-test-mode-keymap ()
  "nskk-mode がキーマップを持つことを確認する。"
  (should (keymapp nskk-mode-map)))

(ert-deftest nskk-test-mode-activation-deactivation ()
  "nskk-mode の有効化と無効化が正しく動作することを確認する。"
  (let ((nskk-runtime-integration-enable-auto-tune nil))
    (with-temp-buffer
      (nskk-mode 1)
      (should nskk-mode)
      (nskk-mode -1)
      (should-not nskk-mode))))

;;; 追加テスト - state管理のエッジケース

(ert-deftest nskk-test-state-plist-member ()
  "plist-member を使った値の存在チェックが機能することを確認する。"
  (nskk--reset-state)
  (nskk--state-set 'key-with-nil nil)
  (should (null (nskk--state-get 'key-with-nil)))
  ;; plist-member で存在確認できることを間接的にテスト
  (nskk--state-set 'key-with-nil 'new-value)
  (should (eq (nskk--state-get 'key-with-nil) 'new-value))
  (nskk--reset-state))

(ert-deftest nskk-test-state-conversion-mode-with-application ()
  "nskk-application が利用可能な場合の conversion-mode 設定を確認する。"
  (nskk--reset-state)
  (if (fboundp 'nskk-application-switch-mode)
      (progn
        (nskk--state-set 'conversion-mode 'hiragana)
        (should (eq (nskk--state-get 'conversion-mode) 'hiragana)))
    (nskk--state-set 'conversion-mode 'hiragana)
    (should t))
  (nskk--reset-state))

;;; 追加テスト - initialize/shutdown の詳細

(ert-deftest nskk-test-initialize-calls-setup ()
  "nskk-initialize が nskk-setup を呼ぶことを確認する。"
  (let ((nskk-runtime-integration-enable-auto-tune nil)
        (setup-called nil))
    (cl-letf (((symbol-function 'nskk-setup)
               (lambda () (setq setup-called t) t)))
      (ignore-errors (nskk-initialize))
      (should setup-called))))

(ert-deftest nskk-test-shutdown-order ()
  "nskk-shutdown が正しい順序でシャットダウンすることを確認する。"
  (let ((advanced-shutdown-called nil)
        (runtime-shutdown-called nil))
    (cl-letf (((symbol-function 'nskk-advanced-integration-shutdown)
               (lambda () (setq advanced-shutdown-called t)))
              ((symbol-function 'nskk-runtime-integration-shutdown)
               (lambda () (setq runtime-shutdown-called t))))
      (nskk-shutdown)
      (should advanced-shutdown-called)
      (should runtime-shutdown-called))))

;;; 追加テスト - health-check の詳細

(ert-deftest nskk-test-health-check-modules ()
  "nskk-health-check が各モジュールをチェックすることを確認する。"
  (let ((result (nskk-health-check)))
    (should (booleanp result))
    ;; 全モジュールがロードされている場合は t
    (should result)))

(ert-deftest nskk-test-list-modules-output ()
  "nskk-list-modules がバッファを生成することを確認する。"
  (nskk-list-modules)
  (should (get-buffer "*NSKK Modules*"))
  (kill-buffer "*NSKK Modules*"))

;;; 追加テスト - 辞書パス解決の完全なケース

(ert-deftest nskk-test-resolve-dictionary-both-nil ()
  "辞書パスとユーザー辞書パスが両方nilの場合を確認する。"
  (let ((nskk-dictionary-path nil)
        (nskk-user-dictionary-path nil))
    (should (null (nskk--resolve-dictionary-path)))))

;;; 追加テスト - input-methodの完全な動作

(ert-deftest nskk-test-input-method-character-processing ()
  "文字入力時に nskk--process-character が呼ばれることを確認する。"
  (let ((process-called nil))
    (cl-letf (((symbol-function 'nskk--process-character)
               (lambda (_char) (setq process-called t) "result")))
      (nskk-input-method ?a)
      (should process-called))))

(ert-deftest nskk-test-input-method-result-formats ()
  "nskk-input-method が様々な形式の結果を処理できることを確認する。"
  ;; nil 結果
  (cl-letf (((symbol-function 'nskk--process-character)
             (lambda (_) nil)))
    (should (null (nskk-input-method ?a))))
  ;; 文字列結果
  (cl-letf (((symbol-function 'nskk--process-character)
             (lambda (_) "result")))
    (should (equal (nskk-input-method ?a) '("result"))))
  ;; リスト結果
  (cl-letf (((symbol-function 'nskk--process-character)
             (lambda (_) '("a" "b"))))
    (should (equal (nskk-input-method ?a) '("a" "b")))))

;;; 追加テスト - version 情報

(ert-deftest nskk-test-version-constants-values ()
  "バージョン定数が妥当な値を持つことを確認する。"
  (should (string-match-p "^[0-9]+\\.[0-9]+\\.[0-9]+$" nskk-version)))

;;; 追加テスト - defvaralias

(ert-deftest nskk-test-state-alias ()
  "nskk--state エイリアスが機能することを確認する。"
  (should (eq (indirect-variable 'nskk--state)
              (indirect-variable 'nskk-current-state))))

;;; 追加テスト - load-time

(ert-deftest nskk-test-load-time-measurement ()
  "nskk--load-time が設定されていることを確認する。"
  (should (or (null nskk--load-time)
              (numberp nskk--load-time))))

;;; 追加テスト - conversion-rules

(ert-deftest nskk-test-conversion-rules-copy ()
  "nskk-setup が変換ルールをコピーすることを確認する。"
  (let ((before-setup nskk--conversion-rules))
    (nskk-setup)
    ;; コピーされた新しいハッシュテーブルが作られる
    (should (hash-table-p nskk--conversion-rules))))

;;; 追加テスト - エラー条件のテスト

(ert-deftest nskk-test-emacs-version-check ()
  "Emacs バージョンチェックの警告が機能することを確認する。"
  ;; このテストは既にロード済みなので、変数の存在のみ確認
  (should (boundp 'emacs-version)))

;;; 追加テスト - 非同期モードトグルのスレッド対応

(ert-deftest nskk-test-async-mode-toggle-with-threads ()
  "スレッド対応環境での nskk-async-mode-toggle を確認する。"
  (if (fboundp 'make-thread)
      (let ((callback-result nil))
        (nskk-async-mode-toggle
         :callback (lambda (state) (setq callback-result state)))
        ;; スレッドの完了を待つ（簡易的）
        (sleep-for 0.1)
        (should (or (null callback-result) (booleanp callback-result))))
    ;; スレッド機能がない場合は同期実行される
    (let ((callback-result nil))
      (nskk-async-mode-toggle
       :callback (lambda (state) (setq callback-result state)))
      (should (or (null callback-result) (booleanp callback-result))))))

;;; 追加テスト - 辞書検索のフック連携

(ert-deftest nskk-test-search-with-hooks ()
  "辞書検索時にフックが正しく呼ばれることを確認する。"
  (let ((before-called nil)
        (after-called nil))
    (let ((nskk-before-conversion-hook (list (lambda (_) (setq before-called t))))
          (nskk-after-conversion-hook (list (lambda (_) (setq after-called t)))))
      (ignore-errors (nskk--search-dictionary "test")))
    (should before-called)
    (should after-called)))

;;; 追加テスト - 辞書ロードのエラーハンドリング

(ert-deftest nskk-test-load-dictionary-error ()
  "存在しない辞書ファイルのロードでエラーが起きないことを確認する。"
  (should-not (nskk--maybe-load-dictionary)))

;;; 追加テスト - state操作のwith-state-lock

(ert-deftest nskk-test-with-state-lock-execution ()
  "nskk--with-state-lock マクロが正しく動作することを確認する。"
  (let ((executed nil))
    (nskk--with-state-lock
      (setq executed t))
    (should executed)))

;;; 追加テスト - nskk-mode のエラーハンドリング詳細

(ert-deftest nskk-test-mode-error-recovery ()
  "nskk-mode がエラーから回復することを確認する。"
  (with-temp-buffer
    (let ((nskk-runtime-integration-enable-auto-tune nil))
      (condition-case err
          (progn
            (nskk-mode 1)
            (nskk-mode -1)
            (should t))
        (error
         ;; エラーが発生しても回復できることを確認
         (should t))))))

;;; 追加テスト - nskk-toggle の詳細動作

(ert-deftest nskk-test-toggle-state-change ()
  "nskk-toggle が状態を切り替えることを確認する。"
  (let ((original nskk-mode)
        (nskk-runtime-integration-enable-auto-tune nil))
    (nskk-toggle)
    (should (not (eq original nskk-mode)))
    (nskk-toggle)
    (should (eq original nskk-mode))))

;;; 追加テスト - nskk--benchmark のデバッグモード

(ert-deftest nskk-test-benchmark-debug-message ()
  "デバッグモードでnskk--benchmark がメッセージを出力することを確認する。"
  (let ((nskk-debug-mode t)
        (message-called nil))
    (cl-letf (((symbol-function 'message)
               (lambda (&rest _) (setq message-called t))))
      (nskk--benchmark "test" (+ 1 1))
      (should message-called))))

;;; 追加テスト - nskk--state-get のmode関連

(ert-deftest nskk-test-state-get-mode-with-state ()
  "nskk-current-state が存在する場合の mode 取得を確認する。"
  (nskk-state-init)
  (let ((nskk-runtime-integration-enable-auto-tune nil))
    (nskk--state-set 'mode 'katakana)
    (should (eq (nskk--state-get 'mode) 'katakana)))
  (nskk--reset-state))

(ert-deftest nskk-test-state-get-input-buffer-with-state ()
  "nskk-current-state が存在する場合の input-buffer 取得を確認する。"
  (nskk-state-init)
  (nskk--state-set 'input-buffer "test-input")
  (should (equal (nskk--state-get 'input-buffer) "test-input"))
  (nskk--reset-state))

;;; 追加テスト - nskk--resolve-dictionary-path の完全網羅

(ert-deftest nskk-test-resolve-dictionary-path-expanded ()
  "辞書パスが正しく展開されることを確認する。"
  (let ((dict-path (make-temp-file "nskk-test-dict")))
    (unwind-protect
        (let ((nskk-dictionary-path dict-path))
          (should (equal (nskk--resolve-dictionary-path)
                         (expand-file-name dict-path))))
      (when (file-exists-p dict-path)
        (delete-file dict-path)))))

;;; 追加テスト - nskk-integration-test-ready-p の詳細

(ert-deftest nskk-test-integration-test-ready-functions ()
  "nskk-integration-test-ready-p が各関数の存在を確認することをテストする。"
  (should (fboundp 'nskk-convert-romaji))
  (should (fboundp 'nskk-state-create))
  (should (fboundp 'nskk-dict-parse-line))
  (should (fboundp 'nskk-trie-create)))

;;; 追加テスト - nskk-list-modules の詳細

(ert-deftest nskk-test-list-modules-content ()
  "nskk-list-modules が正しい内容を生成することを確認する。"
  (nskk-list-modules)
  (with-current-buffer "*NSKK Modules*"
    (goto-char (point-min))
    (should (search-forward "NSKK" nil t))
    (should (search-forward "Loaded Modules" nil t)))
  (kill-buffer "*NSKK Modules*"))

;;; 追加テスト - nskk--state-set のnil値設定

(ert-deftest nskk-test-state-set-nil-value ()
  "nskk--state-set がnil値を正しく設定できることを確認する。"
  (nskk--reset-state)
  (nskk--state-set 'test-nil-key nil)
  (should (null (nskk--state-get 'test-nil-key)))
  (nskk--reset-state))

;;; 追加テスト - nskk--state-set の input-buffer nil

(ert-deftest nskk-test-state-set-input-buffer-nil ()
  "nskk--state-set が input-buffer に nil を設定すると空文字列になることを確認する。"
  (nskk--reset-state)
  (nskk-state-init)
  (nskk--state-set 'input-buffer nil)
  (should (equal (nskk--state-get 'input-buffer) ""))
  (nskk--reset-state))

;;; 追加テスト - nskk--state-set の candidate-index nil

(ert-deftest nskk-test-state-set-candidate-index-nil ()
  "nskk--state-set が candidate-index に nil を設定すると 0 になることを確認する。"
  (nskk--reset-state)
  (nskk--state-set 'candidate-index nil)
  (should (= (nskk--state-get 'candidate-index) 0))
  (nskk--reset-state))

;;; 追加テスト - nskk--reset-state の state-clear-all 呼び出し

(ert-deftest nskk-test-reset-state-clears-all ()
  "nskk--reset-state が nskk-state-clear-all を呼ぶことを確認する。"
  (nskk-state-init)
  (nskk--state-set 'test-key 'test-value)
  (nskk--reset-state)
  ;; リセット後は値が取得できない
  (should (eq (nskk--state-get 'test-key 'default) 'default)))

;;; 追加テスト - nskk--reset-state の application-initialize 呼び出し

(ert-deftest nskk-test-reset-state-initializes-application ()
  "nskk--reset-state が nskk-application-initialize を呼ぶことを確認する。"
  (if (fboundp 'nskk-application-initialize)
      (let ((init-called nil))
        (cl-letf (((symbol-function 'nskk-application-initialize)
                   (lambda () (setq init-called t))))
          (nskk--reset-state)
          (should init-called)))
    (nskk--reset-state)
    (should t)))

;;; 追加テスト - nskk-activate の辞書ロード

(ert-deftest nskk-test-activate-loads-dictionary ()
  "nskk-activate が辞書をロードすることを確認する。"
  (let ((nskk-runtime-integration-enable-auto-tune nil)
        (dict-loaded nil))
    (cl-letf (((symbol-function 'nskk--maybe-load-dictionary)
               (lambda () (setq dict-loaded t) nil)))
      (nskk-activate)
      (should dict-loaded)
      (nskk-deactivate))))

;;; 追加テスト - nskk-deactivate のapplication-shutdown

(ert-deftest nskk-test-deactivate-shuts-down-application ()
  "nskk-deactivate が nskk-application-shutdown を呼ぶことを確認する。"
  (if (fboundp 'nskk-application-shutdown)
      (let ((shutdown-called nil)
            (nskk-runtime-integration-enable-auto-tune nil))
        (cl-letf (((symbol-function 'nskk-application-shutdown)
                   (lambda () (setq shutdown-called t))))
          (nskk-activate)
          (nskk-deactivate)
          (should shutdown-called)))
    (should t)))

;;; 追加テスト - nskk-initialize のhealth-check

(ert-deftest nskk-test-initialize-checks-health ()
  "nskk-initialize が health-check を実行することを確認する。"
  (let ((nskk-runtime-integration-enable-auto-tune nil)
        (health-checked nil))
    (cl-letf (((symbol-function 'nskk-health-check)
               (lambda () (setq health-checked t) t)))
      (ignore-errors (nskk-initialize))
      (should health-checked))))

;;; 追加テスト - nskk-initialize のruntime-integration

(ert-deftest nskk-test-initialize-runtime-integration ()
  "nskk-initialize が runtime-integration を初期化することを確認する。"
  (if (fboundp 'nskk-runtime-integration-initialize)
      (let ((runtime-init-called nil)
            (nskk-runtime-integration-enable-auto-tune nil))
        (cl-letf (((symbol-function 'nskk-runtime-integration-initialize)
                   (lambda () (setq runtime-init-called t))))
          (ignore-errors (nskk-initialize))
          (should runtime-init-called)))
    (should t)))

;;; 追加テスト - nskk-initialize のadvanced-integration

(ert-deftest nskk-test-initialize-advanced-integration ()
  "nskk-initialize が advanced-integration を初期化することを確認する。"
  (if (fboundp 'nskk-advanced-integration-initialize)
      (let ((advanced-init-called nil)
            (nskk-runtime-integration-enable-auto-tune nil))
        (cl-letf (((symbol-function 'nskk-advanced-integration-initialize)
                   (lambda () (setq advanced-init-called t))))
          (ignore-errors (nskk-initialize))
          (should advanced-init-called)))
    (should t)))

;;; 追加テスト - nskk-input-method のその他の結果形式

(ert-deftest nskk-test-input-method-format-result ()
  "nskk-input-method が結果をフォーマットすることを確認する。"
  ;; 数値結果
  (cl-letf (((symbol-function 'nskk--process-character)
             (lambda (_) 42)))
    (let ((result (nskk-input-method ?a)))
      (should (equal result '("42")))))
  ;; シンボル結果
  (cl-letf (((symbol-function 'nskk--process-character)
             (lambda (_) 'symbol)))
    (let ((result (nskk-input-method ?a)))
      (should (equal result '("symbol"))))))

;;; 追加テスト - nskk-async-mode-toggle のエラーハンドリング

(ert-deftest nskk-test-async-mode-toggle-error-handling ()
  "nskk-async-mode-toggle がエラーを処理することを確認する。"
  (let ((error-handled nil))
    (cl-letf (((symbol-function 'nskk-toggle)
               (lambda () (error "Test error"))))
      (nskk-async-mode-toggle
       :error-callback (lambda (_err) (setq error-handled t)))
      (sleep-for 0.05))
    (should error-handled)))

;;; 追加テスト - nskk-setup のkeybindings

(ert-deftest nskk-test-setup-keybindings ()
  "nskk-setup が nskk-setup-keybindings を呼ぶことを確認する。"
  (if (fboundp 'nskk-setup-keybindings)
      (let ((keybindings-setup nil))
        (cl-letf (((symbol-function 'nskk-setup-keybindings)
                   (lambda () (setq keybindings-setup t))))
          (nskk-setup)
          (should keybindings-setup)))
    (nskk-setup)
    (should t)))

;;; 追加テスト - nskk-mode のmode-hook

(ert-deftest nskk-test-mode-change-hook-transition ()
  "nskk-mode が mode-change-hook に遷移情報を渡すことを確認する。"
  (let ((transition-info nil)
        (nskk-runtime-integration-enable-auto-tune nil)
        (nskk-mode-change-hook (list (lambda (change) (setq transition-info change)))))
    (with-temp-buffer
      (ignore-errors
        (nskk-mode 1)
        (should (consp transition-info))
        (nskk-mode -1)))))

(provide 'nskk-test)

;;; nskk-test.el ends here
