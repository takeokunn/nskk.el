;;; nskk-test-macros.el --- Advanced test macros for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, testing
;; Version: 0.1.0
;; Package-Requires: ((emacs "31.0"))

;; This file is part of NSKK.

;;; Commentary:

;; このファイルはNSKKの高度なテストマクロを実装します。
;;
;; 特徴:
;; - プロパティベーステスト（Property-Based Testing）
;; - パフォーマンステスト（Benchmarking）
;; - 統合テスト（Integration Testing）
;; - パラメータ化テスト（Parameterized Tests）
;;
;; 使用例:
;;
;; ;; プロパティテスト
;; (nskk-property-test reversible-conversion
;;   "ひらがな↔カタカナ変換の可逆性"
;;   :iterations 100
;;   :property (lambda ()
;;               (let ((input (nskk-test-random-hiragana 10)))
;;                 (equal input
;;                        (nskk-katakana-to-hiragana
;;                         (nskk-hiragana-to-katakana input))))))
;;
;; ;; パフォーマンステスト
;; (nskk-performance-test fast-lookup
;;   "辞書検索は50ms以内"
;;   :max-time 0.050
;;   :iterations 10
;;   (nskk-dictionary-lookup "あい"))

;;; Code:

(require 'nskk-test-framework)
(require 'cl-lib)

;;; カスタマイズ変数

(defcustom nskk-test-property-default-iterations 100
  "プロパティテストのデフォルト反復回数。"
  :type 'integer
  :group 'nskk-test)

(defcustom nskk-test-performance-warmup-iterations 3
  "パフォーマンステストのウォームアップ反復回数。"
  :type 'integer
  :group 'nskk-test)

(defcustom nskk-test-integration-timeout 10.0
  "統合テストのタイムアウト時間（秒）。"
  :type 'float
  :group 'nskk-test)

;;; 内部変数

(defvar nskk-test--property-failures nil
  "プロパティテストの失敗ケースリスト。")

(defvar nskk-test--performance-results nil
  "パフォーマンステストの結果リスト。")

;;; プロパティベーステスト

(defmacro nskk-property-test (name description &rest args)
  "プロパティベーステストを定義する。

NAME: テスト名（シンボル）
DESCRIPTION: テストの説明（文字列）
ARGS: キーワード引数とテスト本体
  :iterations N    - 反復回数（デフォルト: 100）
  :property FORM   - テストするプロパティ（ラムダ式）
  :generator FORM  - テストデータ生成関数（省略可）
  :tags TAGS       - テストタグ

使用例:
  (nskk-property-test my-prop
    \"String concatenation is associative\"
    :iterations 50
    :property (lambda ()
                (let ((a (nskk-test-random-string 5))
                      (b (nskk-test-random-string 5))
                      (c (nskk-test-random-string 5)))
                  (equal (concat (concat a b) c)
                         (concat a (concat b c))))))"
  (declare (indent 2) (debug (symbolp stringp &rest form)))
  (let ((iterations nskk-test-property-default-iterations)
        (property nil)
        (generator nil)
        (tags '(:property))
        (remaining args))

    ;; キーワード引数を解析
    (while (keywordp (car remaining))
      (pcase (pop remaining)
        (:iterations (setq iterations (pop remaining)))
        (:property (setq property (pop remaining)))
        (:generator (setq generator (pop remaining)))
        (:tags (setq tags (append tags (pop remaining))))
        (other (error "Unknown keyword: %s" other))))

    (unless property
      (error "Property test requires :property argument"))

    `(nskk-deftest ,name
       ,description
       :tags ',tags
       (setq nskk-test--property-failures nil)
       (let ((success-count 0)
             (failure-count 0))
         (dotimes (i ,iterations)
           (condition-case err
               (let ((result ,(if generator
                                 `(funcall ,property (funcall ,generator))
                               `(funcall ,property))))
                 (if result
                     (setq success-count (1+ success-count))
                   (setq failure-count (1+ failure-count))
                   (push (list :iteration i :result nil) nskk-test--property-failures)))
             (error
              (setq failure-count (1+ failure-count))
              (push (list :iteration i :error err) nskk-test--property-failures))))

         (when nskk-test-verbose
           (message "Property test %s: %d/%d passed"
                   ',name success-count ,iterations))

         (when (> failure-count 0)
           (ert-fail (list 'property-test-failed
                          :test ',name
                          :iterations ,iterations
                          :failures failure-count
                          :first-failure (car (reverse nskk-test--property-failures)))))

         (should (= failure-count 0))))))

;;; パフォーマンステスト

(defmacro nskk-performance-test (name description &rest args)
  "パフォーマンステストを定義する。

NAME: テスト名（シンボル）
DESCRIPTION: テストの説明（文字列）
ARGS: キーワード引数とテスト本体
  :max-time SECONDS  - 最大許容時間（秒）
  :iterations N      - 測定反復回数（デフォルト: 10）
  :warmup N          - ウォームアップ反復回数（デフォルト: 3）
  :tags TAGS         - テストタグ

使用例:
  (nskk-performance-test fast-conversion
    \"Conversion should be fast\"
    :max-time 0.001
    :iterations 100
    (nskk-romaji-to-hiragana \"aiueo\"))"
  (declare (indent 2) (debug (symbolp stringp &rest form)))
  (let ((max-time nil)
        (iterations 10)
        (warmup nskk-test-performance-warmup-iterations)
        (tags '(:performance))
        (body nil)
        (remaining args))

    ;; キーワード引数を解析
    (while (and remaining (keywordp (car remaining)))
      (pcase (pop remaining)
        (:max-time (setq max-time (pop remaining)))
        (:iterations (setq iterations (pop remaining)))
        (:warmup (setq warmup (pop remaining)))
        (:tags (setq tags (append tags (pop remaining))))
        (other (error "Unknown keyword: %s" other))))

    (setq body remaining)

    (unless max-time
      (error "Performance test requires :max-time argument"))

    `(nskk-deftest ,name
       ,description
       :tags ',tags
       ;; ウォームアップ
       (dotimes (_ ,warmup)
         ,@body)

       ;; 実測定
       (let ((times nil))
         (dotimes (_ ,iterations)
           (let ((start (current-time)))
             ,@body
             (push (float-time (time-since start)) times)))

         (let* ((avg-time (/ (apply #'+ times) (float ,iterations)))
                (min-time (apply #'min times))
                (max-time-measured (apply #'max times)))

           (when nskk-test-verbose
             (message "Performance test %s: avg=%.6fs min=%.6fs max=%.6fs"
                     ',name avg-time min-time max-time-measured))

           (push (list :test ',name
                      :avg avg-time
                      :min min-time
                      :max max-time-measured
                      :threshold ,max-time)
                 nskk-test--performance-results)

           (should (<= avg-time ,max-time))
           (when (> avg-time ,max-time)
             (ert-fail (list 'performance-test-failed
                            :test ',name
                            :expected ,max-time
                            :actual avg-time
                            :slowdown-factor (/ avg-time ,max-time)))))))))

;;; 統合テスト

(defmacro nskk-integration-test (name description &rest args)
  "統合テストを定義する。

NAME: テスト名（シンボル）
DESCRIPTION: テストの説明（文字列）
ARGS: キーワード引数とテスト本体
  :setup FORM      - セットアップコード
  :teardown FORM   - クリーンアップコード
  :timeout SECONDS - タイムアウト時間
  :tags TAGS       - テストタグ

使用例:
  (nskk-integration-test full-conversion-flow
    \"Test complete conversion workflow\"
    :setup (nskk-mode 1)
    :teardown (nskk-mode -1)
    :timeout 5.0
    (insert \"aiueo\")
    (should (equal (buffer-string) \"あいうえお\")))"
  (declare (indent 2) (debug (symbolp stringp &rest form)))
  (let ((setup nil)
        (teardown nil)
        (timeout nskk-test-integration-timeout)
        (tags '(:integration))
        (body nil)
        (remaining args))

    ;; キーワード引数を解析
    (while (and remaining (keywordp (car remaining)))
      (pcase (pop remaining)
        (:setup (setq setup (pop remaining)))
        (:teardown (setq teardown (pop remaining)))
        (:timeout (setq timeout (pop remaining)))
        (:tags (setq tags (append tags (pop remaining))))
        (other (error "Unknown keyword: %s" other))))

    (setq body remaining)

    `(nskk-deftest ,name
       ,description
       :tags ',tags
       (nskk-test-with-temp-buffer
         (let ((timeout-timer nil)
               (timed-out nil))
           (unwind-protect
               (progn
                 ;; タイムアウトタイマー設定
                 (setq timeout-timer
                       (run-at-time ,timeout nil
                                   (lambda ()
                                     (setq timed-out t))))

                 ;; セットアップ
                 ,setup

                 ;; テスト本体
                 ,@body

                 ;; タイムアウトチェック
                 (when timed-out
                   (ert-fail (list 'integration-test-timeout
                                  :test ',name
                                  :timeout ,timeout))))

             ;; クリーンアップ
             (when timeout-timer
               (cancel-timer timeout-timer))
             ,teardown))))))

;;; パラメータ化テスト

(defmacro nskk-parameterized-test (name description params &rest body)
  "パラメータ化テストを定義する。

NAME: テスト名ベース（シンボル）
DESCRIPTION: テストの説明（文字列）
PARAMS: パラメータリスト（各要素は (param-name . param-values)）
BODY: テスト本体（各パラメータにアクセス可能）

使用例:
  (nskk-parameterized-test conversion-test
    \"Test various conversions\"
    ((input . (\"a\" \"i\" \"u\"))
     (expected . (\"あ\" \"い\" \"う\")))
    (should (equal (nskk-romaji-to-hiragana input) expected)))"
  (declare (indent 3) (debug (symbolp stringp sexp &rest form)))
  (let ((param-names (mapcar #'car params))
        (param-values (mapcar #'cdr params))
        (test-forms nil))

    ;; 全組み合わせを生成
    (cl-labels ((generate-combinations (names values)
                  (if (null names)
                      (list nil)
                    (let ((rest-combinations
                           (generate-combinations (cdr names) (cdr values))))
                      (cl-mapcan
                       (lambda (value)
                         (mapcar (lambda (rest)
                                   (cons (cons (car names) value) rest))
                                 rest-combinations))
                       (car values))))))

      (let ((combinations (generate-combinations param-names param-values))
            (index 0))
        (dolist (combination combinations)
          (let ((test-name (intern (format "%s-%d" name index)))
                (bindings (cl-loop for (name . value) in combination
                                  collect `(,name ',value))))
            (push `(nskk-deftest ,test-name
                     ,(format "%s [%d]" description index)
                     :tags '(:parameterized)
                     (let ,bindings
                       ,@body))
                  test-forms)
            (setq index (1+ index))))))

    `(progn ,@(reverse test-forms))))

;;; テストスイート

(defmacro nskk-test-suite (name &rest tests)
  "テストスイートを定義する。

NAME: スイート名（シンボル）
TESTS: テスト名のリスト

使用例:
  (nskk-test-suite conversion-suite
    test-romaji-to-hiragana
    test-hiragana-to-katakana)"
  (declare (indent 1) (debug (symbolp &rest symbolp)))
  `(defun ,(intern (format "%s-run" name)) ()
     ,(format "Run all tests in %s suite" name)
     (interactive)
     (ert '(member ,@tests))))

;;; ヘルパー関数

(defun nskk-test-clear-property-failures ()
  "プロパティテストの失敗ケースをクリアする。"
  (interactive)
  (setq nskk-test--property-failures nil))

(defun nskk-test-show-property-failures ()
  "プロパティテストの失敗ケースを表示する。"
  (interactive)
  (if nskk-test--property-failures
      (with-current-buffer (get-buffer-create "*NSKK Property Test Failures*")
        (erase-buffer)
        (insert "=== Property Test Failures ===\n\n")
        (dolist (failure (reverse nskk-test--property-failures))
          (insert (format "Iteration %d:\n" (plist-get failure :iteration)))
          (when (plist-get failure :error)
            (insert (format "  Error: %s\n" (plist-get failure :error))))
          (when (plist-get failure :result)
            (insert (format "  Result: %s\n" (plist-get failure :result))))
          (insert "\n"))
        (display-buffer (current-buffer)))
    (message "No property test failures recorded")))

(defun nskk-test-show-performance-report ()
  "パフォーマンステストの結果レポートを表示する。"
  (interactive)
  (if nskk-test--performance-results
      (with-current-buffer (get-buffer-create "*NSKK Performance Report*")
        (erase-buffer)
        (insert "=== Performance Test Results ===\n\n")
        (insert (format "%-40s %10s %10s %10s %10s\n"
                       "Test" "Avg (s)" "Min (s)" "Max (s)" "Threshold"))
        (insert (make-string 80 ?-) "\n")
        (dolist (result (reverse nskk-test--performance-results))
          (insert (format "%-40s %10.6f %10.6f %10.6f %10.6f %s\n"
                         (plist-get result :test)
                         (plist-get result :avg)
                         (plist-get result :min)
                         (plist-get result :max)
                         (plist-get result :threshold)
                         (if (<= (plist-get result :avg)
                                (plist-get result :threshold))
                             "✓" "✗"))))
        (display-buffer (current-buffer)))
    (message "No performance test results recorded")))

(defun nskk-test-clear-performance-results ()
  "パフォーマンステストの結果をクリアする。"
  (interactive)
  (setq nskk-test--performance-results nil))

(provide 'nskk-test-macros)

;;; nskk-test-macros.el ends here
