;;; nskk-test-fixtures.el --- Test fixtures and mocks for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, testing
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0"))

;; This file is part of NSKK.

;;; Commentary:

;; このファイルはNSKKのテストフィクスチャとモックシステムを実装します。
;;
;; 特徴:
;; - テストデータファクトリー
;; - モックオブジェクト生成
;; - スタブ関数システム
;; - フィクスチャ管理
;; - 呼び出し記録・検証
;;
;; 使用例:
;;
;; ;; テストデータ生成
;; (nskk-fixture-create-state :mode 'hiragana)
;; (nskk-fixture-create-dict-entry "あい" '("愛" "哀"))
;;
;; ;; モック関数
;; (nskk-with-mock-function 'nskk-dictionary-lookup
;;   (lambda (key) '("テスト"))
;;   (should (equal (nskk-dictionary-lookup "test") '("テスト"))))
;;
;; ;; スタブ関数
;; (nskk-with-stub 'file-exists-p t
;;   (should (file-exists-p "nonexistent.txt")))

;;; Code:

(require 'nskk-test-framework)
(require 'cl-lib)

;;; カスタマイズ変数

(defcustom nskk-fixture-data-dir
  (expand-file-name "fixtures/" (file-name-directory load-file-name))
  "テストフィクスチャデータのディレクトリ。"
  :type 'directory
  :group 'nskk-test)

;;; 内部変数

(defvar nskk-fixture--mock-calls nil
  "モック関数の呼び出し記録。
形式: ((function-name args result) ...)")

(defvar nskk-fixture--active-mocks nil
  "現在アクティブなモックのリスト。")

(defvar nskk-fixture--recording-p nil
  "モック呼び出し記録中かどうかのフラグ。
再帰呼び出しを防ぐために使用。")

;;; テストデータファクトリー

(defun nskk-fixture-create-state (&rest args)
  "テスト用の状態オブジェクトを生成する。

ARGS: キーワード引数
  :mode MODE           - 入力モード
  :input-buffer STR    - 入力バッファ
  :candidates LIST     - 候補リスト
  :candidate-index N   - 候補インデックス

戻り値: 状態オブジェクト（plist）"
  (let ((mode (or (plist-get args :mode) 'hiragana))
        (input-buffer (or (plist-get args :input-buffer) ""))
        (conversion-buffer (or (plist-get args :conversion-buffer) ""))
        (candidates (plist-get args :candidates))
        (candidate-index (or (plist-get args :candidate-index) 0)))
    (list :mode mode
          :input-buffer input-buffer
          :conversion-buffer conversion-buffer
          :candidates candidates
          :candidate-index candidate-index
          :timestamp (current-time))))

(defun nskk-fixture-create-dict-entry (key &optional values)
  "テスト用の辞書エントリを生成する。

KEY: 見出し語
VALUES: 候補リスト（省略時はランダム生成）

戻り値: (key . values) の cons セル"
  (cons key
        (or values
            (list (nskk-test-random-hiragana 3)
                  (nskk-test-random-hiragana 3)))))

(defun nskk-fixture-create-dict-entries (count)
  "COUNT 個の辞書エントリを生成する。

戻り値: エントリのリスト"
  (cl-loop repeat count
           collect (nskk-fixture-create-dict-entry
                   (nskk-test-random-hiragana 2))))

(defun nskk-fixture-create-candidate-list (&optional count)
  "候補リストを生成する。

COUNT: 候補数（デフォルト: 3-10のランダム）

戻り値: 候補の文字列リスト"
  (let ((n (or count (+ 3 (random 8)))))
    (cl-loop repeat n
             collect (nskk-test-random-hiragana 2))))

(defun nskk-fixture-create-buffer-content (&optional mode)
  "テスト用のバッファ内容を生成する。

MODE: 内容の種類
  'empty    - 空
  'simple   - 簡単なテキスト
  'complex  - 複雑なテキスト
  'japanese - 日本語テキスト

戻り値: 文字列"
  (pcase (or mode 'simple)
    ('empty "")
    ('simple "test content")
    ('complex "Line 1\nLine 2\nLine 3")
    ('japanese (concat (nskk-test-random-hiragana 5) "\n"
                      (nskk-test-random-katakana 5)))
    (_ "default content")))

;;; モックオブジェクト

(cl-defstruct (nskk-mock
               (:constructor nskk-mock--create)
               (:copier nil))
  "モックオブジェクト。

スロット:
  name        - 関数名
  behavior    - モック動作（関数）
  calls       - 呼び出し記録
  call-count  - 呼び出し回数"
  (name nil :type symbol)
  (behavior nil :type function)
  (calls nil :type list)
  (call-count 0 :type integer))

(defun nskk-fixture-create-mock (name behavior)
  "モックオブジェクトを作成する。

NAME: モック対象の関数名
BEHAVIOR: モック動作を定義する関数

戻り値: モックオブジェクト"
  (nskk-mock--create
   :name name
   :behavior behavior
   :calls nil
   :call-count 0))

(defun nskk-fixture-mock-record-call (mock args result)
  "モック呼び出しを記録する。

MOCK: モックオブジェクト
ARGS: 呼び出し引数
RESULT: 戻り値"
  (push (list :args args :result result :time (current-time))
        (nskk-mock-calls mock))
  (setf (nskk-mock-call-count mock) (1+ (nskk-mock-call-count mock)))
  (push (list (nskk-mock-name mock) args result)
        nskk-fixture--mock-calls))

;;; モック関数マクロ

(defmacro nskk-with-mock-function (function mock-fn &rest body)
  "FUNCTION をモック関数 MOCK-FN で置き換えて BODY を実行する。

FUNCTION: モック対象の関数名（シンボル）
MOCK-FN: モック動作を定義する関数
BODY: テスト本体

使用例:
  (nskk-with-mock-function 'some-function
    (lambda (&rest args) \"mocked\")
    (should (equal (some-function 1 2) \"mocked\")))"
  (declare (indent 2) (debug (symbolp form body)))
  (let ((mock-var (make-symbol "mock"))
        (original-var (make-symbol "original")))
    `(let ((,mock-var (nskk-fixture-create-mock ,function ,mock-fn))
           (,original-var (symbol-function ,function)))
       (unwind-protect
           (progn
             (push ,function nskk-fixture--active-mocks)
             (fset ,function
                   (lambda (&rest args)
                     (let ((result (apply (nskk-mock-behavior ,mock-var) args)))
                       ;; 再帰呼び出しを防ぐグローバルガード
                       (unless nskk-fixture--recording-p
                         (setq nskk-fixture--recording-p t)
                         (unwind-protect
                             (nskk-fixture-mock-record-call ,mock-var args result)
                           (setq nskk-fixture--recording-p nil)))
                       result)))
             ,@body)
         (fset ,function ,original-var)
         (setq nskk-fixture--active-mocks
               (delq ,function nskk-fixture--active-mocks))))))

(defmacro nskk-with-mock-functions (mocks &rest body)
  "複数の関数をモックして BODY を実行する。

MOCKS: ((function mock-fn) ...) のリスト
BODY: テスト本体

使用例:
  (nskk-with-mock-functions
      ((func1 (lambda () 1))
       (func2 (lambda () 2)))
    (should (= (func1) 1))
    (should (= (func2) 2)))"
  (declare (indent 1) (debug ((&rest (symbolp form)) body)))
  (let ((mock-vars nil)
        (original-vars nil)
        (mock-setups nil)
        (mock-cleanups nil))
    (dolist (mock mocks)
      (let ((func (car mock))
            (mock-fn (cadr mock))
            (mock-var (make-symbol (format "mock-%s" (car mock))))
            (original-var (make-symbol (format "original-%s" (car mock)))))
        (push `(,mock-var (nskk-fixture-create-mock ',func ,mock-fn)) mock-vars)
        (push `(,original-var (symbol-function ',func)) original-vars)
        (push `(progn
                 (push ',func nskk-fixture--active-mocks)
                 (fset ',func
                       (lambda (&rest args)
                         (let ((result (apply (nskk-mock-behavior ,mock-var) args)))
                           (nskk-fixture-mock-record-call ,mock-var args result)
                           result))))
              mock-setups)
        (push `(progn
                 (fset ',func ,original-var)
                 (setq nskk-fixture--active-mocks
                       (delq ',func nskk-fixture--active-mocks)))
              mock-cleanups)))
    `(let (,@(reverse mock-vars) ,@(reverse original-vars))
       (unwind-protect
           (progn
             ,@(reverse mock-setups)
             ,@body)
         ,@(reverse mock-cleanups)))))

;;; スタブ関数

(defmacro nskk-with-stub (function return-value &rest body)
  "FUNCTION を常に RETURN-VALUE を返すスタブに置き換えて BODY を実行する。

FUNCTION: スタブ対象の関数名
RETURN-VALUE: 常に返す値
BODY: テスト本体

使用例:
  (nskk-with-stub 'file-exists-p t
    (should (file-exists-p \"/nonexistent\")))"
  (declare (indent 2) (debug (symbolp form body)))
  `(nskk-with-mock-function ,function
     (lambda (&rest _args) ,return-value)
     ,@body))

(defmacro nskk-with-stubs (stubs &rest body)
  "複数の関数をスタブ化して BODY を実行する。

STUBS: ((function return-value) ...) のリスト
BODY: テスト本体"
  (declare (indent 1) (debug ((&rest (symbolp form)) body)))
  `(nskk-with-mock-functions
       ,(mapcar (lambda (stub)
                  (list (car stub)
                        `(lambda (&rest _args) ,(cadr stub))))
                stubs)
     ,@body))

;;; 呼び出し検証

(defun nskk-fixture-clear-mock-calls ()
  "モック呼び出し記録をクリアする。"
  (setq nskk-fixture--mock-calls nil))

(defun nskk-fixture-was-called-p (function)
  "FUNCTION が呼び出されたか確認する。

戻り値: 呼び出された場合 t、されていない場合 nil"
  (cl-some (lambda (call)
            (eq (car call) function))
          nskk-fixture--mock-calls))

(defun nskk-fixture-call-count (function)
  "FUNCTION の呼び出し回数を返す。

戻り値: 呼び出し回数"
  (cl-count function nskk-fixture--mock-calls
           :key #'car))

(defun nskk-fixture-get-call-args (function &optional nth)
  "FUNCTION の呼び出し引数を取得する。

NTH: 何番目の呼び出しか（0始まり、デフォルト: 0）

戻り値: 引数リスト"
  (let ((calls (cl-remove-if-not
               (lambda (call) (eq (car call) function))
               nskk-fixture--mock-calls)))
    (when calls
      (cadr (nth (or nth 0) (reverse calls))))))

(defun nskk-fixture-assert-called (function)
  "FUNCTION が呼び出されたことをアサートする。"
  (unless (nskk-fixture-was-called-p function)
    (ert-fail (list 'mock-not-called
                   :function function))))

(defun nskk-fixture-assert-called-times (function expected-count)
  "FUNCTION が EXPECTED-COUNT 回呼び出されたことをアサートする。"
  (let ((actual-count (nskk-fixture-call-count function)))
    (unless (= actual-count expected-count)
      (ert-fail (list 'mock-call-count-mismatch
                     :function function
                     :expected expected-count
                     :actual actual-count)))))

(defun nskk-fixture-assert-called-with (function &rest expected-args)
  "FUNCTION が EXPECTED-ARGS で呼び出されたことをアサートする。"
  (let ((actual-args (nskk-fixture-get-call-args function)))
    (unless (equal actual-args expected-args)
      (ert-fail (list 'mock-args-mismatch
                     :function function
                     :expected expected-args
                     :actual actual-args)))))

;;; フィクスチャ管理

(cl-defstruct (nskk-fixture
               (:constructor nskk-fixture--create)
               (:copier nil))
  "フィクスチャオブジェクト。

スロット:
  name     - フィクスチャ名
  setup    - セットアップ関数
  teardown - ティアダウン関数
  data     - フィクスチャデータ"
  (name nil :type symbol)
  (setup nil :type (or null function))
  (teardown nil :type (or null function))
  (data nil))

(defvar nskk-fixture--registry (make-hash-table :test 'eq)
  "登録されたフィクスチャのレジストリ。")

(defun nskk-fixture-register (name setup &optional teardown)
  "フィクスチャを登録する。

NAME: フィクスチャ名
SETUP: セットアップ関数（データを返す）
TEARDOWN: ティアダウン関数（省略可）

戻り値: フィクスチャオブジェクト"
  (let ((fixture (nskk-fixture--create
                 :name name
                 :setup setup
                 :teardown teardown)))
    (puthash name fixture nskk-fixture--registry)
    fixture))

(defun nskk-fixture-get (name)
  "登録されたフィクスチャを取得する。

NAME: フィクスチャ名

戻り値: フィクスチャオブジェクト"
  (gethash name nskk-fixture--registry))

(defmacro nskk-with-fixture (name &rest body)
  "フィクスチャ NAME をセットアップして BODY を実行する。

NAME: フィクスチャ名
BODY: テスト本体

使用例:
  (nskk-fixture-register 'my-fixture
    (lambda () (list :data \"test\")))

  (nskk-with-fixture my-fixture
    (should (equal fixture-data (list :data \"test\"))))"
  (declare (indent 1) (debug (symbolp body)))
  `(let* ((fixture-obj (nskk-fixture-get ',name))
          (fixture-data (when (nskk-fixture-setup fixture-obj)
                         (funcall (nskk-fixture-setup fixture-obj)))))
     (unwind-protect
         (progn ,@body)
       (when (nskk-fixture-teardown fixture-obj)
         (funcall (nskk-fixture-teardown fixture-obj) fixture-data)))))

;;; 標準フィクスチャ

(nskk-fixture-register 'empty-buffer
  (lambda ()
    (generate-new-buffer " *nskk-test-empty*"))
  (lambda (buf)
    (when (buffer-live-p buf)
      (kill-buffer buf))))

(nskk-fixture-register 'simple-state
  (lambda ()
    (nskk-fixture-create-state :mode 'hiragana)))

(nskk-fixture-register 'dict-entries
  (lambda ()
    (nskk-fixture-create-dict-entries 10)))

;;; ヘルパー関数

(defun nskk-fixture-active-mocks ()
  "現在アクティブなモックのリストを返す。"
  nskk-fixture--active-mocks)

(defun nskk-fixture-mock-calls-summary ()
  "モック呼び出しの要約を返す。

戻り値: ((function-name . call-count) ...) の alist"
  (let ((summary (make-hash-table :test 'eq)))
    (dolist (call nskk-fixture--mock-calls)
      (let ((fn (car call)))
        (puthash fn (1+ (gethash fn summary 0)) summary)))
    (let ((result nil))
      (maphash (lambda (k v) (push (cons k v) result)) summary)
      (sort result (lambda (a b) (> (cdr a) (cdr b)))))))

(provide 'nskk-test-fixtures)

;;; nskk-test-fixtures.el ends here
